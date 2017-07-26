(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype
open Cil_types
open Sa_helpers
open Sa_types

let with_alarms = CilE.warn_none_mode

module type FunctionCallHandler = sig
  val handle :
    Lmap_pointer_aliasing.t -> Db.Value.state -> Cil_datatype.Lval.t option ->
    Cil_types.exp -> Cil_types.exp list -> Lmap_pointer_aliasing.t
end

let ival_of_expr state expr =
  let loc = !Db.Value.eval_expr ~with_alarms state expr in
  try Cvalue.V.project_ival loc
  with Cvalue.V.Not_based_on_null -> Ival.top

let get_byte_size i =
  Int_Base.inject (Integer.mul (Bit_utils.sizeofchar ()) i)

let paste_effective_type lmap size dest ety =
  match Ival.min_and_max size with
  | None, None | None, Some _ | Some _, None ->
    assert false
  | Some imin, Some imax ->
    let write_part ~exact lmap size bytes =
      let dest_zone =
        let bits = Locations.loc_bytes_to_loc_bits bytes in
        let location = Locations.make_loc bits (get_byte_size size) in
        Locations.enumerate_valid_bits ~for_writing:true location
      in
      Lmap_pointer_aliasing.add_binding ~reducing:true ~exact lmap dest_zone ety
    in
    let lmap = write_part ~exact:true lmap imin dest in
    let unexact_part =
      Locations.Location_Bytes.shift
        (Ival.inject_singleton imin)
        dest
    in
    write_part ~exact:false lmap (Integer.sub imax imin) unexact_part

module MemcpyHandler = struct
  (* This is a little bit nasty, but this function ensures that EVERY type
     is correctly given to the destination.
     Otherwise, static types (that are not stored in the [lmap]) won't appear
     in the destination. *)
  let make_fake_static lmap zone =
    let old = Lmap_pointer_aliasing.find lmap zone in
    let lmap' =
      Locations.Zone.fold_bases
        (fun base acc ->
           let location = Locations.loc_of_base base in
           let typ = Sa_types.effective_type_of_location location in
           let lmap' = lmap_from_empty zone typ in
           Lmap_pointer_aliasing.join acc lmap')
        zone lmap
    in
    old, lmap'

  let handle lmap state _lval_opt _fname formals =
    match formals with
    | [dest; src; size] ->
      let src_bytes = !Db.Value.eval_expr ~with_alarms state src in
      let src_zone = ZoneExpr.eval_static ~for_writing:false state src in
      let old_typ, lmap' = make_fake_static lmap src_zone in
      let dest_bytes = !Db.Value.eval_expr ~with_alarms state dest in
      let size = ival_of_expr state size in
      let lmap' =
        Lmap_pointer_aliasing.copy_paste
          lmap' src_bytes dest_bytes size
      in
      lmap_replace lmap' src_zone old_typ
    | _ ->
      Sa_parameters.fatal ~current:true "wrong argument list for memcpy"
end

module FreadHandler = struct
  let handle lmap state _lval_opt _fname formals =
    match formals with
    | [dest; size; nmemb; _file] ->
      let size =
        let elt_size = ival_of_expr state size in
        let nmemb = ival_of_expr state nmemb in
        Ival.mul elt_size nmemb
      in
      let dest_bytes = !Db.Value.eval_expr ~with_alarms state dest in
      paste_effective_type lmap size dest_bytes Sa_types.et_first_access
    | _ ->
      Sa_parameters.fatal ~current:true "wrong argument list for fread"
end

module ReadHandler = struct
  let handle lmap state _lval_opt _fname formals =
    match formals with
    | [_fd; dest; count] ->
      let size = ival_of_expr state count in
      let dest_bytes = !Db.Value.eval_expr ~with_alarms state dest in
      paste_effective_type lmap size dest_bytes Sa_types.et_first_access
    | _ ->
      Sa_parameters.fatal ~current:true "wrong argument list for read"
end

module MemsetHandler = struct
  let handle lmap state _lval_opt _fname formals =
    match formals with
    | [dest; _byte; size] ->
      let size = ival_of_expr state size in
      let dest_bytes = !Db.Value.eval_expr ~with_alarms state dest in
      paste_effective_type lmap size dest_bytes Sa_types.et_first_access
    | _ ->
      Sa_parameters.fatal ~current:true "wrong argument list for memset"
end

module NoActionHandler = struct
  let handle lmap _state _lval_opt _fname _formals = lmap
end

module Builtins = struct
  let level = 0
  let current = true
  let dump fmt = Sa_parameters.debug ~level ~current fmt

  let is_show_each = ( = ) "tis_sa_show_each"

  module ShowEach = struct
    let dump_expression lmap state e =
      let zone = match e.enode with
        | Lval lval -> ZoneLval.eval ~for_writing:false state lval
        | _ -> ZoneExpr.eval ~for_writing:false state e
      in
      let ety  = Lmap_pointer_aliasing.find lmap zone in
      dump "`%a' ==> %a@\n" Exp.pretty e EffectiveType.pretty ety

    let handle lmap state _lval_opt _fname formals =
      dump "===== POINTER ALIASING SHOW EACH =====@\n";
      List.iter (dump_expression lmap state) formals;
      dump "======================================@\n@\n";
      lmap
  end

  let is_dump_each = ( = ) "tis_sa_dump_each"

  module DumpEach = struct
    let dump_ety zone ety () =
      dump "`%a' ==> %a@\n" Locations.Zone.pretty zone EffectiveType.pretty ety

    let handle lmap _state _lval_opt _fname _formals =
      let open Lmap_pointer_aliasing in
      dump "===== POINTER ALIASING ANALYSIS STATE =====@\n";
      begin match lmap with
        | Top -> dump "TOP@\n"
        | Bottom -> dump "BOTTOM@\n"
        | Map map -> fold dump_ety map ()
      end;
      dump "===========================================@\n@\n";
      lmap
  end

  let is_sa_builtin n = is_show_each n || is_dump_each n

  let is_sa_builtin_kf kf = is_sa_builtin (Kernel_function.get_name kf)

  let is_sa_builtin_exp e =
    match Kernel_function.get_called e with
    | None -> false
    | Some kf -> is_sa_builtin_kf kf
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
