(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cvalue
open Abstract_interp
open Locations
open Value_util

(* define helper functions and values*)
module Aux = Builtins_lib_tis_aux
let register_builtin = Aux.register_builtin
let dkey = Aux.dkey

(*  Implementation of [memset] that accepts imprecise arguments. Assumes
    the syntactic context is positioned. *)
let tis_memset_imprecise state dst v size =
  let size_char = Bit_utils.sizeofchar () in
  let with_alarms = warn_all_quiet_mode () in
  let size_min, size_max_bytes =
    try
      let size = Cvalue.V.project_ival size in
      let min,max = Ival.min_and_max size in
      let min = match min with
        | None -> Int.zero
        | Some m -> Int.mul size_char (Int.max m Int.zero)
      and max = match max with
        | None -> Bit_utils.max_bit_address ()
        | Some m -> m
      in min, max
    with V.Not_based_on_null -> Int.zero, Bit_utils.max_bit_address ()
  in
  let left = loc_bytes_to_loc_bits dst in
  (* Write [v] everywhere that might be written, ie between
     [dst] and [dst+size-1]. *)
  let new_state =
    if Int.gt size_max_bytes Int.zero then
      let shift =
        Ival.inject_range (Some Int.zero) (Some (Int.pred size_max_bytes))
      in
      let loc = Location_Bytes.shift shift dst in
      let loc = loc_bytes_to_loc_bits loc in
      let loc = make_loc loc (Int_Base.inject size_char) in
      Eval_op.add_binding ~with_alarms
        ~remove_invalid:true ~exact:false state loc v
    else state
  in
  (* Write "sure" bytes in an exact way: they exist only if there is only
     one base, and within it, size_min+leftmost_loc > rightmost_loc *)
  let new_state' =
    try
      let base, offset = Location_Bits.find_lonely_key left in
      let minb, maxb = match Ival.min_and_max offset with
        | Some minb, Some maxb -> minb, maxb
        | _ -> raise Not_found
      in
      let sure = Int.sub (Int.add minb size_min) maxb in
      if Int.gt sure Int.zero then
        let left' = Location_Bits.inject base (Ival.inject_singleton maxb) in
        let vuninit = V_Or_Uninitialized.initialized v in
        let from = V_Offsetmap.create ~size:sure vuninit ~size_v:size_char in
        Eval_op.paste_offsetmap ~with_alarms ~remove_invalid:true
          ~reducing:false ~from ~dst_loc:left' ~size:sure ~exact:true new_state
      else
        new_state
    with Not_found -> new_state (* from find_lonely_key + explicit raise *)
  in
  { Value_types.c_values = [  Value_types.StateOnly(Eval_op.wrap_ptr dst, new_state') ];
    c_clobbered = Base.SetLattice.bottom;
    c_cacheable = Value_types.Cacheable;
    c_from = None; (* TODO?*)
    c_sureouts = None;
  }

let tis_memset state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin memset(%a)%t"
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [(exp_dst, dst, _) as actual; (_exp_v, v, _); (exp_size, size, _)] ->
    begin
      Aux.additional_ptr_validity_check_for_size_zero ~for_writing:true ~size actual;
      (* Position syntactic context *)
      let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
      let array_dst = Logic_utils.array_with_range exp_dst term_size in
      Valarms.set_syntactic_context (Valarms.SyMemLogic array_dst);
      (* Keep only the first byte of the argument *)
      let _, v = Cvalue.V.extract_bits
          ~topify:Origin.K_Misalign_read
          ~start:Int.zero ~stop:(Int.pred (Bit_utils.sizeofchar ()))
          ~size:(Int.of_int (Cil.bitsSizeOfInt IInt))
          v
      in
      tis_memset_imprecise state dst v size
    end
  | _ ->
    Value_parameters.result
      "Invalid call to tis_memset builtin %a%t"
      pretty_actuals actuals Value_util.pp_callstack;
    raise Db.Value.Aborted

let () = register_builtin "tis_memset" tis_memset
