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
open Locations
open Sa_types


(* Shortcuts to defined functions in the Lmap_pointer_aliasing module. *)

let lmap_find lmap zone =
  Lmap_pointer_aliasing.find lmap zone

let lmap_replace ?(exact=true) ?(reducing=true) lmap zone et =
  Lmap_pointer_aliasing.(add_binding ~exact ~reducing lmap zone et)

let lmap_from_empty ?(exact=true) ?(reducing=true) zone et =
  lmap_replace ~exact ~reducing Lmap_pointer_aliasing.empty zone et

let lmap_replace_offset lmap base_src base_dest =
  match lmap with
  | Lmap_pointer_aliasing.Map map ->
    Lmap_pointer_aliasing.fold_base
      (fun bsrc offset lmap ->
         if Locations.Zone.mem_base bsrc base_src
         then
           Locations.Zone.fold_bases
             (fun bdst lmap -> Lmap_pointer_aliasing.add_base bdst offset lmap)
             base_dest
             lmap
         else lmap)
      map
      lmap
  | _ -> lmap


(* Typing functions *)

let rec typeOf_expression ~keep_cast expr =
  match expr.enode with
  | CastE (typ, expr') when not keep_cast && Cil.isPointerType typ ->
    typeOf_expression ~keep_cast expr'
  | _ -> Cil.typeOf expr

let is_unwanted_cast typ =
  Cil.isPointerType typ || Cil.isFunctionType typ

let rec remove_pointer_casts expr = match expr.enode with
  | CastE (typ, expr') when is_unwanted_cast typ -> remove_pointer_casts expr'
  | _ -> expr

let is_null expression =
  let typ = Cil.(unrollType (typeOf expression)) in
  Cil.isPointerType typ && Cil.isZero (remove_pointer_casts expression)

let complete_typing ~keep_cast state e =
  let typ = typeOf_expression ~keep_cast e in
  let ety =
    if is_null e then bottom
    else effective_type_of_expression ~keep_cast state e
  in
  (typ, ety)

let complete_typing_lval state lval =
  let typ = Cil.typeOfLval lval in
  let ety = effective_type_of_lvalue state lval in
  (typ, ety)


(* Expression function *)

let rec extract_pointer_variable expr =
  match expr.enode with
  | Lval (Var _, _ as lval) when Cil.(isPointerType (typeOfLval lval)) ->
    Some lval
  | Info (e, _) -> extract_pointer_variable e
  | _ -> None


(* Translation functions *)

let int_base_of_int i =
  Int_Base.inject (Integer.of_int i)


(* Zone helpers *)

let with_alarms = CilE.warn_none_mode

module type Stategy = sig
  val eval : ?off:Ival.t -> for_writing:bool -> Base.t -> Ival.t ->
    Int_Base.t -> Zone.t
end

module BitsStrategy = struct
  let eval ?(off = Ival.zero) ~for_writing base ival size =
    let ival = Ival.add_int ival off in
    let loc = Locations.make_loc (Location_Bits.inject base ival) size in
    enumerate_valid_bits ~for_writing loc
end

module BytesStrategy = struct
  let eval ?(off = Ival.zero) ~for_writing base ival size =
    let loc_bytes = Location_Bytes.inject base (Ival.add_int ival off) in
    let loc_bits = Locations.loc_bytes_to_loc_bits loc_bytes in
    let loc = Locations.make_loc loc_bits size in
    enumerate_valid_bits ~for_writing loc
end

module EvalLocation(L : module type of Location_Bytes)(S : Stategy) = struct
  let split_static_and_allocated ?(off = Ival.zero) ~for_writing loc loc_size =
    match loc with
    | L.Top _ -> Zone.top, Zone.top
    | L.Map map ->
      L.M.fold
        (fun base ival (static, allocated) ->
           (* get the zone for every character, not only the first one. *)
           let loc_size = match base with
             | Base.String _ -> Base.bits_sizeof base
             | _ -> loc_size
           in
           let zone = S.eval ~off ~for_writing base ival loc_size in
           match base with
           | Base.Allocated _ -> (static, Zone.join allocated zone)
           | _ -> (Zone.join static zone, allocated))
        map
        Zone.(bottom, bottom)

  let get_zone ?(top=Zone.top) ?(base=Zone.bottom) ~for_writing loc loc_size =
    match loc with
    | L.Top _ -> top
    | L.Map map ->
      L.M.fold
        (fun base ival acc ->
           Zone.join acc (S.eval ~for_writing base ival loc_size))
        map
        base
end

module EvalBits = EvalLocation(Location_Bits)(BitsStrategy)
module EvalBytes = EvalLocation(Location_Bytes)(BytesStrategy)

module type EvalZone = sig
  type t
  val eval : for_writing:bool -> Db.Value.state -> t -> Zone.t
  val eval_static : for_writing:bool -> Db.Value.state -> t -> Zone.t
  val eval_allocated : for_writing:bool -> Db.Value.state -> t -> Zone.t
end

module ZoneLval = struct
  let eval ~for_writing state lval =
    let loc = !Db.Value.lval_to_loc_state state lval in
    enumerate_valid_bits ~for_writing loc

  let eval_separated ?(off=Ival.zero) ~for_writing state lval =
    let loc = !Db.Value.lval_to_loc_state state lval in
    EvalBits.split_static_and_allocated ~off ~for_writing loc.loc loc.size

  let eval_static ~for_writing state t =
    fst (eval_separated ~for_writing state t)

  let eval_allocated ~for_writing state t =
    snd (eval_separated ~for_writing state t)
end

module ZoneExpr = struct
  let eval ~for_writing state e =
    let lvals = !Db.Value.find_lv_plus state e in
    List.fold_left
      (fun base (lval, _) ->
         let location = !Db.Value.lval_to_loc_state state lval in
         EvalBits.get_zone ~base ~for_writing location.loc location.size)
      Zone.bottom
      lvals

  let eval_separated ?(off=Ival.zero) ~for_writing state e =
    let model = !Db.Value.eval_expr ~with_alarms state e in
    let typ = Cil.(typeOf_pointed (unrollType (typeOf e))) in
    let size = int_base_of_int (Cil.bitsSizeOf typ) in
    EvalBytes.split_static_and_allocated ~off ~for_writing model size

  let eval_static ~for_writing state t =
    fst (eval_separated ~for_writing state t)

  let eval_allocated ~for_writing state t =
    snd (eval_separated ~for_writing state t)
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
