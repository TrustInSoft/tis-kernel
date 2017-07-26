(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp

let register_builtin = Builtins.register_builtin
let dkey = Value_parameters.register_category "imprecision"

(** Strongly set the C variable errno if it is declared and
    if the received value is nonzero *)

let optionally_set_errno errno state =
  if Ival.is_zero errno
  then state
  else
    let errno = Ival.diff_if_one errno Ival.zero in
    try
      let scope = Cil_types.VGlobal in
      let errno_var = Globals.Vars.find_from_astinfo "__FC_errno" scope in
      let errno_loc = Locations.loc_of_varinfo errno_var in
      Eval_op.add_binding ~exact:true ~with_alarms:CilE.warn_none_mode
        state errno_loc (Cvalue.V.inject_ival errno)
    with _ -> (* FIXME *)
      state


(*  Helper functions for detection of overlap of memory locations *)

type overlap_status_t = Overlap | Separated | MayOverlap
exception Overlap_status_uncertain

let overlap_status_loc_bits ?(size_in_bytes = false)
    loc1 size_loc1 loc2 size_loc2 =
  (* This function checks for overlaps in two zones defined by a Location_Bits
   * and a size in bits (or bytes)  *)

  let bit_units =  if size_in_bytes then Bit_utils.sizeofchar() else Int.one in

  try (
    let min1,max1 = Ival.min_and_max size_loc1 in
    let min2,max2 = Ival.min_and_max size_loc2 in

    let get_int_base i =
      let v = match i with
        |Some m ->  m
        |None -> raise Overlap_status_uncertain
      in
      let v = Int.mul bit_units v in
      let v = Int_Base.inject v in
      v
    in

    let max1_bits = get_int_base max1  in
    let max2_bits = get_int_base max2  in
    let min1_bits = get_int_base min1  in
    let min2_bits = get_int_base min2  in

    let is_zero n = Int_Base.equal Int_Base.zero n in

    if is_zero max1_bits || is_zero max2_bits
    then
      Separated
    else
      begin
        (* check separation in worst-case scenario to guarantee no overlap *)
        let location1_max = Locations.make_loc loc1 max1_bits in
        let location2_max = Locations.make_loc loc2 max2_bits in

        let loc1_zone_max =
          Locations.enumerate_valid_bits ~for_writing:false location1_max
        in
        let loc2_zone_max =
          Locations.enumerate_valid_bits ~for_writing:true location2_max
        in

        if not (Locations.Zone.valid_intersects loc1_zone_max loc2_zone_max)
        then (* Separation is certain *)
          Separated
        else
          begin
            (* We cannot guarantee separation. We try to guarantee
               overlap. If anything goes wrong, it means we can't and therefore
               the status is MayOverlap *)

            (* if one of the locations has more than one base, we cannot
               guarantee overlap. The Not_found exception will be caught *)
            let _, offsets1 = Locations.Location_Bits.find_lonely_key loc1 in
            let _, offsets2 = Locations.Location_Bits.find_lonely_key loc2 in

            (* from now on we can suppose that each location has only one base,
             * and it must be the same for both locations as otherwise we would
             * have already returned Separated *)

            let min_offsets1, max_offsets1 = Ival.min_and_max offsets1 in
            let min_offsets2, max_offsets2 = Ival.min_and_max offsets2 in

            let min_offsets1 = match min_offsets1 with
              |None -> Int.zero | Some m -> Int.max m Int.zero in
            let min_offsets2 = match min_offsets2 with
              |None -> Int.zero | Some m -> Int.max m Int.zero in

            let max_offsets1 = match max_offsets1 with
              |None -> Int.zero | Some m -> Int.max m Int.zero in
            let max_offsets2 = match max_offsets2 with
              |None -> Int.zero | Some m -> Int.max m Int.zero in

            let size1 = Int_Base.project min1_bits in
            let size2 = Int_Base.project min2_bits in

            (* (leq3 a b c) is true iff  a <= b <= c *)
            let leq3 a b c = (Int.le a b) && (Int.le b c) in

            if (
              leq3 min_offsets1 max_offsets2 (Int.add min_offsets1 size1)
              ||leq3 max_offsets2 min_offsets1 (Int.add max_offsets2 size2)
            ) &&
              ( leq3 min_offsets2 max_offsets1 (Int.add min_offsets2 size2)
                ||leq3 max_offsets1 min_offsets2 (Int.add max_offsets1 size1)
              )
            then
              Overlap
            else
              MayOverlap
          end;
      end
  ) with
  (* from find_lonely_key or find_lonely_binding *)
  | Not_found | Overlap_status_uncertain -> MayOverlap

(* Checks overlap status of two loc_bytes *)
let overlap_status_loc_bytes loc1 size1 loc2 size2 =
  (* sizes should be in bytes *)
  let loc1_bits = Locations.loc_bytes_to_loc_bits loc1
  and loc2_bits = Locations.loc_bytes_to_loc_bits loc2
  in

  overlap_status_loc_bits ~size_in_bytes:true loc1_bits size1 loc2_bits size2

(* Checks overlap status of two locations (which can have different sizes) *)
let overlap_status_loc loc1 loc2 =
  let size1 = Ival.inject_singleton (Int_Base.project (Locations.loc_size loc1))
  in
  let size2 = Ival.inject_singleton (Int_Base.project (Locations.loc_size loc2))
  in
  let loc1bits =
    Locations.loc_bytes_to_loc_bits (Locations.loc_to_loc_without_size loc1)
  in
  let loc2bits =
    Locations.loc_bytes_to_loc_bits (Locations.loc_to_loc_without_size loc2)
  in

  overlap_status_loc_bits loc1bits size1 loc2bits size2

(** Returns a location of size sizeof_loc
    (the size is {Bit_utils.sizeofchar ()} by default).*)
let location_of_cvalue ?(sizeof_loc=(Bit_utils.sizeofchar ())) cvalue =
  Locations.make_loc
    (Locations.loc_bytes_to_loc_bits cvalue)
    (Int_Base.inject sizeof_loc)

let additional_ptr_validity_check_for_size_zero ~for_writing ~size (exp, cvalue, _) =
  let location = location_of_cvalue cvalue in
  if Cvalue.V.contains_zero size &&
     not (Locations.is_valid ~for_writing location) then begin
    Value_parameters.warning ~current:true
      "@[invalid pointer %a.@ assert(%a is a valid pointer for %s)@]%t"
      Cvalue.V.pretty cvalue
      Cil_printer.pp_exp exp
      (if for_writing then "writing" else "reading")
      Value_util.pp_callstack
  end

module StringAndArrayUtilities = struct

  (* The pseudo-infinity length for string operations. *)
  let str_infinity = Integer.two_power_of_int 120

  (* Character Strings / Wide Character Strings *)
  type libc_character_kind =
    | Character
    | WideCharacter

  let get_character_bits string_kind =
    let char_typ =
      match string_kind with
      | Character     -> Cil.charType
      | WideCharacter -> Cil.(theMachine.wcharType)
    in
    Int_Base.project (Bit_utils.sizeof char_typ)

  (* TODO: Required for abstract_strcpy and abstract_strcmp
           to avoid a forward reference. *)
  module Strlen = struct

    type strlen_alarm_context = {
      strlen_alarm_invalid_string : unit -> unit
    }

    type abstract_strlen_type =
      (character_bits:Integer.t ->
       max:Abstract_interp.Int.t ->
       emit_alarm:strlen_alarm_context ->
       Locations.Location_Bytes.t -> Cvalue.Model.t -> Ival.t)

    let abstract_strlen_ref : abstract_strlen_type ref =
      ref (fun ~character_bits ~max ~emit_alarm str state ->
          ignore(character_bits, max, emit_alarm, str, state);
          assert false)

    let get_abstract_strlen () = !abstract_strlen_ref

  end

end

(*
  Local Variables:
  compile-command: "make -C ../../../../.."
  End:
*)
