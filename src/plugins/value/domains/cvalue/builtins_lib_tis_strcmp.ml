(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cvalue
open Abstract_interp
open Locations
open Value_util

(* define helper functions *)
module Aux = Builtins_lib_tis_aux
let register_builtin = Aux.register_builtin

open Aux.StringAndArrayUtilities

(* TODO: Required for abstract_strcmp to avoid a forward reference. *)
open Strlen

(* str(n)cmp builtins *)

type strcmp_alarm_context = {
  strcmp_alarm_invalid_string : Cil_types.exp -> unit;
}

let abstract_strcmp ~character_bits ~signed ~min ~max
    ~(emit_alarm : strcmp_alarm_context) str1 syn1 str2 syn2 state =
  (* Checks whether the call to strcmp respects the preconditions:
     Calls emit_alarm syn? (e.g. to emit \valid_string(...))
     if anything at all is wrong.
     Returns top_int in most complicated cases
     (e.g. too many offsets to consider)
  *)
  let plevel = Value_parameters.ArrayPrecisionLevel.get() in
  let make_with_alarms r =
    let emit = { CilE.a_log = false; CilE.a_call = r } in
    { CilE.imprecision_tracing = CilE.a_ignore;
      defined_logic = emit;
      unspecified =   emit;
      others =        emit;
    }
  in
  ( match str1, str2 with
    | Location_Bytes.Top _, _ | _, Location_Bytes.Top _  ->
      emit_alarm.strcmp_alarm_invalid_string syn1;
      emit_alarm.strcmp_alarm_invalid_string syn2;
      Ival.join Ival.minus_one Ival.zero_or_one
    | Location_Bytes.Map _, Location_Bytes.Map _ ->
      let negative_res = ref false in
      let positive_res = ref false in
      let zero_res = ref false in
      let min = ref min in
      let max = ref max in
      let warn1 = ref false in
      let warn2 = ref false in
      let set_warn1 () = warn1 := true in
      let set_warn2 () = warn2 := true in
      let with_alarms1 = make_with_alarms set_warn1 in
      let with_alarms2 = make_with_alarms set_warn2 in
      let locb1 = ref (Locations.loc_bytes_to_loc_bits str1) in
      let locb2 = ref (Locations.loc_bytes_to_loc_bits str2) in
      let size_size = Int_Base.inject character_bits in
      let size_ival = Ival.inject_singleton character_bits in
      while Int.gt !max Int.zero do
        let read_char_and_filter_0 warn with_alarms lobc
            (chars_acc, locb_acc as acc) =
          let loc = Locations.make_loc lobc size_size in
          let char = Eval_op.find ~with_alarms state loc in
          let char, address =
            Cvalue.V.split Base.null char
          in
          if not (Cvalue.V.is_bottom address) then warn := true;
          if Ival.is_bottom char
          then acc
          else begin
            let char = Ival.cast ~size:character_bits ~signed ~value:char in
            Ival.join char chars_acc,
            if Ival.is_zero char
            then locb_acc
            else
              Location_Bits.join loc.Locations.loc locb_acc
          end
        in
        let acc = Ival.bottom, Location_Bits.bottom in
        let chars1, new_locb1 =
          try
            ignore (Location_Bits.cardinal_less_than !locb1 plevel);
            Location_Bits.fold_enum
              (read_char_and_filter_0 warn1 with_alarms1)
              !locb1
              acc
          with Abstract_interp.Not_less_than ->
            read_char_and_filter_0 warn1 with_alarms1 !locb1 acc
        in
        let chars2, new_locb2 =
          try
            ignore (Location_Bits.cardinal_less_than !locb2 plevel);
            Location_Bits.fold_enum
              (read_char_and_filter_0 warn2 with_alarms2)
              !locb2
              acc
          with Abstract_interp.Not_less_than ->
            read_char_and_filter_0 warn2 with_alarms2 !locb2 acc
        in
        (*      Format.printf "iteration %a %a %a %a %B %B@."
                Ival.pretty chars1 Ival.pretty chars2
                Location_Bits.pretty new_locb1
                Location_Bits.pretty new_locb2
                !warn1 !warn2; *)

        (* Prepare abstract_strlen with simplified parameters. *)
        let abstract_strlen ~emit_alarm =
          let emit_alarm = {strlen_alarm_invalid_string = emit_alarm} in
          (get_abstract_strlen ()) ~emit_alarm
        in

        if Ival.is_bottom chars1
        then begin
          (* alarm already scheduled for str1, but what about str2? *)
          if not !warn2
          then ignore(abstract_strlen ~character_bits ~max:!max
                        ~emit_alarm:set_warn2 str2 state);
          max := Int.minus_one (* exit loop immediately *)
        end
        else if Ival.is_bottom chars2
        then begin
          if not !warn1
          then ignore(abstract_strlen ~character_bits ~max:!max
                        ~emit_alarm:set_warn1 str1 state);
          max := Int.minus_one (* exit loop immediately *)
        end
        else
          let min1, max1 = Ival.min_and_max chars1 in
          let min2, max2 = Ival.min_and_max chars2 in
          if Ival.compare_min_max min1 max2 < 0 then negative_res := true;
          if Ival.compare_min_max min2 max1 < 0 then positive_res := true;
          (*        Format.printf "iteration--- pos:%B zero:%B neg:%B@."
                    !positive_res !zero_res !negative_res; *)
          if not (Ival.intersects chars1 chars2)
          then begin
            (* check for well-formedness of the strings now since we
               aren't going to finish reading them *)
            (let max = !max in
             if not !warn1
             then ignore(abstract_strlen ~character_bits ~max
                           ~emit_alarm:set_warn1 str1 state);
             if not !warn2
             then ignore(abstract_strlen ~character_bits ~max
                           ~emit_alarm:set_warn2 str2 state));
            max := Int.minus_one (* exit loop immediately *)
          end
          else begin
            if Ival.contains_zero chars1 && Ival.contains_zero chars2
            then zero_res := true;

            if Location_Bits.is_bottom new_locb1
            then begin
              if not !warn2
              then ignore(abstract_strlen ~character_bits ~max:!max
                            ~emit_alarm:set_warn2 str2 state);
              max := Int.minus_one (* exit loop immediately *)
            end
            else if Location_Bits.is_bottom new_locb2
            then begin
              if not !warn1
              then ignore(abstract_strlen ~character_bits ~max:!max
                            ~emit_alarm:set_warn1 str1 state);
              max := Int.minus_one (* exit loop immediately *)
            end
            else begin
              (* continue the loop; prepare next iteration *)
              locb1 := Location_Bits.shift size_ival new_locb1;
              locb2 := Location_Bits.shift size_ival new_locb2;
              max := Int.pred !max;
              min := Int.pred !min;
            end
          end;
      done;
      if !warn1 then emit_alarm.strcmp_alarm_invalid_string syn1;
      if !warn2 then emit_alarm.strcmp_alarm_invalid_string syn2;
      let res =
        if !zero_res || Int.le !min Int.zero
        then Ival.zero
        else Ival.bottom
      in
      let res = if !positive_res then Ival.join Ival.one res else res in
      if !negative_res then Ival.join Ival.minus_one res else res)

let tis_strcmp ~str_or_wcs state actuals =
  let name, signed, character_bits =
    match str_or_wcs with
    | Character ->
      let name = "strcmp" in
      let signed = false (* C11 7.24.4:1 *) in
      let character_bits = Bit_utils.sizeofchar () in
      (name, signed, character_bits)
    | WideCharacter ->
      let name = "wcscmp" in
      let wchar = Cil.theMachine.Cil.wcharType in
      let signed = Cil.isSignedInteger wchar in
      let character_bits = Int.of_int (Cil.bitsSizeOf wchar) in
      Value_parameters.warning ~once:true
        "wcscmp builtin untested and known to emit the wrong alarm";
      (name, signed, character_bits)
  in
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      name
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [ exp1, str1, _; exp2, str2, _ ] ->
    let emit_alarm =
      let emit_alarm_invalid_string str =
        Valarms.set_syntactic_context (Valarms.SyUnOp str) ;
        Valarms.warn_valid_string (warn_all_quiet_mode ())
      in
      {strcmp_alarm_invalid_string = emit_alarm_invalid_string}
    in
    let value =
      abstract_strcmp ~character_bits ~min:str_infinity ~max:str_infinity
        ~signed ~emit_alarm str1 exp1 str2 exp2 state
    in
    if Ival.is_bottom value
    then
      { Value_types.c_values =
          [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    else
      { Value_types.c_values =
          [ Value_types.StateOnly
              (Eval_op.wrap_int (V.inject_ival value), state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted

let () = register_builtin "tis_strcmp" (tis_strcmp ~str_or_wcs:Character)
let () = register_builtin "tis_wcscmp" (tis_strcmp ~str_or_wcs:WideCharacter)

let tis_strncmp ~str_or_wcs state actuals =
  let name, signed, character_bits =
    match str_or_wcs with
    | Character ->
      let name = "strncmp" in
      let signed = false (* C11 7.24.4:1 *) in
      let character_bits = Bit_utils.sizeofchar () in
      (name, signed, character_bits)
    | WideCharacter ->
      let name = "wcsncmp" in
      let wchar = Cil.theMachine.Cil.wcharType in
      let signed = Cil.isSignedInteger wchar in
      let character_bits = Int.of_int (Cil.bitsSizeOf wchar) in
      Value_parameters.warning ~once:true
        "wcsncmp builtin untested and known to emit the wrong alarm";
      (name, signed, character_bits)
  in
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      name pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [ (exp1, str1, _) as a1; (exp2, str2, _) as a2; _, n, _ ] ->
    let emit_alarm =
      let notyet_invalid_string = ref true in
      let emit_alarm_invalid_string e =
        if !notyet_invalid_string
        then begin
          notyet_invalid_string := false;
          Value_parameters.warning ~once:true ~current:true
            "assert (string %a valid up to n)" Printer.pp_exp e;
        end
      in
      {strcmp_alarm_invalid_string = emit_alarm_invalid_string}
    in
    let n' = V.project_ival n in
    let min = Extlib.the (Ival.min_int n') in
    let max = Extlib.the (Ival.max_int n') in
    Aux.additional_ptr_validity_check_for_size_zero
      ~for_writing:false
      ~size:n
      a1;
    Aux.additional_ptr_validity_check_for_size_zero
      ~for_writing:false
      ~size:n
      a2;
    let value =
      abstract_strcmp ~character_bits ~min ~max ~emit_alarm ~signed
        str1 exp1 str2 exp2 state
    in
    if Ival.is_bottom value
    then
      { Value_types.c_values =
          [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    else
      { Value_types.c_values =
          [ Value_types.StateOnly
              (Eval_op.wrap_int (V.inject_ival value), state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted

let () = register_builtin "tis_strncmp" (tis_strncmp ~str_or_wcs:Character)
let () = register_builtin "tis_wcsncmp" (tis_strncmp ~str_or_wcs:WideCharacter)
