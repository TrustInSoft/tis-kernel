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

(* helper functions and values *)
module Aux = Builtins_lib_tis_aux
let register_builtin = Aux.register_builtin

open Aux.StringAndArrayUtilities

(* strlen builtin *)
exception Abort_to_top

(* TODO: Required for abstract_strcpy and abstract_strcmp
         to avoid a forward reference. *)
open Strlen

let abstract_strlen ~character_bits ~max ~(emit_alarm : strlen_alarm_context) str state =
  (* Checks whether the call to strlen would provoke a rte:
     Calls emit_alarm (e.g. to emit \valid_string(...)) if anything at all
     is wrong.
     Returns top_int in most complicated cases
     (e.g. too many offsets to consider)
  *)
  try
    match str with
    | Location_Bytes.Top(_,_) ->
      emit_alarm.strlen_alarm_invalid_string ();
      Ival.top
    | Location_Bytes.Map _ when
        (* TODO: fix temporaire pour limiter la complexité.
           L'algorithme peut encore être lent car il lit chaque caractère,
           retirer ce cas quand l'algorithme aura été optimisé *)
        (try ignore (Location_Bytes.cardinal_less_than str 100_000); false
         with Abstract_interp.Not_less_than -> true) ->
      emit_alarm.strlen_alarm_invalid_string ();
      Ival.top
    | Location_Bytes.Map m ->
      let range = Ival.inject_range None (Some max) in
      let character_bits_base = Int_Base.inject character_bits in
      let do_one_offsetmap base ival =
        (* Algorithm:
           start from min of ival,
           look successively at each char
           a possible zero char means the string can end here
              -> accumulate current_len into all_possible_len
           add zero to current_len when crossing start of next ival value.
           If bottoming, start again from next ival value.
        *)
        match Cvalue.Model.find_base_or_default base state with
        | `Bottom -> emit_alarm.strlen_alarm_invalid_string (); Ival.bottom
        | `Top -> raise Abort_to_top
        | `Map offsetmap ->
          let validity = Base.validity base in
          let reduced_ival =
            Locations.reduce_offset_by_validity ~for_writing:false
              base ival character_bits_base
          in
          if not (Ival.equal ival reduced_ival)
          then emit_alarm.strlen_alarm_invalid_string ();
          let ival = reduced_ival in
          let min =
            match Ival.min_int ival with
            | None -> assert false (* TODO *)
            | Some m -> m
          in
          let i = ref min in
          let current_len = ref Ival.zero in
          let all_possible_len = ref Ival.bottom in
          let continue = ref true in
          let move_to_next_offset add_current =
            (* passer a la valeur suivante dans ival *)
            let remaining =
              Ival.narrow ival
                (Ival.inject_range (Some (Int.succ !i)) None)
            in
            if Ival.is_bottom remaining
            then continue := false
            else
              let next = Extlib.the (Ival.min_int remaining) in
              if add_current then
                all_possible_len := Ival.join !all_possible_len !current_len;
              current_len := Ival.zero;
              i := next
          in
          while !continue do
            let alarm, char =
              Cvalue.V_Offsetmap.find
                ~validity
                ~offsets:(Ival.inject_singleton (Int.mul character_bits !i))
                ~size:character_bits
                offsetmap
            in
            if alarm then emit_alarm.strlen_alarm_invalid_string ();
            if Cvalue.V_Or_Uninitialized.is_indeterminate char
            then emit_alarm.strlen_alarm_invalid_string ();
            let char = Cvalue.V_Or_Uninitialized.get_v char in
            let char, address =
              Cvalue.V.split Base.null char
            in
            if not (Cvalue.V.is_bottom address)
            then emit_alarm.strlen_alarm_invalid_string ();

            let contain_zero = Ival.intersects char Ival.zero in
            if contain_zero
            then
              all_possible_len := Ival.join !all_possible_len !current_len;

            if Ival.is_included char Ival.zero
            then move_to_next_offset contain_zero
            else begin
              i := Int.succ !i;
              let new_current_len = Ival.add_int Ival.one !current_len in
              let reduced_current_len = Ival.narrow new_current_len range in
              if not (Ival.equal reduced_current_len new_current_len)
              then
                all_possible_len :=
                  Ival.join !all_possible_len (Ival.inject_singleton max);

              let final_current_len =
                if Ival.is_included (Ival.inject_singleton !i) ival
                then  Ival.join Ival.zero reduced_current_len
                else reduced_current_len
              in
              if Ival.is_bottom final_current_len
              then move_to_next_offset false
              else current_len := final_current_len;
            end
          done;
          !all_possible_len

      in
      let accumulate_one_offsetmap base ival acc =
        Ival.join acc (do_one_offsetmap base ival)
      in
      let computed_strlen =
        Cvalue.V.M.fold accumulate_one_offsetmap m Ival.bottom
      in
      Value_parameters.debug " size is %a" Ival.pretty computed_strlen ;
      computed_strlen
  with
  | Abort_to_top ->
    emit_alarm.strlen_alarm_invalid_string ();
    Ival.top

(* Implements built-ins:
   - strlen
   - wcslen *)
let tis_strlen ~str_or_wcs state actuals =
  let builtin_name =
    match str_or_wcs with
    | Character     -> "strlen"
    | WideCharacter -> "wcslen"
  in
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      builtin_name pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [ exp_str, str, _ ] ->
    let emit_alarm =
      let notyet_invalid_string = ref true in
      let emit_alarm_invalid_string () =
        if !notyet_invalid_string
        then begin
          notyet_invalid_string := false;
          Valarms.set_syntactic_context (Valarms.SyUnOp exp_str) ;
          Valarms.warn_valid_string (warn_all_quiet_mode ())
        end
      in
      {strlen_alarm_invalid_string = emit_alarm_invalid_string}
    in
    let character_bits = get_character_bits str_or_wcs in
    let value =
      abstract_strlen ~character_bits ~max:str_infinity ~emit_alarm str state
    in
    if Ival.is_bottom value
    then
      { Value_types.c_values = [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    else
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_size_t (V.inject_ival value), state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ -> raise Db.Value.Aborted

let () = register_builtin "tis_strlen" (tis_strlen ~str_or_wcs:Character)
let () = register_builtin "tis_wcslen" (tis_strlen ~str_or_wcs:WideCharacter)

(* Implements built-ins:
   - strnlen
   - wcsnlen *)
let tis_strnlen ~str_or_wcs state actuals =
  let builtin_name =
    match str_or_wcs with
    | Character     -> "strnlen"
    | WideCharacter -> "wcsnlen"
  in
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      builtin_name pretty_actuals actuals Value_util.pp_callstack;
  let character_bits = get_character_bits str_or_wcs in
  let emit_alarm =
    let notyet_invalid_string = ref true in
    let emit_alarm_invalid_string () =
      if !notyet_invalid_string
      then begin
        notyet_invalid_string := false;
        Value_parameters.warning ~once:true ~current:true "assert (string valid up to n)";
      end
    in
    {strlen_alarm_invalid_string = emit_alarm_invalid_string}
  in
  match actuals with
  | [ (_exp_str, str, _) as actual; (_, size, _) ] ->
    let n, n_addr_component = Cvalue.V.split Base.null size in
    if not (Cvalue.V.is_bottom n_addr_component)
    then
      Value_parameters.warning ~once:true ~current:true
        "assert(no address in second argument of strnlen)";
    let max = match Ival.max_int n with Some m -> m | None -> str_infinity in
    Aux.additional_ptr_validity_check_for_size_zero ~for_writing:false ~size actual;
    let value = abstract_strlen ~character_bits ~max ~emit_alarm str state in
    if Ival.is_bottom value
    then
      { Value_types.c_values = [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    else
      let rangevalue =
        Ival.inject_range (Some Int.zero) (Ival.max_int value)
      in
      (* add in [value] all possible values of [n]
         which are less than the greater value of [value] *)
      let value = Ival.join value (Ival.narrow rangevalue n) in
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_size_t (V.inject_ival value), state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted

let () = register_builtin "tis_strnlen" (tis_strnlen ~str_or_wcs:Character)
let () = register_builtin "tis_wcsnlen" (tis_strnlen ~str_or_wcs:WideCharacter)

let () = abstract_strlen_ref := abstract_strlen

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
