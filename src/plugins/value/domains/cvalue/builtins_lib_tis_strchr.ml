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
open Locations
open Value_util

(* strchr builtin *)
exception Abort_to_top

module Aux = Builtins_lib_tis_aux
open Aux.StringAndArrayUtilities

(* TODO: Required for abstract_strcpy to avoid a forward reference. *)
open Strlen

type strchr_alarm_context = {
  strchr_alarm_invalid_string : unit -> unit
}

let abstract_strchr
    ~(emit_alarm : strchr_alarm_context)
    ~character_bits
    str
    initial_chr_to_locate
    state =
  (* Checks whether the call to strchr would cause undefined behavior:
     Calls emit_alarm (e.g. to emit \valid_string(...)) if anything at all
     is wrong.
     Returns top_int in most complicated cases
     (e.g. too many offsets to consider) *)

  try begin
    match str with
    | Location_Bytes.Top(_,_) ->
      emit_alarm.strchr_alarm_invalid_string ();
      (* The correct results are an offset of str or 0, all of which are
       * already contained in str *)
      str
    | Location_Bytes.Map m ->
      let null_present_in_result = ref false in
      let do_one_offsetmap base ival =
        (* Algorithm:
           start from min of ival, look successively at each char
           -> if the char may contain one we are looking for,
           then accumulate current address into current_possible_addr
           -> if the current char is a singleton, then we are sure we got a hit,
           and we remove this char from char_to_locate.
           -> a possible zero char means the string can end here.
           This validates the candidates in current_possible_addr as
           belonging to a well-formed string, so we copy current_possible_addr
           into all_possible_addr.
           Note that NULL can be an answer, unless we are specifically looking
           for '\0' in which case we accumulate the current address.
           -> when crossing to next offset, we start looking again
           for all chars in the original set
           If bottoming, start again from next offset.
           ** When the set of chars to find is empty, we continue to search
           for a terminating '\0' in order to verify that the string is
           well formed.
           If it is not, the addresses will not be added **
        *)

        match Cvalue.Model.find_base_or_default base state with
        | `Bottom -> emit_alarm.strchr_alarm_invalid_string (); Ival.bottom
        | `Top -> raise Abort_to_top
        | `Map offsetmap ->
          let validity = Base.validity base in
          let reduced_ival =
            Locations.reduce_offset_by_validity ~for_writing:false
              base ival (Int_Base.inject character_bits)
          in
          if not (Ival.equal ival reduced_ival)
          then emit_alarm.strchr_alarm_invalid_string ();
          let ival = reduced_ival in
          let min =
            match Ival.min_int ival with
            | None -> assert false (* TODO *)
            | Some m -> m
          in
          let i = ref min in
          let char_to_locate = ref initial_chr_to_locate in
          let all_possible_addr = ref Ival.bottom in
          let current_possible_addr = ref Ival.bottom in
          let continue = ref true in
          let move_to_next_offset ~add_current =
            let remaining =
              Ival.narrow ival
                (Ival.inject_range (Some (Int.succ !i)) None)
            in
            char_to_locate := initial_chr_to_locate;
            if Ival.is_bottom remaining
            then continue := false
            else
              let next = Extlib.the (Ival.min_int remaining) in
              if add_current
              then
                (* add_stored addresses and start again from empty set *)
                all_possible_addr :=
                  Ival.join !all_possible_addr !current_possible_addr;

              current_possible_addr := Ival.bottom;
              i := next
          in

          while !continue do
            let offset = Ival.inject_singleton (Int.mul character_bits !i) in
            let alarm, current_char =
              Cvalue.V_Offsetmap.find
                ~validity ~offsets:offset ~size:character_bits
                offsetmap
            in
            if alarm then emit_alarm.strchr_alarm_invalid_string ();

            if Cvalue.V_Or_Uninitialized.is_indeterminate current_char
            then emit_alarm.strchr_alarm_invalid_string ();

            if not (Cvalue.V_Or_Uninitialized.is_initialized current_char)
            then emit_alarm.strchr_alarm_invalid_string ();


            let current_char = Cvalue.V_Or_Uninitialized.get_v current_char in
            let current_char, current_addr =
              Cvalue.V.split Base.null current_char
            in
            if not (Cvalue.V.is_bottom current_addr)
            then emit_alarm.strchr_alarm_invalid_string ();

            if Ival.is_bottom current_char
            then begin
              move_to_next_offset ~add_current:false;
            end
            else begin
              (* critical part of the algorithm *)
              let contains_chr = Ival.intersects current_char !char_to_locate
              in

              if contains_chr
              then current_possible_addr :=
                  Ival.join !current_possible_addr offset;

              let may_terminate_at_this_char =
                Ival.intersects current_char Ival.zero in

              (* if current_shar can be '\0',
                 then the string *might* be well terminated *)
              if may_terminate_at_this_char
              then begin
                all_possible_addr :=
                  Ival.join !current_possible_addr !all_possible_addr;

                (*  add NULL to result unless
                    1- we had found all the searched characters and
                    we were just checking for the validity of the string
                    (char_to_locate bottom), or
                    2- we were looking for '\0'. *)
                if not (Ival.is_included !char_to_locate Ival.zero)
                then null_present_in_result := true;
              end;

              (* if current_char = {'x'} then we have found 'x' and remove it
                 from the set of char_to_locate *)
              if Ival.is_singleton_int current_char
              then begin
                char_to_locate :=
                  Ival.diff_if_one !char_to_locate current_char;
              end;

              let normal_end =
                (Ival.is_bottom !char_to_locate)
                || (Ival.is_zero current_char)
              in

              if normal_end && may_terminate_at_this_char then begin
                (* and we put back all the initial chars in char_to_locate *)
                move_to_next_offset ~add_current:true
              end
              else begin
                let ii = Int.succ !i in
                i := ii;
                if Ival.is_included (Ival.inject_singleton ii) ival
                (* we crossed to next offset *)
                then char_to_locate := initial_chr_to_locate
              end
            end
          done;

          !all_possible_addr

      in
      let accumulate_one_offsetmap base ival acc =
        let offsets = do_one_offsetmap base ival in
        let char_bits = Bit_utils.sizeofchar () in (* CHECK! Is this right? *)
        let locs = Locations.Location_Bytes.inject base
            (Ival.scale_div ~pos:true char_bits offsets)
        in
        Cvalue.V.join acc locs
      in
      let computed_strchr =
        Cvalue.V.M.fold accumulate_one_offsetmap m Cvalue.V.bottom
      in
      if !null_present_in_result then
        Cvalue.V.join Cvalue.V.singleton_zero computed_strchr
      else
        computed_strchr
  end
  with Abort_to_top -> Cvalue.V.top

(* Implements built-ins:
   - strchr
   - wcschr *)
let tis_strchr ~str_or_wcs state actuals =
  let builtin_name =
    match str_or_wcs with
    | Character     -> "strchr"
    | WideCharacter -> "wcschr"
  in
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      builtin_name pretty_actuals actuals Value_util.pp_callstack;

  match actuals with
  | [ (exp_str, str, _) ; (exp_chr, chr, _) ] ->
    let chr, chr_addr_component =
      Cvalue.V.split Base.null chr
    in
    if not (Cvalue.V.is_bottom chr_addr_component)
    then
      Value_parameters.warning ~once:true ~current:true
        "assert(no address in second argument of %s)" builtin_name;
    let emit_alarm =
      let notyet_invalid_string = ref true in
      let emit_alarm_invalid_string () =
        if !notyet_invalid_string
        then begin
          notyet_invalid_string := true;
          Valarms.set_syntactic_context (Valarms.SyUnOp exp_str) ;
          Valarms.warn_valid_string (warn_all_quiet_mode ());
          Valarms.set_syntactic_context (Valarms.SyUnOp exp_chr) ;
        end
      in
      {strchr_alarm_invalid_string = emit_alarm_invalid_string}
    in
    let character_bits = get_character_bits str_or_wcs in
    let value =
      abstract_strchr ~character_bits ~emit_alarm str chr state
    in
    if Cvalue.V.is_bottom value
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
          [ Value_types.StateOnly(Eval_op.wrap_ptr value, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted


let () =
  Builtins.register_builtin "tis_strchr" (tis_strchr ~str_or_wcs:Character)
let () =
  Builtins.register_builtin "tis_wcschr" (tis_strchr ~str_or_wcs:WideCharacter)

(* strcpy also placed in this file because it is used for strcat
   (also in this file) *)

type memcpy_alarm_context = Builtins_lib_tis_memcpy.memcpy_alarm_context

type strcpy_alarm_context = {
  strcpy_alarm_strlen_alarm_context : strlen_alarm_context;
  strcpy_alarm_invalid_string : Cvalue.V.t -> unit;
  strcpy_alarm_memcpy_alarm_context : memcpy_alarm_context;
  strcpy_alarm_invalid_copy   : Cvalue.V.t -> Cvalue.V.t -> unit
}

let abstract_strcpy ~character_bits ~(emit_alarm : strcpy_alarm_context)
    (dst : Location_Bits.t) (src : Location_Bits.t) (state : Cvalue.Model.t) =

  (* 1. Reduce the source location to only valid string addresses,
        get each string's possible sizes. *)

  let valid_strings_with_length : (Location_Bits.t * Ival.t) list =
    let handle_one_input_string singleton_str valid_strings_with_length_acc =
      let singleton_str_bytes =
        Locations.loc_bits_to_loc_bytes singleton_str in
      (* Check if the string is valid and
         get its length using "abstract_strlen". *)
      let str_length : Ival.t =
        (* No max string size. *)
        let max = str_infinity in
        let abstract_strlen = get_abstract_strlen () in
        let emit_alarm = emit_alarm.strcpy_alarm_strlen_alarm_context in
        abstract_strlen ~character_bits ~max ~emit_alarm
          singleton_str_bytes state
      in
      (* Proceed with the string or skip it. *)
      if Ival.is_bottom str_length
      then begin
        (* This string is invalid, emit a warning. *)
        emit_alarm.strcpy_alarm_invalid_string singleton_str_bytes;
        (* Skip the string. *)
        valid_strings_with_length_acc
      end
      else begin
        (* Valid string: add it to the list together with the length info *)
        (singleton_str, str_length) :: valid_strings_with_length_acc
      end
    in
    try
      Location_Bits.fold_enum handle_one_input_string src []
    with Location_Bits.Error_Top ->
      raise Db.Value.Aborted
  in

  (* 2. Use memcpy to copy the source string to the destination location. *)

  let dst_bytes = Locations.loc_bits_to_loc_bytes dst in
  (* Check if the destination is a singleton location or not. *)
  let is_destination_precise =
    (* TODO: assert not bottom ? *)
    Location_Bits.cardinal_zero_or_one dst
  in
  (* Flag to indicate if there was any source string which was correctly
     copied to the destination. *)
  let any_successful_memcpy = ref false in
  (* Copy each source to each destination when it is possible. *)
  let new_state =
    List.fold_left (fun state (src, src_length) ->
        let src_bytes = Locations.loc_bits_to_loc_bytes src in
        (* If the destination is precise, then the first successful update
           should be strong, as we want to erase what was before at the
           destination location. *)
        let exact = is_destination_precise && (not !any_successful_memcpy) in
        (* The actual number of characters to copy is one character bigger than
           the length of the string: we take into account the 0 at the end of
           the string. *)
        Value_parameters.debug "original length: %a" Ival.pretty src_length;
        let size = Ival.add_int src_length Ival.one in
        Value_parameters.debug "updated  length: %a" Ival.pretty size;
        (* Perform this source to the destination using memcpy. *)
        let (state_after_memcpy : Cvalue.Model.t), _c_from, _sure_zone =
          let abstract_memcpy = Builtins_lib_tis_memcpy.abstract_memcpy in
          let emit_alarm = emit_alarm.strcpy_alarm_memcpy_alarm_context in
          abstract_memcpy ~exact ~emit_alarm ~character_bits ~size
            src_bytes dst_bytes state
        in
        (* Was this source copied correctly? *)
        (* TODO: Is this the right way of checking? *)
        (* PC: it is nonsense. the world is not made only of successful copies
           and copies that returns unreachable states. *)
        let was_memcpy_successful =
          Cvalue.Model.is_reachable state_after_memcpy
        in
        if was_memcpy_successful
        then begin
          (* The copy succeeded: set the flag. *)
          any_successful_memcpy := true;
          (* Continue with the state with this source string copied. *)
          state_after_memcpy
        end else begin
          (* There was no way to correctly copy this source string. *)
          (* Emit a warning. *)
          emit_alarm.strcpy_alarm_invalid_copy src_bytes dst_bytes;
          (* Skip it! *)
          state
        end
      ) state valid_strings_with_length
  in
  (* If there was at least one case when the source string was correctly
     copied, then the new state is correct. Otherwise the state becomes
     bottom. *)
  if !any_successful_memcpy
  then new_state
  else Cvalue.Model.bottom

(* Implements built-ins:
   - strcpy
   - wcscpy *)
let tis_strcpy ~str_or_wcs state actuals =
  let builtin_name =
    match str_or_wcs with
    | Character     -> "strcpy"
    | WideCharacter -> "wcscpy"
  in
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      builtin_name pretty_actuals actuals Value_util.pp_callstack;

  match actuals with
  | [ (_exp_dst, dst, offsm_dst) ; (_exp_src, src, _) ] ->
    let notyet_invalid_string = ref true in
    let emit_alarm_invalid_string () =
      if !notyet_invalid_string
      then begin
        notyet_invalid_string := true;
        Value_parameters.warning ~current:true ~once:true
          "%s: assert(arguments must be compatible)" builtin_name
      end
    in
    let emit_alarm = {
      strcpy_alarm_strlen_alarm_context = {
        strlen_alarm_invalid_string = emit_alarm_invalid_string
      };
      strcpy_alarm_invalid_string = (fun singleton_str_bytes ->
          Value_parameters.warning ~current:true
            "the string pointed by %a,@ passed as@ the source argument@ to the \
             %s function,@ is invalid;@ assert %s arguments are valid"
            Cvalue.V.pretty singleton_str_bytes
            builtin_name builtin_name);
      strcpy_alarm_memcpy_alarm_context =
        Builtins_lib_tis_memcpy.memcpy_alarm_context_none;
      strcpy_alarm_invalid_copy = (fun src_bytes dst_bytes ->
          Value_parameters.warning ~current:true
            "the string pointed by %a,@ passed as@ the source argument@ to the \
             %s function,@ could not be correctly copied to the destination \
             %a;@ assert the string pointed by the %s src argument@ can be \
             correctly copied@ to the location@ pointed by the dst argument"
            Cvalue.V.pretty src_bytes
            builtin_name
            Cvalue.V.pretty dst_bytes
            builtin_name)
    }
    in
    let character_bits = get_character_bits str_or_wcs in
    let state =
      let dst = loc_bytes_to_loc_bits dst in
      let src = loc_bytes_to_loc_bits src in
      abstract_strcpy ~character_bits ~emit_alarm dst src state
    in
    if Cvalue.Model.is_reachable state
    then
      { Value_types.c_values = [ Value_types.StateOnly(Some offsm_dst, state) ];
        c_clobbered = Location_Bytes.get_bases dst;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    else
      { Value_types.c_values =
          [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted

let () =
  Builtins.register_builtin "tis_strcpy" (tis_strcpy ~str_or_wcs:Character)
let () =
  Builtins.register_builtin "tis_wcscpy" (tis_strcpy ~str_or_wcs:WideCharacter)

type strcat_alarm_context = {
  strcat_alarm_strchr_alarm_context : strchr_alarm_context;
  strcat_alarm_strcpy_alarm_context : strcpy_alarm_context;
}

let abstract_strcat
    ~(character_bits : Integer.t)
    ~(emit_alarm : strcat_alarm_context)
    (str1 : Location_Bits.t)
    (str2 : Location_Bits.t)
    (state : Cvalue.Model.t) =

  (* Use strchr to get a pointer to the zero byte terminating the
     first string. *)
  let str1_zero_terminator_ptr =
    let zero_char = Ival.zero in
    let str = loc_bits_to_loc_bytes str1 in
    let emit_alarm = emit_alarm.strcat_alarm_strchr_alarm_context in
    abstract_strchr ~character_bits ~emit_alarm str zero_char state
  in

  (* Use strcpy to append the second string to the end of the first string. *)
  if Cvalue.V.is_bottom str1_zero_terminator_ptr
  then Cvalue.Model.bottom
  else
    let dst = loc_bytes_to_loc_bits str1_zero_terminator_ptr in
    let src = str2 in
    let emit_alarm = emit_alarm.strcat_alarm_strcpy_alarm_context in
    abstract_strcpy ~character_bits ~emit_alarm dst src state

(* Implements built-ins:
   - strcat
   - wcscat *)
let tis_strcat ~str_or_wcs state actuals =
  let builtin_name =
    match str_or_wcs with
    | Character     -> "strcat"
    | WideCharacter -> "wcscat"
  in
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      builtin_name pretty_actuals actuals Value_util.pp_callstack;

  match actuals with
  | [ (exp_str1, str1, offsm_str1) ; (_exp_str2, str2, _) ] ->
    let notyet_1st_string_invalid = ref true in
    let emit_alarm =
      let emit_alarm_1st_string_invalid () =
        if !notyet_1st_string_invalid
        then begin
          notyet_1st_string_invalid := true;
          Valarms.set_syntactic_context (Valarms.SyUnOp exp_str1);
          Valarms.warn_valid_string (warn_all_quiet_mode ());
        end
      in
      {
        strcat_alarm_strchr_alarm_context = {
          strchr_alarm_invalid_string = emit_alarm_1st_string_invalid
        };
        strcat_alarm_strcpy_alarm_context = {
          strcpy_alarm_strlen_alarm_context = {
            strlen_alarm_invalid_string = emit_alarm_1st_string_invalid
          };
          strcpy_alarm_invalid_string = (fun singleton_str_bytes ->
              Value_parameters.warning ~current:true
                "the string pointed by %a,@ passed as@ the second argument@ to \
                 the %s function,@ is invalid;@ assert %s arguments are valid"
                Cvalue.V.pretty singleton_str_bytes
                builtin_name builtin_name);
          strcpy_alarm_memcpy_alarm_context =
            Builtins_lib_tis_memcpy.memcpy_alarm_context_none;
          strcpy_alarm_invalid_copy =
            (fun src_bytes dst_bytes ->
               Value_parameters.warning ~current:true
                 "the string pointed by %a,@ passed as@ the second argument@ \
                  to the %s function,@ could not be correctly appended to \
                  the string %a,@ passed as the first argument;@ assert \
                  the string pointed by the %s second argument@ can be \
                  correctly appended@ to the string pointed by@ its \
                  first argument"
                 Cvalue.V.pretty src_bytes
                 builtin_name
                 Cvalue.V.pretty dst_bytes
                 builtin_name)
        }
      }
    in
    let character_bits = get_character_bits str_or_wcs in
    let state =
      let emit_alarm = emit_alarm in
      let str1 = loc_bytes_to_loc_bits str1 in
      let str2 = loc_bytes_to_loc_bits str2 in
      abstract_strcat ~character_bits ~emit_alarm str1 str2 state
    in
    if Cvalue.Model.is_reachable state
    then
      { Value_types.c_values =
          [ Value_types.StateOnly(Some offsm_str1, state) ];
        c_clobbered = Location_Bytes.get_bases str1;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    else
      { Value_types.c_values =
          [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted

let () =
  Builtins.register_builtin "tis_strcat" (tis_strcat ~str_or_wcs:Character)
let () =
  Builtins.register_builtin "tis_wcscat" (tis_strcat ~str_or_wcs:WideCharacter)

(*
  Local Variables:
  compile-command: "make -C ../../../../.."
  End:
*)
