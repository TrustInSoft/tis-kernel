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
open Queue


type word =  {
  validity : Abstract_interp.Int.t ;
  mutable chars_to_locate : Ival.t
}

type alarm_type =
    Char_to_locate | Current_char | String | Out_of_bounds | Uninitialized

module Aux = Builtins_lib_tis_aux
let register_builtin = Builtins.register_builtin
let dkey = Value_parameters.register_category "imprecision"

open Aux.StringAndArrayUtilities

exception Abort_to_top
exception Extlib_the_exn

let abstract_memchr ~min_len ~max_len ~emit_alarm str chr state =
  (* Checks whether the call to memchr would cause undefined behavior:
     Calls emit_alarm if anything at all is wrong. *)

  Value_parameters.debug "string offsetmap is %a"
    Location_Bytes.pretty str;

  let not_yet_null = ref true in (* true until we add NULL to return addr *)

  (* list of min_len word of each offsetmap which are still relevant *)
  let short_words = Queue.create () in
  let all_possible_addr = ref Cvalue.V.bottom in
  let null_pointer = Cvalue.V.singleton_zero  in

  (* Remove short_words with validity shorter than current position.
   * If chars_to_locate of the removed word is not empty, add
   * NULL to addr *)
  let remove_expired_words words curr_addr i ?(out_of_bounds = false) () =
    try
      while
        Ival.is_bottom (Queue.peek words).chars_to_locate
        || Int.lt (Queue.peek words).validity !i
      do
        let  curr_word = Queue.pop words in
        if not (Ival.is_bottom curr_word.chars_to_locate)
        && Int.lt curr_word.validity !i && not out_of_bounds && !not_yet_null
        then begin
          curr_addr := Cvalue.V.join !curr_addr null_pointer;
          not_yet_null:= false;
        end;
      done
    with Empty -> ()
  in

  let value =
    try
      begin
        match str with
        | Location_Bytes.Top(_,_) ->
          (* The correct results are an offset of str or 0, all of which are
           * already contained in str *)
          emit_alarm ();
          str
        | Location_Bytes.Map m ->
          let do_one_offsetmap base ival =
            (* Algorithm: start from smallest ival. For each ival, the minimum
               lenght passed to memchr (min_len) defines the validity
               of the min_len_word.  We keep a list of these words, their
               validity and the chars we are still looking for in each one,
               as we move to other ivals.
               At each offset we check if there are short_words which have
               validity smaller than current offset AND whose char_to_locate
               list is not empty. If we find any, then we add NULL to the
               possible return addr and STOP LOOKING at short_words.
               The validity of max_len_word is defined as 'ival + max_len'. We
               don't keep track of all the max_len_words, but only look at the
               most recent, since we would add an address only if the
               max_len_word in the most recent ival contains the corresponding
               char in its char_to_locate list.

               **NOTE: the algorithm might have to keep track of many
               short_words for different offsets and thus run inefficiently in
               some cases until returning the first NULL. Afterwards, it will
               only take into account one max_len_word, thus being a lot faster.
            *)
            Value_parameters.debug "Base %a" Base.pretty base;
            Value_parameters.debug "Ival is %a" Ival.pretty ival;

            match Cvalue.Model.find_base_or_default base state with
            | `Bottom -> emit_alarm(); Cvalue.V.bottom
            | `Top -> raise Abort_to_top
            | `Map offsetmap ->
              Value_parameters.debug "omap is %a"
                Cvalue.V_Offsetmap.pretty offsetmap;

              let validity = Base.validity base in
              let reduced_ival =
                Locations.reduce_offset_by_validity ~for_writing:false
                  base ival (Int_Base.inject Int.eight)
              in
              let ival = reduced_ival in
              let min =
                match Ival.min_int ival with
                | None -> assert false (* TODO *)
                | Some m -> m
              in
              let i = ref min in

              (* biggest chunk in offsetmap *)
              let max_len_word = ref {
                  validity = Int.add min (Int.sub max_len Int.one);
                  chars_to_locate = chr
                } in
              (* smallest chunk in offsetmap *)
              let min_len_word = {
                validity = Int.add min (Int.sub min_len Int.one);
                chars_to_locate = chr
              } in
              Queue.push min_len_word short_words;

              if not (Ival.equal ival reduced_ival) then emit_alarm ();


              let continue = ref true in

              (* return NULL if maxlength is zero *)
              if max_len = Int.zero then begin
                continue := false;
                all_possible_addr := null_pointer
              end;

              let rec move_to_next_offset () =

                if !not_yet_null
                then begin
                  let succ = ref (Int.succ !i) in
                  remove_expired_words short_words all_possible_addr succ
                    ~out_of_bounds:false ();
                end;

                let remaining =
                  Ival.narrow ival
                    (Ival.inject_range (Some (Int.succ !i)) None)
                in

                if Ival.is_bottom remaining then continue := false
                else begin
                  let next = Extlib.the (Ival.min_int remaining) in
                  i := next;

                  (* make max_len_word relative to new offset and put back all
                     the initial chars *)
                  max_len_word := {
                    validity = Int.add !i (Int.sub max_len Int.one);
                    chars_to_locate = chr
                  };

                  (* add min_len_word for new offset to list *)
                  let min_len_word = {
                    validity = Int.add !i (Int.sub min_len Int.one);
                    chars_to_locate = chr
                  } in
                  Queue.push min_len_word short_words ;

                  if (Int.lt !max_len_word.validity !i)
                  then
                    begin
                      move_to_next_offset ();
                    end
                end;
              in

              while !continue do
                let offset = Ival.inject_singleton (Int.mul Int.eight !i) in
                let alarm, current_char =
                  Cvalue.V_Offsetmap.find
                    ~validity ~offsets:offset ~size:Int.eight offsetmap
                in
                (* alarms *)
                if alarm
                || Cvalue.V_Or_Uninitialized.is_indeterminate current_char
                then begin
                  emit_alarm ();
                  Queue.clear short_words;
                end;

                (* get current char *)
                let current_char, current_addr =
                  let current_char =
                    Cvalue.V_Or_Uninitialized.get_v current_char
                  in
                  Cvalue.V.split Base.null current_char
                in

                if not (Cvalue.V.is_bottom current_addr)
                then emit_alarm ();

                (* we are not allowed to read current char in memory *)
                if Ival.is_bottom current_char
                then begin
                  emit_alarm ();
                  Queue.clear short_words;
                  move_to_next_offset ();
                end;

                if !not_yet_null
                then
                  remove_expired_words short_words all_possible_addr i
                    ~out_of_bounds:false ();

                let contains_chr =
                  Ival.intersects current_char !max_len_word.chars_to_locate
                in

                if contains_chr
                then begin
                  let loc =
                    Locations.Location_Bytes.inject base
                      (Ival.scale_div ~pos:true Int.eight offset)
                  in
                  all_possible_addr := Cvalue.V.join !all_possible_addr loc;

                  (* if current_char = {'x'} then we have found 'x' and remove
                   * it from max_len_word and all short words also! *)
                  if Ival.is_singleton_int current_char
                  then begin
                    let remove_char c word =
                      word.chars_to_locate <-
                        Ival.diff_if_one word.chars_to_locate c
                    in
                    remove_char current_char !max_len_word;
                    Queue.iter (remove_char current_char) short_words;
                  end;
                end; (* end if contains_char *)

                let no_chars_left =
                  Ival.is_bottom !max_len_word.chars_to_locate
                in

                (* if char_to_locate = {} or we are past validity
                   then we move to next offset *)
                if no_chars_left || (Int.le !max_len_word.validity  !i)
                then begin
                  move_to_next_offset ()
                end
                else begin
                  i := Int.succ !i;

                  if Ival.is_included (Ival.inject_singleton !i) ival
                  (* we crossed to next offset *)
                  then begin
                    if !not_yet_null
                    then begin
                      remove_expired_words short_words all_possible_addr i
                        ~out_of_bounds:false ();

                      (* add min_len_word for new offset to list *)
                      let min_len_word = {
                        validity = Int.add !i (Int.sub min_len Int.one);
                        chars_to_locate = chr
                      } in
                      Queue.push min_len_word short_words ;

                    end;

                    (* make max_len_word relative to new offset and
                       put back all the initial chars *)
                    max_len_word := {
                      validity = Int.add !i (Int.sub max_len Int.one);
                      chars_to_locate = chr
                    };

                  end; (* end of crossed to next offset *)
                end;
              done;

              if !not_yet_null
              then
                begin
                  i := Int.succ !i;
                  remove_expired_words short_words all_possible_addr i
                    ~out_of_bounds:true ();
                end;

              !all_possible_addr

          in
          let accumulate_one_offsetmap base ival acc =
            Cvalue.V.join (do_one_offsetmap base ival) acc
          in
          let computed_memchr =
            Cvalue.V.M.fold accumulate_one_offsetmap m Cvalue.V.bottom
          in
          computed_memchr
      end
    with
    | Abort_to_top -> Cvalue.V.top
  in
  value

let tis_memchr state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin memchr(%a)%t"
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [ (_, bytes, _) as actual; (_, chr, _); (_, n, _) ] ->

    Aux.additional_ptr_validity_check_for_size_zero
      ~for_writing:false ~size:n actual;

    let chr, chr_addr_component = Cvalue.V.split Base.null chr in
    if not (Cvalue.V.is_bottom chr_addr_component)
    then Value_parameters.warning ~once:true ~current:true
        "assert(no address in second argument of memchr)";

    let n, n_addr_component = Cvalue.V.split Base.null n in
    if not (Cvalue.V.is_bottom n_addr_component)
    then Value_parameters.warning ~once:true ~current:true
        "assert(no address in third argument of memchr)";

    let max_len = match Ival.max_int n with Some m -> m | None -> str_infinity in
    let min_len = match Ival.min_int n with Some m -> m | None -> Int.zero in

    let not_yet = ref true in
    let emit_alarm () =
      (* TODO : write a specific alarm type and an ACSL predicate.
         Note: this would need modifications to valarms.ml *)
      if !not_yet
      then begin
        not_yet := false;
        Value_parameters.feedback ~current:true
          "warning: assert preconditions for function memchr in call to \
           builtin memchr(%a)%t"
          pretty_actuals actuals Value_util.pp_callstack;
      end
    in

    let value =
      abstract_memchr ~min_len ~max_len ~emit_alarm bytes chr state
    in
    if Cvalue.V.is_bottom value
    then
      { Value_types.c_values = [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    else
      { Value_types.c_values = [ Value_types.StateOnly(Eval_op.wrap_ptr value, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted

let () = register_builtin "tis_memchr" tis_memchr

(*
  Local Variables:
  compile-command: "make -C ../../../../.."
  End:
*)
