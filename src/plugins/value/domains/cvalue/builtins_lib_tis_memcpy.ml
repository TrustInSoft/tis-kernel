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
open Builtins_lib_tis_aux (* overlap status *)

exception Memcpy_result of
    Cvalue.Model.t * Function_Froms.froms * Zone.t (* Sureouts *)
exception Indeterminate of V_Or_Uninitialized.t
exception Do_Bottom
let msg_emitter = Lattice_messages.register "Builtinmemcpy"

module Aux = Builtins_lib_tis_aux
let register_builtin = Builtins.register_builtin
let dkey = Value_parameters.register_category "imprecision"

open Aux.StringAndArrayUtilities

(** Called by the [memcpy] builtin. Warns when the offsetmap contains
    an indterminate value, when the imprecision category is enabled *)
let memcpy_check_indeterminate_offsetmap offsm =
  if Value_parameters.is_debug_key_enabled dkey then
    try
      let aux_offset _ (v, _, _) =
        match v with
        | V_Or_Uninitialized.C_init_noesc _ -> ()
        | _ -> raise (Indeterminate v)
      in
      V_Offsetmap.iter aux_offset offsm
    with Indeterminate v ->
      Value_parameters.debug ~current:true ~dkey ~once:true
        "@[In memcpy@ builtin:@ precise@ copy of@ indeterminate@ values %a@]%t"
        V_Or_Uninitialized.pretty v Value_util.pp_callstack

type memcpy_alarm_context = {
  (* The warn mode. *)
  memcpy_alarm_warn_mode : CilE.warn_mode;
  (* Function setting the syntactic context to the source. *)
  memcpy_alarm_set_syntactic_context_array_src : unit -> unit;
  (* Function setting the syntactic context to the destination. *)
  memcpy_alarm_set_syntactic_context_array_dst : unit -> unit
}

(* Alarm context used when necessary information about source and destination
   is hard to get. *)
let memcpy_alarm_context_none = {
  memcpy_alarm_warn_mode = (CilE.warn_none_mode);
  memcpy_alarm_set_syntactic_context_array_src = (fun () -> ());
  memcpy_alarm_set_syntactic_context_array_dst = (fun () -> ())
}

(* Create a dependency [\from arg_n] where n is the nth argument of the
   currently called function. *)
let deps_nth_arg n =
  let kf =
    match Value_util.call_stack () with
    | (kf, _) :: _ -> kf
    | [] -> assert false
  in
  try
    let vi = List.nth (Kernel_function.get_formals kf) n in
    Function_Froms.Deps.add_data_dep
      Function_Froms.Deps.bottom
      (Locations.zone_of_varinfo vi)
  with Failure _ ->
    Kernel.fatal "%d arguments expected" n

let abstract_memcpy ?(exact=true) ~(character_bits : Integer.t) ~emit_alarm
    ~(size: Ival.t) (* Number of characters to copy. *)
    (src : Location_Bytes.t) (dst : Location_Bytes.t) (state : Model.t)
  : Model.t * Function_Froms.froms * Zone.t =

  let plevel = Value_parameters.ArrayPrecisionLevel.get() in
  let with_alarms = emit_alarm.memcpy_alarm_warn_mode in

  let min,max = Ival.min_and_max size in
  let min = match min with None -> Int.zero | Some m -> Int.max m Int.zero in
  let char_bits = Bit_utils.sizeofchar () in
  let size_min = Int.mul character_bits min in

  let right = loc_bytes_to_loc_bits src in
  let left = loc_bytes_to_loc_bits dst in

  let deps_return = deps_nth_arg 0 in
  let empty_cfrom =
    { Function_Froms.deps_table = Function_Froms.Memory.empty; deps_return; }
  in

  let precise_copy state =
    (* First step: copy the bytes we are sure to copy *)
    if Int.gt size_min Int.zero then begin
      emit_alarm.memcpy_alarm_set_syntactic_context_array_src ();
      match Eval_op.copy_offsetmap ~with_alarms right size_min state with
      | `Bottom -> (* Read failed. Source was invalid, but must be read, we
                      stop the analysis *)
        raise (Memcpy_result (Cvalue.Model.bottom, empty_cfrom, Zone.bottom))
      | `Map offsetmap ->
        memcpy_check_indeterminate_offsetmap offsetmap;
        (* Read succeeded. We write the result *)
        emit_alarm.memcpy_alarm_set_syntactic_context_array_dst ();
        let new_state =
          Eval_op.paste_offsetmap ~with_alarms ~remove_invalid:true
            ~reducing:false ~from:offsetmap ~dst_loc:left ~size:size_min
            ~exact:exact state
        in
        let loc_left = make_loc left (Int_Base.inject size_min) in
        let loc_right = make_loc right (Int_Base.inject size_min) in
        let deps_table, sure_zone =
          let zone_left = enumerate_valid_bits ~for_writing:true loc_left in
          let zone_right= enumerate_valid_bits ~for_writing:false loc_right in
          let deps =
            Function_Froms.Deps.add_data_dep
              Function_Froms.Deps.bottom zone_right
          in
          (* Note: actually a part may be written for sure (if the
             difference between the offsets in loc_left is smaller
             than size), but keeping it imprecise reflects more the
             imprecision of the value analysis here. *)
          let exact = Location_Bits.cardinal_zero_or_one left in
          let deps_table =
            Function_Froms.Memory.add_binding
              ~exact Function_Froms.Memory.empty zone_left deps
          in
          let sure_zone = if exact then zone_left else Zone.bottom in
          deps_table, sure_zone
        in
        new_state, deps_table, sure_zone
      | `Top ->
        Warn.warn_top ()
    end
    else
      (* Nothing certain can be copied *)
      state, Function_Froms.Memory.empty, Zone.bottom
  in
  let imprecise_copy new_state precise_deps_table sure_zone =
    (* Second step. Size is imprecise, we will now copy some bits
       that we are not sure to copy *)
    let size_min_ival = Ival.inject_singleton size_min in
    let left = Location_Bits.shift size_min_ival left in
    let right = Location_Bits.shift size_min_ival right in
    (* Size remaining to copy imprecisely *)
    let diff =
      match max with
      | Some max -> Some (Int.mul char_bits (Int.pred (Int.sub max min)))
      | None -> None
    in
    let range = Ival.inject_top (Some Int.zero) diff Int.zero char_bits in
    let size_char = Int_Base.inject char_bits in
    let loc_right = make_loc (Location_Bits.shift range right) size_char in
    let loc_left = make_loc (Location_Bits.shift range left) size_char in
    let c_from =
      let zone_right = enumerate_valid_bits ~for_writing:false loc_right in
      let zone_left = enumerate_valid_bits ~for_writing:true loc_left in
      let deps =
        Function_Froms.Deps.add_data_dep Function_Froms.Deps.bottom zone_right
      in
      let deps_table =
        Function_Froms.Memory.add_binding
          ~exact:false precise_deps_table zone_left deps
      in
      { Function_Froms.deps_table; deps_return }
    in
    try
      (* We try to iter on all the slices inside the value of slice.
         If there are more too many of them, we use a backup solution *)
      ignore (Ival.cardinal_less_than size plevel);
      let do_size s (left, right, prev_size, state) =
        let s = Int.mul character_bits s in
        let diff = Int.sub s prev_size in
        if Int.equal s size_min then
          (* occurs the very first time. This copy has already been
             performed at the beginning, skip *)
          (left, right, s, state)
        else begin
          (* Copy data between prev_size and s *)
          emit_alarm.memcpy_alarm_set_syntactic_context_array_src ();
          match Eval_op.copy_offsetmap ~with_alarms right diff state with
          | `Bottom ->
            (* This size is completely invalid. The following ones
               will also be invalid, stop now with current result *)
            raise (Memcpy_result (state, c_from, sure_zone))
          | `Top -> Warn.warn_top ();
          | `Map offsetmap ->
            memcpy_check_indeterminate_offsetmap offsetmap;
            emit_alarm.memcpy_alarm_set_syntactic_context_array_dst ();
            let new_state =
              Eval_op.paste_offsetmap ~with_alarms ~reducing:false
                ~remove_invalid:true ~from:offsetmap ~dst_loc:left
                ~size:diff ~exact:false state
            in
            if Db.Value.is_reachable new_state then
              let diffi = Ival.inject_singleton diff in
              let left = Location_Bits.shift diffi left in
              let right = Location_Bits.shift diffi right in
              (left, right, s, new_state)
            else (* As above, invalid size, this time for the destination.
                    We stop there *)
              raise (Memcpy_result (state, c_from, sure_zone))
        end
      in
      let _, _, _, state =
        Ival.fold_int do_size size (left, right, Int.zero, new_state)
      in
      raise (Memcpy_result (state, c_from, sure_zone))
    with
    | Abstract_interp.Not_less_than ->
      Lattice_messages.emit_approximation msg_emitter
        "too many different sizes for memcpy builtin (%s/%d). \
         Increase plevel to gain precision."
        (match Ival.cardinal size with
         | None -> "--"
         | Some i -> Integer.to_string i)
        plevel;
      (* Too many slices in the size. We read the entire range
         src+(size_min..size_max-1) in one step, and write the result in
         dst+(size_min..size_max-1) *)
      let diff = match max with
        | Some max -> Some (Int.mul character_bits (Int.pred (Int.sub max min)))
        | None -> None
      in
      (* By using ranges modulo character_bits, we read and write
         byte-by-byte, which can preserve some precision *)
      let range = Ival.inject_top (Some Int.zero) diff Int.zero character_bits in
      let right = Location_Bits.shift range right in
      let size_char = Int_Base.inject character_bits in
      let loc_right = make_loc right size_char in
      let left = Location_Bits.shift range left in
      let loc_left = make_loc left size_char in
      let alarm, v = (* conflate_bottom=false: we want to copy padding bits *)
        Model.find_unspecified ~conflate_bottom:false state loc_right
      in
      if alarm then begin
        emit_alarm.memcpy_alarm_set_syntactic_context_array_src ();
        Valarms.warn_mem_read with_alarms;
      end;
      begin match v with
        | V_Or_Uninitialized.C_init_noesc _ -> ()
        | _ -> Value_parameters.result ~dkey ~current:true ~once:true
                 "@[In memcpy@ builtin:@ imprecise@ copy of@ indeterminate@ \
                  values@]%t"
                 Value_util.pp_callstack
      end;
      emit_alarm.memcpy_alarm_set_syntactic_context_array_dst ();
      let updated_state =
        Eval_op.add_binding_unspecified ~with_alarms
          ~remove_invalid:true ~exact:false new_state loc_left v
      in
      raise
        (Memcpy_result
           ((Cvalue.Model.join updated_state new_state), c_from, sure_zone))
  in
  try
    if Ival.is_zero size then
      raise (Memcpy_result (state, empty_cfrom, Zone.bottom));
    let precise_state, precise_deps_table, sure_zone = precise_copy state in
    if Extlib.may_map ~dft:false (Int.equal min) max then begin
      let c_from =
        { Function_Froms.deps_table = precise_deps_table; deps_return }
      in
      raise (Memcpy_result (precise_state, c_from, sure_zone))
    end;
    imprecise_copy precise_state precise_deps_table sure_zone
  with Memcpy_result (new_state, c_from, sure_zone) ->
    new_state, c_from, sure_zone

(* Implements built-ins:
   - tis_memcpy
   - tis_wmemcpy
   - tis_memmove
   - tis_wmemmove *)
let tis_memcpy ~str_or_wcs ~check_overlap state actuals =
  let builtin_name =
    match check_overlap, str_or_wcs with
    | true,  Character     -> "memcpy"
    | true,  WideCharacter -> "wmemcpy"
    | false, Character     -> "memmove"
    | false, WideCharacter -> "wmemmove"
  in
  let compute ((exp_dst,dst,_) as a1) ((exp_src,src,_) as a2) (exp_size,size,_) =
    if Value_parameters.ValShowProgress.get () then
      Value_parameters.feedback ~current:true "Call to builtin tis_%s(%a)%t"
        builtin_name pretty_actuals actuals Value_util.pp_callstack;
    Aux.additional_ptr_validity_check_for_size_zero ~for_writing:true ~size a1;
    Aux.additional_ptr_validity_check_for_size_zero ~for_writing:false ~size a2;

    let size =
      try Cvalue.V.project_ival size
      with Cvalue.V.Not_based_on_null -> Ival.top
    in
    let character_bits = get_character_bits str_or_wcs in
    let right = loc_bytes_to_loc_bits src in
    let left = loc_bytes_to_loc_bits dst in

    let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
    let array_src = Logic_utils.array_with_range exp_src term_size in
    let array_dst = Logic_utils.array_with_range exp_dst term_size in

    if check_overlap then
      begin
        match overlap_status_loc_bits ~size_in_bytes:true left size right size
        with
        | Overlap ->
          Value_parameters.error ~current:true
            "overlapping memory zones in call to %s(%a, %a, %a); \
             assert(no overlap between source and destination objects). \
             Will return bottom for this execution path."
            builtin_name
            Cil_datatype.Term.pretty array_src
            Cil_datatype.Term.pretty array_dst
            Cil_datatype.Term.pretty term_size;
          raise Do_Bottom
        | MayOverlap ->
          Value_parameters.warning ~current:true
            "possible overlapping memory zones in call to %s(%a, %a, %a); \
             assert(no overlap between source and destination objects)."
            builtin_name
            Cil_datatype.Term.pretty array_src
            Cil_datatype.Term.pretty array_dst
            Cil_datatype.Term.pretty term_size
        (** TODO: Add assert message **)
        | Separated -> ()
      end;

    let new_state, c_from, sure_zone =
      let emit_alarm =
        { memcpy_alarm_warn_mode = warn_all_quiet_mode ();
          memcpy_alarm_set_syntactic_context_array_src =
            (fun () ->
               Valarms.set_syntactic_context (Valarms.SyMemLogic array_src));
          memcpy_alarm_set_syntactic_context_array_dst =
            (fun () ->
               Valarms.set_syntactic_context (Valarms.SyMemLogic array_dst)); }
      in
      abstract_memcpy ~emit_alarm ~character_bits ~size src dst state
    in
    if Model.is_reachable new_state then
      (* Copy at least partially succeeded (with perhaps an
         alarm for some of the sizes *)
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_ptr dst, new_state) ];
        c_clobbered = Location_Bits.get_bases left;
        c_cacheable = Value_types.Cacheable;
        c_from = Some (Value_types.Froms c_from);
        c_sureouts = Some sure_zone; }
    else
      { Value_types.c_values =
          [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = Some (Value_types.Froms c_from);
        c_sureouts = Some sure_zone; }
  in
  try
    match actuals with
    | [dst; src; size] -> compute dst src size
    | _ -> raise Db.Value.Aborted
  with
  | V.Not_based_on_null
  | Db.Value.Outside_builtin_possibilities as e ->
    Value_parameters.result
      "Invalid call to tis_%s builtin %a%t"
      builtin_name pretty_actuals actuals Value_util.pp_callstack;
    raise e
  | Db.Value.Aborted ->
    Value_parameters.error
      "Invalid call to tis_%s%a"
      builtin_name pretty_actuals actuals;
    raise Db.Value.Aborted
  | Do_Bottom ->
    { Value_types.c_values = [  Value_types.StateOnly(None, Cvalue.Model.bottom) ];
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.Cacheable;
      c_from = None; (* TODO?*)
      c_sureouts = None;
    }

let () =
  register_builtin "tis_memcpy"
    (tis_memcpy ~str_or_wcs:Character ~check_overlap:true);
  register_builtin "tis_wmemcpy"
    (tis_memcpy ~str_or_wcs:WideCharacter ~check_overlap:true);
  register_builtin "tis_memmove"
    (tis_memcpy ~str_or_wcs:Character ~check_overlap:false);
  register_builtin "tis_wmemmove"
    (tis_memcpy ~str_or_wcs:WideCharacter ~check_overlap:false)

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
