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

exception DontKnow

(* Checks whether the call to memcmp respects the preconditions:
   Calls emit_alarm_validity syn_ if the pointer is not to fully valid memory
   Calls emit_alarm_contents () if addresses or indeterminate bits
   are read before the first difference.
   In the latter case the problem is with the quadruple:
   (syn1, syn2, size, memory state). Ways to avoid the alarm are
   to have a different ptr1, a different ptr2, a different size or
   different contents in memory (with a difference that comes earlier).
   Therefore the emitted alarm is something like
   \comparable_char_blocks{Here}(syn1, syn2, size).
*)
let abstract_memcmp =
  let warn_contents = ref false in
  let set_warn_contents () = warn_contents := true in
  let with_alarms = CilE.warn_call_fun_on_unspecified set_warn_contents in
  let negative_res = ref false in
  let positive_res = ref false in
  let zero_res = ref false in

  fun ~size ~signed ~emit_alarm_validity ~emit_alarm_contents
    mem1 syn1 mem2 syn2 n synn state ->
      let n, n_addr = Cvalue.V.split Base.null n in
      if not (Cvalue.V.is_bottom n_addr)
      then begin
        Value_parameters.warning ~current:true
          "memcmp() seems to be passed a mix of addresses and \
       integers for size argument; assert(%a an integer)%t"
          Cil_datatype.Exp.pretty synn
          Value_util.pp_callstack;
  (* TODO: add alarm (need to add a new alarm type?) *)
      end;
      if (Ival.is_bottom n) then Ival.bottom
      else
        try begin
          match mem1, mem2 with
          | Location_Bytes.Top _, _ | _, Location_Bytes.Top _ ->
             raise DontKnow
          | Location_Bytes.Map _, Location_Bytes.Map _ ->
             let size_size = Int_Base.inject size in
             let size_ival = Ival.inject_singleton size in
             let min, max =
               match Ival.min_and_max n with
               | Some min, Some max -> min, max
               | _ -> raise DontKnow
             in
             let min_total_size =
               Int_Base.inject
                 (if Int.is_zero min then Int.one else Int.mul size min)
             in
             let max_total_size = Int_Base.inject (Int.mul size max) in
             let locb mem syn =
               let locbb = Locations.loc_bytes_to_loc_bits mem in
               let locmax = make_loc locbb max_total_size in
               if Locations.is_valid ~for_writing:false locmax
               then locbb
               else
                 let locmin = make_loc locbb min_total_size in
                 emit_alarm_validity syn;
                 (valid_part ~for_writing:false locmin).loc
             in
             let locb1 = locb mem1 syn1 in
             let locb2 = locb mem2 syn2 in
             let read_char lobc (chars_acc, locb_acc as acc) =
               let loc = Locations.make_loc lobc size_size in
               let char, address =
                 let char = Eval_op.find ~with_alarms state loc in
                 Cvalue.V.split Base.null char
               in
               if not (Cvalue.V.is_bottom address) then set_warn_contents ();
               if Ival.is_bottom char
               then acc
               else begin
                 let char = Ival.cast ~size ~signed ~value:char in
                 Ival.join char chars_acc,
                 Location_Bits.join lobc locb_acc
               end
             in
             let rec iterate i locb1 locb2 =
               if Int.equal i min then zero_res := true;
               if Int.lt i max
               then
                 let acc = Ival.bottom, Location_Bits.bottom in
                 let chars1, new_locb1 =
                   Location_Bits.fold_enum
                     read_char
                     locb1
                     acc
                 in
                 let chars2, new_locb2 =
                   Location_Bits.fold_enum
                     read_char
                     locb2
                     acc
                 in
                 if Ival.is_bottom chars1 || Ival.is_bottom chars2
                 then begin
                 (* exit loop immediately *)
                 end
                 else begin
              (* Format.printf "iteration--- %a %a@."
                      Ival.pretty chars1 Ival.pretty chars2; *)
                   let min1, max1 = Ival.min_and_max chars1 in
                   let min2, max2 = Ival.min_and_max chars2 in
                   if Ival.compare_min_max min1 max2 < 0
                   then negative_res := true;
                   if Ival.compare_min_max min2 max1 < 0
                   then positive_res := true;
              (* Format.printf "iteration--- pos:%B zero:%B neg:%B@."
                     !positive_res !zero_res !negative_res; *)
                   if Ival.intersects chars1 chars2
                   then begin
                     (* continue the loop *)
                     let new_locb1 = Location_Bits.shift size_ival new_locb1 in
                     let new_locb2 = Location_Bits.shift size_ival new_locb2 in
                     let new_i = Integer.succ i in
                     iterate new_i new_locb1 new_locb2
                   end;
                 end;
             in
             negative_res := false;
             positive_res := false;
             zero_res := false;
             warn_contents := false;
             iterate Int.zero locb1 locb2;
             if !warn_contents then emit_alarm_contents ();
             let res =
               if !zero_res
               then Ival.zero
               else Ival.bottom
             in
             let res = if !positive_res then Ival.join Ival.one res else res in
             if !negative_res then Ival.join Ival.minus_one res else res
        end
        with DontKnow ->
          emit_alarm_validity syn1;
          emit_alarm_validity syn2;
          emit_alarm_contents ();
          Ival.join Ival.minus_one Ival.zero_or_one

let tis_memcmp name ~size ~signed state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin %s(%a)%t"
      name
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [ (exp1, mem1, _) as a1; (exp2, mem2, _) as a2; expn, n, _ ] ->
    let emit_alarm_validity syn =
      let term_size = Logic_utils.expr_to_term ~cast:true expn in
      let array = Logic_utils.array_with_range syn term_size in
      Valarms.set_syntactic_context (Valarms.SyMemLogic array);
      Valarms.warn_mem_read (warn_all_quiet_mode ())
    in
    let emit_alarm_contents () =
      Valarms.warn_comparable_char_blocks
        (warn_all_quiet_mode ()) exp1 exp2 expn
    in
    Builtins_lib_tis_aux.additional_ptr_validity_check_for_size_zero
      ~for_writing:false ~size:n a1;
    Builtins_lib_tis_aux.additional_ptr_validity_check_for_size_zero
      ~for_writing:false ~size:n a2;
    let value =
      abstract_memcmp ~size ~signed ~emit_alarm_validity ~emit_alarm_contents
        mem1 exp1 mem2 exp2 n expn state
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
      let result = V.inject_ival value in
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_int result, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
  | _ ->
    raise Db.Value.Aborted

let () =
  Builtins_lib_tis_aux.register_builtin
    "tis_memcmp"
    (tis_memcmp "memcmp" ~signed:false ~size:(Bit_utils.sizeofchar ()))


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
