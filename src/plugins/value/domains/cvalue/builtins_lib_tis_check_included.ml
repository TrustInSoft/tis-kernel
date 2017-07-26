(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)


(*
   void tis_check_included(const void *src1, size_t n, const void *src2);

   This builtin tests that a memory chunk (src1) is included in
   another memory chunk (src2).
*)

module TisCheckIncludedStopAtFirst=
  Value_parameters.False
    (struct
      let option_name = "-val-tis-check-included-stop-at-first"
      let help = "Stop on the first memory chunk that is not included \
                  into another one"
    end)

let tis_check_included state actuals =
  match actuals with
  | [(exp_src1, src1, _); (exp_size, size, _); (exp_src2, src2, _)] ->
    let with_alarms = Value_util.warn_all_quiet_mode () in
    let size =
      try
        let size = Cvalue.V.project_ival size in
        let char_bits = Bit_utils.sizeofchar () in
        Abstract_interp.Int.mul char_bits (Ival.project_int size)
      with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Db.Value.Outside_builtin_possibilities
    in
    if Abstract_interp.Int.le size Abstract_interp.Int.zero
    then raise Db.Value.Outside_builtin_possibilities
    else begin
      let term_size = Logic_utils.expr_to_term ~cast:true exp_size in
      let get_offsetmap exp_src src =
        let bits_src = Locations.loc_bytes_to_loc_bits src in
        let array_src = Logic_utils.array_with_range exp_src term_size in
        Valarms.set_syntactic_context (Valarms.SyMemLogic array_src);
        match Eval_op.copy_offsetmap ~with_alarms bits_src size state with
        | `Bottom -> raise Db.Value.Outside_builtin_possibilities
        | `Map x -> x
        | `Top -> raise Db.Value.Outside_builtin_possibilities
      in
      let offsetmap1 = get_offsetmap exp_src1 src1 in
      let offsetmap2 = get_offsetmap exp_src2 src2 in
      (* Pretty print the offsetmap under the hypothesis that src_n is
         of the same size that src1 *)
      let offsetmap_typ =
        match exp_size.Cil_types.enode with
        | Cil_types.SizeOf t -> t
        | _ -> Cil_types.TPtr (Cil_types.TVoid [], [])
      in
      let pp_offsm fmt offsetmap =
        Cvalue.V_Offsetmap.pretty_generic ~typ:offsetmap_typ () fmt offsetmap;
        Eval_op.pretty_stitched_offsetmap fmt offsetmap_typ offsetmap
      in
      (* Print the result of the inclusion and pretty print each variables. *)
      Value_parameters.result ~current:true
        "tis-check-included called:@\n\
         %a %s %a@\n\
         === Begin of %a ===@\n@[%a@]@\n=== End of %a ===@\n@\n\
         === Begin of %a ===@\n@[%a@]@\n=== End of %a ==="
        Printer.pp_exp exp_src1
        (if Cvalue.V_Offsetmap.is_included offsetmap1 offsetmap2
         then Unicode.is_included_string ()
         else Unicode.not_included_string ())
        Printer.pp_exp exp_src2
        Printer.pp_exp exp_src1 pp_offsm offsetmap1
        Printer.pp_exp exp_src1 Printer.pp_exp exp_src2
        pp_offsm offsetmap2 Printer.pp_exp exp_src2;
      if TisCheckIncludedStopAtFirst.get () then raise Db.Value.Aborted
    end;
    { Value_types.c_values = [ Value_types.StateOnly(None , state) ];
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.NoCacheCallers;
      c_from = None; (* TODO?*)
      c_sureouts = None;
    }
  | _ ->
    Value_parameters.error "Invalid arguments for builtin tis_check_included";
    raise Db.Value.Aborted

let () = Builtins.register_builtin "tis_check_included" tis_check_included


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
