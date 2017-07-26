(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2017 TrustInSoft                                      *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp

let tis_interval_split state actuals =
  try
    match actuals with
    | [_, lower, _; _, upper, _ ] ->
      let upper = Ival.project_int (Cvalue.V.project_ival upper) in
      let lower = Ival.project_int (Cvalue.V.project_ival lower) in
      let rec build_states acc i =
        if Int.le i upper then
          let s =
            Value_types.StateOnly
              (Eval_op.wrap_int (Cvalue.V.inject_int i), state)
          in
          build_states (s :: acc) (Int.succ i)
        else acc
      in
      let c_values = build_states [] lower in
      { Value_types.c_values;
        c_clobbered = Base.SetLattice.bottom;
        c_from = None;
        c_cacheable = Value_types.Cacheable;
        c_sureouts = None; }
    | _ ->
      raise (Builtins.Invalid_nb_of_args 2)
  with
  | Cvalue.V.Not_based_on_null
  | Ival.Not_Singleton_Int ->
    Value_parameters.error
      "Invalid call to tis_interval_split%a"
      Value_util.pretty_actuals actuals;
    raise Db.Value.Aborted

let () = Builtins.register_builtin "tis_interval_split" tis_interval_split

let frama_c_copy_block state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback ~current:true "Call to builtin copy_block(%a)%t"
      Value_util.pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [(_, block, _); (_, size,_); (_, length, _)] ->
    let size = Ival.project_int (Cvalue.V.project_ival size) in
    let size = Int.mul size (Bit_utils.sizeofchar()) in
    let length = Ival.project_int (Cvalue.V.project_ival length) in
    let start = Locations.loc_bytes_to_loc_bits block in
    let with_alarms = CilE.warn_none_mode in
    let offsetmap =
      Eval_op.copy_offsetmap ~with_alarms start size state
    in
    begin match offsetmap with
      | `Bottom -> assert false
      | `Top -> Warn.warn_top ()
      | `Map offsetmap ->
        let size_ival = Ival.inject_singleton size in
        let state = ref state in
        let dest = ref start in
        let i = ref Int.one in
        while Int.lt !i length do
          dest := Locations.Location_Bits.shift size_ival !dest;
          state :=
            Eval_op.paste_offsetmap ~with_alarms ~remove_invalid:true
              ~reducing:false ~from:offsetmap ~dst_loc:!dest ~size
              ~exact:true !state;
          i := Int.succ !i;
        done;
        { Value_types.c_values = [ Value_types.StateOnly (None, !state) ];
          c_clobbered =  Base.SetLattice.bottom;
          c_from = None;
          c_cacheable = Value_types.Cacheable;
          c_sureouts = None; }
    end
  | _ ->
    raise (Builtins.Invalid_nb_of_args 3)


let () = Builtins.register_builtin "Frama_C_copy_block" frama_c_copy_block

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
