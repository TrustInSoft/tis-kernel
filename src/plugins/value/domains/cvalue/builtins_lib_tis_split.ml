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

open Value_util

let tis_variable_split state actuals =
  if Value_parameters.ValShowProgress.get () then
    Value_parameters.feedback "Call to builtin tis_variable_split(%a)%t"
      pretty_actuals actuals Value_util.pp_callstack;
  match actuals with
  | [(_, dst, _); (_, size, _); (_, limit, _)] ->
    let size =
      try
        let size = Cvalue.V.project_ival size in
        Int.mul Int.eight (Ival.project_int size)
      with V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Db.Value.Outside_builtin_possibilities
    in
    let limit =
      try
        let limit = Cvalue.V.project_ival limit in
        Ival.project_int limit
      with V.Not_based_on_null | Ival.Not_Singleton_Int ->
        raise Db.Value.Outside_builtin_possibilities
    in
    let loc_bits = Locations.loc_bytes_to_loc_bits dst in
    if not (Locations.Location_Bits.cardinal_zero_or_one loc_bits)
    then begin
      Value_parameters.error ~current:true ~once:true
        "tis_variable_split(%a):@ the first argument does not \
         evaluate to a singleton:@ %a%t"
        pretty_actuals actuals
        V.pretty dst
        Value_util.pp_callstack;
      raise Db.Value.Aborted
    end;
    let loc = Locations.make_loc loc_bits (Int_Base.inject size) in
    let _, value = Model.find ~conflate_bottom:true state loc in

    let i = ref Int.zero in
    let f val1 acc =
      let ii = Int.succ !i in
      if Int.gt ii limit
      then begin
        Value_parameters.error ~current:true ~once:true
          "tis_variable_split(%a): cardinal of value %a is over the limit%t"
          pretty_actuals actuals
          V.pretty value
          Value_util.pp_callstack;
        raise Db.Value.Outside_builtin_possibilities
      end
      else
        let new_state = Model.reduce_binding state loc val1 in
        i := ii;
        (Value_types.StateOnly(None, new_state)) :: acc
    in
    let states =
      try
        V.fold_enum f value []
      with V.Error_Top ->
        Value_parameters.error ~current:true ~once:true
          "tis_variable_split(%a): value %a too imprecise to split%t"
          pretty_actuals actuals
          V.pretty value
          Value_util.pp_callstack;
        raise Db.Value.Outside_builtin_possibilities
    in
    { Value_types.c_values = states;
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.NoCache;
      c_from = None; (* TODO?*)
      c_sureouts = None;
    }
  | _ -> assert false



let () = Builtins.register_builtin "tis_variable_split" tis_variable_split
