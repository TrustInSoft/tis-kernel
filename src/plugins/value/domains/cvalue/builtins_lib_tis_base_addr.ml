(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2015-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)


let tis_base_addr state actuals =
  match actuals with
  | [_, ptr, _] ->
    let sizeofptr = Cil.bitsSizeOf Cil.voidPtrType in
    let sizeitv =
      Int_Intervals.inject_bounds
        Abstract_interp.Int.zero
        (Abstract_interp.Int.of_int (sizeofptr-1))
    in
    let value,deps =
      let open Locations.Location_Bytes in
      try fold_bases
            (fun b (a,deps) ->
               join a (inject b Ival.zero),
               Function_Froms.Deps.add_data_dep deps
                 (Locations.Zone.inject b sizeitv))
            ptr
            (bottom,Function_Froms.Deps.bottom)
      with Error_Top ->
        Value_parameters.fatal ~current:true
          "Builtin tis_base_addr applied on a very imprecise value."
    in
    { Value_types.c_values =
        [ Value_types.StateOnly(Eval_op.wrap_ptr value, state) ];
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.Cacheable;
      c_from =
        Some (Value_types.Froms
                (Function_Froms.
                   ({deps_return=
                       Memory.add_to_return
                         ~size: (Int_Base.inject (Integer.of_int sizeofptr))
                         deps;
                     deps_table = Memory.empty})));
      c_sureouts = None; }
  | _ ->
    Value_parameters.error "Invalid arguments for builtin tis_base_addr";
    raise Db.Value.Aborted

let () = Builtins.register_builtin "tis_base_addr" tis_base_addr
