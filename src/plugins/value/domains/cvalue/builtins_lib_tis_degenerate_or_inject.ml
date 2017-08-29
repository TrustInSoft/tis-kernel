(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(* Degenerate or inject a value depending on command-line options. *)
module DegenerateOrInject =
  Value_parameters.Int
    (struct
      let option_name = "-tis-degenerate-or-inject"
      let default = -1
      let arg_name = "n"
      let help =
        "force tis_degenerate_or_inject to return <n> (defaults to -1)"
    end)

let tis_degenerate_or_inject state actuals =
  let v_to_inject = DegenerateOrInject.get () in
  if DegenerateOrInject.is_default () then begin
    Eval_slevel.signal_abort ();
    { Value_types.c_values = [ ];
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.Cacheable;
      c_from = None; (* TODO?*)
      c_sureouts = None;
    }
  end
  else
    begin match actuals with
      | [] ->
        let v = Cvalue.V.inject_int (Integer.of_int v_to_inject) in
        { Value_types.c_values =
            [ Value_types.StateOnly(Eval_op.wrap_int v, state) ];
          c_clobbered = Base.SetLattice.bottom;
          c_cacheable = Value_types.Cacheable;
          c_from = None; (* TODO?*)
          c_sureouts = None;
        }
      | _ -> raise Db.Value.Outside_builtin_possibilities
    end

let () =
  Builtins.register_builtin "tis_degenerate_or_inject" tis_degenerate_or_inject

(*
  Local Variables:
  compile-command: "make -C ../../../../.."
  End:
*)
