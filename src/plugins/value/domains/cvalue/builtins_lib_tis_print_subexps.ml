(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Aux = Builtins_lib_tis_aux
let register_builtin = Aux.register_builtin

(** Built-in printing all the sub-expressions of the provided expression with
    their values. *)
let tis_print_subexps
    (state : Cvalue.Model.t)
    (actuals : (Cil_types.exp * Cvalue.V.t * Cvalue.V_Offsetmap.t) list)
  : Value_types.call_result =

  (* Get the expressions description string. *)
  let (description, actuals) =
    match actuals with
    (* The description should be provided in the first argument as
       a C string literal. *)
    | ({ enode = Const CStr description; _ }, _v, _offsetmap)
      :: rest_of_actuals ->
      (description, rest_of_actuals)
    (* If the description is not there in the correct form, we abort. *)
    | _ ->
      Value_parameters.abort
        "Built-in \"tis_print_subexps\" must be called with a string \
         literal as the first argument!"
  in

  (* Prepare the pretty printer. *)
  let pp_subexps_of_exp =
    (* Prepare the eval_exp argument. *)
    let (eval_expr : Cil_types.exp -> Cvalue.V.t) =
      let with_alarms = CilE.warn_none_mode in
      Eval_exprs.eval_expr ~with_alarms state
    in
    (* Include the top expression when printing. *)
    let include_top_exp = true
    in
    Print_subexps.abstract_print_subexps
      ~include_top_exp eval_expr
  in

  (* Check how many expressions are there to print. *)
  let number_of_arguments = List.length actuals in

  (* Iterate on the expressions given as arguments. *)
  List.iteri (fun i (exp, _v, _offsetmap) ->
      (* If there are many expressions given, print them with indices. *)
      let description =
        if number_of_arguments == 1
        then description
        else Printf.sprintf "%s (expr %d)" description (i + 1)
        (* Print the expression with a header based on the description. *)
      in
      Value_parameters.feedback ~current:true "%a"
        pp_subexps_of_exp (description, exp)
    ) actuals;

  (* Finally: return no value and an unchanged abstract state. *)
  {
    Value_types.c_values    = [Value_types.StateOnly(None, state)];
    Value_types.c_clobbered = Base.SetLattice.bottom;
    Value_types.c_cacheable = Value_types.NoCache;
    c_from = None; (* TODO?*)
    c_sureouts = None;
  }

let () = register_builtin "tis_print_subexps" tis_print_subexps


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
