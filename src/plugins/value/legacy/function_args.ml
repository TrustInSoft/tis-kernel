(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*  TrustInSoft Kernel is a fork of Frama-C. All the differences are:     *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

exception Actual_is_bottom
exception WrongFunctionType (* at a call through a pointer *)

(* We cannot statically check that a call through a function pointer is
   correct wrt the number of arguments and their types (see the examples at
   the end of tests/misc/fun_ptr.i). Thus, we make additional checks  here:
   the arguments size are correct, and the number of arguments is sufficient.*)
let check_arg_size expr formal =
  try
    if Cil.bitsSizeOf (Cil.typeOf expr) <> Cil.bitsSizeOf formal.vtype
    then raise WrongFunctionType
  with Cil.SizeOfError _ -> raise WrongFunctionType

let rec fold_left2_best_effort f acc l1 l2 =
  match l1,l2 with
  | _,[] -> acc
  | [],_ -> raise WrongFunctionType (* Too few arguments *)
  | (x1::r1),(x2::r2) -> fold_left2_best_effort f (f acc x1 x2) r1 r2

let debug_print_all_formals kf =
  let formals = Kernel_function.get_formals kf in
  if formals <> [] then
    begin
      let level = 2 in
      let indexed_formals =
        List.mapi (fun i varinfo -> (i, varinfo)) formals
      in
      let pp_indexed_varinfos =
        let pp_indexed_varinfo fmt (i, varinfo) =
          Format.fprintf fmt "%d : %a" i Cil_printer.pp_varinfo varinfo
        in
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n+ ")
          pp_indexed_varinfo
      in
      Value_parameters.debug ~level
        "@\n--------@\nFormals of function {%s}:@\n+ %a@\n--------@\n"
        (Kernel_function.get_name kf)
        pp_indexed_varinfos indexed_formals
    end

let debug_print_surplus_actuals kf actuals =
  if actuals <> [] then
    begin
      let level = 2 in
      let indexed_actuals =
        List.mapi (fun i actual -> (i, actual)) actuals
      in
      let pp_indexed_actuals =
        let pp_indexed_actual fmt actual =
          let index, (exp, offsetmap) = actual in
          let typ = Cil.typeOf exp in
          Format.fprintf fmt
            "actual %2d:@\n   exp    = (%a) %a@\n   offset = %a"
            index
            Cil_printer.pp_typ        typ
            Cil_printer.pp_exp        exp
            Cvalue.V_Offsetmap.pretty offsetmap
        in
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n+ ")
          pp_indexed_actual
      in
      Value_parameters.debug ~level
        "@\n--------@\nSurplus actuals for function {%s}:@\n+ %a@\n--------@\n"
        (Kernel_function.get_name kf)
        pp_indexed_actuals indexed_actuals
    end

let actualize_formals
    ?(check = (fun _ _ -> ()))
    (kf: Kernel_function.t)
    (call_site: kinstr)
    (state: Cvalue.Model.t)
    (actuals: (exp * Cvalue.Model.offsetmap) list) =
  (* Get the function's formal arguments. *)
  let formals = Kernel_function.get_formals kf in
  (* Debug: print all formals. *)
  debug_print_all_formals kf;
  (* First: treat the actuals matching formals. *)
  let state, surplus_actuals =
    (* Treat an argument passed to the function which corresponds to a
       function's formal argument. *)
    let treat_one_formal
        (state : Cvalue.Model.t)
        ((actual_expr, actual_offsetmap) : (exp * Cvalue.Model.offsetmap))
        (formal : varinfo) : Cvalue.Model.t =
      (check actual_expr formal: unit);
      let actual_offsetmap =
        (* Structure padding has unspecified value upon assignment. *)
        let typ = Cil.typeOf actual_expr in
        Cvalue.V_Offsetmap.uninitialize_padding actual_offsetmap typ
      in
      Cvalue.Model.add_base (Base.of_varinfo formal) actual_offsetmap state
    in
    (* Treat all the matching pairs formal-actual, returns the new state
       and a list of remaining actuals (i.e. the surplus arguments which
       are not matching any formals). *)
    let rec treat_formals_matching_actuals state actuals formals =
      match actuals, formals with
      (* All matching formals and actuals have been treated,
         maybe some surplus actuals are still there. *)
      | surplus_actuals, [] -> state, surplus_actuals
      (* Too few actuals (the function has been called with
         not enough arguments). *)
      | [], _ -> raise WrongFunctionType
      (* There is an actual matching a formal. *)
      | actual::rest_of_actuals, formal::rest_of_formals ->
        treat_formals_matching_actuals
          (treat_one_formal state actual formal)
          rest_of_actuals rest_of_formals
    in
    treat_formals_matching_actuals state actuals formals
  in
  (* Second: treat the surplus actuals passed to the function. *)
  let treat_surplus_actuals
      (state : Cvalue.Model.t)
      (surplus_actuals : (exp * Cvalue.Model.offsetmap) list)
    : Cvalue.Model.t =
    (* Debug: print all the surplus actuals (if there are any). *)
    debug_print_surplus_actuals kf surplus_actuals;
    (* Prepare the new state with variadic arguments. *)
    Value_variadic.add_variadic_arguments_to_state
      kf call_site surplus_actuals state
  in
  (* Note: even if no surplus arguments were actually passed, a variadic
     function needs to assign a value to the va_args variable for
     consistency reasons (as variadic macros may still be called inside
     correctly if they do not attempt to actually read any argument). *)
  (* TODO: Check carefully if these conditions are enough.... *)
  if
    (* The function is variadic. *)
    (Value_util.is_function_variadic kf) &&
    (* The function is not a built-in. *)
    (not (Builtins.is_function_a_builtin kf)) &&
    (* The function is not just a declaration. *)
    (Kernel_function.is_definition kf)
  then treat_surplus_actuals state surplus_actuals
  else state

(*  For all formals of [kf] whose address is taken, merge their values
    in [prev_state] and [new_state], and update [new_state]. This is
    useful to handle recursive calls. *)
let merge_referenced_formals kf prev_state new_state =
  let formals = Kernel_function.get_formals kf in
  let aux state vi =
    if vi.vaddrof then
      let b = Base.of_varinfo vi in
      let prev_offsm = Cvalue.Model.find_base_or_default b prev_state in
      let new_offsm = Cvalue.Model.find_base_or_default b new_state in
      match Cvalue.V_Offsetmap.join_top_bottom prev_offsm new_offsm with
      | `Top -> assert false
      | `Bottom -> Cvalue.Model.bottom
      | `Map m -> Cvalue.Model.add_base b m state
    else state
  in
  List.fold_left aux new_state formals

let main_initial_state_with_formals kf (state:Cvalue.Model.t) =
  match kf.fundec with
  | Declaration (_, _, None, _) -> state
  | Declaration (_, _, Some l, _)
  | Definition ({ sformals = l; _ }, _) ->
    if Value_parameters.InterpreterMode.get()
    then begin
      (* Check if argc type is int. *)
      let is_argc_of_correct_type argc_varinfo =
        match Cil.unrollType argc_varinfo.vtype with
        | TInt(IInt, []) -> true
        | _ -> false
      in
      (* Check if argv type is char**. *)
      let is_argv_of_correct_type argv_varinfo =
        match Cil.unrollType argv_varinfo.vtype with
        | TPtr (t, []) when Cil.isCharPtrType t -> true
        | _ -> false
      in
      (* Check if both argc and argv have correct types. *)
      let are_main_arguments_ok argc_varinfo argv_varinfo =
        (is_argc_of_correct_type argc_varinfo) &&
        (is_argv_of_correct_type argv_varinfo)
      in
      match l with
      | [] -> state
      | [argc_varinfo; argv_varinfo]
      | [argc_varinfo; argv_varinfo; _]
        when are_main_arguments_ok argc_varinfo argv_varinfo ->
        Initial_state.initialize_args argc_varinfo argv_varinfo state
      | _ ->
        Value_parameters.error "Entry point %a has unexpected arguments"
          Kernel_function.pretty kf;
        raise Db.Value.Aborted
    end
    else
      List.fold_right
        Initial_state.initialize_var_using_type
        l
        state


let compute_actual ~with_alarms ~warn_indeterminate state e =
  let warn kind =
    if with_alarms.CilE.imprecision_tracing.CilE.a_log then
      Value_parameters.result ~current:true ~once:true
        "completely invalid@ %s in evaluation of@ argument %a"
        kind Printer.pp_exp e;
    raise Actual_is_bottom
  in
  match e with
  | { enode = Lval lv; _ }
    when not (Eval_typ.is_bitfield (Cil.typeOfLval lv)) ->
    let ploc, state, o =
      try Eval_exprs.offsetmap_of_lv ~with_alarms state lv
      with Int_Base.Error_Top ->
        Value_parameters.abort
          ~current:true
          "Function argument %a has unknown size. Aborting"
          Printer.pp_exp e;
    in
    begin
      match o with
      | `Map o ->
        let typ_lv = Cil.typeOfLval lv in
        let o, state =
          if warn_indeterminate then
            match Warn.warn_reduce_indeterminate_offsetmap
                    ~with_alarms typ_lv o (`PreciseLoc ploc) state
            with
            | `Bottom -> warn "value"
            | `Res r -> r
          else o, state
        in
        begin match Warn.offsetmap_contains_imprecision o with
          | Some v ->
            let loc = Precise_locs.imprecise_location ploc in
            Warn.warn_imprecise_lval_read ~with_alarms lv loc v
          | None -> ()
        end;
        o, state
      | `Bottom -> warn "location"
      | `Top -> Warn.warn_top ()
    end
  | _ ->
    let state, _, interpreted_expr =
      Eval_exprs.eval_expr_with_deps_state ~with_alarms None state e
    in
    if Cvalue.V.is_bottom interpreted_expr then warn "value";
    let typ = Cil.typeOf e in
    Eval_op.offsetmap_of_v ~typ interpreted_expr, state

let () =
  Db.Value.add_formals_to_state :=
    (fun state kf call_site exps ->
       try
         let compute_actual =
           compute_actual
             ~with_alarms:CilE.warn_none_mode
             ~warn_indeterminate:false
         in
         let actuals =
           List.map (fun e -> e, fst (compute_actual state e)) exps
         in
         actualize_formals kf call_site state actuals
       with Actual_is_bottom -> Cvalue.Model.bottom)

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
