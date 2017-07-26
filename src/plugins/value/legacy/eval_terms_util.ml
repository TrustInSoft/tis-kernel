(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2013-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eval_terms

let pp_eval_error fmt e =
  if e <> CAlarm then
    Format.fprintf fmt "@ (%a)" pretty_logic_evaluation_error e

let froms_contents env pretty (_, ins as asgn) =
  let treat_content acc { it_content = t; _ } =
    if List.mem "indirect" t.term_name then acc
    else
      let r = eval_term ~with_alarms:CilE.warn_none_mode env t in
      Cvalue.V.join acc (Cvalue.V.topify_leaf_origin r.eover)
  in
  match ins with
  | FromAny -> Cvalue.V.top_int
  | From from_deps ->
    begin
      try List.fold_left treat_content Cvalue.V.top_int from_deps
      with LogicEvalError e ->
        Value_util.warning_once_current
          "cannot interpret@ 'from' clause@ '%a'@ of %t%a"
          Printer.pp_from asgn
          pretty
          pp_eval_error e;
        Cvalue.V.top
    end

let treat_output_loc state out sclob froms_contents loc =
  let valid = Locations.valid_part ~for_writing:true loc in
  if Locations.is_bottom_loc valid
  then begin
    if not (Locations.is_bottom_loc loc)
    then begin
      Value_parameters.warning ~current:true ~once:true
        "@[Completely invalid destination@ for assigns@ clause %a.@ \
         Ignoring.@]"
        Printer.pp_term out
    end;
    state
  end
  else begin
    Locals_scoping.remember_if_locals_in_value sclob loc froms_contents;
    let state' =
      snd (Cvalue.Model.add_binding ~exact:false state loc froms_contents)
    in
    if Cvalue.Model.equal Cvalue.Model.top state'
    then begin
      Value_parameters.error ~once:true ~current:true
        "Cannot@ handle@ assigns@ for %a,@ location@ is@ too@ imprecise@ \
         (%a).@ Assuming@ it@ is@ not@ assigned,@ but@ be@ aware@ this@ \
         is@ incorrect." Printer.pp_term out Locations.pretty loc;
      state
    end
    else state'
  end

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
