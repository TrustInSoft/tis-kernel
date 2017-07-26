(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2013-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** Utilities to evaluate assigns clauses. *)

open Cil_types
open Eval_terms

(** [pp_eval_error fmt err] do a pretty print of the logic evaliation error
    [err] if and only if it's not a CAlarm. *)
val pp_eval_error : Format.formatter -> logic_evaluation_error -> unit

(** [froms_contents eval_env pretty assign] evaluates the from part of the
    assign clause [assign] using evaluation's environment [eval_env]. If a
    warning has to be shown, the [pretty] function is used to display the
    element (a Statement or a Kernel_function) concerned by the assign
    clause. *)
val froms_contents :
  eval_env ->
  (Format.formatter -> unit) ->
  identified_term * identified_term deps ->
  Cvalue.V.t

(** [treat_output_loc state out sclob froms_contents loc] updates the [state] by
    adding a binding of the location [loc] with [from_contents].
    The term [out] is only used to be pretty printed when an error occurs. *)
val treat_output_loc :
  Cvalue.Model.t ->
  Cil_types.term ->
  Locals_scoping.clobbered_set ->
  Cvalue.V.t ->
  Locations.location ->
  Cvalue.Model.t
