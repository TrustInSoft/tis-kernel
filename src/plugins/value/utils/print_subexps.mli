(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** Printing all subexpression of an expression (including or excluding the
    top expression itself). *)

val pp_all_subexps :
  ?include_top_exp:bool ->
  (Cil_types.exp -> Cvalue.V.t) ->
  Format.formatter -> Cil_types.exp -> unit
(** [pp_all_subexps ?(include_top_exp=false) eval_exp fmt exp] is a pretty
    printer that prints a list of sub-expressions of a given expression [exp]
    with their values.

    If [include_top_exp] is [true], then it also prints the top expression
    itself, otherwise it only prints its strict sub-expressions. Attention:
    this should be always [false] when the value of the top expression cannot
    be evaluated.

    [eval_exp exp] should be a function that takes an expression [exp] and that
    evaluates it and returns its value.
    Usually [Eval_exprs.eval_expr ~with_alarms state] (with appropriate
    [~with_alarms] and [state] parameters) should be given here; it is not used
    by default mainly because of a forward reference issue.
*)

val abstract_print_subexps :
  ?include_top_exp:bool ->
  (Cil_types.exp -> Cvalue.V.t) ->
  Format.formatter ->
  (string * Cil_types.exp) -> unit
(** [abstract_print_subexps ?(include_top_exp=false) eval_exp fmt (descr, exp)]
    is a version of the pretty printer which is used in the corresponding
    built-in [tis_print_subexps]. It simply adds a nice header before printing
    the sub-expressions of the expression [exp], [descr] should be a string
    describing the expression and it will be included literally in the header. *)
