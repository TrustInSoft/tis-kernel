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

module Split_strategy: sig
  type split_strategy =
      NoSplit | SplitAuto | SplitEqList of Datatype.Integer.t list | FullSplit
  include Datatype.S_with_collections with type t = split_strategy
  exception ParseFailure of string
  val of_string: string -> t (* @raise ParseFailure *)
  val to_string: t -> string
end

(** Analysis for values and pointers *)
module Value_parameters: sig
  include Plugin.General_services

  module BuiltinsOverrides:
    Parameter_sig.Map with type key = Cil_types.kernel_function
                       and type value = string
  module ForceValues: Parameter_sig.With_output
  module ResultsSlevel: Parameter_sig.Bool
  module SemanticUnrollingLevel: Parameter_sig.Int
  module SlevelFunction:
    Parameter_sig.Map with type key = Cil_types.kernel_function
                       and type value = int
  module SlevelMergeAfterLoop: Parameter_sig.Kernel_function_set
  module SplitReturn: Parameter_sig.String
  module SplitReturnFunction:
    Parameter_sig.Map with type key = Cil_types.kernel_function
                       and type value = Split_strategy.t
  module SplitGlobalStrategy: State_builder.Ref
    with type data = Split_strategy.t
  module ValShowProgress: Parameter_sig.Bool
end

(** Most functions are directly exported: they are registered in {!Db.Value}. *)
module State_set: sig
  include module type of State_set
  (** Polymorphic ==> include module type fails... *)
  val fold : ('a -> Cvalue.Model.t -> 'a) -> 'a -> t -> 'a
end

module Value_util:sig
  include module type of Value_util
  (* Dependency on a module containing polymorphic types prevents
     simple include module type. *)
  val bind_block_locals: State_set.t -> Cil_types.block -> State_set.t
  val get_slevel: Kernel_function.t -> Value_parameters.SlevelFunction.value
end

module Value_messages:module type of Value_messages
module Value_results:module type of Value_results
module Value_stat:module type of Value_stat
module Eval_exprs: module type of Eval_exprs
module Eval_op: module type of Eval_op
module Eval_slevel:sig val signal_abort: unit -> unit end
module Value_variadic:
sig
  include module type of Value_variadic
  (* Dependency on a module containing polymorphic types prevents
     simple include module type. *)
  val bind_block_variadic_locals : State_set.t -> Cil_types.block -> State_set.t
end

module Eval_terms:
sig
  include module type of Eval_terms
  (* Dependency on a module containing polymorphic types prevents
     simple include module type. *)
  val split_disjunction_and_reduce :
    reduce:bool ->
    env:eval_env ->
    Cvalue.Model.t ->
    slevel:int ->
    Cil_types.predicate Cil_types.named -> Property.t -> State_set.t
end

module Gui_types: sig
  type gui_offsetmap_res
  val equal_gui_offsetmap_res: gui_offsetmap_res -> gui_offsetmap_res -> bool
  val join_gui_offsetmap_res:
    gui_offsetmap_res -> gui_offsetmap_res -> gui_offsetmap_res
  val pretty_gui_offsetmap_res:
    ?typ:Cil_types.typ -> Format.formatter -> gui_offsetmap_res -> unit
  type gui_selection
  type gui_res
  val pretty_gui_res: Format.formatter -> gui_res -> unit
  val equal_gui_res: gui_res -> gui_res -> bool

  type gui_loc =
    | GL_Stmt of Kernel_function.t * Cil_types.stmt
    | GL_Pre of Kernel_function.t (* pre-state of a function *)
    | GL_Post of Kernel_function.t (* post-state of a function *)
  module Gui_loc: Datatype.S_with_collections with type t = gui_loc

  val kf_of_gui_loc: gui_loc -> Kernel_function.t

  type gui_callstack
  module GCallstackMap : FCMap.S with type key = gui_callstack
end

module Gui_eval: sig
  type ('env, 'expr, 'v) evaluation_functions = {
    eval_and_warn: 'env -> 'expr -> 'v * bool;
    env: Cvalue.Model.t -> Value_callstack.callstack -> 'env;
    equal: 'v -> 'v -> bool;
    bottom: 'v;
    join: 'v -> 'v -> 'v;
    expr_to_gui_selection: 'expr -> Gui_types.gui_selection;
    res_to_gui_res: 'expr -> 'v -> Gui_types.gui_res;
  }
  val lval_ev:
    (Cvalue.Model.t, Cil_types.lval,
     Gui_types.gui_offsetmap_res) evaluation_functions

  val lval_zone_ev:
    (Cvalue.Model.t, Cil_types.lval, Locations.Zone.t) evaluation_functions

  val null_ev:
    (Cvalue.Model.t, unit, Gui_types.gui_offsetmap_res) evaluation_functions

  val exp_ev:
    (Cvalue.Model.t, Cil_types.exp, Cvalue.V.t) evaluation_functions

  val tlval_ev: Gui_types.gui_loc ->
    (Eval_terms.eval_env, Cil_types.term,
     Gui_types.gui_offsetmap_res) evaluation_functions

  val tlval_zone_ev: Gui_types.gui_loc ->
    (Eval_terms.eval_env, Cil_types.term,
     Locations.Zone.t) evaluation_functions

  val term_ev:
    Gui_types.gui_loc ->
    (Eval_terms.eval_env, Cil_types.term, Cvalue.V.t) evaluation_functions

  val predicate_ev: Gui_types.gui_loc ->
    (Eval_terms.eval_env, Cil_types.predicate Cil_types.named,
     Eval_terms.predicate_status Bottom.or_bottom)
      evaluation_functions

  val gui_loc_logic_env: Gui_types.gui_loc -> Logic_typing.Lenv.t

  type states_by_callstack = {
    states_before: Cvalue.Model.t Value_callstack.Callstack.Hashtbl.t;
    states_after: Cvalue.Model.t Value_callstack.Callstack.Hashtbl.t option;
  }
  val callstacks_at_gui_loc: Gui_types.gui_loc -> states_by_callstack option

  val classify_pre_post: Kernel_function.t -> Property.t
    -> Gui_types.gui_loc option
end

module Gui_callstacks_filters: module type of Gui_callstacks_filters

(*module Eval_stmt:module type of Eval_stmt*)

module Builtins : sig
  val is_function_a_builtin: Kernel_function.t -> bool
  val overridden_by_builtin: Kernel_function.t -> bool
  val get_abstract_function_if_builtin: Kernel_function.t ->
    (string * Db.Value.builtin_sig * bool) option
end (* module Builtins *)

module Builtins_lib_tis_printf: module type of Builtins_lib_tis_printf
