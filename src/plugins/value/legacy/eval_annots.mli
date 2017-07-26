(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2017 TrustInSoft                                      *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

module ActiveBehaviors: sig
  val header: Cil_types.funbehavior -> string

  type t = private {
    init_state: Cvalue.Model.t;
    funspec: Cil_types.funspec;
    is_active: Cil_types.funbehavior -> Eval_terms.predicate_status
  }

  val create_from_spec: Cvalue.Model.t -> Cil_types.funspec -> t

  val create: Cvalue.Model.t -> Cil_types.kernel_function -> t

  val is_active: t -> Cil_types.funbehavior -> bool

  exception No_such_behavior

  (** @raise [No_such_behavior] *)
  val behavior_from_name: t -> string -> Cil_types.funbehavior

  val active_behaviors: t -> Cil_types.funbehavior list
end

val has_requires: Cil_types.funspec -> bool

type postcondition_kf_kind = PostLeaf | PostBody | PostUseSpec
type p_kind =
  | Precondition
  | Postcondition of postcondition_kf_kind
  | Assumes

val process_inactive_behaviors:
  Cil_types.kernel_function
  -> ActiveBehaviors.t
  -> Cvalue.Model.t
  -> unit

val process_inactive_postconds:
  Cil_types.kernel_function
  -> (Cil_types.funbehavior * Cvalue.Model.t) list
  -> unit

val eval_and_reduce_p_kind:
  Cil_types.kernel_function
  -> Cil_types.funbehavior
  -> active:bool
  -> p_kind
  -> Cil_types.identified_predicate list
  -> (Cil_types.identified_predicate -> Property.t)
  -> (Cvalue.Model.t -> Eval_terms.eval_env)
  -> State_set.t
  -> State_set.t

val check_fct_postconditions_for_behavior:
  Cil_types.kernel_function
  -> ?kinstr:Cil_types.kinstr
  -> ActiveBehaviors.t
  -> Cil_types.funbehavior
  -> Cil_types.termination_kind
  -> result:Cil_types.varinfo option
  -> per_behavior:bool
  -> pre_state:Cvalue.Model.t
  -> State_set.t
  -> State_set.t

val check_fct_postconditions:
  Cil_types.kernel_function
  -> ?kinstr:Cil_types.kinstr
  -> ActiveBehaviors.t
  -> Cil_types.funbehavior list
  -> Cil_types.termination_kind
  -> result:Cil_types.varinfo option
  -> per_behavior:bool
  -> pre_state:Cvalue.Model.t
  -> State_set.t
  -> State_set.t

val check_fct_preconditions_for_behavior:
  Cil_types.kernel_function
  -> ActiveBehaviors.t
  -> per_behavior:bool
  -> Cil_types.kinstr
  -> State_set.t
  -> Cil_types.funbehavior
  -> State_set.t

val check_fct_preconditions:
  Cil_types.kernel_function
  -> ActiveBehaviors.t
  -> Cil_types.kinstr
  -> Cvalue.Model.t
  -> State_set.t

val get_assigns_for_behavior:
  ActiveBehaviors.t
  -> Cil_types.funbehavior
  -> Cil_types.identified_term Cil_types.assigns

val interp_annot:
  Cil_types.kernel_function
  -> ?stmt_spec:bool
  -> ActiveBehaviors.t
  -> Cvalue.Model.t
  -> int
  -> State_set.t
  -> Cil_types.stmt
  -> Cil_types.code_annotation
  -> bool
  -> State_set.t

val mark_unreachable: unit -> unit

val mark_rte: unit -> unit

val c_labels:
  Cil_types.kernel_function
  -> Value_callstack.callstack
  -> Db.Value.state Cil_datatype.Logic_label.Map.t

val mark_green_and_red: unit -> unit

val mark_invalid_initializers: unit -> unit
