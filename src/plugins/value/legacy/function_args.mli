(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2017 TrustInSoft                                      *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

exception Actual_is_bottom
exception WrongFunctionType

(** @raise [WrongFunctionType] *)
val check_arg_size: Cil_types.exp -> Cil_types.varinfo -> unit

(** @raise [WrongFunctionType] *)
val fold_left2_best_effort:
  ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a

(** @raise [WrongFunctionType] *)
val actualize_formals:
  ?check:(Cil_types.exp -> Cil_types.varinfo -> unit)
  -> Cil_types.kernel_function
  -> Cil_types.kinstr
  -> Cvalue.Model.t
  -> (Cil_types.exp * Cvalue.Model.offsetmap) list
  -> Cvalue.Model.t

val merge_referenced_formals:
  Cil_types.kernel_function
  -> Cvalue.Model.t
  -> Cvalue.Model.t
  -> Cvalue.Model.t

(** @raise [Db.Value.Aborted] *)
val main_initial_state_with_formals:
  Cil_types.kernel_function -> Cvalue.Model.t -> Cvalue.Model.t

(** @raise [Actual_is_bottom] *)
val compute_actual:
  with_alarms:CilE.warn_mode
  -> warn_indeterminate:bool
  -> Cvalue.Model.t
  -> Cil_types.exp
  -> Cvalue.V_Offsetmap.t * Cvalue.Model.t
