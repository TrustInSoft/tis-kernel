(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype
open Value_callstack

type t

(** Explain the context of the Return action. *)
and return_kind =
  | BuiltinOrContract
  (** The Return action comes from a builtin, or a function using a
      contract. The associated node is the caller. *)
  | NormalCall
  (** A simple function call. The associated node is the last node
      of the callee. *)

and last_action =
  | Init
  | Statement
  | InterpAnnot of string
  | Join of string
  | MergeIncludedStates
  | Widen
  | MergeAfterLoop
  | Edge
  | Guard of Exp.t
  | ComputeFirstPredecessor
  | CombinePredecessors
  | OpenBlock
  | CloseBlock
  | PostAnnot
  | Return of (return_kind * t)
  | Externalize
  | InterpCall
  | Reduce of last_action



include Datatype.S_with_collections with type t := t

(** State_node constructor. Maintains the following invariants :
    - Node ids are unique per project
    - Tuples (call stack, statement, action, state) are unique in the table passed to get  *)

module Data: Datatype.S_with_collections
  with type t = Callstack.t * Stmt.t * last_action * Cvalue.Model.t

val get : t Data.Hashtbl.t -> Callstack.t -> Stmt.t -> last_action -> Cvalue.Model.t -> t

(** State_node constructor. Maintains only the invariant on ids. *)
val create_new : Callstack.t -> Stmt.t -> last_action -> Cvalue.Model.t -> t

val pretty_no_state : Format.formatter -> t -> unit

(** Field access. *)

val id : t -> int
val callstack : t -> Callstack.t
val stmt : t -> Stmt.t
val action : t -> last_action
val state : t -> Cvalue.Model.t

val with_state : (Cvalue.Model.t -> 'a) -> t -> 'a
val with_state_node : (Cvalue.Model.t -> t -> 'a) -> t -> 'a
val with_stmt_state : (Stmt.t -> Cvalue.Model.t -> 'a) -> t -> 'a
val with_stmt_state_node : (Stmt.t -> Cvalue.Model.t -> t -> 'a) -> t -> 'a
val with_action : (last_action -> 'a) -> t -> 'a
val with_fields : (Stmt.t -> last_action -> Cvalue.Model.t -> 'a) -> t -> 'a
val with_all : (int -> Callstack.t -> Stmt.t -> last_action -> Cvalue.Model.t -> 'a) -> t -> 'a

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
