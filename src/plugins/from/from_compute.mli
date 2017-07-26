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

(** Module implementing the computation of functional dependencies *)

open Cil_types

(** The signature [To_Use] is used to describe the function used to
    get the current state, during the analysis, according to a statement. *)
module type To_Use =
sig
  (** How to find the state of Value at a given statement during the analysis.*)
  val get_value_state : stmt -> Db.Value.state
end

module Kf_FromResults: Datatype.S with type t = Value_types.FromResult.t Kernel_function.Hashtbl.t

(** Function that compute the Froms from a given prototype, called
    in the given state *)
val compute_using_prototype_for_state :
  Db.Value.state -> Kernel_function.t -> Function_Froms.froms


(** Direct computation of the dependencies on expressions, offsets and
    lvals. The state at the statement is taken from Values_To_Use *)
val find_deps_no_transitivity :
  Db.Value.state -> exp -> Function_Froms.Deps.t
val find_deps_lval_no_transitivity :
  Db.Value.state -> lval -> Function_Froms.Deps.t

(** Functor computing the functional dependencies *)
module GMake (X: sig
    val graph : Nodegraph.t
    val initial_nodes : State_node.Set.t
  end) : sig
  (** Compute the dependencies of the given function, and return them *)
  val compute_and_return : Kernel_function.t -> Function_Froms.t
  (** Compute the dependencies of the given function *)
  val compute : Kernel_function.t -> unit
end

(** Computes the whole-program functional dependencies *)
module WholeProgram : sig
  val compute : unit -> unit
end

(** Functor computing the functional dependencies *)
module Make (To_Use: To_Use) : sig
  (** Compute the dependencies of the given function, and return them *)
  val compute_and_return : Kernel_function.t -> Function_Froms.t
  (** Compute the dependencies of the given function *)
  val compute : Kernel_function.t -> unit
end

(** Exception indicating that a given call statement was not evaluated. *)
exception Call_did_not_take_place
