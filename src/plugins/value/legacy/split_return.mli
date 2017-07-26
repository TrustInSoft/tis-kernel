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

(** This module is used to merge together the final states of a function
    according to a given strategy. Default is to merge all states together *)

(** Join the given state_set. The strategy is defined according to
    the name of the function. *)
val join_final_states:
  Cil_types.kernel_function ->
  return_lv:Cil_types.lval option ->
  get_node:(State_node.t -> Cil_datatype.Stmt.t -> State_node.last_action ->
            Cvalue.Model.t -> State_node.t) ->
  State_node.t list ->
  State_node.t list

val pretty_strategies: unit -> unit

val kf_strategy: Kernel_function.t -> Split_strategy.t

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
