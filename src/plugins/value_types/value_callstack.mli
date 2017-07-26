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

(* TODO: These types are already defined in Value_util. *)
type call_site = kernel_function * kinstr
type callstack = call_site list
(** Value callstacks, as used e.g. in Db.Value hooks *)

module Callsite: Datatype.S_with_collections with type t = call_site
module Callstack: Datatype.S_with_collections with type t = callstack

(*

  The Shared_callstack module provides more sharing for callstacks. It
  does so by plugging itself in some callstack producers. Currently,
  only the Value_util.call_stack hidden reference is updated.

  Ideally we would like the callstack type to be abstract and
  hash-consed. This change would impact a lot of places, so this
  module may be used to do a soft transition by updating only
  callstack producers to use this abstract type internally but provide
  the concrete callstack type. As long as callstack users do not touch
  these callstacks, they will remain shared and the memory footprint
  will be smaller.

*)
module Shared_callstack : sig

  (* Abstract type for shared callstacks. *)
  type t

  (* [empty] is the equivalent of [[]]. *)
  val empty : t

  (* [push c cs] is the equivalent of [c :: cs]. *)
  val push : call_site -> t -> t (* O(1) *)

  (* [pop cs] is the equivalent of [List.tl cs].
     [cs] must not be empty. *)
  val pop : t -> t (* O(1) *)

  (* [get cs] returns the concrete version of [cs]. *)
  val get : t -> callstack (* O(1) *)

  (* [put cs] returns the abstract version of [cs]. *)
  val put : callstack -> t (* O(n) *)
end
