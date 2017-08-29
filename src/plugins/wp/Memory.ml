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
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Memory Model Interface                                             --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes

type 'a sequence = { pre : 'a ; post : 'a }

(** Memory Values *)

type acs =
  | RW (** Read-Write Access *)
  | RD (** Read-Only Access *)

type 'a value =
  | Val of Lang.F.term
  | Loc of 'a

type 'a rloc =
  | Rloc of c_object * 'a
  | Rrange of 'a * c_object * Lang.F.term option * Lang.F.term option
  (** a contiguous set of location *)

type 'a sloc =
  | Sloc of 'a
  | Sarray of 'a * c_object * int (** full sized range (optimized assigns) *)
  | Srange of 'a * c_object * Lang.F.term option * Lang.F.term option
  | Sdescr of Lang.F.var list * 'a * Lang.F.pred
  (** a set of location *)

type 'a logic =
  | Vexp of Lang.F.term
  | Vloc of 'a
  | Vset of Vset.set
  | Lset of 'a sloc list

(** Memory Variables

    The memory is partitionned into chunk, set of memory location.
*)

module type Chunk =
sig

  type t
  val self : string (** Chunk names, for pretty-printing *)
  val hash : t -> int
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val tau_of_chunk : t -> Lang.F.tau
  val basename_of_chunk : t -> string
  val is_framed : t -> bool
  (** Whether the Chunk is local to a function.
      Means the chunk is separated from any call side-effect. *)

end

(** Memory Environment

    Represent the content of the memory
*)

module type Sigma =
sig

  type chunk
  type domain
  type t

  val create : unit -> t
  val copy : t -> t
  val merge : t -> t -> t * Passive.t * Passive.t
  val join : t -> t -> Passive.t (** pairwise equal *)
  val assigned : t -> t -> domain -> Lang.F.pred Bag.t (** equal chunks outside domain *)

  val mem : t -> chunk -> bool
  val get : t -> chunk -> Lang.F.var
  val value : t -> chunk -> Lang.F.term
  val iter : (chunk -> Lang.F.var -> unit) -> t -> unit
  val iter2 : (chunk -> Lang.F.var option -> Lang.F.var option -> unit) -> t -> t -> unit
  val havoc : t -> domain -> t
  val havoc_chunk : t -> chunk -> t
  val havoc_any : call:bool -> t -> t
  val domain : t -> domain
  val union : domain -> domain -> domain
  val empty : domain

  val pretty : Format.formatter -> t -> unit

end

(** Memory Model *)

module type Model =
sig

  val configure : Model.tuning
  val datatype : string (** for projectification *)
  val separation : unit -> Separation.clause list

  module Chunk : Chunk

  module Heap : Qed.Collection.S
    with type t = Chunk.t

  module Sigma : Sigma
    with type chunk = Chunk.t
     and type domain = Heap.set

  type loc
  (** Representation of the memory location in the model. *)

  type chunk = Chunk.t
  type sigma = Sigma.t
  type segment = loc rloc

  val pretty : Format.formatter -> loc -> unit
  (** pretty printing of memory location *)

  val vars : loc -> Lang.F.Vars.t
  (** Return the logic variables from which the given location depend on. *)

  val occurs : Lang.F.var -> loc -> bool
  (** Test if a location depend on a given logic variable *)

  val null : loc
  (** Return the location of the null pointer *)

  val literal : eid:int -> Cstring.cst -> loc
  (** Return the memory location of a constant string,
      the id is a unique identifier.
  *)

  val cvar : varinfo -> loc
  (** Return the location of a C variable *)

  val pointer_loc : Lang.F.term -> loc
  (** ??? *)

  val pointer_val : loc -> Lang.F.term
  (** ??? *)

  val field : loc -> fieldinfo -> loc
  (** Return the memory location obtained by field access from a given
      memory location
  *)

  val shift : loc -> c_object -> Lang.F.term -> loc
  (** Return the memory location obtained by array access at an index
      represented by the given {!term}. The element of the array are of
      the given {!c_object} type. *)

  val base_addr : loc -> loc
  (** Return the memory location of the base address of a given memory
      location *)

  val block_length : sigma -> c_object -> loc -> Lang.F.term
  (**  Returns the length (in bytes) of the allocated block containing
       the given location *)

  val cast : c_object sequence -> loc -> loc
  (** Cast a memory location into another memory location.
      For [cast ty loc] the cast is done from [ty.pre] to [ty.post]
  *)

  val loc_of_int : c_object -> Lang.F.term -> loc
  (** Cast a term representing a pointer to a c_object into a memory
      location *)

  val int_of_loc : c_int -> loc -> Lang.F.term
  (** Cast a memory location into an integer of the given type *)

  val domain : c_object -> loc -> Heap.set
  (** Give the set of chunk where an object of the given type at the
      given location is stored. Over approximation of this set is
      allowed.
  *)

  val load : sigma -> c_object -> loc -> loc value
  (** Return the value of the object of the given type at the given
      location in the given memory state *)

  val copied : sigma sequence -> c_object -> loc -> loc -> Lang.F.pred list
  (** Return a set of formula that express a copy between two memory state.
      [copied sigma ty loc1 loc2] returns a set of formula that express that
      the content for an object [ty] is the same in [sigma.pre] at [loc1] and
      in [sigma.post] at [loc2]
  *)

  val stored:
    sigma sequence -> c_object -> loc -> Lang.F.term -> Lang.F.pred list
  (** Return a set of formula that express a modification between two
      memory state.
      [copied sigma ty loc t] returns a set of formula that express that
      [sigma.pre] and [sigma.post] are identical except for an object [ty] at
      location [loc] which is represented by [t] in [sigma.post].
  *)

  val assigned : sigma sequence -> c_object -> loc sloc -> Lang.F.pred list
  (**
     Return a set of formula that express that two memory state are the same
     except at the given set of memory location. This function can
     over-approximate the set of given memory location (e.g it can
     return [true] as if the all set of memory location was given)
  *)

  val is_null : loc -> Lang.F.pred
  (** Return the formula that check if a given location is null *)

  val loc_eq : loc -> loc -> Lang.F.pred
  val loc_lt : loc -> loc -> Lang.F.pred
  val loc_neq : loc -> loc -> Lang.F.pred
  val loc_leq : loc -> loc -> Lang.F.pred
  (** Memory location comparisons *)

  val loc_diff : c_object -> loc -> loc -> Lang.F.term
  (** Compute the length in bytes between two memory locations *)

  val valid : sigma -> acs -> segment -> Lang.F.pred
  (** Return the formula that tests if a memory state is valid
      (according to {!acs}) in the given memory state at the given
      segment.
  *)

  val scope : sigma -> Mcfg.scope -> varinfo list -> sigma * Lang.F.pred list
  (** Manage the scope of variables.  Returns the updated memory model
      and hypotheses modeling the new validity-scope of the variables. *)

  val global : sigma -> Lang.F.term -> Lang.F.pred
  (** Given a pointer value [p], assumes this pointer [p] (when valid)
      is allocated outside the function frame under analysis. This means
      separated from the formals and locals of the function. *)

  val included : segment -> segment -> Lang.F.pred
  (** Return the formula that tests if two segment are included *)

  val separated : segment -> segment -> Lang.F.pred
  (** Return the formula that tests if two segment are separated *)

end
