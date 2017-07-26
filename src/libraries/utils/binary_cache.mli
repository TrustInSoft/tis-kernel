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

(** Very low-level abstract functorial caches. Do not use them unless
    you understand what happens in this module, and do not forget that
    those caches are not aware of projects. *)

val cache_size: int
(** Size of the caches. Controlled by environment variable
    [memory_footprint_var_name]. *)

module type Cacheable = sig
  type t
  val hash : t -> int
  val sentinel : t
  val equal : t -> t -> bool
end

module type Result = sig
  type t
  val sentinel : t
end

type elt1 = A | B of int

module Symmetric_Binary(H : Cacheable)(R : Result): sig
  val clear : unit -> unit
  val merge : (H.t -> H.t -> R.t) -> H.t -> H.t -> R.t
end

module Binary_Predicate(H0 : Cacheable)(H1 : Cacheable): sig
  val clear : unit -> unit
  val merge : (H0.t -> H1.t -> bool) -> H0.t -> H1.t -> bool
end

module Symmetric_Binary_Predicate(H0 : Cacheable): sig
  val clear : unit -> unit
  val merge : (H0.t -> H0.t -> bool) -> H0.t -> H0.t -> bool
end

module Arity_One(H : Cacheable)(R : Result): sig
  val clear : unit -> unit
  val merge : (H.t -> R.t) -> H.t -> R.t
end

module Arity_Two(H0 : Cacheable)(H1 : Cacheable)(R : Result): sig
  val clear : unit -> unit
  val merge : (H0.t -> H1.t -> R.t) -> H0.t -> H1.t -> R.t
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
