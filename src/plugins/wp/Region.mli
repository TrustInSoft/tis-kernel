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
(* --- Logic Path and Regions                                             --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Vset

(** {2 Paths} *)

type path = offset list
and offset =
  | Oindex of Lang.F.term
  | Ofield of field

val access : Lang.F.term -> path -> Lang.F.term
val update : Lang.F.term -> path -> Lang.F.term -> Lang.F.term

(** {2 Regions} *)

type rpath = roffset list
and roffset =
  | Rindex of set
  | Rfield of field

type region

val empty : region
val full : region
val path : path -> region (** Empty, but Full for the path *)
val rpath : rpath -> region (** Empty, but Full for the r-paths *)
val merge : region -> region -> region

val disjoint : region -> region -> Lang.F.pred
val subset : region -> region -> Lang.F.pred
val equal_but : tau -> region -> Lang.F.term -> Lang.F.term -> Lang.F.pred

val vars : region -> Lang.F.Vars.t
val occurs : Lang.F.var -> region -> bool
val pretty : Format.formatter -> region -> unit
