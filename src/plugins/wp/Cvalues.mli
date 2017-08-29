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
(* --- Lifting Operations over Memory Values                              --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Memory

(** {2 Int-As-Boolans} *)

val bool_val : Lang.F.unop
val bool_eq : Lang.F.binop
val bool_lt : Lang.F.binop
val bool_neq : Lang.F.binop
val bool_leq : Lang.F.binop
val bool_and : Lang.F.binop
val bool_or  : Lang.F.binop
val is_true  : Lang.F.pred -> Lang.F.term (** [p ? 1 : 0] *)
val is_false : Lang.F.pred -> Lang.F.term (** [p ? 0 : 1] *)

(** {2 Null Values} *)

(** test for null pointer value *)
val null : (Lang.F.term -> Lang.F.pred) Context.value

val is_null : c_object -> Lang.F.term -> Lang.F.pred

(** {2 Typing and Sub-Typing for C and ACSL Types} *)

val is_object : c_object -> 'a value -> Lang.F.pred
val has_ctype : typ -> Lang.F.term -> Lang.F.pred
val has_ltype : logic_type -> Lang.F.term -> Lang.F.pred

val cdomain : typ -> (Lang.F.term -> Lang.F.pred) option
val ldomain : logic_type -> (Lang.F.term -> Lang.F.pred) option

(** {2 ACSL Equality} *)

val equal_object : c_object -> Lang.F.term -> Lang.F.term -> Lang.F.pred
val equal_comp : compinfo -> Lang.F.term -> Lang.F.term -> Lang.F.pred
val equal_array : Matrix.matrix -> Lang.F.term -> Lang.F.term -> Lang.F.pred

(** {2 C and ACSL Constants} *)

val ainf : Lang.F.term option (** Array lower-bound, ie `Some(0)` *)
val asup : int -> Lang.F.term option (** Array upper-bound, ie `Some(n-1)` *)

val constant : constant -> Lang.F.term
val logic_constant : logic_constant -> Lang.F.term
val constant_exp : exp -> Lang.F.term
val constant_term : Cil_types.term -> Lang.F.term

(** {2 Lifting Operations over Memory Values} *)

val map_sloc : ('a -> 'b) -> 'a Memory.sloc -> 'b Memory.sloc
val map_value : ('a -> 'b) -> 'a Memory.value -> 'b Memory.value
val map_logic : ('a -> 'b) -> 'a Memory.logic -> 'b Memory.logic
val plain : logic_type -> Lang.F.term -> 'a Memory.logic

(** {2 ACSL Utilities} *)

type polarity = [ `Positive | `Negative | `NoPolarity ]
val negate : polarity -> polarity

module Logic(M : Memory.Model) :
sig

  open M
  type logic = M.loc Memory.logic

  (** {3 Projections} *)

  val value : logic -> Lang.F.term
  val loc   : logic -> loc
  val vset  : logic -> Vset.set
  val sloc  : logic -> loc sloc list
  val rdescr : loc sloc -> Lang.F.var list * loc * Lang.F.pred

  (** {3 Morphisms} *)

  val map : Lang.F.unop -> logic -> logic
  val map_opp : logic -> logic
  val map_loc : (loc -> loc) -> logic -> logic
  val map_l2t : (loc -> Lang.F.term) -> logic -> logic
  val map_t2l : (Lang.F.term -> loc) -> logic -> logic

  val apply : Lang.F.binop -> logic -> logic -> logic
  val apply_add : logic -> logic -> logic
  val apply_sub : logic -> logic -> logic

  (** {3 Locations} *)

  val field : logic -> fieldinfo -> logic
  val shift : logic -> c_object -> ?size:int -> logic -> logic
  val load : Sigma.t -> c_object -> logic -> logic

  (** {3 Sets of loc-or-values} *)

  val union : logic_type -> logic list -> logic
  val inter : logic_type -> logic list -> logic
  val subset : logic_type -> logic -> logic_type -> logic -> Lang.F.pred

  (** {3 Regions} *)

  type region = loc sloc list

  val separated : (c_object * region) list -> Lang.F.pred
  val included : c_object -> region -> c_object -> region -> Lang.F.pred
  val valid : Sigma.t -> acs -> c_object -> region -> Lang.F.pred

end
