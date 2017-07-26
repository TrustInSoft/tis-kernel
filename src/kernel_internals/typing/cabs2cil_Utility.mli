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

(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(** Mutually independent helper functions of Cabs2cil. *)

open Cil_types

module Parameters : sig

  (** A hook into the code that creates temporary local vars. By default this
      is the identity function, but you can overwrite it if you need to change
      the types of cabs2cil-introduced temp variables. *)
  val typeForInsertedVar : (typ -> typ) ref

  (** Like [typeForInsertedVar], but for casts.
      Casts in the source code are exempt from this hook. *)
  val typeForInsertedCast : (exp -> typ -> typ -> typ) ref

  (** To avoid generating backward gotos, we treat while loops as non-while
      ones, adding a marker for continue. (useful for Jessie) *)
  val doTransformWhile : bool ref

  (** If called, sets a flag so that [continue] in while loops get transformed
      into forward gotos, like it is already done in do-while and for loops. *)
  val setDoTransformWhile : unit -> unit

  (** To avoid generating forward ingoing gotos, we translate conditionals in
      an alternate way. (useful for Jessie) *)
  val doAlternateConditional : bool ref
  val setDoAlternateConditional : unit -> unit

end

(** Check if the expression is a lvalue of type void and abort the analysis with
    appropriate message if it is. *)
val checkVoidLval : exp -> typ -> unit

(** Unknown dummy begin and end locations whose dummy filename is suffixed by
    the passed string. This is used as a placeholder and shall not be in the
    final AST. *)
val cabslu : string -> Lexing.position * Lexing.position

(** Dummy function declaration placeholder, should cause an error if actually
    used anywhere. *)
val dummy_function : fundec

(** Search a statement for labels. *)
val canDropStatement : stmt -> bool

(** Drop all casts wrapping an expression. *)
val dropCasts : exp -> exp

(** C99 6.3.2.1:2: l-values used as r-values lose their qualifier. By default,
    we drop qualifiers, and recover them for the few operators that are
    exceptions, also listed in 6.3.2.1:2 *)
val dropQualifiers : typ -> typ

(** Check if a type is a transparent union, and return the first field if it
    is. *)
val isTransparentUnion : typ -> fieldinfo option

(** Make a full Cabs expression out of a Cabs expression location and a Cabs
    expression node. *)
val cabs_exp : Cabs.cabsloc -> Cabs.cabsexp -> Cabs.expression

(** Check if the passed string is the name of the old-style MSVC GCC before 3.4
    variadic argument name. *)
val isOldStyleVarArgName : string -> bool

(** Check if the passed string is the name of the old-style MSVC GCC before 3.4
    variadic argument type. *)
val isOldStyleVarArgTypeName : string -> bool

(** Apply [mkAddrOf] after marking variable whose address is taken. *)
val mkAddrOfAndMark : location -> lhost * offset -> exp

(** Make a [StartOf] expression of the provided lvalue. *)
val mkStartOfAndMark : location -> lhost * offset -> exp

(** Evaluate constants to [CTrue] (non-zero) or [CFalse] (zero). *)
val isConstTrueFalse : constant -> [ `CTrue | `CFalse ]

(** Evaluate expressions to [`CTrue], [`CFalse] or [`CUnknown]. *)
val isExpTrueFalse : exp -> [ `CTrue | `CFalse | `CUnknown ]

(** Is this expression a zero constant (potentially wrapped in casts)? *)
val isCabsZeroExp : Cabs.expression -> bool

(** Our own version of [Cil.addAttributes] that does not allow duplicates. *)
val cabsAddAttributes : attribute list -> attributes -> attributes

(** Our own version of [Cil.typeAddAttributes]. *)
val cabsTypeAddAttributes : attributes -> typ -> typ

(** [replaceLastInList lst how] returns the list [lst] with the function [how]
    applied only to its last element and all the other elements unchanged. *)
val replaceLastInList : 'a list -> ('a -> 'a) -> 'a list

(** Convert a Cabs binary operator to its corresponding Cil version. *)
val convBinOp : Cabs.binary_operator -> binop

module Is_dangerous : sig
  (** Is expression potentially volatile? *)
  val exp : exp -> bool
end

module Check_no_locals : sig
  (** Check for accesses to local variables in a static initializer. *)
  val in_initializer : init -> unit
end

(** Const-fold any expressions that appear as array lengths in this type. *)
val constFoldType : typ -> typ

(** Our own version of [Cil.lenOfArray]. *)
val integerArrayLength : exp option -> int

module StripParen : sig
  (** Strip parentheses from every expression in a file. *)
  val file : Cabs.file -> Cabs.file
end

(** Count the named fields of a structure. *)
val namedMembersCount : Cabs.field_group list -> int

(** Checks if given compinfo is used in the given type (useful to check a
    composite type's circularity). *)
val comp_used_in_typ : compinfo -> typ -> bool

(** Check if the type pointed to (by the pointer type is of size zero). The only
    types that have size equal zero are: the void type, a zero-length array
    (which is a GNU extension), and an empty structure (also a GCC extension).
*)
val isPointedTypeSizeZero : typ -> bool

(**
   The way formals are handled now might generate incorrect types, in the
   sense that they refer to a varinfo (in the case of VLA depending on a
   previously declared formal) that exists only during the call to do_type.
   We replace them here with the definitive version of the formals' varinfos.
*)
val fixFormalsType : varinfo list -> unit
