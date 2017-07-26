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

open CilE

(** Emission of alarms. *)

type syntactic_context =
  | SyNone
  | SyCallResult
  | SyBinOp of Cil_types.exp * Cil_types.binop * Cil_types.exp * Cil_types.exp
  | SyUnOp of  Cil_types.exp
  | SyMem of  Cil_types.lval
  | SyMemLogic of Cil_types.term
  | SySep of Cil_types.lval * Cil_types.lval

val start_stmt : Cil_types.kinstr -> unit
val end_stmt : unit -> unit
val current_stmt : unit -> Cil_types.kinstr

val set_syntactic_context : syntactic_context -> unit

val push_syntactic_context : syntactic_context -> unit
(** Save the current syntactic context (by pushing ot on the stack), then set
    the syntactic context to the new value. *)

val pop_syntactic_context : unit -> unit
(** Load the last syntactic context value that was saved (i.e. pop it from the
    stack). *)

val pp_current_syntactic_context : Format.formatter -> unit

val do_warn: alarm_behavior -> (unit -> unit) -> unit

val warn_pointer_arithmetic : warn_mode -> unit
(** pointer arithmetic going out of bounds *)

val warn_div : warn_mode -> addresses:bool -> unit
(** division. If [addresses] holds, also emit an alarm about the denominator
    not being comparable to \null. *)

val warn_div_overflow : warn_mode -> unit
(** Warn when the numerator contains its minimal value and the denominator
    contains -1. *)

val warn_shift : warn_mode -> int option -> unit
(** Warn that the RHS of a shift operator must be positive, and optionnally
    less than the given size. *)

val warn_shift_left_positive : warn_mode -> unit
(** Warn that the LHS of the left shift operator must be positive. *)

val warn_mem_read : warn_mode -> unit
val warn_mem_write : warn_mode -> unit
val warn_integer_overflow :
  warn_mode -> signed:bool -> min:Integer.t option -> max:Integer.t option -> unit
val warn_float_to_int_overflow:
  warn_mode ->
  Integer.t option -> Integer.t option -> (Format.formatter -> unit) -> unit

val warn_index : warn_mode -> non_negative:bool -> respects_upper_bound:bool ->
  range:string -> unit
(** [warn_index w ~for_access ~non_negative ~range] emits a warning
    signaling an out of bounds access.
    The expression used as index is taken from the syntactic context.
    [range] is used to display the inferred values for the index.
    If [non_negative] is false, an assertion of the form "0 <= e" is generated,
    if [respects_upper_bound] is false, an assertion of the form
    "e < upper_bound" is generated; and if both are false, both assertions are
    generated.
*)

val warn_index_for_address : warn_mode -> allow_one_past: bool ->
  non_negative:bool -> respects_upper_bound:bool -> range:string -> unit
(** Same as [warn_index] but for building an address without dereferencing it.
    if [allow_one_past], "e <= upper_bound" is generated instead of
    "e < upper_bound" to account for pointers "one past".
*)

val warn_pointer_comparison : Cil_types.typ -> warn_mode -> unit
(** warn on invalid pointer comparison. The first argument is the type
    of the arguments of the comparison *)

val warn_valid_string : warn_mode -> unit
val warn_valid_wstring : warn_mode -> unit

val warn_comparable_char_blocks :
  warn_mode -> Cil_types.exp -> Cil_types.exp -> Cil_types.exp -> unit
val warn_comparable_wchar_blocks :
  warn_mode -> Cil_types.exp -> Cil_types.exp -> Cil_types.exp -> unit

val warn_pointer_subtraction : warn_mode -> unit
val warn_nan_infinite:
  warn_mode -> Cil_types.fkind option -> (Format.formatter -> unit) -> unit
val warn_uninitialized : warn_mode -> unit
val warn_escapingaddr : warn_mode -> unit
(** warning to be emitted when two incompatible accesses to a location are
    done in unspecified order. Must be called in a [SyNone] or [SySep] context.
*)
val warn_separated : warn_mode -> unit
val warn_overlap : (Format.formatter -> unit) -> warn_mode -> unit

val warn_incompatible_fun_pointer:
  Eval_typ.functions_resolution -> warn_mode -> unit

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
