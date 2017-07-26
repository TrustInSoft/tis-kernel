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

(** Value analysis builtin shipped with Frama-C, more efficient than their
    equivalent in C *)

exception Invalid_nb_of_args of int

(** Register the given OCaml function as a builtin, that will be used
    instead of the Cil C function of the same name *)
val register_builtin: string -> Db.Value.builtin_sig -> unit

(** Returns a list of the pairs (name, builtin_sig) registered via
    [register_builtin].
    @since Magnesium-20151001+dev *)
val registered_builtins: unit -> (string * Db.Value.builtin_sig) list

(** Find a previously registered builtin. Raises [Not_found] if no such
    builtin exists. *)
val find_builtin: string -> Db.Value.builtin_sig

(** If the function is a builtin it returns the corresponding triple (name,
    builtin_sig, override). If [override] is true, then it is a kind of builtin
    which overrides a Cil function with a Caml one, but uses the Cil one as
    backup if the Caml one fails. *)
val get_abstract_function_if_builtin : Kernel_function.t ->
  (string * Db.Value.builtin_sig * bool) option

(** Returns true iff the function is a builtin. *)
val is_function_a_builtin : Kernel_function.t -> bool

(** Does the builtin table contain an entry for [name]? *)
val mem_builtin: string -> bool

(** Should the given function be replaced by a call to a builtin *)
val overridden_by_builtin: Kernel_function.t -> bool

val clobbered_set_from_ret: Cvalue.Model.t -> Cvalue.V.t -> Base.SetLattice.t
(** [clobbered_set_from_ret state ret] can be used for functions that return
    a pointer to where they have written some data. It returns all the bases
    of [ret] whose contents may contain local variables. *)


(** Builtins with multiple names; the lookup is done using a distinctive
    prefix *)
(* TODO: move the lookup mechanism into find_builtin *)
val dump_state: Db.Value.builtin_sig
val dump_args: string -> Db.Value.builtin_sig
val dump_state_file: string -> Db.Value.builtin_sig


(** Emit an ACSL assert (using \warning predicate) to signal that the
    builtin encountered an alarm described by the string.
    [kind] is used to describe the alarm, similarly to [Alarms.get_name]. *)
val emit_alarm: kind:string -> text:string -> unit


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
