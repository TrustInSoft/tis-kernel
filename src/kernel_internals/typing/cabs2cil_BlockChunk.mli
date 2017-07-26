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

(** Cabs2cil chunks. *)

open Cil_types

type effects = {
  modified : lval list;
  writes : lval list;
  reads : lval list;
}

val no_effects : effects
val make_effects :
  modified : lval list -> writes : lval list -> reads : lval list -> effects

type calls = stmt ref list
type stmt_with_effects = stmt * effects * calls

type t = {
  stmts_with_effects : stmt_with_effects list;
  (** Statements of the chunk.

      This list is built on reverse order.

      Each statements comes with the list of pending modified, written and read
      values. The first category represents values which are to be modified
      during the execution of the chunk and whose new value depends on the
      statement (hence, it is legal to read them). They are removed
      syntactically from the list of reads, but we keep them to avoid spurious
      warnings in presence of aliases. The order of the write is supposed to be
      fixed at this level.

      We also maintain a list of function calls inside the chunk.
      E.g. for G[i] = j, the written lval is G[i], and the read lval are
      G, i, and j.
  *)

  unspecified_order : bool;
  (** Order of evaluation of statements in the chunk is unspecified. *)

  locals : varinfo list;
  (** Variables that are local to the chunk. *)

  cases : stmt list;
  (** A list of case statements (statements with [Case] labels) visible at the
      outer level. *)
}

val d_chunk : Format.formatter -> t -> unit

(** An empty chunk. *)
val empty : t

val is_empty : t -> bool
val is_not_empty : t -> bool

val of_stmt_and_effects : stmt * effects -> t

(** [of_stmt] must not be used during expression translation, as it does not
    take care of the effects of the statement. Use [of_stmt_and_effects]
    instead. *)
val of_stmt : stmt -> t

(** Transforms a chunk into a block. Note that if the chunk has its
    unspecified_order flag set, the resulting block contains a single
    UnspecifiedSequence statement.
    If the chunk consists in a single block, this block will get returned
    directly, unless collapse_block is set to false. *)
val to_block : ghost:bool -> ?collapse_block:bool -> t -> block

(** Encapsulates a chunk in a block and makes a chunk out of it. *)
val of_chunk_encapsulate_in_block : ghost:bool -> t -> t

(** Keep track of the gotos. *)
val add_goto : string -> stmt ref -> unit

(** Keep track of the labels. *)
val add_label : string -> stmt -> unit
val remove_labels : t -> unit

(** Keep track of the stack of statements inside which break instruction can
    be found. *)
val enter_break_env : unit -> unit
val exit_break_env : unit -> unit

(** Make a chunk of code preserving the evaluation of the given expression. *)
val keep_pure_exp :
  make_new_tmp_var:(string -> bool -> typ -> varinfo) ->
  ghost:bool -> exp -> location -> t

(** We can drop a chunk if it does not have labels inside. *)
val can_drop : t -> bool

(** Add a statement at the end. Never refer to this statement again
    after you call this. *)
val (+++) : t -> stmt * effects -> t

(** Append two chunks. Never refer to the original chunks after you call
    this. And especially never share c2 with somebody else. *)
val (@@) : t -> t * bool -> t

(** We can duplicate a chunk if it has a few simple statements, and if it does
    not have cases. Raises [Failure] if you should not duplicate this chunk. *)
val duplicate : t -> t

val unspecified_chunk : t -> t
val local_var_chunk : t -> varinfo -> t
val remove_effects : t -> t

(** Get the first statement in a chunk. Might need to change the statements in
    the chunk. *)
val get_first_stmt_in_chunk : ghost:bool -> loc:location -> t ->
  stmt * stmt_with_effects list

val add_reads : location -> lval list -> t -> t

(** Remove all generated labels in a chunk from the previous tables; useful
    when dropping code, like expressions in arguments of sizeof or
    __builtin_constant_p. *)
val remove_reads : lval -> t -> t

module Make : sig
  val skip_chunk : t
  val return_chunk : ghost:bool -> exp option -> location -> t
  val if_chunk : ghost:bool -> exp -> location -> t -> t -> t
  val loop_chunk : ghost:bool -> code_annotation list -> t -> t
  val break_chunk : ghost:bool -> location -> t
  val continue_chunk : ghost:bool -> location -> t
  val goto_chunk : ghost:bool -> string -> location -> t
  val case_range_chunk : ghost:bool -> case list -> location -> t -> t
  val default_chunk : ghost:bool -> location -> t -> t
  val switch_chunk : ghost:bool -> exp -> t -> location -> t
end

val mk_function_body : ghost:bool -> t -> block

module Logic_labels : sig
  (** On the contrary to C, use of labels in the logic
      obeys block scope rules. We keep track of these scopes here. *)
  val find_label : string -> stmt ref
  val enter_scope : unit -> unit
  val exit_scope : unit -> unit
  val add_current_label : string -> unit
  val reset_current_label : unit -> unit
end
