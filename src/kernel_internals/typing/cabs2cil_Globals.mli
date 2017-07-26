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

(** Cabs2cil helpers for file globals handling. *)

open Cil_types

(** [update_fundec_in_theFile vi f] calls [f] on the declaration of the function
    corresponding to varinfo [vi]. *)
val update_fundec_in_theFile  : varinfo -> (global -> unit) -> unit

(** [update_funspec_in_theFile vi spec] merges the provided specification [spec]
    and the specification of the function corresponding to varinfo [vi].*)
val update_funspec_in_theFile : varinfo -> funspec -> unit

(** Return the list of ACSL behavior names attached to a function declaration
    or definition. *)
val find_existing_behaviors : varinfo -> string list

(** [get_formals vi] retrieves the formal variables of a function. *)
val get_formals : varinfo -> varinfo list

(** Register a new global variable at the end of the global file. *)
val cabsPushGlobal : global -> unit

(** Return the final list of globals that are in the current file. Also fix-up
    the location of global C variables that must be turned into definitions. *)
val fileGlobals : unit -> global list

module MustTurnIntoDef : sig
(** Keep track of some variable ids that must be turned into definitions. We do
    this when we encounter what appears a definition of a global but without
    initializer. We leave it a declaration because maybe down the road we see
    another definition with an initializer. But if we don't see any then we turn
    the last such declaration into a definition without initializer. *)
  val mem : varinfo -> bool
  val add : varinfo -> unit
  val remove : varinfo -> unit
end

module AlreadyDefined : sig
  (** Globals that have already been defined. Indexed by the variable name. *)
  val mem : varinfo -> bool
  val find : varinfo -> location
  val add : varinfo -> location -> unit
end

module StaticLocals : sig
  (** Globals that were created due to static local variables. We chose their
      names to be distinct from any global encountered at the time. But we might
      see a global with conflicting name later in the file. *)
  val mem : varinfo -> bool
  val find : varinfo -> varinfo
  val add : varinfo -> unit
  val remove : varinfo -> unit
end

module Typedefs : sig
  (** Typedefs. We chose their names to be distinct from any global encounterd
      at the time. But we might see a global with conflicting name later in the
      file. *)
  val mem : varinfo -> bool
  val find : varinfo -> typeinfo
  val add : string -> typeinfo -> unit
  val remove : varinfo -> unit
end

val clear_env : unit -> unit
