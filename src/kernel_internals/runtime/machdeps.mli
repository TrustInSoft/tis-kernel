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

type t = private {
  short_name: string;
  macro: string option;
  mach: Cil_types.mach;
}

(** [register_machdep ?all_projects ?macro ~short_name mach]
    Register a new machdep and return [mach].

    @param all_projects is set to [true], the new machdep will be
    registered for all new created projects and the current one.
    Otherwise, the machdep will be registered only for the current
    project (Default [false]).
    @param short_name is the name used to set the machdep from the
    command line.
    @param macro is the optional name of the macro to set for the
    preprocessor (to the -D option). This allows to easily switch
    between different machdeps for the same analyzed code. (Default:
    no macro). *)
val register_machdep:
  ?all_projects:bool
  -> ?macro:string
  -> short_name:string
  -> Cil_types.mach
  -> t

(** Returns the list of registered machdeps. *)
val get_registered_machdeps: unit -> t list

(** [get_machdep short_name]
    Return the associated machdep registered with the given
    [short_name]. Return [None] if [short_name] do no match any
    registered machdep.  *)
val get_machdep: string -> t option

(** Equivalent to [get_machdep (Kernel.Machdep.get ())] *)
val get_current_machdep: unit -> t option


(* ************************************************************************* *)
(** {2 Predefined Machdeps} *)
(* ************************************************************************* *)

(** Some predefined {!Cil_types.mach} which specifies machine-dependent
    data about C programs. *)

val x86_16: t
val gcc_x86_16: t
val x86_32: t
val gcc_x86_32: t
val x86_64: t
val gcc_x86_64: t
val ppc_32: t
val apple_ppc_32: t
