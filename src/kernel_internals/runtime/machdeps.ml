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

open Cil_types

let repr_x86_32 = {
  version = "gcc 4.0.3 - X86-32bits mode";
  compiler = "generic";
  sizeof_short = 2;
  sizeof_int = 4;
  sizeof_long = 4;
  sizeof_longlong = 8;
  sizeof_int128 = 0; (* not applicable *)
  sizeof_ptr = 4;
  sizeof_float = 4;
  sizeof_double = 8;
  sizeof_longdouble = 16;
  sizeof_void = 1;
  sizeof_fun = 1;
  size_t = "unsigned int";
  wchar_t = "int";
  ptrdiff_t = "int";
  alignof_short = 2;
  alignof_int = 4;
  alignof_long = 4;
  alignof_longlong = 4;
  alignof_int128 = 0; (* not applicable *)
  alignof_ptr = 4;
  alignof_float = 4;
  alignof_double = 4;
  alignof_longdouble = 4;
  alignof_str = 1;
  alignof_fun = 1;
  alignof_aligned= 16;
  char_is_unsigned = false;
  char_bit = 8;
  const_string_literals = true;
  little_endian = true;
  has__builtin_va_list = true;
  __thread_is_keyword = true;
  has_int128 = false;
}

type t = {
  short_name: string;
  macro: string option;
  mach: Cil_types.mach;
}

module T = Datatype.Make_with_collections
    (struct
      include Datatype.Serializable_undefined
      type nonrec t = t
      let reprs = [ { short_name = ""; macro = None; mach = repr_x86_32; } ]
      let name = "Machdeps.T"
      let compare = Pervasives.compare
      let equal = Pervasives.(=)
      let hash = Hashtbl.hash
      let copy = Datatype.identity
    end)

module Machdeps =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl) (* the machdep's shortname *)
    (Datatype.Pair
       (T)
       (Datatype.Bool)) (* if an "all project" machdep or not *)
    (struct
      let name = "Machdeps.Machdeps"
      let size = 5
      let dependencies = []
    end)

(* Note: since it should persist between project, it should not
   be a state builder *)
let machdeps_all_project: t list ref = ref []

let is_loading_stage_done = ref false
let () =
  Cmdline.run_after_loading_stage (fun () -> is_loading_stage_done := true)

let register_machdep ?(all_projects=false) ?macro ~short_name mach =
  if Machdeps.mem short_name then
    Kernel.abort
      "machdep under name `%s' is already registered"
      short_name
  else begin
    let t = { short_name; macro; mach } in
    (* Note: "all projects" machdeps should be registered now in order
       to be recognized on the command line. However we should keep
       the information if the machdep is an "all projects" one or not
       in order to not register it twice with the
       [Project.register_create_hook] *)
    if all_projects then
      machdeps_all_project := t :: !machdeps_all_project;
    Machdeps.add short_name (t, all_projects);
    t
  end

let () =
  Project.register_create_hook
    (fun p ->
       Project.on
         ~selection:(State_selection.singleton Machdeps.self)
         p
         (fun machdeps ->
            List.iter
              (fun t ->
                 let reg_m =
                   try Some (Machdeps.find t.short_name)
                   with Not_found -> None
                 in
                 match reg_m with
                 | Some (_, false) ->
                   (* Machdeps that do not persist for "all projects" cannot
                      be registered here in this hook *)
                   assert false
                 | Some (_, true) ->
                   (* Otherwise, during the first initial project creation,
                      an "all project" machdep can already be registered,
                      in this case: do nothing *)
                   ()
                 | None ->
                   (* If not registered, then register it *)
                   Machdeps.add t.short_name (t, true))
              machdeps)
         !machdeps_all_project)

let internal_register_machdep ~short_name ~macro mach =
  let t = register_machdep ~all_projects:true ~macro ~short_name mach in
  t.mach

let get_registered_machdeps () =
  Machdeps.fold (fun _ (t, _) acc -> t :: acc) []

let get_machdep sn =
  try Some (fst (Machdeps.find sn))
  with Not_found -> None

let get_current_machdep () =
  get_machdep (Kernel.Machdep.get ())


(* ************************************************************************* *)
(** {2 Predefined Machdeps} *)
(* ************************************************************************* *)

let macro_x86_16 = "__FC_MACHDEP_X86_16"
let macro_x86_32 = "__FC_MACHDEP_X86_32"
let macro_x86_64 = "__FC_MACHDEP_X86_64"

let x86_16 =
  internal_register_machdep
    ~short_name:"x86_16"
    ~macro:macro_x86_16
    { version =
        "x86 16 bits mode (gcc like compiler) with big or huge memory model";
      compiler = "generic";
      sizeof_short = 2;
      sizeof_int = 2;
      sizeof_long = 4;
      sizeof_longlong = 8;
      sizeof_int128 = 0; (* not applicable *)
      sizeof_ptr = 4;
      sizeof_float = 4;
      sizeof_double = 8;
      sizeof_longdouble = 12;
      sizeof_void = 1;
      sizeof_fun = 1;
      size_t = "unsigned int";
      wchar_t = "int";
      ptrdiff_t = "int";
      alignof_short = 2;
      alignof_int = 2;
      alignof_long = 4;
      alignof_longlong = 4;
      alignof_int128 = 0; (* not applicable *)
      alignof_ptr = 4;
      alignof_float = 2;
      alignof_double = 8;
      alignof_longdouble = 16;
      alignof_str = 1;
      alignof_fun = 1;
      alignof_aligned= 8;
      (* I don't know if attribute aligned is supported by any 16bits
         compiler. *)
      char_is_unsigned = false;
      char_bit = 8;
      const_string_literals = true;
      little_endian = true;
      has__builtin_va_list = true;
      __thread_is_keyword = true;
      has_int128 = false; }

let gcc_x86_16 =
  internal_register_machdep
    ~short_name:"gcc_x86_16"
    ~macro:macro_x86_16
    { x86_16 with compiler = "gcc" }

let x86_32 =
  internal_register_machdep
    ~short_name:"x86_32"
    ~macro:macro_x86_32
    repr_x86_32

let gcc_x86_32 =
  internal_register_machdep
    ~short_name:"gcc_x86_32"
    ~macro:macro_x86_32
    { x86_32 with compiler = "gcc" }

let x86_64 =
  internal_register_machdep
    ~short_name:"x86_64"
    ~macro:macro_x86_64
    { version = "gcc 4.0.3 AMD64";
      compiler = "generic";
      sizeof_short = 2;
      sizeof_int = 4;
      sizeof_long = 8;
      sizeof_longlong = 8;
      sizeof_int128 = 0; (* not applicable *)
      sizeof_ptr = 8;
      sizeof_float = 4;
      sizeof_double = 8;
      sizeof_longdouble = 16;
      sizeof_void = 1;
      sizeof_fun = 1;
      size_t = "unsigned long";
      wchar_t = "int";
      ptrdiff_t = "long";
      alignof_short = 2;
      alignof_int = 4;
      alignof_long = 8;
      alignof_longlong = 8;
      alignof_int128 = 0; (* not applicable *)
      alignof_ptr = 8;
      alignof_float = 4;
      alignof_double = 8;
      alignof_longdouble = 16;
      alignof_str = 1;
      alignof_fun = 1;
      alignof_aligned= 16;
      char_is_unsigned = false;
      char_bit = 8;
      const_string_literals = true;
      little_endian = true;
      has__builtin_va_list = true;
      __thread_is_keyword = true;
      has_int128 = false; }

let gcc_x86_64 =
  internal_register_machdep
    ~short_name:"gcc_x86_64"
    ~macro:macro_x86_64
    { x86_64 with
      compiler = "gcc";
      has_int128 = true;
      sizeof_int128 = 16;
      alignof_int128 = 16; }

let ppc_32 =
  internal_register_machdep
    ~short_name:"ppc_32"
    ~macro:"__FC_MACHDEP_PPC_32"
    { version = "32-bit PowerPC platform";
      compiler = "generic";
      sizeof_short = 2;
      sizeof_int = 4;
      sizeof_long = 4;
      sizeof_longlong = 8;
      sizeof_int128 = 0; (* not applicable *)
      sizeof_ptr = 4;
      sizeof_float = 4;
      sizeof_double = 8;
      sizeof_longdouble = 16;
      sizeof_void = 1;
      sizeof_fun = 1;
      size_t = "unsigned int";
      wchar_t = "int";
      ptrdiff_t = "int";
      alignof_short = 2;
      alignof_int = 4;
      alignof_long = 4;
      alignof_longlong = 4;
      alignof_int128 = 0; (* not applicable *)
      alignof_ptr = 4;
      alignof_float = 4;
      alignof_double = 4;
      alignof_longdouble = 16;
      alignof_str = 1;
      alignof_fun = 4;
      alignof_aligned = 16;
      char_is_unsigned = true;
      char_bit = 8;
      const_string_literals = true;
      little_endian = false;
      has__builtin_va_list = true;
      __thread_is_keyword = true;
      has_int128 = false; }

let apple_ppc_32 =
  internal_register_machdep
    ~short_name:"apple_ppc_32"
    ~macro:"__FC_MACHDEP_APPLE_PPC_32"
    { ppc_32 with
      version = "4.0.1 (Apple Computer, Inc. build 5367)";
      compiler = "standard";
      char_is_unsigned = false; }


(* ************************************************************************* *)
(** {2 Additional behaviors of the -machdep option} *)
(* ************************************************************************* *)

let pretty_machdeps fmt =
  Machdeps.iter (fun x _ -> Format.fprintf fmt "@ %s" x)

(* If "help" is given to the option -machdep, prints the list of registered
   machdeps and exits *)
let () =
  let machdep_help () =
    let m = Kernel.Machdep.get () in
    if m = "help" then begin
      Kernel.feedback
        "@[supported machines are%t@ (default is x86_32).@]"
        pretty_machdeps;
      raise Cmdline.Exit
    end else
      Cmdline.nop
  in
  Cmdline.run_after_exiting_stage machdep_help

(* Check if the given machdep on the command line exists among the
   registered ones, otherwise fails now *)
let () =
  let check_machdep () =
    let m = Kernel.Machdep.get () in
    if not (Machdeps.mem m) then
      Kernel.abort
        "@[unsupported machine %s.@ Try one of%t.@]"
        m pretty_machdeps
  in
  Cmdline.run_after_configuring_stage check_machdep
