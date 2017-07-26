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

(** Cabs2cil helpers for handling pragmas. *)

(*
  DEPENDS ON: Nothing.
*)

(*
  ENVIRONMENT:
  + current_pragma_align
  + pragma_align_by_struct
  + packing_pragma_stack
  + current_packing_pragma
*)

(*
  EXPORTS: Nothing.
*)

open Cil_types
open Cil

module H = Hashtbl

(* ICC align/noalign pragmas (not supported by GCC/MSVC with this syntax).
   Implemented by translating them to 'aligned' attributes. Currently,
   only default and noalign are supported, not explicit alignment values.
   Cf. www.slac.stanford.edu/grp/cd/soft/rmx/manuals/IC_386.PDF *)
let current_pragma_align = ref (None : bool option)
let pragma_align_by_struct = H.create 17

let process_align_pragma name args =
  let aux pname v =
    (if Cil.msvcMode () || Cil.gccMode ()
     then Kernel.warning else Kernel.debug ~level:1 ?dkey:None)
      ~current:true "Parsing ICC '%s' pragma." pname;
    match args with
    | [] -> current_pragma_align := Some v
    | l ->
      List.iter
        (function
          | AStr s | ACons (s, _) -> H.replace pragma_align_by_struct s v
          | _ -> Kernel.warning ~current:true
                   "Unsupported '%s' pragma not honored by Frama-C." pname
        ) l
  in
  match name with
  | "align" -> aux "align" true
  | "noalign" -> aux "noalign" false
  | _ -> ()

let align_pragma_for_struct sname =
  try Some (H.find pragma_align_by_struct sname)
  with Not_found -> !current_pragma_align

(* The syntax and semantics for the pack pragmas are GCC's.
   The MSVC ones seems quite different and specific code should
   be written so support it. *)

(* The pack pragma stack *)
let packing_pragma_stack = Stack.create ()

(* The current pack pragma *)
let current_packing_pragma = ref None
let process_pack_pragma name args =
  begin match name with
    | "pack" -> begin
        if Cil.msvcMode () then
          Kernel.warning ~current:true
            "'pack' pragmas are probably incorrect in MSVC mode. \
             Using GCC like pragmas.";
        match args with
        | [] (*  #pragma pack() *) ->
          current_packing_pragma := None; None
        | [AInt n] (* #pragma pack(n) *) ->
          current_packing_pragma := Some n; None
        | [ACons ("push",[])] (* #pragma pack(push) *) ->
          Stack.push !current_packing_pragma packing_pragma_stack; None
        | [ACons ("push",[]); AInt n] (* #pragma pack(push,n) *) ->
          Stack.push !current_packing_pragma packing_pragma_stack;
          current_packing_pragma:= Some n; None
        | [ACons ("pop",[])] (* #pragma pack(pop) *) ->
          begin try
              current_packing_pragma := Stack.pop packing_pragma_stack;
              None
            with Stack.Empty ->
              Kernel.warning ~current:true
                "Inconsistent #pragma pack(pop). Using default packing.";
              current_packing_pragma := None; None
          end
        | [ACons ("show",[])] (* #pragma pack(show) *) ->
          Some (Attr (name, args))
        | _ ->
          Kernel.warning ~current:true
            "Unsupported packing pragma not honored by TrustInSoft Kernel.";
          Some (Attr (name, args))
      end
    | _ -> Some (Attr (name, args))
  end

let force_packed_attribute a =
  if hasAttribute "packed" a then a
  else addAttribute (Attr("packed",[])) a

let add_packing_attributes s a =
  match !current_packing_pragma, align_pragma_for_struct s.corig_name with
  | None, None -> a
  | Some n, _ -> (* ignore 'align' pragma if some 'pack' pragmas are present
                    (no known compiler support both syntaxes) *)
    let with_aligned_attributes =
      match filterAttributes "aligned" a with
      | [] (* no aligned attributes yet. Add the global one. *) ->
        addAttribute (Attr("aligned",[AInt n])) a
      | [Attr("aligned",[AInt local])]
        (* The largest aligned wins with GCC. Don't know
           with other compilers. *) ->
        addAttribute (Attr("aligned",[AInt (Integer.max local n)]))
          (dropAttribute "aligned" a)
      | [Attr("aligned",[])] -> (* This one always wins as it is the
                                   biggest available on the plateform. *)
        a
      | _ -> Kernel.warning ~current:true
               "Unknown aligned attribute syntax: keeping it as is and \
                adding new one.";
        addAttribute (Attr("aligned",[AInt n])) a
    in
    force_packed_attribute with_aligned_attributes

  | None, Some true ->
    dropAttribute "aligned" a
  | None, Some false ->
    force_packed_attribute
      (addAttribute
         (Attr("aligned",[AInt Integer.one]))
         (dropAttribute "aligned" a))

let clear_env () =
  Stack.clear packing_pragma_stack;
  current_packing_pragma := None;
  H.clear pragma_align_by_struct;
  current_pragma_align := None
