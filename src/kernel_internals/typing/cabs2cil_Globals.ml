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

(*
  DEPENDS ON: Nothing.
*)

(*
  ENVIRONMENT:
  + theFile
  + theFileTypes
  + theFileVars
  + mustTurnIntoDef
  + alreadyDefined
  + staticLocals
  + typedefs
*)

(*
  EXPORTS: Nothing.
*)

open Cil_types

module H = Hashtbl
module IH = Datatype.Int.Hashtbl

(* We collect here the program. *)
let theFile : global list ref = ref []
let theFileTypes : global list ref = ref []
(* This [hashtbl] contains the varinfo-indexed globals of theFile.
   They are duplicated here for faster lookup *)
let theFileVars : global Cil_datatype.Varinfo.Hashtbl.t =
  Cil_datatype.Varinfo.Hashtbl.create 13

let findVarInTheFile variable =
  try List.rev (Cil_datatype.Varinfo.Hashtbl.find_all theFileVars variable)
  with Not_found -> []

let update_fundec_in_theFile varinfo (f : global -> unit) =
  let rec aux = function
    | [] -> assert false
    | (GFunDecl _ as g) :: _ -> f g
    | _ :: tl -> aux tl
  in
  aux (findVarInTheFile varinfo)

let update_funspec_in_theFile varinfo funspec =
  let rec aux = function
    | [] -> assert false
    | GFun (fundec, _) :: _ ->
      Cil.CurrentLoc.set varinfo.vdecl;
      let merging_verbosity = Logic_utils.MergeVerbosely fundec.svar in
      Logic_utils.merge_funspec merging_verbosity fundec.sspec funspec
    | _ :: remaining_globals -> aux remaining_globals
  in
  aux (findVarInTheFile varinfo)

let find_existing_behaviors varinfo =
  let behaviors spec =
    List.map (fun behavior -> behavior.b_name) spec.spec_behavior
  in
  let aux acc = function
    | GFun(fundec, _) -> (behaviors fundec.sspec) @ acc
    | GFunDecl (funspec, _, _)  -> behaviors funspec @ acc
    | _ -> acc
  in
  List.fold_left aux [] (findVarInTheFile varinfo)

let get_formals varinfo =
  let rec aux = function
    | [] -> assert false
    | GFun (fundec, _)::_ -> fundec.sformals
    | _ :: remaining_globals -> aux remaining_globals
  in
  aux (findVarInTheFile varinfo)

let required_builtins = [ "Frama_C_bzero"; "Frama_C_copy_block" ]

let cabsPushGlobal global =
  begin
    match global with
    | GFun ({ svar = varinfo; _ },_) | GFunDecl (_, varinfo, _)
      when List.mem varinfo.vname required_builtins ->
      ignore (Cil.Frama_c_builtins.memo (fun _ -> varinfo) varinfo.vname)
    | _ -> ();
  end;
  Cil.pushGlobal global ~types:theFileTypes ~variables:theFile;
  match global with
  | GVar (varinfo, _, _)
  | GVarDecl (varinfo, _)
  | GFun ({svar = varinfo; _ }, _)
  | GFunDecl (_, varinfo, _) ->
    (* Do 'add' and not 'replace' here, as we may store both
       declarations and definitions for the same varinfo *)
    Cil_datatype.Varinfo.Hashtbl.add theFileVars varinfo global
  | _ -> ()

module MustTurnIntoDef = struct
  (* Keep track of some variable ids that must be turned into definitions. We
     do this when we encounter what appears a definition of a global but
     without initializer. We leave it a declaration because maybe down the road
     we see another definition with an initializer. But if we don't see any
     then we turn the last such declaration into a definition without
     initializer *)
  let mustTurnIntoDef: bool IH.t = IH.create 117
  let mem varinfo = IH.mem mustTurnIntoDef varinfo.vid
  let add varinfo = Datatype.Int.Hashtbl.add mustTurnIntoDef varinfo.vid true
  let remove varinfo = IH.remove mustTurnIntoDef varinfo.vid
  let clear () = IH.clear mustTurnIntoDef
end

module AlreadyDefined = struct
  (* Globals that have already been defined. Indexed by the variable name. *)
  let alreadyDefined : (string, location) H.t = H.create 117
  let find varinfo = Hashtbl.find alreadyDefined varinfo.vname
  let add varinfo location = Hashtbl.add alreadyDefined varinfo.vname location
  let mem varinfo = Hashtbl.mem alreadyDefined varinfo.vname
  let clear () = H.clear alreadyDefined
end

module StaticLocals = struct
  (* Globals that were created due to static local variables. We chose their
     names to be distinct from any global encountered at the time. But we might
     see a global with conflicting name later in the file. *)
  let staticLocals : (string, varinfo) H.t = H.create 13
  let find varinfo = Hashtbl.find staticLocals varinfo.vname
  let add varinfo = Hashtbl.add staticLocals varinfo.vname varinfo
  let mem varinfo = Hashtbl.mem staticLocals varinfo.vname
  let remove varinfo = Hashtbl.remove staticLocals varinfo.vname
  let clear () = H.clear staticLocals
end

module Typedefs = struct
  (* Typedefs. We chose their names to be distinct from any global encounterd
     at the time. But we might see a global with conflicting name later in the
     file *)
  let typedefs : (string, typeinfo) H.t = H.create 13
  let find varinfo = Hashtbl.find typedefs varinfo.vname
  let add name typeinfo = Hashtbl.add typedefs name typeinfo
  let mem varinfo = Hashtbl.mem typedefs varinfo.vname
  let remove varinfo = Hashtbl.remove typedefs varinfo.vname
  let clear () = H.clear typedefs
end

let fileGlobals () =
  let rec rev_onto (tail : global list) = function
    | [] -> tail
    | GVarDecl (varinfo, _) :: remaining_globals
      when varinfo.vstorage != Extern
        && MustTurnIntoDef.mem varinfo ->
      MustTurnIntoDef.remove varinfo;
      (* Use the location of [varinfo] instead of the one carried by [GVarDecl].
         Maybe we found in the same file a declaration and then a tentative
         definition. In this case, both are [GVarDecl], but the location carried
         by [varinfo] is the location of the tentative definition, which is more
         useful. *)
      rev_onto
        (GVar (varinfo, {init = None}, varinfo.vdecl) :: tail)
        remaining_globals
    | global :: remaining_globals ->
      rev_onto (global :: tail) remaining_globals
  in
  rev_onto (rev_onto [] !theFile) !theFileTypes

let clear_env () =
  (* The internal global variables. *)
  theFile := [];
  theFileTypes := [];
  Cil_datatype.Varinfo.Hashtbl.clear theFileVars;
  (* The exported global variables. *)
  MustTurnIntoDef.clear ();
  AlreadyDefined.clear ();
  StaticLocals.clear ();
  Typedefs.clear ()

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
