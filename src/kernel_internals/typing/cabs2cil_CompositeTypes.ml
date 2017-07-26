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

(** Cabs2cil some helpers for handling composite type fields, and creating
    compinfo and enuminfo. *)

(*
  DEPENDS ON: Nothing.
*)

(*
  ENVIRONMENT:
  + CompInfo.compInfoNameEnv
  + CompInfo.enumInfoNameEnv
  + CompField.anonCompFieldNameId
*)

(*
  NEEDS: Nothing.
*)

(*
  EXPORTS: Nothing.
*)

(*
  EXPORTS:
  + CompField.anonCompFieldName
  + CompField.find_field_offset
*)

open Cil_types
open Cil

module CompInfo = struct

  module H = Hashtbl

  (* Keep a set of self compinfo for composite types *)
  let compInfoNameEnv : (string, compinfo) H.t = H.create 113
  let enumInfoNameEnv : (string, enuminfo) H.t = H.create 113

  let clear_env () =
    H.clear compInfoNameEnv;
    H.clear enumInfoNameEnv

  (* Create the self ref cell and add it to the map. Return also an indication
     if this is a new one. *)
  let createCompInfo (iss: bool) (n: string) ~(norig: string) : compinfo * bool =
    (* Add to the self cell set *)
    let key = (if iss then "struct " else "union ") ^ n in
    try
      H.find compInfoNameEnv key, false (* Only if not already in *)
    with Not_found -> begin
        (* Create a compinfo. This will have "cdefined" false. *)
        let res = mkCompInfo iss n ~norig (fun _ -> []) [] in
        H.add compInfoNameEnv key res;
        res, true
      end

  (* Create the self ref cell and add it to the map. Return an indication
     whether this is a new one. *)
  let createEnumInfo (n: string) ~(norig:string) : enuminfo * bool =
    (* Add to the self cell set *)
    try
      H.find enumInfoNameEnv n, false (* Only if not already in *)
    with Not_found -> begin
        (* Create a enuminfo *)
        let enum = { eorig_name = norig; ename = n; eitems = [];
                     eattr = []; ereferenced = false; ekind = IInt ; }
        in
        H.add enumInfoNameEnv n enum;
        enum, true
      end

end

module CompField = struct

  let anonCompFieldNameId = ref 0
  let anonCompFieldName = "__anonCompField"

  let nextAnonCompFieldNameWithId () =
    incr anonCompFieldNameId;
    anonCompFieldName ^ (string_of_int !anonCompFieldNameId)

  let clear_env () =
    anonCompFieldNameId := 0

  (** Check that [s] starts with the prefix [p]. *)
  let prefix p s =
    let lp = String.length p in
    let ls = String.length s in
    lp <= ls && String.sub s 0 lp = p

  (** returns the offset (can be more than one field in case of unnamed members)
      corresponding to the first field matching the condition.
      @raise Not_found if no such field exists.
  *)
  let find_field_offset cond (fidlist: fieldinfo list) : offset =
    (* Depth first search for the field. This appears to be what GCC does.
       MSVC checks that there are no ambiguous field names, so it does not
       matter how we search *)
    let rec search = function
        [] -> raise Not_found
      | fid :: _ when cond fid ->
        Field(fid, NoOffset)
      | fid :: rest when prefix anonCompFieldName fid.fname -> begin
          match unrollType fid.ftype with
          | TComp (ci, _, _) ->
            (try let off = search ci.cfields in Field(fid,off)
             with Not_found -> search rest  (* Continue searching *))
          | _ ->
            Kernel.abort ~current:true "unnamed field type is not a struct/union"
        end
      | _ :: rest -> search rest
    in
    search fidlist

  let findField n fidlist =
    try
      find_field_offset (fun x -> x.fname = n) fidlist
    with Not_found ->
      Kernel.abort ~current:true "Cannot find field %s" n

end
