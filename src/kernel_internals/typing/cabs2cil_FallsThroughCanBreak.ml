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

(** Cabs2cil fooFallsThrough and fooCanBreak. *)

(*
  DEPENDS ON:
  + BlockChunk.chunk (type)
*)

(*
  ENVIRONMENT: None.
*)

(*
  EXPORTS: Nothing.
*)

open Cil_types
open Cil
open Cil_datatype

module Chunk = Cabs2cil_BlockChunk

(* weimer: Sat Dec 8 17:30:47 2001 MSVC NT kernel headers include functions like
   "long convert(x) { __asm { mov eax, x \n cdq } }" that set a return value via
   an ASM statement. As a result, I am changing this so a final ASM statement
   does not count as "fall through" for the purposes of this warning. *)
(* matth: But it's better to assume assembly will fall through, since most such
   blocks do. It's probably better to print an unnecessary warning than to break
   CIL's invariant that return statements are inserted properly.  *)
let rec compute_from_root f = function
  | [] -> false
  (* We have a label, perhaps we can jump here. *)
  | stmt :: remaining_stmts when stmt.labels <> [] ->
    Kernel.debug ~level:4 "computeFromRoot call f from stmt %a"
      Cil_printer.pp_location (Stmt.loc stmt);
    f (stmt :: remaining_stmts)
  | _ :: remaining_stmts -> compute_from_root f remaining_stmts

let instr_falls_through instr =
  match instr with
  | Call (None, {enode = Lval (Var varinfo, NoOffset); _ }, _, _) ->
    (* See if this is exit, or if it has the [noreturn] attribute. *)
    not (varinfo.vname = "exit" || hasAttribute "noreturn" varinfo.vattr)
  | Call _
  | Set _ | Asm _ | Skip _ | Code_annot _ -> true

let stmt_is_labeled_as_default stmt =
  List.exists (function Default _ -> true | _ -> false) stmt.labels

let rec stmt_falls_through stmt : bool =
  Kernel.debug ~level:4 "stmt_falls_through stmt %a"
    Cil_printer.pp_location (Stmt.loc stmt);
  match stmt.skind with
  | Instr instr ->
    instr_falls_through instr
  | UnspecifiedSequence unspecified_seq ->
    block_falls_through (block_from_unspecified_sequence unspecified_seq)
  | Return _ | Break _ | Continue _ | Throw _ | Goto _ -> false
  | If (_, then_block, else_block, _) ->
    block_falls_through then_block || block_falls_through else_block
  | Switch (_exp, _block, targets_stmts, _)
    (* See if there is a "default" case. *)
    when not (List.exists stmt_is_labeled_as_default targets_stmts) ->
    (* We fall through because there is no default. *)
    true
  | Switch (_exp, block, _targets_stmts, _) ->
    (* We must examine all cases. If any falls through, then the switch falls
       through. *)
    block_falls_through block || block_can_break block
  | Loop (_, block, _, _, _) ->
    (* A loop falls through if it can break. *)
    block_can_break block
  | Block block -> block_falls_through block
  | TryCatch (block, catch_binders_with_blocks, _) ->
    block_falls_through block ||
    List.exists
      (fun (_catch_binder, block) -> block_falls_through block)
      catch_binders_with_blocks
  | TryFinally (_, block, _) -> block_falls_through block
  | TryExcept (_, _, _block, _) -> true (* Conservative. *)

and stmts_fall_through = function
  | [] -> true
  | stmt :: remaining_stmts when stmt_falls_through stmt ->
    stmts_fall_through remaining_stmts
  | _ :: remaining_stmts ->
    (* If we are not falling through then maybe there are labels who are. *)
    compute_from_root stmts_fall_through remaining_stmts

and block_falls_through block = stmts_fall_through block.bstmts

(* Will we leave this statement or block with a break command? *)
and stmt_can_break stmt : bool =
  Kernel.debug ~level:4 "stmt_can_break stmt %a"
    Cil_printer.pp_location (Stmt.loc stmt);
  match stmt.skind with
  | Instr _ | Return _ | Continue _ | Goto _ | Throw _ -> false
  | Break _ -> true
  | UnspecifiedSequence unspecified_seq ->
    block_can_break (block_from_unspecified_sequence unspecified_seq)
  | If (_, then_block, else_block, _) ->
    block_can_break then_block || block_can_break else_block
  | Switch _ | Loop _ ->
    (* Switches and loops catch any breaks in their bodies. *)
    false
  | Block block -> block_can_break block
  | TryCatch (block, catch_binders_with_blocks, _) ->
    block_can_break block ||
    List.exists
      (fun (_catch_binder, block) -> block_can_break block)
      catch_binders_with_blocks
  | TryFinally (try_block, finally_block, _) ->
    block_can_break try_block || block_can_break finally_block
  | TryExcept (try_block, _, except_block, _) ->
    block_can_break try_block || block_can_break except_block

and block_can_break block =
  let rec block_stmts_can_break stmts : bool =
    match stmts with
    | [] -> false
    | stmt :: _remaining_stmts when stmt_can_break stmt -> true
    | stmt :: remaining_stmts when stmt_falls_through stmt ->
      assert (not (stmt_can_break stmt));
      block_stmts_can_break remaining_stmts
    | stmt :: remaining_stmts ->
      assert (not (stmt_can_break stmt));
      assert (not (stmt_falls_through stmt));
      compute_from_root block_stmts_can_break remaining_stmts
  in
  block_stmts_can_break block.bstmts

let chunk_falls_through chunk =
  let stmts =
    let get_stmt (stmt, _effects, _calls) = stmt in
    List.rev_map get_stmt chunk.Chunk.stmts_with_effects
  in
  stmts_fall_through stmts
