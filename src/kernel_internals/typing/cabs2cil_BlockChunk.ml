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

(*
  DEPENDS ON:
  + Utility.cabslu
  + Utility.canDropStatement
  + Casts.makeCast
*)

(*
  ENVIRONMENT:
  + backPatchGotos
  + labelStmt
  + Logic_labels.labels
  + Logic_labels.label_current
  + Logic_labels.scope
  + break_env
*)

(*
  NEEDS:
  + make_new_tmp_var function
    from Cabs2cil
    for: keep_pure_exp ~make_new_tmp_var
*)

(*
  EXPORTS: Nothing.
*)

open Cil_types

let cabslu = Cabs2cil_Utility.cabslu
let canDropStatement = Cabs2cil_Utility.canDropStatement
let makeCast = Cabs2cil_Casts.makeCast

let category_chunk = Kernel.register_category "cabs2cil:chunk"

type effects = {
  modified : lval list;
  writes : lval list;
  reads : lval list;
}

let make_effects ~modified ~writes ~reads = { modified; writes; reads }
let no_effects = { modified = []; writes = []; reads = [] }

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
      G, i, and j. *)

  unspecified_order :bool; (** Order of evaluation of statements in the
                               chunk is unspecified. *)
  locals : varinfo list;   (** Variables that are local to the chunk. *)
  cases : stmt list;       (** A list of case statements
                               (statements with Case labels)
                               visible at the outer level. *)
}

let d_stmt_chunk fmt (stmt, {modified; writes; reads}, calls) =
  let pp_lvals = Pretty_utils.pp_list ~sep:",@ " Cil_printer.pp_lval in
  let pp_calls =
    Pretty_utils.pp_list ~sep:",@ "
      (fun fmt stmt_ref -> Cil_printer.pp_stmt fmt !stmt_ref)
  in
  Format.fprintf fmt "@[<v 0>/*@[(%a) %a@ <-@ %a@]@;Calls:@;%a@;*/@;%a@]"
    pp_lvals modified pp_lvals writes pp_lvals reads pp_calls calls
    Cil_printer.pp_stmt stmt

let d_chunk fmt chunk =
  Format.fprintf fmt "@[<v 0>@[%a%a@]@;@[<v 2>{%a@]}@]"
    (fun fmt is_order_unspecified ->
       if is_order_unspecified
       then Format.fprintf fmt "/* UNDEFINED ORDER */@\n")
    chunk.unspecified_order
    (Pretty_utils.pp_list ~sep:";" Cil_printer.pp_varinfo) chunk.locals
    (Pretty_utils.pp_list ~sep:";@\n" d_stmt_chunk)
    (List.rev chunk.stmts_with_effects)

let empty : t =
  { stmts_with_effects = [];
    cases = [];
    locals = [];
    unspecified_order = false; }

let stmts_of_unspecified_seq unspecified_seq : stmt list =
  match unspecified_seq with
  | UnspecifiedSequence unspecified_seq ->
    List.map (fun (stmt, _, _, _, _) -> stmt) unspecified_seq
  | _ -> assert false

let stmts_of_stmts_with_effects
    (stmts_with_effects : stmt_with_effects list) : stmt list =
  List.map (fun (stmt, _effects, _calls) -> stmt) stmts_with_effects

let undefined_seq_of_chunk chunk =
  let undefined_seq_of_stmts_with_effects
      (stmt, {modified; writes; reads}, calls) =
    (stmt, modified, writes, reads, calls)
  in
  let undefined_seq_rev =
    List.rev_map
      undefined_seq_of_stmts_with_effects chunk.stmts_with_effects
  in
  UnspecifiedSequence undefined_seq_rev

let stmts_with_effects_of_undefined_seq
    (stmt, modified, writes, reads, calls) =
  (stmt, {modified; writes; reads}, calls)

let are_stmts_with_effects_empty
    (stmts_with_effects : stmt_with_effects list) : bool =
  let rec is_empty_stmt stmt =
    match stmt.skind with
    | Instr (Skip _) -> stmt.labels = []
    | Block block -> block.battrs = [] &&
                     List.for_all is_empty_stmt block.bstmts
    | UnspecifiedSequence _ as unspecified_seq ->
      let stmts = stmts_of_unspecified_seq unspecified_seq in
      List.for_all is_empty_stmt stmts
    | _ -> false
  in
  List.for_all is_empty_stmt (stmts_of_stmts_with_effects stmts_with_effects)

let is_empty chunk : bool =
  are_stmts_with_effects_empty chunk.stmts_with_effects

let is_not_empty chunk : bool = not (is_empty chunk)

let of_stmt_and_effects (stmt, effects) : t =
  let calls =
    match stmt.skind with
    | Instr(Call _) -> [ref stmt]
    | _ -> []
  in
  let stmts_with_effects = [(stmt, effects, calls)] in
  { empty with stmts_with_effects }

(* Keep track of the gotos. *)
let back_patch_gotos : (string, stmt ref list ref) Hashtbl.t = Hashtbl.create 17

let add_goto (label_name : string) (stmt_ref : stmt ref) : unit =
  let gotos =
    try Hashtbl.find back_patch_gotos label_name
    with Not_found ->
      let gotos = ref [] in
      Hashtbl.add back_patch_gotos label_name gotos;
      gotos
  in
  gotos := stmt_ref :: !gotos

(* Keep track of the labels. *)
let label_stmt : (string, stmt) Hashtbl.t = Hashtbl.create 17

let init_labels () =
  Hashtbl.clear back_patch_gotos;
  Hashtbl.clear label_stmt

let resolve_gotos () =
  Hashtbl.iter
    (fun label_name gotos ->
       try
         let dest_stmt = Hashtbl.find label_stmt label_name in
         List.iter (fun goto_stmt_ref -> goto_stmt_ref := dest_stmt) !gotos
       with Not_found ->
         Kernel.error ~once:true ~current:true "Label %s not found" label_name)
    back_patch_gotos

module Logic_labels = struct
  (* On the contrary to C, use of labels in the logic obeys block scope rules.
     We keep track of these scopes here. *)
  let labels : (string, stmt) Hashtbl.t = Hashtbl.create 7

  (* label held by the current statement. *)
  let label_current = ref []

  let add_current_label label = label_current := label :: !label_current

  (* Don't remove all current label at once, as there might be some
     labels on nested statements. See bts 1536. *)
  let reset_current_label () =
    label_current:= List.tl !label_current

  let scope = Stack.create ()

  let enter_scope () = Stack.push (ref []) scope
  let exit_scope () =
    let scope_labels = Stack.pop scope in
    List.iter (Hashtbl.remove labels) !scope_labels

  let add_label label stmt =
    let scope = Stack.top scope in
    scope := label::!scope;
    Hashtbl.add labels label stmt

  let find_label label =
    try ref (Hashtbl.find labels label)
    with Not_found when List.mem label !label_current ->
      let stmt_ref = ref (Cil.mkEmptyStmt ~loc:(cabslu "_find_label") ()) in
      add_goto label stmt_ref;
      stmt_ref
end

let add_label label stmt =
  Logic_labels.add_label label stmt;
  Hashtbl.add label_stmt label stmt

(* Removes all generated labels in a chunk from the previous tables;
   useful when dropping code, like expressions in arguments of [sizeof] or
   [__builtin_constant_p]. *)
let remove_labels chunk =
  (* Gets all labels in the dropped chunk. *)
  let get_label chunk =
    let (stmt, _effects, _calls) = chunk in
    let find_labels = object
      inherit Cil.nopCilVisitor
      val mutable labels = []
      method get_labels = labels
      method! vstmt stmt =
        List.iter
          (function
            | Label (label_name, _, false) -> labels <- label_name :: labels
            | _ -> ())
          stmt.labels;
        Cil.DoChildren
    end
    in
    ignore (Cil.visitCilStmt (find_labels :> Cil.cilVisitor) stmt);
    find_labels#get_labels
  in
  (* Flatten the list of names in the list of chunks. *)
  let label_names =
    List.fold_left
      (fun acc chum ->
         match (get_label chum) with
         | [] -> acc
         | l -> List.rev_append l acc)
      [] chunk.stmts_with_effects
  in
  (* Remove the labels from the tables. *)
  List.iter
    (fun label ->
       Hashtbl.remove label_stmt label;
       Hashtbl.remove Logic_labels.labels label;
       Hashtbl.remove back_patch_gotos label)
    label_names

(* Transforms a chunk into a block.
   Note that if the chunk has its [unspecified_order] flag set, the resulting
   block contains a single [UnspecifiedSequence] statement.
   If the chunk consists of a single block, this block will get returned
   directly, unless [collapse_block] is set to false. *)
let to_block ~ghost ?(collapse_block=true) chunk : block =
  if chunk.unspecified_order then
    let bstmts =
      let undefined_seq = undefined_seq_of_chunk chunk in
      [Cil.mkStmt ~ghost undefined_seq]
    in
    { battrs = [];
      blocals = chunk.locals;
      bstmts }
  else
    match chunk.stmts_with_effects with
    | [({ skind = Block block; _ } as stmt, _effects, _calls)]
      when collapse_block && stmt.labels = [] ->
      block.blocals <- chunk.locals @ block.blocals;
      block
    | stmts ->
      (* Block has no locals by itself. We must add them now. *)
      { blocals = chunk.locals;
        battrs = [];
        bstmts = List.rev_map (fun (stmt, _effects, _calls) -> stmt) stmts; }

(* Converts a chunk into a statement. *)
let to_stmt ~ghost chunk =
  let stmtkind =
    let unspecified_stmtkind = lazy (undefined_seq_of_chunk chunk) in
    match chunk.unspecified_order, chunk.locals with
    | true, [] -> Lazy.force unspecified_stmtkind
    | true, _ ->
      let bstmts = [Cil.mkStmt ~ghost (Lazy.force unspecified_stmtkind)] in
      Block { battrs = [];
              blocals = chunk.locals;
              bstmts }
    | false, _ ->
      let block = to_block ~ghost chunk in
      Block block
  in
  Cil.mkStmt ~ghost stmtkind

let merge_effects
    ({modified = modified_1; writes = writes_1; reads = reads_1}, calls_1)
    ({modified = modified_2; writes = writes_2; reads = reads_2}, calls_2) =
  let merge_lval_lists : lval list -> lval list -> lval list =
    let add_unique lvals lval_to_add =
      if List.exists (Cil_datatype.Lval.equal lval_to_add) lvals
      then lvals
      else lval_to_add :: lvals
    in
    List.fold_left add_unique
  in
  let effects =
    { modified = merge_lval_lists modified_1 modified_2;
      writes = merge_lval_lists writes_1 writes_2;
      reads = merge_lval_lists reads_1 reads_2 }
  in
  let calls = calls_1 @ calls_2 in
  (effects, calls)

let get_chunk_effects chunk =
  let no_effects_no_calls = (no_effects, []) in
  List.fold_left
    (fun effects_acc (_stmt, effects, calls) ->
       merge_effects effects_acc (effects, calls))
    no_effects_no_calls chunk.stmts_with_effects

let to_stmt_with_effects ~ghost chunk =
  let (effects, calls) = get_chunk_effects chunk in
  (to_stmt ~ghost chunk, effects, calls)

let unspecified_chunk chunk : t =
  (* To restore previous behavior (where unspecified evaluation order was not
     explicitly marked), comment out the line below and make [unspecified_chunk]
     the identity function. *)
  { chunk with unspecified_order = true }

let local_var_chunk chunk varinfo : t =
  { chunk with locals = varinfo :: chunk.locals }

(* Add a statement at the end. Never refer to this statement again after you
   call this. *)
let (+++) chunk (stmt, effects) : t =
  let calls =
    match stmt.skind with
    | Instr (Call _) -> [ref stmt]
    | _ -> []
  in
  let stmts_with_effects = (stmt, effects, calls) :: chunk.stmts_with_effects in
  { chunk with stmts_with_effects }

(* Append two chunks. Never refer to the original chunks after you call this.
   And especially never share c2 with somebody else. *)
let (@@) chunk_1 (chunk_2, ghost) : t =
  let result_chunk =
    if chunk_1.unspecified_order = chunk_2.unspecified_order
    then
      { stmts_with_effects =
          chunk_2.stmts_with_effects @ chunk_1.stmts_with_effects;
        cases = chunk_1.cases @ chunk_2.cases;
        locals = chunk_1.locals @ chunk_2.locals;
        unspecified_order = chunk_1.unspecified_order; }
    else
      match chunk_2.stmts_with_effects with
      | [] ->
        begin
          match chunk_2.locals with
          | [] -> chunk_1
          | chunk_2_locals ->
            { chunk_1 with locals = chunk_1.locals @ chunk_2_locals }
        end
      | [({ skind = UnspecifiedSequence unspecified_seq; labels = []; _ },
          _effects, _calls) ]
        when chunk_1.unspecified_order ->
        let stmts_with_effects =
          let stmts_with_effects' =
            List.map stmts_with_effects_of_undefined_seq unspecified_seq
          in
          List.rev_append stmts_with_effects' chunk_1.stmts_with_effects
        in
        { stmts_with_effects;
          cases = chunk_1.cases @ chunk_2.cases;
          locals = chunk_1.locals @ chunk_2.locals;
          unspecified_order = chunk_1.unspecified_order; }
      | [stmt_with_effects'] ->
        { stmts_with_effects = stmt_with_effects' :: chunk_1.stmts_with_effects;
          cases = chunk_1.cases @ chunk_2.cases;
          locals = chunk_1.locals @ chunk_2.locals;
          unspecified_order = chunk_1.unspecified_order; }
      | _ ->
        let locals = chunk_1.locals @ chunk_2.locals in
        (* The lifespan of the locals is the whole chunk, not just [chunk_2],
           which may be transformed artificially in a block at this point. *)
        let chunk_2 = { chunk_2 with locals = [] } in
        let stmts_with_effects =
          to_stmt_with_effects ~ghost chunk_2 :: chunk_1.stmts_with_effects
        in
        { stmts_with_effects;
          cases = chunk_1.cases @ chunk_2.cases;
          locals;
          unspecified_order = chunk_1.unspecified_order; }
  in
  Kernel.debug ~dkey:category_chunk
    "Concat:@\n%a@\nWITH@\n%a@\nLEADS TO@\n%a@."
    d_chunk chunk_1 d_chunk chunk_2 d_chunk result_chunk;
  result_chunk

let of_chunk_encapsulate_in_block ~ghost chunk =
  let chunk_without_locals = { chunk with locals = [] } in
  let stmt_with_effects = to_stmt_with_effects ~ghost chunk_without_locals in
  { stmts_with_effects = [stmt_with_effects];
    cases = chunk.cases;
    locals = chunk.locals;
    unspecified_order = false; }
(* TODO: Not sure what is the difference between doing what we are doing and
   that:  [ empty +++ (stmt, (modified, writes, reads)) ]
   Which one is more correct? *)

let remove_reads lval_to_remove chunk : t =
  Kernel.debug ~dkey:category_chunk
    "Removing %a from chunk@\n%a@."
    Cil_printer.pp_lval lval_to_remove d_chunk chunk;
  let remove_from_list =
    List.filter
      (fun lval' -> not (Cil_datatype.LvalStructEq.equal lval_to_remove lval'))
  in
  let remove_from_reads =
    List.map
      (fun (stmt, {modified; writes; reads}, calls) ->
         let modified = lval_to_remove :: modified in
         let reads = remove_from_list reads in
         (stmt, {modified; writes; reads}, calls))
  in
  { chunk with stmts_with_effects = remove_from_reads chunk.stmts_with_effects; }

let remove_effects chunk : t =
  let remove_effects_stmt_with_effects (stmt, _effects, _calls) =
    (stmt, no_effects, [])
  in
  let stmts_with_effects =
    List.map remove_effects_stmt_with_effects chunk.stmts_with_effects
  in
  { chunk with stmts_with_effects }


(* Stack of statements inside which break instruction can be found. *)
let break_env = Stack.create ()

let enter_break_env () = Stack.push () break_env

let exit_break_env () =
  if Stack.is_empty break_env then
    Kernel.fatal ~current:true
      "trying to exit a breakable env without having entered it";
  ignore (Stack.pop break_env)

(* Get the first statement in a chunk. Might need to change the statements in
   the chunk (i.e. add an empty statement). *)
let get_first_stmt_in_chunk ~ghost ~loc chunk : stmt * stmt_with_effects list =
  (* Get the first statement and add the label to it. *)
  match chunk.stmts_with_effects with
  | [] -> (* Add a statement. *)
    let stmt = Cil.mkEmptyStmt ~ghost ~loc () in
    let stmt_with_effects = [(stmt, no_effects, [])] in
    (stmt, stmt_with_effects)
  | stmt_with_effects ->
    let (stmt, _effects, _calls) = Extlib.last stmt_with_effects in
    (stmt, stmt_with_effects)

let keep_pure_exp ~make_new_tmp_var ~ghost exp location : t =
  let exp_typ, exp =
    let exp_typ = Cil.typeOf exp in
    if Cil.isVoidType exp_typ
    then
      let exp = Cil.stripCastsToVoid exp in
      let exp_typ = Cil.typeOf exp in
      assert (not (Cil.isVoidType exp_typ));
      exp_typ, exp
    else
      exp_typ, exp
  in
  let tmp_varinfo = make_new_tmp_var "Pure expression" true exp_typ in
  let stmt =
    let tmp_varinfo_lval = Cil.var tmp_varinfo in
    Cil.mkStmtOneInstr ~ghost (Set (tmp_varinfo_lval, exp, location))
  in
  { empty with
    stmts_with_effects = [(stmt, no_effects, [])];
    locals = [tmp_varinfo]; }

(* We can duplicate a chunk if it has a few simple statements, and if it does
   not have cases. *)
(* Raises [Failure] if you should not duplicate this chunk. *)
let duplicate chunk : t =
  if not (Kernel.AllowDuplication.get ())
  then raise (Failure "cannot duplicate: disallowed by user");
  if chunk.locals != []
  then raise (Failure "cannot duplicate: has locals");
  if chunk.cases != []
  then raise (Failure "cannot duplicate: has cases");
  let instruction_count = ref 0 in
  let duplicate_stmts_with_effects (stmt, effects, calls) =
    if stmt.labels != []
    then raise (Failure "cannot duplicate: has labels");
    begin
      match stmt.skind with
      | If _ | Switch _ | Loop _ | Block _ | UnspecifiedSequence _
      | TryCatch _ | Throw _ | TryFinally _ | TryExcept _ ->
        raise (Failure "cannot duplicate: complex stmt")
      | Instr _ | Goto _ | Return _ | Break _ | Continue _ ->
        incr instruction_count
    end;
    if !instruction_count > 5 then raise
        (Failure ("cannot duplicate: too many instr"));
    (* We can just copy it because there is nothing to share here. Except maybe
       for the ref cell in [Goto] but it is ok to share that, I think *)
    let stmt' = { stmt with sid = stmt.sid} in
    let calls' =
      match stmt.skind with
      | Instr (Call _) -> [ref stmt']
      | Instr _ | TryExcept _ | TryFinally _ | TryCatch _ | Throw _
      | UnspecifiedSequence _| Block _ | Loop _
      | Switch _ | If _ | Continue _| Break _ | Goto _ | Return _ ->
        assert (calls = []);
        []
    in
    (stmt', effects, calls')
  in
  { stmts_with_effects =
      List.map duplicate_stmts_with_effects chunk.stmts_with_effects;
    cases = [];
    unspecified_order = chunk.unspecified_order;
    locals = chunk.locals; (* [varinfos] must be shared anyway. *) }

(* We can drop a chunk if it does not have labels inside. *)
let can_drop chunk : bool =
  List.for_all
    (fun (stmt, _effects, _calls) -> canDropStatement stmt)
    chunk.stmts_with_effects

(* [of_stmt] must not be used during expression translation, as it does not take
   care of the effects of the statement. Use [of_stmt_and_effects] instead. *)
let of_stmt stmt : t =
  { stmts_with_effects = [(stmt, no_effects, [])];
    cases = [];
    unspecified_order = false;
    locals = []; }


module Make = struct
  (* The chunks below are used in statements translation. Hence, their order of
     evaluation is always specified, and we can forget their effects. *)

  let skip_chunk = empty

  (* return can be ghost but only in ghost functions *)
  let return_chunk ~ghost exp_option location : t =
    let stmt = Cil.mkStmt ~ghost (Return (exp_option, location)) in
    { stmts_with_effects = [(stmt, no_effects, [])];
      cases = [];
      locals = [];
      unspecified_order = false; }

  let if_chunk ~ghost cond_exp location then_chunk else_chunk : t =
    let then_effects = get_chunk_effects then_chunk in
    let else_effects = get_chunk_effects else_chunk in
    let (effects, calls) = merge_effects then_effects else_effects in
    let stmt =
      let then_block = to_block ~ghost then_chunk in
      let else_block = to_block ~ghost else_chunk in
      Cil.mkStmt ~ghost (If (cond_exp, then_block, else_block, location))
    in
    { stmts_with_effects = [(stmt, effects, calls)];
      cases = then_chunk.cases @ else_chunk.cases;
      locals = [];
      unspecified_order = false; }

  let loop_chunk ~ghost code_annotations loop_body_chunk : t =
    let loop_stmt =
      Cil.mkStmt ~ghost
        (Loop (code_annotations,
               to_block ~ghost loop_body_chunk,
               Cil.CurrentLoc.get (),
               None,
               None))
    in
    { stmts_with_effects = [(loop_stmt, no_effects, [])];
      cases = loop_body_chunk.cases;
      unspecified_order = false;
      locals = []; }

  (* Can be ghost inside a ghost loop. *)
  let break_chunk ~ghost location : t =
    if Stack.is_empty break_env
    then Kernel.abort ~current:true "break outside of a loop or switch"
    else
      let stmt = Cil.mkStmt ~ghost (Break location) in
      { stmts_with_effects = [(stmt, no_effects, [])];
        cases = [];
        unspecified_order = false;
        locals = []; }

  (* Can be ghost inside a ghost loop. *)
  let continue_chunk ~ghost location : t =
    let stmt = Cil.mkStmt ~ghost (Continue location) in
    { stmts_with_effects = [(stmt, no_effects, [])];
      cases = [];
      unspecified_order = false;
      locals = []; }

  let goto_chunk ~ghost label_name location : t =
    let goto_stmt_ref = ref Cil.dummyStmt in
    add_goto label_name goto_stmt_ref;
    let stmt = Cil.mkStmt ~ghost (Goto (goto_stmt_ref, location)) in
    { stmts_with_effects = [(stmt, no_effects, [])];
      cases = [];
      locals = [];
      unspecified_order = false; }

  let case_range_chunk ~ghost cases loc next_chunk : t =
    let first_stmt, stmts_with_effects = get_first_stmt_in_chunk ~ghost ~loc next_chunk in
    let labels = List.map (fun case -> Case (case, loc)) cases in
    first_stmt.labels <- labels @ first_stmt.labels;
    { next_chunk with stmts_with_effects;
                      cases = first_stmt :: next_chunk.cases;
                      unspecified_order = false; }

  let default_chunk ~ghost loc next_chunk : t =
    let first_stmt, stmts_with_effects =
      get_first_stmt_in_chunk ~ghost ~loc next_chunk
    in
    let label = Default loc in
    first_stmt.labels <- label :: first_stmt.labels;
    { next_chunk with stmts_with_effects;
                      cases = first_stmt :: next_chunk.cases;
                      unspecified_order = false }

  let switch_chunk ~ghost exp body_chunk location : t =
    (* Make the statement. *)
    let encountered_default_case = ref false in
    let exp_typ = Cil.typeOf exp in
    let check_for_case exp =
      let exp' =
        (* If needed, convert [exp] to type [exp_typ], and check in case the label
           was too big *)
        let exp' = makeCast ~e:exp ~newt:exp_typ in
        if Cil.(theMachine.lowerConstants)
        then Cil.constFold true exp'
        else exp'
      in
      begin
        match Cil.constFoldToInt exp, Cil.constFoldToInt exp' with
        | Some exp_integer, Some exp'_integer
          when not (Integer.equal exp_integer exp'_integer) ->
          Kernel.feedback ~once:true ~source:(fst exp.eloc)
            "Case label %a exceeds range of %a for switch expression. \
             Nothing to worry."
            Cil_printer.pp_exp exp Cil_printer.pp_typ exp_typ;
        | _ -> ()
      end;
      exp'
    in
    let check_for_default_and_cast label =
      match label with
      | Default _ ->
        if !encountered_default_case then
          Kernel.error ~once:true ~current:true
            "Switch statement at %a has duplicate default entries."
            Cil_printer.pp_location location;
        encountered_default_case := true;
        label
      | Label _ -> label
      | Case (Simple exp, location) ->
        Case (Simple (check_for_case exp), location)
      | Case (Range (range_exp_1, range_exp_2), location) ->
        Case (Range (check_for_case range_exp_1, check_for_case range_exp_2),
              location)
    in
    let block = to_block ~ghost body_chunk in
    let cases_stmts =
      (* Eliminate duplicate entries from [body.cases]. A statement is added to
         [body.cases] for each case label it has. *)
      List.fold_right
        (fun stmt acc ->
           if List.memq stmt acc
           then acc
           else begin
             stmt.labels <- List.map check_for_default_and_cast stmt.labels;
             stmt :: acc
           end)
        body_chunk.cases
        []
    in
    let switch_stmt =
      Cil.mkStmt ~ghost (Switch (exp, block, cases_stmts, location))
    in
    { stmts_with_effects = [(switch_stmt, no_effects, [])];
      cases = [];
      locals = [];
      unspecified_order = false; }

end

exception Found_stmt

let find_stmt block label_name stmt : unit =
  let find_stmt_visitor = object
    inherit Cil.nopCilVisitor
    method! vstmt stmt' =
      if stmt == stmt'
      then raise Found_stmt
      else Cil.DoChildren
  end
  in
  try
    ignore (Cil.visitCilBlock find_stmt_visitor block);
    Kernel.warning ~current:true
      "Inconsistent AST: Statement %a,@ with label %s is not in the AST"
      Cil_printer.pp_stmt stmt label_name;
  with Found_stmt -> ()

class cleanUnspecified = object(self)
  inherit Cil.nopCilVisitor

  val unspecified_stack = Stack.create ()

  val mutable replace_table = []

  (* We start in a deterministic block. *)
  initializer Stack.push false unspecified_stack

  method private push : 'a. bool -> 'a -> 'a Cil.visitAction =
    fun flag x ->
      Stack.push flag unspecified_stack;
      Cil.ChangeDoChildrenPost
        (x, fun x -> ignore(Stack.pop unspecified_stack); x)

  method! vblock block =
    let bstmts =
      List.rev
        (List.fold_left
           (fun stmts_acc stmt ->
              match stmt.skind with
              | Block block'
                when not (Stack.top unspecified_stack)
                  && block'.battrs = []
                  && block'.blocals = []
                  && stmt.labels = [] ->
                List.rev_append block'.bstmts stmts_acc
              | _ -> stmt :: stmts_acc)
           [] block.bstmts)
    in
    block.bstmts <- bstmts;
    Cil.DoChildren


  method private change_label_stmt stmt stmt' =
    List.iter
      (function
        | Label (label_name, _, _) -> Hashtbl.replace label_stmt label_name stmt'
        | Case _ | Default _ ->
          replace_table <- (stmt, stmt') :: replace_table
      ) stmt.labels;
    stmt'.labels <- stmt.labels @ stmt'.labels

  method! vstmt stmt =
    match stmt.skind with
    | UnspecifiedSequence [(stmt', _modified, _writes, _reads, _calls)] ->
      self#change_label_stmt stmt stmt';
      Cil.ChangeDoChildrenPost(stmt', fun stmt -> stmt)
    | UnspecifiedSequence [] ->
      let stmt' = Cil.mkEmptyStmt ~loc:(cabslu "_useq") () in
      self#change_label_stmt stmt stmt';
      Cil.ChangeTo stmt'
    | UnspecifiedSequence _ -> self#push true stmt
    | Block { battrs = []; blocals = []; bstmts = [stmt']} ->
      self#change_label_stmt stmt stmt';
      Cil.ChangeDoChildrenPost (stmt', fun stmt -> stmt)
    | Block _ | If _ | Loop _
    | TryFinally _ | TryExcept _ | Throw _ | TryCatch _ -> self#push false stmt
    | Switch _ ->
      let change_cases stmt =
        match stmt.skind with
        | Switch(exp, body_block, cases_stmts, location) ->
          let cases_stmts' =
            List.map
              (fun stmt ->
                 try List.assq stmt replace_table
                 with Not_found -> stmt)
              cases_stmts
          in
          stmt.skind <- Switch (exp, body_block, cases_stmts', location);
          ignore (Stack.pop unspecified_stack);
          stmt
        | _ -> assert false
      in
      Stack.push false unspecified_stack;
      Cil.ChangeDoChildrenPost(stmt, change_cases)
    | Instr _ | Return _ | Goto _ | Break _ | Continue _ -> Cil.DoChildren
end

let mk_function_body ~ghost chunk : block =
  if chunk.cases <> []
  then
    Kernel.error ~once:true ~current:true
      "Switch cases not inside a switch statement\n";
  (* Cleanup empty blocks and unspecified sequences.
     This can change some labels (the one attached to removed blocks), so it has
     to be done before [resolveGotos]. *)
  let result_block =
    let block = to_block ~ghost chunk in
    Cil.visitCilBlock (new cleanUnspecified) block
  in
  Hashtbl.iter (find_stmt result_block) label_stmt;
  resolve_gotos ();
  init_labels ();
  result_block

let add_reads loc reads chunk : t =
  match reads with
  | [] -> chunk
  | _ ->
    let effects = { modified = []; writes = []; reads } in
    chunk +++ (Cil.mkEmptyStmt ~loc (), effects)
