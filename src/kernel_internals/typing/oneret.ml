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
open Cil
open Logic_const


(* Drop the attributes that will be expanded. *)
let dropExpandedCleanups vi =
  vi.vattr <-
    Cil.dropAttributes
      (if gccMode() then
         ["cleanup" ; "tis_cleanup" ]
       else
         ["tis_cleanup"])
      vi.vattr

(* Expand the cleanup attributes of the passed block local variables.
   This mutates its arguments and takes care of not sharing nodes
   within the result.
   After this function call, the CFG needs to be recomputed as the SIDs
   are no longer correct.
   Note that it will not expand the cleanup statements for the
   Return statements.
*)
let expand_cleanup add block =
  let cleanup_stmts =
    List.fold_right
      (fun vi acc -> match Cil.get_cleanup_stmt vi with
         | None -> acc
         | Some stmt ->
           (* cleanup attributes will be expanded: drop them. *)
           dropExpandedCleanups vi;
           stmt::acc)
      block.blocals
      []
  in
  if cleanup_stmts = [] then block
  else
    let fresh_cleanup () = (* Prevent sharing of the inserted statements.*)
      List.fold_left
        (fun acc s ->
           (mkStmt ~ghost:s.ghost ~valid_sid:true s.skind)::acc)
        []
        cleanup_stmts
    in
    let out_edges = Stmts_graph.get_all_block_last_stmts block in
    (* For each outgoing edge, we insert a block calling the cleanup functions
       before the statement that will escape the block.*)
    List.iter
      (fun in_stmt ->
         (match in_stmt.skind with
          | Goto _ | Break _| Continue _ | Return _ | Throw _ ->
            (* Add the cleanup before changing the control flow.
               TODO: Maybe evaluate the Throw expression before cleaning up.*)
            add in_stmt true (fresh_cleanup())
          | _ -> (* For other kinds, we go out of the block normally. We need
                    to clean up after the statement.*)
            add in_stmt false (fresh_cleanup())))
      out_edges;
    block

(* Recursively expand the cleanup statements of each statement
   contained in the passed block.
   This mutates the statements inside the block.
*)
let expand_all_cleanup f =
  let stmt_to_insert = Cil_datatype.Stmt.Hashtbl.create 7 in
  let add stmt (before:bool) (cleanup:Cil_datatype.Stmt.t list) =
    try
      let before',previous =
        Cil_datatype.Stmt.Hashtbl.find stmt_to_insert stmt
      in
      assert (before = before');
      Cil_datatype.Stmt.Hashtbl.replace stmt_to_insert stmt
        ((before, (previous@cleanup)))
    with Not_found ->
      Cil_datatype.Stmt.Hashtbl.add stmt_to_insert
        stmt
        (before, cleanup)
  in
  let vis = object
    inherit Cil.nopCilVisitor
    method! vblock block = ChangeDoChildrenPost (block, expand_cleanup add)
  end
  in
  List.iter (fun s -> ignore (visitCilStmt vis s)) f.sbody.bstmts;
  Cil_datatype.Stmt.Hashtbl.iter
    (fun stmt (before, cleanups) ->
       (* Make a fresh statement to store inside the current block. *)
       let new_stmt =
         mkStmt ~ghost:stmt.ghost ~valid_sid:true stmt.skind
       in
       if before then
         stmt.skind <- Block (mkBlock (cleanups@[new_stmt]))
       else
         stmt.skind <- Block (mkBlock (new_stmt::cleanups)))
    stmt_to_insert


let adjust_assigns_clause loc var code_annot =
  let change_result = object
    inherit Cil.nopCilVisitor
    method! vterm_lhost = function
      | TResult _ -> ChangeTo (TVar var)
      | TVar _ | TMem _ -> DoChildren
  end
  in
  let change_term t = Cil.visitCilTerm change_result t in
  let module M = struct exception Found end in
  let check_var = object
    inherit Cil.nopCilVisitor
    method! vterm_lhost = function
      | TVar v when Cil_datatype.Logic_var.equal var v -> raise M.Found
      | TVar _ | TResult _ | TMem _ -> DoChildren
  end
  in
  let contains_var l =
    try ignore (Cil.visitCilAssigns check_var (Writes l)); false
    with M.Found -> true
  in
  let change_from = function
    | FromAny -> FromAny
    | From l -> From (List.map Logic_const.refresh_identified_term l)
  in
  let adjust_lval (_,assigns as acc) (loc,from) =
    if Logic_utils.contains_result loc.it_content then begin
      true,
      (Logic_const.new_identified_term (change_term loc.it_content),
       change_from from)::assigns
    end else acc
  in
  let adjust_clause b =
    match b.b_assigns with
    | WritesAny -> ()
    | Writes l ->
      if not (contains_var l) then begin
        let (changed, a) = List.fold_left adjust_lval (false,l) l in
        let a =
          if changed then a
          else
            (Logic_const.new_identified_term (Logic_const.tvar ~loc var),
             FromAny)
            :: a
        in
        b.b_assigns <- Writes a
      end
  in
  match code_annot with
  | AStmtSpec (_,s) -> List.iter adjust_clause s.spec_behavior
  | _ -> ()

let oneret (f: fundec) : unit =
  let fname = f.svar.vname in
  (* Get the return type *)
  let retTyp =
    match f.svar.vtype with
      TFun(rt, _, _, _) -> rt
    | _ ->
      Kernel.fatal "Function %a does not have a function type"
        Printer.pp_varinfo f.svar
  in
  (* Does it return anything ? *)
  let hasRet = match unrollType retTyp with TVoid _ -> false | _ -> true in

  (* Function scope level variables may have GCC cleanup attributes attached.
     Let us expand them just before the fresh return.*)
  let has_cleanup = ref false in
  let toplevel_cleanup_stmts =
    List.fold_right
      (fun vi acc -> match Cil.get_cleanup_stmt vi with
         | None -> acc
         | Some stmt ->
           has_cleanup:= true;
           (* cleanup attributes will be expanded: drop them. *)
           dropExpandedCleanups vi;
           stmt::acc)
      f.sbody.blocals
      []
  in
  let first_cleanup_stmt = match List.rev toplevel_cleanup_stmts with
    | [] -> None
    | h::_ -> Some h
  in
  (* Memoize the return result variable. Use only if hasRet *)
  let lastloc = ref Cil_datatype.Location.unknown in
  let getRetVar =
    let retVar : varinfo option ref = ref None in
    fun () ->
      match !retVar with
      | Some rv -> rv
      | None -> begin
          let rv = makeLocalVar f "__retres" retTyp in (* don't collide *)
          retVar := Some rv;
          rv
        end
  in
  let convert_result p =
    let vis = object
      inherit Cil.nopCilVisitor
      method! vterm_lhost = function
        | TResult _ ->
          let v = getRetVar () in
          ChangeTo (TVar (cvar_to_lvar v))
        | TMem _ | TVar _ -> DoChildren
    end
    in visitCilPredicateNamed vis p
  in
  let assert_of_returns ca =
    match ca.annot_content with
    | AAssert _ | AInvariant _ | AVariant _
    | AAssigns _ | AAllocation _ | APragma _ -> ptrue
    | AStmtSpec (_bhvs,s) ->
      let res =
        List.fold_left
          (fun acc bhv ->
             pand
               (acc,
                pimplies
                  (pands
                     (List.map
                        (fun p ->
                           pold ~loc:p.ip_loc
                             (Logic_utils.named_of_identified_predicate p))
                        bhv.b_assumes),
                   pands
                     (List.fold_left
                        (fun acc (kind,p) ->
                           match kind with
                             Returns ->
                             Logic_utils.named_of_identified_predicate p
                             :: acc
                           | Normal | Exits | Breaks | Continues -> acc)
                        [ptrue] bhv.b_post_cond)
                  )))
          ptrue s.spec_behavior
      in convert_result res
  in
  (* Remember if we have introduced goto's *)
  let haveGoto = ref false in
  let getRetStmt =
    (* Memoize the return statement *)
    let retStmt : stmt ref = ref dummyStmt in
    function () ->
      if !retStmt == dummyStmt then begin
        let sr =
          let getLastLoc () = (* CEA modified to have a good [!lastloc] *)
            let rec setLastLoc = function
              | [] -> ()
              | { skind = Block b; _ } :: [] -> setLastLoc b.bstmts
              | { skind = UnspecifiedSequence seq; _ } :: [] ->
                setLastLoc (List.map (fun (x,_,_,_,_) -> x) seq)
              | { skind = _; _ } as s :: [] ->
                lastloc := Cil_datatype.Stmt.loc s
              | { skind = _s; _ } :: l -> setLastLoc l
            in
            setLastLoc f.sbody.bstmts; !lastloc
          in
          let loc = getLastLoc () in
          (* Must create a statement *)
          let rv =
            if hasRet then
              Some (new_exp ~loc (Lval (Var (getRetVar ()), NoOffset)))
            else None
          in
          mkStmt (Return (rv, loc))
        in retStmt := sr;
        sr
      end else
        !retStmt
  in
  (* Stack of predicates that must hold in case of returns
     (returns clause with \old transformed into \at(,L) for a suitable L).
     TODO: split that into behaviors and generates for foo,bar: assert instead
     of plain assert.
  *)
  let returns_clause_stack = Stack.create () in
  let stmt_contract_stack = Stack.create () in
  let rec popn n =
    if n > 0 then begin
      assert (not (Stack.is_empty returns_clause_stack));
      ignore (Stack.pop returns_clause_stack);
      ignore (Stack.pop stmt_contract_stack);
      popn (n-1)
    end
  in
  (* Now scan all the statements. Know if you are the main body of the
     function and be prepared to add new statements at the end.
     popstack indicates whether we should pop the stack after having analyzed
     current statement. It is an int since nothing in ACSL prevents from having
     multiple statement contracts on top of each other before finding an
     actual statement... *)
  let rec scanStmts acc (mainbody: bool) popstack = function
    | [] when mainbody -> (* We are at the end of the function. Now it is
                           * time to add the return statement *)
      let rs = getRetStmt () in
      if !haveGoto then
        begin match first_cleanup_stmt with
          | None ->
            rs.labels <- (Label("return_label", !lastloc, false)) :: rs.labels;
          | Some cleanup_stmt ->
            cleanup_stmt.labels <-
              (Label("return_label", !lastloc, false)) :: cleanup_stmt.labels;
        end;
      List.rev (rs :: toplevel_cleanup_stmts @ acc)

    | [] -> List.rev acc

    | [ { skind = Return (Some ({ enode = Lval (Var _, NoOffset); _ }), _l);
          _ } as s]
      when mainbody && not !haveGoto && not !has_cleanup ->
      (* We're not changing the return into goto, so returns clause will still
         have effect. *)
      popn popstack;
      List.rev (s::acc)

    | ({ skind = Return (retval, loc); _ } as s) :: rests ->
      Cil.CurrentLoc.set loc;
      if hasRet && retval = None then
        Kernel.warning ~current:true
          "Found return without value in function %s" fname;
      if not hasRet && retval <> None then
        Kernel.warning ~current:true "Found return in subroutine %s" fname;
      (* Keep this statement because it might have labels. But change it to
       * an instruction that sets the return value (if any). *)
      s.skind <- begin
        match retval with
          Some rval -> Instr (Set((Var (getRetVar ()), NoOffset), rval, loc))
        | None -> Instr (Skip loc)
      end;
      let returns_assert = ref ptrue in
      Stack.iter (fun p -> returns_assert := pand ~loc (p, !returns_assert))
        returns_clause_stack;
      (match retval with
       | Some _ ->
         Stack.iter
           (adjust_assigns_clause loc (Cil.cvar_to_lvar (getRetVar())))
           stmt_contract_stack;
       | None -> () (* There's no \result: no need to adjust it *)
      );
      let add_assert res =
        match !returns_assert with
        | { content = Ptrue; _ } -> res
        | p ->
          let a =
            Logic_const.new_code_annotation (AAssert ([],p))
          in
          mkStmt (Instr(Code_annot (a,loc))) :: res
      in
      (* See if this is the last statement in function *)
      if mainbody && rests == [] then begin
        popn popstack;
        scanStmts (add_assert (s::acc)) mainbody 0 rests
      end else begin
        (* Add a Goto *)
        let sgref = match first_cleanup_stmt with
          | None -> ref (getRetStmt ())
          | Some cleanup_stmt -> ref cleanup_stmt
        in
        let sg = mkStmt (Goto (sgref, loc)) in
        haveGoto := true;
        popn popstack;
        scanStmts (sg :: (add_assert (s::acc))) mainbody 0 rests
      end

    | ({ skind = If (eb, t, e, l); _ } as s) :: rests ->
      s.skind <- If (eb, scanBlock false t, scanBlock false e, l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({ skind = Loop (a, b, l, lb1, lb2); _ } as s) :: rests ->
      s.skind <- Loop (a, scanBlock false b, l, lb1, lb2);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({ skind = Switch (e, b, cases, l); _ } as s) :: rests ->
      s.skind <- Switch (e, scanBlock false b, cases, l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | [{ skind = Block b; _ } as s] ->
      s.skind <- Block (scanBlock mainbody b);
      popn popstack;
      List.rev (s::acc)
    | ({ skind = Block b; _ } as s) :: rests ->
      s.skind <- Block (scanBlock false b);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | [{ skind = UnspecifiedSequence seq; _ } as s] ->
      s.skind <-
        UnspecifiedSequence
          (List.concat
             (List.map (fun (s,m,w,r,c) ->
                  let res = scanStmts [] mainbody 0 [s] in
                  (List.hd res,m,w,r,c)::
                  (List.map (fun x -> x,[],[],[],[]) (List.tl res)))
                 seq));
      popn popstack;
      List.rev (s::acc)
    | ({ skind = UnspecifiedSequence seq; _ } as s) :: rests ->
      s.skind <-
        UnspecifiedSequence
          (List.concat
             (List.map (fun (s,m,w,r,c) ->
                  let res = scanStmts [] false 0 [s] in
                  (List.hd res,m,w,r,c)::
                  (List.map (fun x -> x,[],[],[],[]) (List.tl res)))
                 seq));
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | { skind = Instr (Code_annot (ca,_)); _ } as s :: rests ->
      let returns = assert_of_returns ca in
      let returns = Logic_utils.translate_old_label s returns in
      Stack.push returns returns_clause_stack;
      Stack.push ca.annot_content stmt_contract_stack;
      scanStmts (s::acc) mainbody (popstack + 1) rests
    | { skind = TryCatch (t, c, l); _ } as s :: rests ->
      let scan_one_catch (e,b) = (e,scanBlock false b) in
      let t = scanBlock false t in
      let c = List.map scan_one_catch c in
      s.skind <- TryCatch(t,c,l);
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests
    | ({ skind =
           (Goto _ | Instr _ | Continue _ | Break _ |
            TryExcept _ | TryFinally _ | Throw _);
         _ } as s)
      :: rests ->
      popn popstack;
      scanStmts (s::acc) mainbody 0 rests

  and scanBlock (mainbody: bool) (b: block) =
    { b with bstmts = scanStmts [] mainbody 0 b.bstmts;}

  in
  (* Force only one Return and expand the toplevel cleanup statements*)
  f.sbody <- scanBlock true f.sbody;
  (* Recursively expand all the other cleanup statements, but first recompute
     the correct CFG for the fundec. *)
  Cfg.clearCFGinfo ~clear_id:false f;
  Cfg.cfgFun f;
  expand_all_cleanup f

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
 *)
