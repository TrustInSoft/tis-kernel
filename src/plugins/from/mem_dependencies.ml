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

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil
open Cil_datatype
open Locations

module StmtToZone = Stmt.Hashtbl.Make(Zone)

module MemdepsData =
  Kernel_function.Make_Table
    (StmtToZone)
    (struct
      let name = "Memory dependency"
      let size = 17
      let dependencies = [ Db.Value.self ]
    end)
let () = Ast.add_monotonic_state MemdepsData.self

type memdeps_analysis_state = {
  current_function : Kernel_function.t;
  callsite : Kinstr.t;
  table_for_calls :
    Zone.t Stmt.Hashtbl.t Kernel_function.Hashtbl.t Kinstr.Hashtbl.t
}

let analysis_stack : memdeps_analysis_state list ref = ref []

let merge_stmt_zone dest src =
  Stmt.Hashtbl.iter (fun stmt zone ->
      try
        Stmt.Hashtbl.replace dest stmt
          (Zone.join zone (Stmt.Hashtbl.find dest stmt))
      with Not_found ->
        Stmt.Hashtbl.add dest stmt zone
    ) src

let merge_pathdeps kf_tbl =
  Kernel_function.Hashtbl.iter (fun kf stmt_tbl ->
      try
        let old = MemdepsData.find kf in
        merge_stmt_zone old stmt_tbl
      with Not_found ->
        MemdepsData.add kf (Cil_datatype.Stmt.Hashtbl.copy stmt_tbl)
    ) kf_tbl

let merge_kernel_functions dest src =
  Kernel_function.Hashtbl.iter (fun kf stmt_tbl ->
      try
        let old = Kernel_function.Hashtbl.find dest kf in
        merge_stmt_zone old stmt_tbl
      with Not_found ->
        Kernel_function.Hashtbl.add dest kf stmt_tbl
    ) src

let add_to_table_for_calls table_for_calls callsite data =
  try
    let prev = Kinstr.Hashtbl.find table_for_calls callsite in
    merge_kernel_functions prev data
  with Not_found ->
    Kinstr.Hashtbl.add table_for_calls callsite data

let make_kf_tbl kf stmt_tbl =
  let tbl = Kernel_function.Hashtbl.create 1 in
  Kernel_function.Hashtbl.add tbl kf stmt_tbl;
  tbl


class do_memdeps froms get_stmt_state callwise_state_with_formals
    table_for_calls =
  object(self)
    inherit Cil.nopCilVisitor
    val mutable inputs : Zone.t Stmt.Hashtbl.t = Stmt.Hashtbl.create 13

    method result = inputs

    method join stmt zone =
      if not (Zone.is_bottom zone) then begin
        let table = Stmt.Hashtbl.create 13 in
        Stmt.Hashtbl.add table stmt zone;
        merge_stmt_zone self#result table
      end

    method! vstmt s =
      if Db.Value.is_reachable (get_stmt_state (Extlib.the self#current_stmt))
      then begin
        match s.skind with
        | UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_,_,_) ->
               ignore (visitCilStmt (self:>cilVisitor) stmt))
            seq;
          SkipChildren (* do not visit the additional lvals *)
        | If (_cond, _th, _el, _) ->
          DoChildren (* for _cond and for the statements in _th, _el *)
        | Loop _ | Block _ ->
          DoChildren (* for the statements *)
        | Switch _ ->
          DoChildren (* for the statements and the expression *)
        | Instr _ ->
          DoChildren (* for Calls *)
        | Return _ | Throw _ ->
          DoChildren
        | Goto _ | Break _ | Continue _ ->
          SkipChildren
        | TryExcept _ | TryFinally _ | TryCatch _ -> assert false
      end
      else SkipChildren

    method stmt_froms =
      let stmt = Extlib.the (self#current_stmt) in
      Stmt.Hashtbl.find froms stmt

    method! vlval lv =
      let stmt = Extlib.the self#current_stmt in
      let deps,_loc =
        !Db.Value.lval_to_loc_with_deps (* loc ignored *)
          ~with_alarms:CilE.warn_none_mode
          ~deps:Zone.bottom
          (Kstmt stmt)
          lv
      in
      let froms = self#stmt_froms in
      let all_f = Function_Froms.Memory.find froms deps in
      self#join stmt all_f;
      SkipChildren

    method! vinst i =
      let current_stmt = Extlib.the self#current_stmt in
      if Db.Value.is_reachable (get_stmt_state current_stmt)
      then begin
        match i with
        | Call (_lv_opt,_expr,_args,_) ->
          let dependencies_by_function =
            Stmt.Hashtbl.find callwise_state_with_formals current_stmt
          in
          let kf_tbl =
            Kinstr.Hashtbl.find table_for_calls (Kstmt current_stmt)
          in
          List.iter (fun (kf, state) ->
              if not (!Db.Value.use_spec_instead_of_definition kf) then begin
                let stmt_tbl = Kernel_function.Hashtbl.find kf_tbl kf in
                Stmt.Hashtbl.iter (fun stmt zone ->
                    let zone = Function_Froms.Memory.find state zone in
                    self#join stmt zone
                  ) stmt_tbl
              end else
                From_parameters.warning ~once:true ~current:true
                  "Assuming library function %a has no mem dependencies@."
                  Kernel_function.pretty kf
            ) dependencies_by_function;
          DoChildren
        | _ -> DoChildren
      end
      else SkipChildren

    method! vexpr exp =
      let join_deps deps =
        let froms = self#stmt_froms in
        let deps_f = Function_Froms.Memory.find froms deps in
        self#join (Extlib.the self#current_stmt) deps_f
      in
      match exp.enode with
      | BinOp ((Mult | Div | Mod), _, _, _)  ->
        let stmt  = Extlib.the self#current_stmt in
        let state = get_stmt_state stmt in
        let deps  = From_compute.find_deps_no_transitivity state exp in
        let deps  = Function_Froms.Deps.to_zone deps in
        join_deps deps;
        SkipChildren

      | AddrOf _lv | StartOf _lv ->
        SkipChildren

      | _ ->
        DoChildren

  end

let register_in_stack (_, callstack) =
  let current_function, callsite = List.hd callstack in
  if not (!Db.Value.use_spec_instead_of_definition current_function) &&
     Kernel_function.is_definition current_function then
    analysis_stack :=
      { current_function;
        callsite;
        table_for_calls = Kinstr.Hashtbl.create 3 } :: !analysis_stack
  else match !analysis_stack with
    | { table_for_calls; _ } :: _ ->
      (* Add declaration with empty table to table avoids some Not_found
         exception when calling the visitor. *)
      Kinstr.Hashtbl.add
        table_for_calls callsite (Kernel_function.Hashtbl.create 0)
    | _ -> () (* main function is not defined. *)

let compute_memdeps (stack, froms,
                     callwise_state_with_formals, get_stmt_state) =
  let kf = Stack.top stack in
  match !analysis_stack with
  | { current_function; callsite; table_for_calls }
    :: ({ table_for_calls = table; _ } :: _ as tail) ->
    assert (Kernel_function.compare kf current_function = 0);
    analysis_stack := tail;
    begin match kf.fundec with
      | Definition (f, _) ->
        let computer =
          new do_memdeps froms get_stmt_state
            callwise_state_with_formals table_for_calls
        in
        ignore (visitCilFunction (computer :> cilVisitor) f);
        let data = computer#result in
        let kf_table = make_kf_tbl kf data in
        add_to_table_for_calls table callsite kf_table;
        merge_pathdeps kf_table
      | Declaration _ ->
        assert false
    end

  | _ ->
    analysis_stack := []


class do_memdeps_graph froms stmt_to_nodes callwise_state_with_formals table_for_calls =
  object(self)
    inherit Cil.nopCilVisitor
    val mutable inputs : Zone.t Stmt.Hashtbl.t = Stmt.Hashtbl.create 13

    method result = inputs

    method is_reachable stmt =
      try State_node.Set.fold
            (State_node.with_state (fun state acc -> acc || Db.Value.is_reachable state))
            (Stmt.Hashtbl.find stmt_to_nodes stmt)
            false
      with Not_found ->  false

    method join stmt zone =
      if not (Zone.is_bottom zone) then begin
        let table = Stmt.Hashtbl.create 13 in
        Stmt.Hashtbl.add table stmt zone;
        merge_stmt_zone self#result table
      end

    method! vstmt s =
      if self#is_reachable (Extlib.the self#current_stmt)
      then begin
        match s.skind with
        | UnspecifiedSequence seq ->
          List.iter
            (fun (stmt,_,_,_,_) ->
               ignore (visitCilStmt (self:>cilVisitor) stmt))
            seq;
          SkipChildren (* do not visit the additional lvals *)
        | If (_cond, _th, _el, _) ->
          DoChildren (* for _cond and for the statements in _th, _el *)
        | Loop _ | Block _ ->
          DoChildren (* for the statements *)
        | Switch _ ->
          DoChildren (* for the statements and the expression *)
        | Instr _ ->
          DoChildren (* for Calls *)
        | Return _ | Throw _ ->
          DoChildren
        | Goto _ | Break _ | Continue _ ->
          SkipChildren
        | TryExcept _ | TryFinally _ | TryCatch _ -> assert false
      end
      else SkipChildren

    method stmt_froms =
      let stmt = Extlib.the (self#current_stmt) in
      Stmt.Hashtbl.find froms stmt

    method! vlval lv =
      let stmt = Extlib.the self#current_stmt in
      let deps,_loc =
        !Db.Value.lval_to_loc_with_deps (* loc ignored *)
          ~with_alarms:CilE.warn_none_mode
          ~deps:Zone.bottom
          (Kstmt stmt)
          lv
      in
      let froms = self#stmt_froms in
      let all_f = Function_Froms.Memory.find froms deps in
      self#join stmt all_f;
      SkipChildren

    method! vinst i =
      let current_stmt = Extlib.the self#current_stmt in
      if self#is_reachable current_stmt
      then begin
        match i with
        | Call(_lv_opt, _expr,_args,_) ->
          let node_tbl = Stmt.Hashtbl.find callwise_state_with_formals current_stmt in
          let do_node _node dependencies_by_function =
            let kf_tbl =
              Kinstr.Hashtbl.find table_for_calls (Kstmt current_stmt)
            in
            List.iter
              (fun (kf, state) ->
                 if not (!Db.Value.use_spec_instead_of_definition kf) then begin
                   let stmt_tbl = Kernel_function.Hashtbl.find kf_tbl kf in
                   Stmt.Hashtbl.iter (fun stmt zone ->
                       let zone = Function_Froms.Memory.find state zone in
                       self#join stmt zone
                     ) stmt_tbl
                 end else
                   From_parameters.warning ~once:true ~current:true
                     "Assuming library function %a has no mem dependencies@."
                     Kernel_function.pretty kf
              ) dependencies_by_function
          in
          State_node.Hashtbl.iter do_node node_tbl;
          DoChildren
        | _ -> DoChildren
      end
      else SkipChildren

    method! vexpr exp =
      let join_deps deps =
        let froms = self#stmt_froms in
        let deps_f = Function_Froms.Memory.find froms deps in
        self#join (Extlib.the self#current_stmt) deps_f
      in
      match exp.enode with
      | BinOp ((Mult | Div | Mod), _, _, _)  ->
        ( match self# current_stmt with
        | Some stmt ->
          let nodes = Stmt.Hashtbl.find stmt_to_nodes stmt in
          State_node.Set.iter
            (State_node.with_state (fun state ->
                 let deps = From_compute.find_deps_no_transitivity state exp in
                 let deps = Function_Froms.Deps.to_zone deps in
                 join_deps deps
               ))
          nodes;
        | None -> () (* a size in a declaration, ignore for now XXX VLA? *));
          SkipChildren
      | AddrOf _lv | StartOf _lv ->
        SkipChildren

      | _ ->
        DoChildren

  end

let compute_memdeps_graph (stack, froms, stmt_to_nodes, callwise_state_with_formals) =
  let kf = Stack.top stack in
  match !analysis_stack with
  | { current_function; callsite; table_for_calls }
    :: ({ table_for_calls = table; _ } :: _ as tail) ->
    assert (Kernel_function.compare kf current_function = 0);
    analysis_stack := tail;
    begin match kf.fundec with
      | Definition (f, _) ->
        let computer =
          new do_memdeps_graph froms stmt_to_nodes
            callwise_state_with_formals table_for_calls
        in
        ignore (visitCilFunction (computer :> cilVisitor) f);
        let data = computer#result in
        let kf_table = make_kf_tbl kf data in
        add_to_table_for_calls table callsite kf_table;
        merge_pathdeps kf_table
      | Declaration _ ->
        assert false
    end

  | _ ->
    analysis_stack := []


let () =
  Cmdline.run_after_configuring_stage
    (fun () ->
       if From_parameters.MemDeps.get ()
       then begin
         Db.Value.Call_Value_Callbacks.extend_once register_in_stack;
         (*Db.From.Record_From_Callbacks.extend_once compute_memdeps;*)
         Db.From.Record_From_Graph_Callbacks.extend_once compute_memdeps_graph;
       end;
       Db.From.MemDeps.iter := MemdepsData.iter;
       Db.From.MemDeps.find := MemdepsData.find)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
