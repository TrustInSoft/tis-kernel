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

open Callwise_types
open Cil_datatype

module Kf_Froms = Kernel_function.Hashtbl.Make(Function_Froms)

module Tbl =
  Cil_state_builder.Kinstr_hashtbl
    (Kf_Froms)
    (struct
      let name = "Callwise dependencies"
      let size = 17
      let dependencies = [ Db.Value.self ]
    end)
let () = From_parameters.ForceCallDeps.set_output_dependencies [Tbl.self]
let () = Ast.add_monotonic_state Tbl.self

module Functionwise_Tbl =
  Kernel_function.Make_Table
    (Function_Froms)
    (struct
      let name = "Functionwise dependencies"
      let size = 17
      let dependencies = [ Tbl.self ]
    end)
let () = From_parameters.ForceDeps.set_output_dependencies
    [Functionwise_Tbl.self]
let () = Ast.add_monotonic_state Functionwise_Tbl.self

let create_kf_froms kf froms =
  let table = Kernel_function.Hashtbl.create 7 in
  Kernel_function.Hashtbl.add table kf froms;
  table

let merge_kernel_call_froms dest src =
  Kernel_function.Hashtbl.iter (fun kf froms ->
      try
        let current = Kernel_function.Hashtbl.find dest kf in
        let new_froms = Function_Froms.join froms current in
        Kernel_function.Hashtbl.replace dest kf new_froms
      with Not_found ->
        Kernel_function.Hashtbl.add dest kf froms
    ) src

let merge_call_froms table callsite froms =
  try
    let current = Kinstr.Hashtbl.find table callsite in
    merge_kernel_call_froms current froms
  with Not_found ->
    Kinstr.Hashtbl.add table callsite (Kernel_function.Hashtbl.copy froms)

let record_callwise_dependencies_in_db call_site froms =
  try
    let previous = Tbl.find call_site in
    merge_kernel_call_froms previous froms
  with Not_found ->
    Tbl.add call_site (Kernel_function.Hashtbl.copy froms)

let call_for_individual_froms (call_type, value_initial_state, call_stack,_) =
  let current_function, call_site = List.hd call_stack in
  let register_from froms =
    match !call_froms_stack with
    | { table_for_calls = table; _ } :: _ ->
      let froms_table = create_kf_froms current_function froms in
      merge_call_froms table call_site froms_table;
      record_callwise_dependencies_in_db call_site froms_table
    | [] -> (* No proper call site: this is an entry point *)
      let table_for_calls = Kinstr.Hashtbl.create 7 in
      let froms_table = create_kf_froms current_function froms in
      merge_call_froms table_for_calls call_site froms_table;
      record_callwise_dependencies_in_db call_site froms_table;
      call_froms_stack :=
        { current_function; value_initial_state; table_for_calls } ::
        !call_froms_stack
  in
  let open Value_types in
  match call_type with
  | `Def | `Memexec ->
    let table_for_calls = Kinstr.Hashtbl.create 7 in
    call_froms_stack :=
      { current_function; value_initial_state; table_for_calls } ::
      !call_froms_stack
  | `Builtin { c_from = Some (Froms result); _ } ->
    register_from result
  | `Spec
  | `Builtin { c_from = None; _ }
  | `Builtin { c_from = Some Closure _; _ } ->
    let froms =
      From_compute.compute_using_prototype_for_state
        value_initial_state current_function
    in
    register_from froms

let end_record call_stack froms =
  let (current_function_value, call_site) = List.hd call_stack in
  record_callwise_dependencies_in_db call_site froms;
  (* pop + record in top of stack the froms of function that just finished *)
  match !call_froms_stack with
  | { current_function; _ } :: ({ table_for_calls = table; _ } :: _ as tail) ->
    if current_function_value != current_function then
      From_parameters.fatal "calldeps %a != %a@."
        Kernel_function.pretty current_function
        Kernel_function.pretty current_function_value;
    call_froms_stack := tail;
    merge_call_froms table call_site froms

  | _ ->  (* the entry point, probably *)
    Tbl.mark_as_computed ();
    call_froms_stack := []


module MemExec =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (Kf_Froms)
    (struct
      let size = 17
      let dependencies = [Tbl.self]
      let name = "From.Callwise.MemExec"
    end)
let () = Ast.add_monotonic_state MemExec.self

let compute_call_from_value_states current_function states =
  let module Callwise_States = struct
    let get_value_state s =
      try Stmt.Hashtbl.find states s
      with Not_found -> Cvalue.Model.bottom
  end in
  let module Callwise_Froms = From_compute.Make(Callwise_States) in
  Callwise_Froms.compute_and_return current_function

let compute_call_from_graph current_function graph =
  let module Builder = Nodegraph.Builder in
  let module Contracter = Vcontraction.Make(struct include Nodegraph include Builder end) in
  let simplified = Contracter.contract (State_node.with_action (function State_node.PostAnnot -> false | _ -> true)) graph in
  let find_init p =
    let rec find_from v acc =
      if p v then State_node.Set.add v acc
      else Nodegraph.fold_succ find_from graph v acc
    in
    Nodegraph.fold_vertex  (fun v acc -> match State_node.action v with State_node.Init -> find_from v acc |_-> acc) graph State_node.Set.empty
  in
  let initial_nodes = find_init (State_node.with_action (fun act -> match act with State_node.PostAnnot -> true | _ -> false))
  in
  let module X = struct
    let graph = simplified
    let initial_nodes = initial_nodes
  end in
  let module Callwise_Froms = From_compute.GMake(X) in
  Callwise_Froms.compute_and_return current_function

let record_for_individual_froms (call_stack, value_res) =
  if From_parameters.GraphDeps.get () then () else
    let froms = match value_res with
      | Value_types.Normal (states, _after_states)
      | Value_types.NormalStore ((states, _after_states), _) ->
        let cur_kf, _ = List.hd call_stack in
        let froms =
          if !Db.Value.no_results cur_kf then
            Function_Froms.top
          else
            compute_call_from_value_states cur_kf (Lazy.force states)
        in
        let pre_state = match !call_froms_stack with
          | [] -> assert false
          | { value_initial_state; _ } :: _ -> value_initial_state
        in
        if From_parameters.VerifyAssigns.get () then
          !Db.Value.verify_assigns_froms cur_kf ~pre:pre_state froms;
        let froms_table = create_kf_froms cur_kf froms in
        begin match value_res with
          | Value_types.NormalStore (_, memexec_counter) ->
            let froms_table = create_kf_froms cur_kf froms in
            MemExec.replace memexec_counter froms_table
          | _ -> ()
        end;
        froms_table
      | Value_types.Reuse counter ->
        MemExec.find counter
    in
    end_record call_stack froms

let record_for_individual_froms_graph (call_stack, value_res) =
  if not (From_parameters.GraphDeps.get ()) then () else
    let froms = match value_res with
      | Value_types.Normal (graph)
      | Value_types.NormalStore (graph,_) ->
        let cur_kf = fst (List.hd call_stack) in
        let froms =
          if !Db.Value.no_results cur_kf then
            Function_Froms.top
          else
            compute_call_from_graph cur_kf graph
        in
        let pre_state = match !call_froms_stack with
          | [] -> assert false
          | { value_initial_state; _ } :: _ -> value_initial_state
        in
        if From_parameters.VerifyAssigns.get () then
          !Db.Value.verify_assigns_froms cur_kf ~pre:pre_state froms;
        let froms_table = create_kf_froms cur_kf froms in
        begin match value_res with
          | Value_types.NormalStore (_, memexec_counter) ->
            MemExec.replace memexec_counter froms_table
          | _ -> ()
        end;
        froms_table
      | Value_types.Reuse counter ->
        MemExec.find counter
    in
    end_record call_stack froms

let force_compute_all_calldeps ()=
  if Db.Value.is_computed () then
    Project.clear
      ~selection:(State_selection.with_dependencies Db.Value.self)
      ();
  !Db.Value.compute ()

(* Register our callbacks inside the value analysis *)
let () =
  Db.Value.Call_Type_Value_Callbacks.extend_once call_for_individual_froms;
  Db.Value.Record_Value_Graph_Callbacks.extend_once
    record_for_individual_froms_graph;
  Db.Value.Record_Value_Callbacks_New.extend_once record_for_individual_froms

let get kf =
  if not (Functionwise_Tbl.is_computed ()) then begin
    Tbl.iter (fun _kinstr kf_table ->
        Kernel_function.Hashtbl.iter (fun kf froms ->
            try
              let old = Functionwise_Tbl.find kf in
              let n   = Function_Froms.join old froms in
              Functionwise_Tbl.replace kf n
            with Not_found ->
              Functionwise_Tbl.add kf froms
          ) kf_table
      );
    Functionwise_Tbl.mark_as_computed ()
  end;
  try
    Functionwise_Tbl.find kf
  with Not_found ->
    Function_Froms.({
        deps_return = Memory.default_return;
        deps_table  = Memory.bottom })

(* Registration for call-wise from *)
let () =
  Db.register_guarded_compute
    "From.compute_all_calldeps"
    (Tbl.is_computed ?project:None)
    Db.From.compute_all_calldeps
    force_compute_all_calldeps;
  Db.From.Callwise.iter := Tbl.iter;
  Db.From.Callwise.find := (fun kinstr ->
      let table = Tbl.find kinstr in
      let froms =
        Kernel_function.Hashtbl.fold (fun _ froms acc ->
            match acc with
            | None -> Some froms
            | Some acc -> Some (Function_Froms.join acc froms)
          ) table None
      in Extlib.the froms
    );
  Db.From.self := Tbl.self;
  (* Db.From.is_computed := Tbl.mem; *)
  (* Db.From.compute := *)
  (*   (fun _ -> ignore (To_Use.memo kf)); *)
  Db.From.get := get;
  Db.From.pretty :=
    (fun fmt v ->
       let deps = !Db.From.get v in
       Function_Froms.pretty_with_type (Kernel_function.get_type v) fmt deps);
  Db.From.find_deps_no_transitivity :=
    (fun stmt lv ->
       let state = Db.Value.get_stmt_state stmt in
       let deps = From_compute.find_deps_no_transitivity state lv in
       Function_Froms.Deps.to_zone deps);
  Db.From.find_deps_no_transitivity_state :=
    (fun s e ->
       let deps = From_compute.find_deps_no_transitivity s e in
       Function_Froms.Deps.to_zone deps);

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
