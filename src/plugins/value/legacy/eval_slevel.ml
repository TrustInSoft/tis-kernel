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

open State_node
open Cil_types
open Cil
open Cil_datatype
open Cvalue
open Value_util
open Eval_exprs

let dkey_callbacks = Value_parameters.register_category "callbacks"

let check_signals, signal_abort =
  let signal_emitted = ref false in
  (fun () ->
     if !signal_emitted then begin
       signal_emitted := false;
       Value_parameters.warning "Stopping analysisat user request@.";
       raise Db.Value.Aborted
     end),
  (fun () -> signal_emitted := true)

module type Arg = sig
  val kf: kernel_function
  val initial_states : State_set.t
  val active_behaviors: Eval_annots.ActiveBehaviors.t
  val caller_node: State_node.t option
end

module Computer(AnalysisParam: Arg) =
struct
  let current_kf = AnalysisParam.kf
  let current_fundec = Kernel_function.get_definition current_kf
  let return = Kernel_function.find_return current_kf
  let return_lv =
    match return.skind with
    | Return (Some ({ enode = Lval lv; _ }),_) -> Some lv
    | Return (None,_) -> None
    | _ -> assert false (* Cil invariant *)

  let obviously_terminates =
    Value_parameters.ObviouslyTerminatesAll.get() (* TODO: by function *)

  (* Widening will be performed at the statements verifying this predicate. *)
  let is_loop =
    if obviously_terminates
    then fun _ -> false
    else
      let is_natural_loop = Loop.is_natural current_kf in
      let is_basic_loop s =
        is_natural_loop s ||
        match s.skind with Loop _ -> true | _ -> false
      in
      let non_natural = Loop.get_non_naturals current_kf in
      if Stmt.Set.is_empty non_natural
      then
        is_basic_loop
      else
        (fun s -> is_basic_loop s || Stmt.Set.mem s non_natural)

  let should_memorize_function =
    Mark_noresults.should_memorize_function current_kf

  let initial_state = State_set.join AnalysisParam.initial_states

  let slevel =
    if obviously_terminates
    then Per_stmt_slevel.Global max_int
    else Per_stmt_slevel.local current_kf

  let slevel stmt = match slevel with
    | Per_stmt_slevel.Global -1 ->
      let tis_slevel_val = Value_util.get_tis_slevel_value initial_state in
      Value_parameters.debug "__tis_slevel variable used: %d" tis_slevel_val;
      tis_slevel_val
    | Per_stmt_slevel.Global i -> i
    | Per_stmt_slevel.PerStmt f -> f stmt

  let save_slevel_results =
    Value_parameters.ResultsSlevel.get () && should_memorize_function

  (* This function decides whether we should merge all incoming states
     on the given statement before treating it. *)
  let merge =
    (* Ideally, we would like to merge only the states propagated along the
       back edges of the loop. Since this is not currently easy, we
       use an approximation that consists in merging all the states on the
       loop node. *)
    let after_loop =
      Kernel_function.Set.mem current_kf
        (Value_parameters.SlevelMergeAfterLoop.get ())
    in
    match Per_stmt_slevel.merge current_kf with
    | Per_stmt_slevel.NoMerge ->
      if after_loop then
        (fun stmt ->
           match stmt.skind with
           | Loop _ -> true
           | _ -> false)
      else (fun _ -> false)
    | Per_stmt_slevel.Merge fun_s ->
      (fun stmt ->
         fun_s stmt ||
         (after_loop && match stmt.skind with Loop _ -> true | _ -> false))


  (*  State propagated by the dataflow, that contains only 'new' states
      (i.e. not propagated before). *)
  type diff = { mutable to_propagate : State_node.t list ; }

  (*  The real state for a given statement, used in particular to detect
      convergence. Stored by us, not by the dataflow itself. *)
  type stmt_state = {
    (* All the state that have been propagated separately, by slevel *)
    superposition : State_imp.t;

    (*  Bottom if we have never consumed all the slevel allocated. If no
        more slevel is available, the state that is being propagated. This
        state is *not* present in [superposition]. *)
    mutable widening_state : State_node.t ;

    (*  should we widen the statement at the current iteration.
        [widening_state] is decremented each time we visit the statement,
        unless it is equal to zero. (In which case we widen, and set
        [widening_state] to a non-zero value, currently 1.) *)
    mutable widening : int;

    (*  Number of states that were put in [superposition];
        i.e. the sum of the cardinals of the state sets that were added with
        [update_and_tell_if_changed]. It may be different (i.e. larger) from
        the cardinal of [state_imp], that merge states that are equal. *)
    mutable counter_unroll : int ;

    (** Reason for precision loss by State_set.join. See
        value_util.mli for details. *)
    mutable join_reason : int;

    (** Each state passed to doInstr and doStmt before and during
        widening. We can read the states and slevel here in a more
        precise way than counter_unroll which is an
        over-approximation. *)
    mutable each_state : (Cvalue.Model.t * Cvalue.Model.t option) list;

    (** The first [widening_length] elements of [each_state] are
        widening states. A value of -1 means not widening yet. It is
        set back to 0 in [update_join_points]. *)
    mutable widening_length : int;
  }

  let update_widening_length current_info length =
    if current_info.widening_length <> -1
    then current_info.widening_length <- current_info.widening_length + length

  module Builder = struct
    include Nodegraph.Builder
    let add_callwise_vertex = Nodegraph.Builder.add_vertex
    let add_wp_vertex = Value_results.add_wp_vertex
    let add_vertex g v =
      ignore (add_wp_vertex v);
      ignore (add_callwise_vertex g v)
    let add_callwise_edge = Nodegraph.Builder.add_edge
    let add_wp_edge = Value_results.add_wp_edge
    let add_edge g v1 v2 =
      ignore (add_wp_edge v1 v2);
      ignore (add_callwise_edge g v1 v2)
  end

  let node_table = State_node.Data.Hashtbl.create 97
  let graph = Builder.empty ()
  let get_node = State_node.get node_table (call_stack ())

  type t = stmt_state Stmt.Hashtbl.t

  let empty_record s = {
    superposition = State_imp.empty () ;
    widening = Value_parameters.WideningLevel.get () ;
    widening_state = get_node s Widen Cvalue.Model.bottom ;
    counter_unroll = 0;
    join_reason = 0;
    each_state = [];
    widening_length = -1;
  }

  let select_state = State_node.state

  let select_states nodes = List.map select_state nodes

  let add_edge = Builder.add_edge graph

  (**
     Adds a state to a node list by creating a new node from it,
     and links it to before in the graph.
     In the whole-program graph, use wpbefore instead of before.
  *)
  let add_and_link state create ?wpbefore diff ~before =
    if Cvalue.Model.is_reachable state
    then
      let node = create state in
      begin match wpbefore with
        | None -> ignore (Builder.add_edge graph before node)
        | Some wpnode ->
          begin
            ignore (Builder.add_wp_edge wpnode node);
            ignore (Builder.add_callwise_edge graph before node);
          end
      end;
      node::diff
    else diff

  let reduce_table = Cvalue.Model.Hashtbl.create 7
  let debug_reduce = false

  let rec find state = (*Path compression*)
    try
      let s = Cvalue.Model.Hashtbl.find reduce_table state in
      if Cvalue.Model.equal s state then s
      else
        let s' = find s in
        Cvalue.Model.Hashtbl.replace reduce_table state s';
        if debug_reduce then assert (Cvalue.Model.is_included state s');
        s'
    with Not_found -> state

  let add_nodes_to_reduce acc len nodes =
    if debug_reduce then assert (List.length acc = len);
    let len = ref len in
    let add_and_reduce acc state =
      (* invariant : len = length acc && forall s in acc, find s == s *)
      if debug_reduce then assert (List.length acc = !len);
      if Cvalue.Model.is_reachable state
      then
        if !len <= 4
        then begin match List.find (Cvalue.Model.is_included state) acc with
          | s -> Cvalue.Model.Hashtbl.replace reduce_table state s; acc
          | exception Not_found ->
            if debug_reduce then assert (Cvalue.Model.equal (find state) state);
            let acc =
              List.fold_right
                (fun s acc ->
                   if Cvalue.Model.is_included s state
                   then begin
                     Cvalue.Model.Hashtbl.replace reduce_table s state;
                     decr len;
                     acc
                   end
                   else s::acc)
                acc
                []
            in
            incr len;
            state::acc
        end
        else (incr len; state::acc)
      else acc
    in
    let states =
      List.fold_left
        (fun acc node -> add_and_reduce acc (State_node.state node))
        acc
        nodes
    in
    states, !len

  let build_edges_to_afternodes get nodes =
    List.iter
      (fun before ->
         let after = get (find (State_node.state before)) in
         ignore (Builder.add_edge graph before after))
      nodes

  (**
      Reduces a list of nodes to a smaller one by eliminating subsumed
      states, and adds edges to the graph so that each node of the
      input is linked to one with a greater state in the output.
  *)
  let reduce stmt act nodes  =
    let get = get_node stmt (Reduce act) in
    let states,_ = add_nodes_to_reduce [] 0 nodes in
    build_edges_to_afternodes get nodes;
    Cvalue.Model.Hashtbl.clear reduce_table;
    List.rev_map get states

  let merge_into = List.rev_append

  let join_and_add_edges s d msg =
    let joinstate =
      List.fold_left
        (fun acc -> with_state (fun s -> Cvalue.Model.join acc s))
        Cvalue.Model.bottom
        d
    in
    let joinnode = get_node s (Join msg) joinstate in
    List.iter (fun node -> ignore (Builder.add_edge graph node joinnode)) d;
    joinnode

  let current_table : t = Stmt.Hashtbl.create 128

  let return_nodes = ref State_node.Set.empty

  let stmt_state s =
    try Stmt.Hashtbl.find current_table s
    with Not_found ->
      let record = empty_record s in
      Stmt.Hashtbl.add current_table s record;
      record

  let lost_precision ci _f v =
    ci.join_reason <- ci.join_reason lor v

  let stmt_widening_info s =
    let r = stmt_state s in
    r.widening, r.widening_state

  (* merges [set] into the state associated to [stmt], and returns the subset
     of [set] that was not already in the superposition. *)
  let update_stmt_states stmt states =
    let record = stmt_state stmt in
    let widennode = record.widening_state in
    let widenstate = select_state widennode in
    if Cvalue.Model.is_reachable widenstate
    then
      let joinnode = join_and_add_edges stmt states "update_stmt_states" in
      let joinstate = select_state joinnode in
      let newnode =
        if Cvalue.Model.is_included joinstate widenstate
        then begin
          let newnode = get_node stmt MergeIncludedStates widenstate in
          ignore (Builder.add_edge graph joinnode newnode);
          newnode
        end
        else
          joinnode
      in
      [newnode]
    else
      begin
        let keep = State_imp.merge_set_return_new states record.superposition in
        (* if this causes stack overflows, rewrite it in fold_left style and
           return List.rev (newstates keep) *)
        let rec newstates keep =
          match keep with
          | State_imp.Empty -> []
          | State_imp.Keep (node, state, acc)
          | State_imp.Discard (node, state, acc) ->
            let succnode = get_node stmt MergeIncludedStates state in
            ignore (Builder.add_edge graph node succnode);
            ( match keep with
              | State_imp.Keep _ -> succnode :: (newstates acc)
              | _ -> newstates acc (* tail-call: do not factor *) )
        in
        newstates keep
      end

  let update_stmt_widening_info stmt wcounter wstate =
    let record = stmt_state stmt in
    record.widening <- wcounter;
    record.widening_state <- wstate

  let states_unmerged_for_callbacks () =
    let r = Stmt.Hashtbl.create (Stmt.Hashtbl.length current_table) in
    let aux stmt record =
      let states = State_imp.to_list record.superposition in
      let wstate = select_state record.widening_state in
      let states =
        if Cvalue.Model.is_reachable wstate
        then wstate :: states
        else states
      in
      Stmt.Hashtbl.add r stmt states
    in
    Stmt.Hashtbl.iter aux current_table;
    r

  let states_for_callbacks () =
    let r = Stmt.Hashtbl.create (Stmt.Hashtbl.length current_table) in
    let aux stmt record =
      Stmt.Hashtbl.add r stmt
        (Cvalue.Model.join
           (State_imp.join record.superposition)
           (select_state record.widening_state))
    in
    Stmt.Hashtbl.iter aux current_table;
    r

  let states_after = Cil_datatype.Stmt.Hashtbl.create 5

  (* During the dataflow analysis, if required by a callback, we store the
     state after a statement, but only if either the following conditions
     is met ([succ] being a successor of [s]):
     - [s] is an instr (the control flow statements such as [goto] and [if]
       do not change the state (union of the states in the case of if))
       AND there is a control-flow join on [succ],
     - [s] is the last instruction of a block that contains local variables.
     For statements for which the function below returns false, we deduce
     the state after by the state before [succ] or another successor of [s].
     This avoids potentially useless computations. *)
  let store_state_after_during_dataflow s succ =
    ((match s.skind with Instr _ -> true | _ -> false) &&
     (match succ.preds with [_] -> false | _ -> true))
    || (let b1 = Kernel_function.find_enclosing_block s in
        let b2 = Kernel_function.find_enclosing_block succ in
        not (Cil_datatype.Block.equal b1 b2) && b1.blocals <> [])

  (* Computation of the per-function 'after statement' states *)
  let local_after_states superposed =
    lazy (
      if not should_memorize_function
      then Value_parameters.fatal
          "An analysis tried to use the after states of function %a which \
           have not been stored." Kernel_function.pretty current_kf;
      let superposed = Lazy.force superposed in
      Stmt.Hashtbl.iter
        (fun stmt state ->
           List.iter
             (fun pred ->
                if not (store_state_after_during_dataflow pred stmt) then
                  try
                    let cur = Stmt.Hashtbl.find states_after pred in
                    Stmt.Hashtbl.replace states_after pred
                      (Cvalue.Model.join state cur)
                  with Not_found -> Stmt.Hashtbl.add states_after pred state
             ) stmt.preds;
        ) superposed;
      (* Since the return instruction has no successor, it is not visited
         by the iter above. We fill it manually *)
      (try
         let s = Stmt.Hashtbl.find superposed return in
         Stmt.Hashtbl.add states_after return s
       with Kernel_function.No_Statement | Not_found -> ()
      );
      states_after
    )

  (* Table storing whether conditions on 'if' have been evaluated
     to true or false *)
  let conditions_table = Cil_datatype.Stmt.Hashtbl.create 5

  let update_join_points cs =
    let module CS = Value_callstack.Callstack.Hashtbl in
    let update s x =
      let h =
        try fst (JoinPoints.find s)
        with Not_found ->
          let h = CS.create 7 in
          let l = if obviously_terminates then -1 else slevel s in
          JoinPoints.add s (h, l);
          h
      in
      let (o_reason, o_slevel, o_widening) =
        try CS.find h cs
        with Not_found -> (0, 0, 0)
      in
      let n_reason = o_reason lor x.join_reason in
      let c_slevel, c_widening =
        if save_slevel_results
        then
          let total = List.length x.each_state in
          if x.widening_length = -1 then x.widening_length <- 0;
          total - x.widening_length, x.widening_length
        else x.counter_unroll, 0
      in
      let n_slevel = max o_slevel c_slevel in
      let n_widening = max o_widening c_widening in
      CS.replace h cs (n_reason, n_slevel, n_widening)
    in
    (* TODO: Ideally, we would like to iter on all statements
       (including unreachable ones). *)
    Stmt.Hashtbl.iter update current_table

  let merge_results () =
    let superposed = lazy (states_for_callbacks ()) in
    let after_full = local_after_states superposed in
    let stack_for_callbacks = call_stack () in
    let memexec_counter =
      if Value_parameters.MemExecAll.get ()
      then Mem_exec.new_counter ()
      else -1 (* this value should never be read *)
    in
    update_join_points stack_for_callbacks;
    if save_slevel_results
    then begin
      let store = not (Db.Value.Record_Value_Slevel_Callbacks.is_empty ()) in
      let hstates = Cil_datatype.Stmt.Hashtbl.create 5 in
      Cil_datatype.Stmt.Hashtbl.iter
        (fun stmt record ->
           let states =
             Array.of_list record.each_state, record.widening_length
           in
           if store then Cil_datatype.Stmt.Hashtbl.add hstates stmt states;
           Db.Value.update_slevel_table stmt stack_for_callbacks states)
        current_table;
      if store then begin
        if Value_parameters.ValShowProgress.get () then
          Value_parameters.debug ~dkey:dkey_callbacks
            "now calling Record_Value_Slevel callbacks";
        if Value_parameters.MemExecAll.get ()
        then Db.Value.Record_Value_Slevel_Callbacks.apply
            (stack_for_callbacks,
             Value_types.NormalStore (hstates, memexec_counter))
        else Db.Value.Record_Value_Slevel_Callbacks.apply
            (stack_for_callbacks, Value_types.Normal hstates)
      end;
    end;
    if should_memorize_function then begin
      Value_results.merge_states_in_db superposed stack_for_callbacks;
      Db.Value.merge_conditions conditions_table;
      Value_results.merge_after_states_in_db after_full stack_for_callbacks;
    end;
    if not (Db.Value.Record_Value_Superposition_Callbacks.is_empty ())
    then begin
      let current_superpositions = lazy (states_unmerged_for_callbacks ()) in
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value_Superposition callbacks";
      Db.Value.Record_Value_Superposition_Callbacks.apply
        (stack_for_callbacks, current_superpositions);
    end ;
    if not (Db.Value.Record_Value_Callbacks.is_empty ())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value callbacks";
      Db.Value.Record_Value_Callbacks.apply
        (stack_for_callbacks, superposed)
    end;
    if not (Db.Value.Record_Value_Callbacks_New.is_empty ())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value_New callbacks";
      if Value_parameters.MemExecAll.get () then
        Db.Value.Record_Value_Callbacks_New.apply
          (stack_for_callbacks,
           Value_types.NormalStore ((superposed, after_full),
                                    memexec_counter))
      else
        Db.Value.Record_Value_Callbacks_New.apply
          (stack_for_callbacks,
           Value_types.Normal (superposed, after_full))
    end;
    if not (Db.Value.Record_Value_Graph_Callbacks.is_empty())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value_Graph callbacks";
      if Value_parameters.MemExecAll.get () then
        Db.Value.Record_Value_Graph_Callbacks.apply
          (stack_for_callbacks,
           Value_types.NormalStore (graph, memexec_counter))
      else

        Db.Value.Record_Value_Graph_Callbacks.apply
          (stack_for_callbacks,
           Value_types.Normal graph);
    end;
    if not (Db.Value.Record_Value_Graph_Callbacks_Progress.is_empty())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value_Graph callbacks";
      Db.Value.Record_Value_Graph_Callbacks_Progress.apply
        (stack_for_callbacks, graph)
    end;
    if not (Db.Value.Record_Value_After_Callbacks.is_empty ())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_After_Value callbacks";
      Db.Value.Record_Value_After_Callbacks.apply
        (stack_for_callbacks, after_full);
    end;
  ;;

  (* Clobbered list for bases containing addresses of local variables. *)
  let clob = Locals_scoping.bottom ()

  let cacheable = ref Value_types.Cacheable

  module DataflowArg: Dataflow2.ForwardsTransfer
    with type t = diff
  = struct

    let debug = false
    let name = "Values analysis"

    module StmtStartData =
      Dataflow2.StartData(struct type t = diff let size = 107 end)

    type t = diff

    let copy (d: t) = d

    let display_one fmt v =
      List.iter
        (with_state (fun values ->
             if not (Cvalue.Model.is_reachable values) then
               Format.fprintf fmt "Statement (x) with trace %a : UNREACHABLE@\n"
                 Trace.pretty Trace.top
             else
               Format.fprintf fmt "Statement (x) with trace %a : @\n%a"
                 Trace.pretty Trace.top Cvalue.Model.pretty values))
        v.to_propagate

    let pretty fmt (d: t) = display_one fmt d

    let do_computeFirstPredecessor (s: stmt) states () =
      let v = states.to_propagate in
      let v =  List.map (with_state_node (fun state node ->
          let newnode = get_node s ComputeFirstPredecessor state in
          ignore (Builder.add_edge graph node newnode);
          newnode))
          v in
      (stmt_state s).counter_unroll <- List.length v;
      (* Create an impure state for this statement. It will be mutated by
         the other functions *)
      { to_propagate = v; }

    let computeFirstPredecessor s states =
      Value_stat.measure_statement s (do_computeFirstPredecessor s states)

    let counter_unroll_target = ref (Value_parameters.ShowSlevel.get())

    let not_return s = match s.skind with Return _ -> false | _ -> true

    let do_combinePredecessors (s: stmt) ~old new_ () =
      let new_v = new_.to_propagate in
      if new_v = []
      then None
      else begin
        let new_v =
          List.map
            (with_state_node (fun st node ->
                 let newnode = get_node s CombinePredecessors st in
                 ignore (Builder.add_edge graph node newnode);
                 newnode))
            new_v
        in
        (* Update loc, which can appear in garbled mix origins. *)
        let old_loc = Cil.CurrentLoc.get () in
        Cil.CurrentLoc.set (Cil_datatype.Stmt.loc s);
        (* Note: When we join traces, they must lead to the same statement;
           thus we need to add the statement here (instead of e.g. in doStmt,
           which would be too late). *)
        (*let new_v = State_set.add_statement new_v s in*)
        let current_info = stmt_state s in
        let old_counter = current_info.counter_unroll in
        (* Check whether there is enough slevel available. If not, merge all
           states together. However, do not perform merge on return
           instructions. This needelessly degrades precision for
           postconditions and option -split-return. *)
        let r =
          if old_counter > slevel s && (not_return s)
          then
            begin
              let new_node = join_and_add_edges
                  s new_v "combinePredecessors_new"
              in
              let old_node = join_and_add_edges
                  s old.to_propagate "combinePredecessors_old"
              in
              let join = join_and_add_edges
                  s [new_node; old_node] "combinePredecessors_all"
              in
              lost_precision current_info "combinePredecessors" 1;
              old.to_propagate <- [join];
              Some old
            end
          else
            begin
              let merged =
                (* Rather than call reduce on new_v@combine_predecessors,
                   we save a list concatenation by calling the subfunctions
                   on both lists separately and clearing the global reduce table
                   only after. *)
                let acc,len = add_nodes_to_reduce [] 0 new_v in
                let acc,_ = add_nodes_to_reduce acc len old.to_propagate in
                let get = get_node s (Reduce CombinePredecessors) in
                build_edges_to_afternodes get new_v;
                build_edges_to_afternodes get old.to_propagate;
                Cvalue.Model.Hashtbl.clear reduce_table;
                List.rev_map get acc
              in
              let length_new = List.length new_v in
              let new_counter_unroll = old_counter + length_new in
              if new_counter_unroll >= !counter_unroll_target
              then begin
                let period = Value_parameters.ShowSlevel.get() in
                let reached = new_counter_unroll / period * period in
                Value_parameters.feedback ~once:true
                  "Semantic level unrolling superposing up to %d states"
                  reached;
                counter_unroll_target := reached + period;
              end;
              current_info.counter_unroll <- new_counter_unroll;
              old.to_propagate <- merged;
              Some old
            end
        in
        Cil.CurrentLoc.set old_loc;
        r
      end

    let combinePredecessors s ~old new_ =
      Value_stat.measure_statement s (do_combinePredecessors s ~old new_)

    let interp_call store stmt lval_to_assign funcexp argl d_value =
      let with_alarms = warn_all_quiet_mode () in
      let interp_call node =
        Eval_stmt.interp_call
          ~with_alarms ~get_node clob stmt lval_to_assign funcexp argl node
      in
      let join_results node (merge_state, return_nodes) (state, ret_node) =
        let action = match ret_node with
          | None -> State_node.Return (State_node.BuiltinOrContract, node)
          | Some node -> State_node.action node
        in
        let get_state_node = get_node stmt action in
        Cvalue.Model.join merge_state state,
        add_and_link state get_state_node return_nodes
          ?wpbefore:ret_node ~before:node
      in
      let do_call nodes_after_call node =
        let results, call_cacheable = interp_call node in
        if call_cacheable = Value_types.NoCacheCallers then
          (* Propagate info that the current call cannot be cached either *)
          cacheable := Value_types.NoCacheCallers;
        let state_after_call, nodes_after_call =
          List.fold_left (join_results node)
            (Cvalue.Model.bottom, nodes_after_call) results
        in
        store (State_node.state node) (Some state_after_call);
        nodes_after_call
      in
      (* reduce should be called by the caller, after potential callbacks on the
         Return nodes*)
      List.fold_left do_call [] d_value

    let do_doInstr
        stmt
        (i: instr)
        (_wl: Dataflow2.ForwardWorklist.t)
        (d: t)
        () =
      !Db.progress ();
      Valarms.start_stmt (Kstmt stmt);
      let d_states = d.to_propagate in
      let unreachable = (d_states = []) in
      let result =
        if unreachable then d
        else begin
          let with_alarms = warn_all_quiet_mode () in
          let store_before_after =
            if save_slevel_results then begin
              let current_info = stmt_state stmt in
              fun before after ->
                current_info.each_state <-
                  (before, after) :: current_info.each_state;
                update_widening_length current_info 1;
            end else fun _ _ -> ()
          in
          let propagate states =
            (* Create a transient propagation result, that will be passed
               to the successors of stmt by the dataflow module *)
            { to_propagate = states }
          in
          let trivial_instr () = List.iter
              (with_state_node (fun state before ->
                   store_before_after state (Some state);
                   let after = get_node stmt Statement state in
                   ignore(Builder.add_edge graph before after)
                 ))
              d.to_propagate;
            d
          in
          let apply_each_state f =
            let states_after_i =
              List.fold_left
                (fun acc ->
                   with_state_node (fun before beforenode ->
                       Value_messages.set_current_state before;
                       let after =
                         try f before
                         with Db.Value.Aborted as e ->
                           store_before_after before None; raise e
                       in
                       store_before_after before (Some after);
                       let get_afternode = get_node stmt Statement in
                       add_and_link after get_afternode acc ~before:beforenode))
                []
                d_states
            in
            propagate (reduce stmt Statement states_after_i)
          in
          (* update current statement *)
          match i with
          | Set (lv,exp,_loc) ->
            apply_each_state
              (fun state_value ->
                 Eval_stmt.do_assign ~with_alarms
                   current_kf clob state_value lv exp)
          (* TODOBY: this should be transferred as a builtin. However, this
             is not possible for va_arg currently *)

          (* Calls to variadic macros va_start, va_copy, va_end, and va_arg are
             pre-processed in [Cabs2cil.do_expression], so here they already
             have specific arguments. *)

          (* [va_start(ap, parmN)] call is converted to [va_start(ap)]
             (only if [parmN] is correct!): [ap] is a va_list variable. *)
          | Call
              (_,
               { enode =
                   Lval (Var { vname = "__builtin_va_start"; _ }, NoOffset);
                 _ },
               [ { enode = Lval va_list_lv; _ } as _va_list_exp ],
               _loc) ->
            apply_each_state
              (Value_variadic.va_start ~with_alarms current_kf va_list_lv)

          (* [va_copy(dest, src)] call: both [dest] and [src] are va_list
             variables. *)
          | Call
              (_,
               { enode =
                   Lval (Var { vname = "__builtin_va_copy"; _ }, NoOffset);
                 _ },
               [ { enode = Lval va_list_dest_lv; _ } as _va_list_dest_exp;
                 { enode = Lval _va_list_src_lv; _ } as va_list_src_exp ],
               _loc) ->
            apply_each_state
              (Value_variadic.va_copy ~with_alarms current_kf
                 (va_list_dest_lv, va_list_src_exp))

          (* [va_end(ap)] call: [ap] is a va_list variable. *)
          | Call
              (_,
               { enode = Lval (Var { vname = "__builtin_va_end"; _ }, NoOffset);
                 _ },
               [ { enode = Lval va_list_lv; _ } ],
               _loc) ->
            apply_each_state
              (Value_variadic.va_end ~with_alarms current_kf va_list_lv)

          (* [va_arg(ap, type)] call is converted to [va_arg(ap, type, dest)]:
             - [ap] is a va_list variable,
             - [type] is the expected type of the next argument,
             - [dest] is the pointer to the lvalue where the result should
               be stored. *)
          | Call
              (_,
               { enode = Lval (Var { vname = "__builtin_va_arg"; _ }, NoOffset);
                 _ },
               [ { enode = Lval va_list_lv; _ } as _va_list_exp;
                 { enode = SizeOf expected_arg_typ; _ } as _size_exp;
                 { enode = CastE (voidPtrType, { enode = AddrOf dest_lv; _ });
                   _ } as _dest_exp ],
               _loc)
            when Cil.isVoidPtrType voidPtrType ->
            apply_each_state
              (Value_variadic.va_arg ~with_alarms current_kf
                 (va_list_lv, expected_arg_typ, dest_lv))

          (* Variadic macros should always match the above patterns!
             If they don't then something went wrong. *)
          | Call
              (_,
               { enode =
                   Lval
                     (Var
                        { vname = ("__builtin_va_start" as va_builtin_name );
                          _ },
                      _); _ }, _, _)
          | Call
              (_,
               { enode =
                   Lval
                     (Var
                        { vname = ("__builtin_va_copy" as va_builtin_name );
                          _ },
                      _); _ }, _, _)
          | Call
              (_,
               { enode =
                   Lval
                     (Var
                        { vname = ("__builtin_va_end" as va_builtin_name ); _ },
                      _); _ }, _, _)
          | Call
              (_,
               { enode =
                   Lval
                     (Var
                        { vname = ("__builtin_va_arg" as va_builtin_name ); _ },
                      _); _ }, _, _) ->
            Value_parameters.fatal ~current:true
              "call to the variadic built-in %s impossible: arguments malformed"
              va_builtin_name

          | Call (lval_to_assign, funcexp, argl, _loc) ->
            if not (Db.Value.Record_Value_Graph_Callbacks_Progress.is_empty())
            then begin
              Db.Value.Record_Value_Graph_Callbacks_Progress.apply
                (call_stack (), graph);
            end;
            let nodes_after_call =
              interp_call
                store_before_after stmt lval_to_assign funcexp argl d_states
            in
            if not
                (Db.Value.Record_Value_Graph_Callbacks_StoreLiveNodes.is_empty
                   ())
            then begin
              let set = State_node.Set.of_list nodes_after_call in
              Db.Value.Record_Value_Graph_Callbacks_StoreLiveNodes.apply
                (call_stack (), Db.Value.ReturnOfFunction set)
            end;
            (* nodes returned by interp_call were not reduced to preserve Return
               info, do it here *)
            propagate (reduce stmt InterpCall nodes_after_call)
          | Asm _ ->
            let compute_annotations state =
              Annotations.fold_code_annot
                (fun _ annot acc ->
                   let state_set =
                     Eval_annots.interp_annot current_kf ~stmt_spec:true
                       AnalysisParam.active_behaviors state (slevel stmt)
                       (State_set.singleton acc) stmt annot true
                   in
                   State_set.join state_set)
                stmt
                state
            in
            apply_each_state compute_annotations
          | Skip _ -> trivial_instr ()
          | Code_annot (_, _) -> (* processed direcly in doStmt from the
                                    annotation table *)
            trivial_instr ()
        end
      in
      Valarms.end_stmt ();
      result

    let doInstr stmt i wl d =
      Value_stat.measure_statement stmt (do_doInstr stmt i wl d)

    let doStmtSpecific s _d states =
      match s.skind with
      | Loop _ ->
        let current_info = stmt_state s in
        let counter = current_info.counter_unroll in
        if counter > slevel s then
          Value_parameters.feedback ~level:1 ~once:true ~current:true
            "entering loop for the first time";
        states

      | UnspecifiedSequence seq ->
        (try
           if Kernel.UnspecifiedAccess.get ()
           then begin
             List.iter
               (with_state (fun state ->
                    Eval_stmt.check_unspecified_sequence state seq))
               states;
           end;
           states
         with Eval_stmt.AlwaysOverlap -> [])
      | Cil_types.Return _ ->
        return_nodes :=
          List.fold_left
            (fun acc n -> State_node.Set.add n acc)
            !return_nodes
            states;
        states
      | _ -> states

    let do_doStmt (s: stmt) (d: t) () =
      Valarms.start_stmt (Kstmt s);
      check_signals ();
      (* Merge incoming states if the user requested it *)
      if merge s then
        begin
          match d.to_propagate with
          | [] | [_] -> ()
          (* This should be fine because update_stmt_states would turn
             the bottom singleton into an empty list. In case of
             obviously_terminates, interp_annot would do it (I guess).*)
          | _ ->
            lost_precision (stmt_state s) "merge_if_loop" 2;
            let joinstate =
              List.fold_left
                (fun acc ->
                   with_state (fun state -> Cvalue.Model.join acc state))
                Cvalue.Model.bottom
                d.to_propagate
            in
            let joinnode = get_node s MergeAfterLoop joinstate in
            List.iter
              (fun node -> ignore (Builder.add_edge graph node joinnode))
              d.to_propagate;
            d.to_propagate <- [joinnode]
        end;
      let states = d.to_propagate in
      Db.Value.Compute_Statement_Callbacks.apply
        (s, call_stack(), select_states states);
      (* Cleanup function, to be called on all exit paths *)
      let ret result =
        (* Do this as late as possible, as a non-empty to_propagate field
           is shown in a special way in case of degeneration *)
        d.to_propagate <- [];
        Valarms.end_stmt ();
        result
      in
      if states = [] then ret Dataflow2.SDefault
      else
        let states =
          if obviously_terminates && (not_return s)
          then states
          (* We don't need to check if we lose precision in
             update_stmt_states because we did it before calling
             update_stmt_widening_info. *)
          else update_stmt_states s states (* Remove states already present *)
        in
        if states = [] then ret Dataflow2.SDefault
        else
          (* We do not interpret annotations that come from statement contracts
             and everything previously emitted by Value (currently, alarms) *)
          let annots =
            Annotations.fold_code_annot
              (fun e ca acc ->
                 if Logic_utils.is_contract ca ||
                    Emitter.equal e Value_util.emitter
                 then acc
                 else ca :: acc)
              s
              []
          in
          let slevel = slevel s in
          let interp_annots s state node record msg =
            let interp_annot states annot  =
              Eval_annots.interp_annot
                current_kf AnalysisParam.active_behaviors initial_state slevel
                states s annot record
            in
            let stateset =
              List.fold_left
                interp_annot
                (State_set.singleton state)
                annots
            in
            let nodes = State_set.fold
                (fun acc newstate ->
                   let get_succnode = get_node s (InterpAnnot msg) in
                   add_and_link newstate get_succnode acc ~before:node)
                []
                stateset
            in
            reduce s (InterpAnnot msg) nodes
          in
          let interp_annots = with_stmt_state_node interp_annots in
          let states =
            List.fold_left
              (fun acc node ->
                 let sid = string_of_int (State_node.id node) in
                 let states = interp_annots node true ("doStmt1_"^sid) in
                 merge_into states acc)
              []
              (List.rev states)
          in
          let states = reduce s (InterpAnnot "doStmt1") states in
          let states =
            List.map
              (with_state_node (fun st node ->
                   let newnode = get_node s PostAnnot st in
                   ignore (Builder.add_edge graph node newnode);
                   newnode))
              states
          in
          (* let interp_annot record states annot =
             Eval_annots.interp_annot
               current_kf AnalysisParam.active_behaviors initial_state slevel
               states s annot record
             in
             let states = List.fold_left (interp_annot true) states annots in
          *)
          if states = [] then ret Dataflow2.SDefault
          else
            let current_info = stmt_state s in
            let old_counter = current_info.counter_unroll in
            let no_slevel_left = old_counter > slevel && (not_return s) in
            let new_states =
              if no_slevel_left
              then (* No slevel left, perform some join and/or widening *)
                let curr_wcounter,  curr_wnode = stmt_widening_info s in
                let curr_wstate = select_state curr_wnode in
                let joinnode = join_and_add_edges s states "doStmt" in
                let joinstate = select_state joinnode in
                let joined = Cvalue.Model.join curr_wstate joinstate in
                if Model.equal joined curr_wstate then
                  begin
                    ignore (Builder.add_edge graph joinnode curr_wnode);
                    [] (* [state] is included in the last propagated state.
                                        Nothing remains to do *)
                  end
                else
                  let wjoinnode = get_node s (Join "doStmt_prewiden") joined in
                  ignore (Builder.add_edge graph joinnode wjoinnode);
                  ignore (Builder.add_edge graph curr_wnode wjoinnode);
                  if obviously_terminates
                  then begin (* User thinks the analysis will terminate:
                                do not widen *)
                    (* We don't lose precision, because update_stmt_states is
                       only called when obviously_terminates does not hold. *)
                    update_stmt_widening_info s 0 wjoinnode;
                    states
                  end
                  else
                    let precision_lost = ref
                        (List.length states > 1
                         || Cvalue.Model.is_reachable curr_wstate)
                    in
                    let r =
                      if is_loop s && curr_wcounter = 0 then
                        let widen_hints = Widen.getWidenHints current_kf s in
                        precision_lost := true;
                        let wstate =
                          Cvalue.Model.widen widen_hints curr_wstate joined
                        in
                        let wnode = get_node s Widen wstate in
                        ignore (Builder.add_edge graph wjoinnode wnode);
                        wnode
                      else
                        wjoinnode
                    in
                    let new_wcounter =
                      if curr_wcounter = 0 then 1 else pred curr_wcounter
                    in
                    let new_state = [r] in
                    if Cvalue.Model.equal (select_state r) joined then begin
                      update_stmt_widening_info s new_wcounter r;
                      if !precision_lost then
                        lost_precision current_info "doStmt1" 4;
                      new_state
                    end
                    else begin (* Try to correct over-widenings *)
                      let new_states =
                        (* Do *not* record the status after
                           interpreting the annotation here. Possible
                           unproven assertions have already been
                           recorded when the assertion has been
                           interpreted the first time higher in this
                           function. *)
                        List.fold_left (fun acc node ->
                            let states = interp_annots node false "doStmt2" in
                            merge_into states acc)
                          [] new_state
                      in
                      let new_joined =
                        join_and_add_edges s new_states "doStmt_overwidenings"
                      in
                      update_stmt_widening_info s new_wcounter new_joined;
                      if !precision_lost || List.length new_states > 1
                      then lost_precision current_info "doStmt2" 8;
                      [new_joined]
                    end
              else states
            in
            let states = doStmtSpecific s d new_states in
            if save_slevel_results then begin
              if no_slevel_left && current_info.widening_length = -1
              then current_info.widening_length <- 0; (* Start widening. *)
              match s.skind with
              | Instr _ ->
                (* Before states are stored in doInstr along with
                   after states. *)
                ()
              | _ ->
                current_info.each_state <-
                  List.fold_left
                    (fun xs -> with_state (fun s -> (s, None) :: xs))
                    current_info.each_state states;
                update_widening_length current_info (List.length states);
            end;
            (* This temporary propagation value will be passed on to
               the successors of [s] *)
            ret (Dataflow2.SUse { to_propagate = states })

    let doStmt s d =
      Value_stat.measure_statement s (do_doStmt s d)

    let do_doEdge s succ d () =
      let kinstr = Kstmt s in
      let states = d.to_propagate in
      Valarms.start_stmt kinstr;
      (* We store the state after the execution of [s] for the
         callback {Value.Record_Value_After_Callbacks}. This is done
         here because we want to see the values of the variables local
         to the block *)
      if should_memorize_function && store_state_after_during_dataflow s succ
      then begin
        let old =
          try Cil_datatype.Stmt.Hashtbl.find states_after s
          with Not_found -> Cvalue.Model.bottom
        in
        let updated =
          List.fold_left
            (fun acc -> with_state (fun state ->
                 Value_messages.set_current_state state;
                 Cvalue.Model.join acc state))
            old
            states
        in
        Cil_datatype.Stmt.Hashtbl.replace states_after s updated
      end;
      (* Variables exiting their scope *)
      let states =
        match Kernel_function.blocks_closed_by_edge s succ with
        | [] -> states
        | closed_blocks ->
          (* Partial application is useful, do not inline *)
          let block_top =
            Locals_scoping.block_top_addresses_of_locals
              current_fundec clob closed_blocks
          in
          let nodes =
            List.fold_left
              (fun acc -> with_state_node (fun state node ->
                   (* Check if all local variables of type va_list which are
                      getting out of scope have been properly uninitialized (or
                      never initialized at all). *)
                   let are_all_va_list_vars_uninitialized =
                     let va_list_vars_uninitialized =
                       List.map
                         (fun block ->
                            Value_variadic.check_variadic_variables
                              current_kf state block.blocals)
                         closed_blocks
                     in
                     (* [Value_variadic.check_variadic_variables]
                        shows warnings as a side effect, so we cannot
                        do [List.for_all] directly or we risk missing
                        some warnings. *)
                     List.for_all (fun x -> x) va_list_vars_uninitialized
                   in
                   if not are_all_va_list_vars_uninitialized
                   then
                     (* Some va_list objects left definitely initialized... *)
                     acc
                   else
                     (* There are no definitely initialized va_list objects. *)
                     let state =
                       (* First remove properly the va_list objects' underlying
                          structure. *)
                       Value_variadic.uninitialize_blocks_variadic_locals
                         closed_blocks state
                     in
                     let state =
                       (* Then uninitialize all local variables. *)
                       Cvalue.Model.uninitialize_blocks_locals
                         closed_blocks state
                     in
                     let state = block_top state in
                     let get_newnode = get_node s CloseBlock in
                     add_and_link state get_newnode acc ~before:node))
              []
              states in
          reduce s CloseBlock nodes
      in
      (* Variables entering in scope *)
      let opened_blocks = Kernel_function.blocks_opened_by_edge s succ in
      let aux state node =
        let newstate =
          List.fold_left
            bind_locals_state
            state
            opened_blocks
        in
        let newstate =
          List.fold_left
            Value_variadic.bind_variadic_locals_state
            newstate
            opened_blocks
        in
        let newnode = get_node succ OpenBlock newstate in
        ignore (Builder.add_edge graph node newnode);
        newnode
      in
      let states = match opened_blocks with
        | [] -> states
        | _ -> List.map (with_state_node aux) states
      in
      Valarms.end_stmt ();
      let states =
        List.map
          (with_state_node
             (fun st node ->
                let newnode = get_node succ Edge st in
                ignore (Builder.add_edge graph node newnode);
                newnode))
          states
      in
      d.to_propagate <- states;
      d

    let doEdge s succ d =
      Value_stat.measure_statement s (do_doEdge s succ d)

    module VPUndefPCPA = Value_parameters.UndefinedPointerComparisonPropagateAll

    let doGuardOneCond stmt context exp t =
      if t.to_propagate = []
      then Dataflow2.GUnreachable
      else begin
        Valarms.start_stmt (Kstmt stmt);
        let with_alarms = warn_all_quiet_mode () in
        let new_values =
          List.fold_left
            (fun acc ->
               with_state_node (fun state node ->
                   let state, _, test =
                     eval_expr_with_deps_state None ~with_alarms state exp
                   in
                   Valarms.set_syntactic_context context;
                   Value_messages.set_current_state state;
                   let warn =
                     not (Warn.are_comparable Abstract_interp.Comp.Eq
                            V.singleton_zero test)
                   in
                   let do_it =
                     (warn && VPUndefPCPA.get ()) ||
                     let t1 = unrollType (typeOf exp) in
                     if isIntegralType t1 || isPointerType t1
                     then V.contains_non_zero test
                     else true (* TODO: a float condition is true iff != 0.0 *)
                   in
                   if do_it then
                     try
                       let newstate =
                         reduce_by_cond state {positive = true; exp = exp}
                       in
                       let get_newnode = get_node stmt (Guard exp) in
                       add_and_link newstate get_newnode acc ~before:node
                     with Reduce_to_bottom -> acc
                   else acc))
            []
            t.to_propagate
        in
        let new_values = reduce stmt (Guard exp) new_values in
        let result =
          if new_values = [] then Dataflow2.GUnreachable
          else Dataflow2.GUse { to_propagate = new_values}
        in
        Valarms.end_stmt ();
        result
      end

    let mask_then = Db.Value.mask_then
    let mask_else = Db.Value.mask_else

    let do_doGuard stmt exp t () =
      let not_exp = new_exp ~loc:exp.eloc (UnOp(LNot, exp, intType)) in
      let th, el as thel =
        let context = Valarms.SyUnOp exp in
        let th = doGuardOneCond stmt context exp t in
        let el = doGuardOneCond stmt context not_exp t in
        th, el
      in
      let th_reachable =
        match th with
          Dataflow2.GUse _ | Dataflow2.GDefault -> mask_then
        | Dataflow2.GUnreachable -> 0
      in
      let el_reachable =
        match el with
          Dataflow2.GUse _ | Dataflow2.GDefault -> mask_else
        | Dataflow2.GUnreachable -> 0
      in
      let reachable = th_reachable lor el_reachable in
      let current_condition_status =
        try
          Cil_datatype.Stmt.Hashtbl.find conditions_table stmt
        with Not_found -> 0
      in
      let new_status =
        current_condition_status lor reachable
      in
      if new_status <> 0
      then Cil_datatype.Stmt.Hashtbl.replace conditions_table stmt new_status;
      Separate.filter_if stmt thel

    let doGuard stmt exp t =
      Value_stat.measure_statement stmt (do_doGuard stmt exp t)

  end

  module Dataflow = Dataflow2.Forwards(DataflowArg)

  (* Walk through all the statements for which [to_propagate] is not empty.
     Those statements are marked as "not fully propagated", for ulterior
     display in the gui. Also mark the current statement as root if relevant.*)
  let mark_degeneration () =
    let cs = call_stack () in
    let add_unpropagated stmt =
      let new_set =
        try
          Value_callstack.Callstack.Set.add cs
            (Value_util.UnpropagatedPoints.find stmt)
        with Not_found -> Value_callstack.Callstack.Set.singleton cs
      in
      Value_util.UnpropagatedPoints.replace stmt new_set
    in
    DataflowArg.StmtStartData.iter
      (fun stmt v -> if v.to_propagate <> [] then add_unpropagated stmt);
    match Valarms.current_stmt () with
    | Kglobal -> ()
    | Kstmt s ->
      let kf = Kernel_function.find_englobing_kf s in
      if Kernel_function.equal kf current_kf then begin
        begin match Value_util.get_degeneration_point () with
          | None -> Value_util.register_degeneration_point s cs
          | Some (_,cs) ->
            assert
              (List.exists
                 (fun (_,ki) -> match ki with
                    | Kglobal -> false
                    | Kstmt stmt -> Cil_datatype.Stmt.equal stmt s)
                 cs)
        end;
        Valarms.end_stmt ()
      end

  (* Check that the dataflow is indeed finished *)
  let checkConvergence () =
    DataflowArg.StmtStartData.iter
      (fun k v ->
         if not (v.to_propagate = []) then
           Value_parameters.fatal "sid:%d@\n%a@\n"
             k.sid
             (fun fmt -> List.iter (State_node.pretty fmt)) v.to_propagate)

  (* Final states of the function, reduced by the post-condition *)
  let final_nodes () =
    let return_nodes = !return_nodes in
    (* Reduce final states according to the function postcondition *)
    let result = match return_lv with
      | Some (Var v, NoOffset) -> Some v
      | Some _ -> assert false
      | None -> None
    in
    let check =  Eval_annots.check_fct_postconditions
        current_kf AnalysisParam.active_behaviors
        (Annotations.behaviors current_kf) ~result ~per_behavior:false
        Normal (* termination kind*)  ~pre_state:initial_state in
    let s, a = return, InterpAnnot "postconditions" in
    let make_postnode_and_link node acc state =
      let node' = get_node s a state in
      Builder.add_edge graph node node';
      node' :: acc
    in
    let do_node node acc =
      let s = State_set.(to_list (check (singleton (select_state node)))) in
      List.fold_left (make_postnode_and_link node) acc s
    in
    State_node.Set.fold do_node return_nodes []

  let externalize nodes =
    Valarms.start_stmt (Kstmt return);
    let with_alarms = warn_all_quiet_mode () in
    let nodes =
      let get_node node s a m =
        let node' = get_node s a m  in
        Builder.add_edge graph node node';
        node'
      in
      Split_return.join_final_states ~return_lv ~get_node current_kf nodes
    in
    let externalize node =
      let get_node s a m =
        let node' = get_node s a m in
        Builder.add_edge graph node node';
        node'
      in
      Eval_stmt.externalize ~with_alarms ~return_lv ~get_node
        current_kf clob node
    in
    let r = List.map externalize nodes in
    Valarms.end_stmt ();
    r

  let results () =
    if DataflowArg.debug then checkConvergence ();
    let final_states = final_nodes () in
    let externalized = externalize final_states in {
      Value_types.c_values =  externalized;
      c_clobbered = clob.Locals_scoping.clob;
      c_cacheable = !cacheable;
      c_from = None;
      c_sureouts = None;
    }

  let compute states =
    let start = Kernel_function.find_first_stmt AnalysisParam.kf in
    let states = bind_block_locals states current_fundec.sbody in
    let states =
      Value_variadic.bind_block_variadic_locals states current_fundec.sbody
    in
    (* Init the dataflow state for the first statement *)
    let nodes =
      List.map
        (fun st ->
           let node = get_node start Init st in
           Builder.add_vertex graph node;
           node)
        (State_set.to_list states)
    in
    if not (Db.Value.Record_Value_Graph_Callbacks_StoreLiveNodes.is_empty ())
    then begin
      let data =
        Db.Value.StartingFunction
          (AnalysisParam.caller_node, State_node.Set.of_list nodes)
      in
      Db.Value.Record_Value_Graph_Callbacks_StoreLiveNodes.apply
        (call_stack (), data)
    end;
    begin match AnalysisParam.caller_node with
      | None ->
        let nodeset = State_node.Set.of_list nodes in
        assert (State_node.Set.is_empty (Db.Value.InitialNodes.get ()));
        Db.Value.InitialNodes.set nodeset
      | Some caller ->
        List.iter (fun node -> Builder.add_wp_edge caller node) nodes
    end;
    let dinit = { to_propagate = nodes } in
    let dinit = DataflowArg.computeFirstPredecessor start dinit in
    DataflowArg.StmtStartData.add start dinit;
    Dataflow.compute [start]

end



(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
