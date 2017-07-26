(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

type node = State_node.t

module type NodeStartData = sig
  type data
  val clear: unit -> unit
  val mem: node -> bool
  val find: node -> data
  val replace: node -> data -> unit
  val add: node -> data -> unit
  val iter: (node -> data -> unit) -> unit
  val length: unit -> int
end

module StartData(X: sig type t val size: int end) = struct
  type data = X.t
  open State_node.Hashtbl
  let nodeStartData = create X.size
  let clear () = clear nodeStartData
  let mem = mem nodeStartData
  let find = find nodeStartData
  let replace = replace nodeStartData
  let add = add nodeStartData
  let iter f = iter f nodeStartData
  let length () = length nodeStartData
end

module type WORKLIST = sig
  type t
  val create: Nodegraph.t -> node list -> t
  val add: t -> node -> unit
  val clear: t -> node -> unit
  val pop_next: t -> node
  exception Empty
  val is_empty: t -> bool
end

module Simple_forward_worklist: WORKLIST = struct
  type ordered_node = int
  type t = { bv: Bitvector.t;
             order: node -> ordered_node;
             unorder: ordered_node -> node;
             mutable index: int }

  let create graph sources =
    let nb_nodes = Nodegraph.nb_vertex graph in
    let node_to_ordered = State_node.Hashtbl.create nb_nodes in
    let ordered_to_node = Array.make nb_nodes (List.hd State_node.reprs) in
    let n = ref 0 in
    let f node =
      ordered_to_node.(!n) <- node;
      State_node.Hashtbl.add node_to_ordered node !n;
      incr n;
    in
    let order = State_node.Hashtbl.find node_to_ordered in
    let unorder = Array.get ordered_to_node in
    Nodegraph.iter_vertex f graph;
    let bv = Bitvector.create nb_nodes in
    let min =
      List.fold_left
        (fun cur_min node ->
           let ordered = order node in
           Bitvector.set bv ordered;
           min cur_min ordered)
        max_int sources
    in
    { bv; index = min; order; unorder}

  let add t node = Bitvector.set t.bv (t.order node)

  let clear t node =
    let i = t.order node in
    Bitvector.clear t.bv i

  exception Empty

  let pop_next t =
    try let next = Bitvector.find_next_true t.bv t.index in
      Bitvector.clear t.bv next;
      t.index <- next;
      t.unorder next
    with Not_found ->
    (* Try to start over *)
    try let next = Bitvector.find_next_true t.bv 0 in
      Bitvector.clear t.bv next;
      t.index <- next;
      t.unorder next
    with Not_found -> raise Empty

  let is_empty t = Bitvector.is_empty t.bv
end

module Scc_worklist: WORKLIST = struct
  type ordered_node = int
  type connex_component = int

  type t =
    {
      bv: Bitvector.t;
      order: node -> ordered_node;
      unorder: ordered_node -> node;
      connex: connex_component array;

      mutable next: ordered_node;
      mutable current_scc: connex_component;
      mutable must_restart_cc: ordered_node option;
    }

  let connex_of_ordered t ordered =
    t.connex.(ordered)

  let create graph sources =
    let (order, unorder, connex) = Nodegraph.get_ordered_nodes graph in
    let nb_nodes = Nodegraph.nb_vertex graph in
    Format.printf "nb nodes %d@." nb_nodes;
    let bv = Bitvector.create nb_nodes in
    let min = List.fold_left (fun cur_min node ->
        let ordered = order node in
        Bitvector.set bv ordered;
        min cur_min ordered)
        max_int sources
    in
    let next = min in
    let current_scc = connex.(next) in
    let must_restart_cc = None in
    { bv; order; unorder; next; current_scc; connex; must_restart_cc }

  let add t node =
    let i = t.order node in
    Bitvector.set t.bv i;
    if i < t.next
    then t.must_restart_cc <-
        match t.must_restart_cc with
        | None -> Some i
        | Some j -> Some (min i j)

  let clear t node =
    let i = t.order node in
    Bitvector.clear t.bv i

  let is_empty t = Bitvector.is_empty t.bv

  exception Empty

  let pop_next t =

    let restart_from i =
      (* We should restart in the same connex component. *)
      assert((connex_of_ordered t i) == t.current_scc);
      t.must_restart_cc <- None;
      i
    in

    let real_next =
      try
        let next_true = Bitvector.find_next_true t.bv t.next in
        let next_true_scc = connex_of_ordered t next_true in
        if next_true_scc = t.current_scc
        then
          (* Continue in the same connex component. *)
          next_true
        else
          (* We reached the end of the current connex component. The
             trick is that OCamlgraph's topological ordering guarantee
             that elements of the same connex component have
             contiguous indexes, so we know that we have reached the
             end of the current connex component. Check if we should
             start over in the same connex component, or continue to
             the next cc. *)
          (* assert (next_true_scc < t.current_scc); *)
          begin match t.must_restart_cc with
            | None -> t.current_scc <- next_true_scc; next_true
            | Some i -> restart_from i
          end
      with Not_found ->
      (* We found no further work, but it could be because the graph
         ends with a non-trival connex component (e.g. the function
         ends with a loop). *)
      match t.must_restart_cc with
      | None -> raise Empty
      | Some i -> restart_from i
    in
    Bitvector.clear t.bv real_next;
    t.next <- real_next +1;
    t.current_scc <- connex_of_ordered t real_next;
    t.unorder real_next

end

module type ForwardsTransfer = sig
  val name:string
  val debug:bool
  type t
  val graph: Nodegraph.t
  val copy: t -> t
  val pretty: Format.formatter -> t -> unit
  val computeFirstPredecessor: node -> t -> t
  val combinePredecessors: node -> old:t -> t -> t option
  val doNode: node -> t -> (node * t) list
  val doEdge: node -> node -> t -> t

  module NodeStartData: NodeStartData with type data = t
end

module Forwards_generic(Worklist:WORKLIST)(T:ForwardsTransfer) = struct

  let reachedNode worklist pred node t =
    let t = T.doEdge pred node t in
    let newdata: T.t option =
      try
        let old = T.NodeStartData.find node in
        match T.combinePredecessors node ~old t with
        |  None -> if T.debug then
            Kernel.debug "FF(%s): reached node %d with %a\n  implies the old state %a\n"
              T.name (State_node.id node) T.pretty t T.pretty old;
          None
        | Some t' -> begin
            if T.debug
            then Kernel.debug "FF(%s): weaken data for block %d: %a\n"
                T.name (State_node.id node) T.pretty t';
            Some t'
          end
      with Not_found -> (* was bottom before *)
        let t' = T.computeFirstPredecessor node t in
        if T.debug then
          Kernel.debug "FF(%s): set data for block %d: %a\n"
            T.name (State_node.id node) T.pretty t';
        Some t'
    in
    match newdata with
    | None -> ()
    | Some t' ->
      T.NodeStartData.replace node t';
      Worklist.add worklist node

  let processNode worklist node =
    Cil.CurrentLoc.set (Cil_datatype.Stmt.loc (State_node.stmt node));
    if T.debug then
      Kernel.debug "FF(%s).node %a at %t@\n" T.name State_node.pretty node Cil.pp_thisloc;
    let init: T.t =
      try T.copy (T.NodeStartData.find node)
      with Not_found ->
        Kernel.fatal ~current:true
          "FF(%s): processing block without data" T.name
    in
    let newdata = T.doNode node init in
    List.iter (fun (succ, data) -> reachedNode worklist node succ data) newdata

  let init_worklist (sources: node list) =
    let worklist = Worklist.create T.graph sources in
    List.iter (fun s -> Worklist.add worklist s) sources;
    worklist

  let check_initial_nodes (sources: node list) =
    List.iter
      (fun node ->
         if not (T.NodeStartData.mem node) then
           Kernel.fatal ~current:true
             "FF(%s): initial node %a does not have data"
             T.name State_node.pretty node)
      sources

  let compute (sources: node list) =
    check_initial_nodes sources;
    if T.debug then
      (Kernel.debug "FF(%s): initialising worklist" T.name);
    let worklist = init_worklist sources in
    if T.debug then
      (Kernel.debug "FF(%s): processing" T.name);
    let rec fixedpoint () =
      let node = Worklist.pop_next worklist in
      processNode worklist node;
      fixedpoint ()
    in
    try
      fixedpoint ()
    with Worklist.Empty ->
      if T.debug then
        Kernel.debug "FF(%s): done" T.name

end

module ForwardsScc = Forwards_generic(Scc_worklist)
module Forwards = Forwards_generic(Simple_forward_worklist)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
