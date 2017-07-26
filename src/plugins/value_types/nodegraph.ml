(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

module NodeGraph = Graph.Imperative.Digraph.ConcreteBidirectional(State_node)
module Builder = Graph.Builder.I(NodeGraph)

let pretty_gen pretty_node fmt g = NodeGraph.iter_vertex
    (fun v -> Format.fprintf fmt "Node %a has successors %a@."
        pretty_node v
        (fun fmt l -> List.iter (fun s -> Format.fprintf fmt "%d " (State_node.id s)) l)
        (NodeGraph.succ g v))
    g

include Datatype.Make(
  struct
    type t = NodeGraph.t
    let name = "Node graph"
    include Datatype.Undefined
    let mem_project = Datatype.never_any_project
    let reprs = [ (NodeGraph.create ~size:0 ()) ]
    let pretty = pretty_gen State_node.pretty
  end
  )

let pretty_no_state = pretty_gen State_node.pretty_no_state

include (NodeGraph : module type of NodeGraph with type t := t)

module Scc = Graph.Components.Make(NodeGraph)

let get_ordered_nodes graph =
  let nb_nodes = NodeGraph.nb_vertex graph in
  let node_to_ordered = State_node.Hashtbl.create nb_nodes in
  let ordered_to_node = Array.make nb_nodes (List.hd State_node.reprs) in
  let n = ref 0 in
  let f node =
    ordered_to_node.(!n) <- node;
    State_node.Hashtbl.add node_to_ordered node !n;
    incr n;
  in
  (*inlining of Graph.Topological to avoid computing sccs twice*)
  let n, scc = Scc.scc graph in
  let vertices = Array.make n [] in
  let edges = Array.make n [] in
  let degree = Array.make n 0 in (* in-degree *)
  let add_vertex x =
    let ix = scc x in
    vertices.(ix) <- x :: vertices.(ix);
    let add_edge y =
      let iy = scc y in
      if ix <> iy then begin
        edges.(ix) <- iy :: edges.(ix);
        degree.(iy) <- degree.(iy) + 1
      end
    in
    NodeGraph.iter_succ add_edge graph x
  in
  NodeGraph.iter_vertex add_vertex graph;
  (* standard topological sort on a DAG *)
  let todo = Queue.create () in
  let rec walk () =
    if Queue.is_empty todo then
      ()
    else
      let i = Queue.pop todo in
      List.iter f vertices.(i);
      List.iter
        (fun j ->
           let d = degree.(j) in
           assert (d > 0); (* no back edge *)
           if d = 1 then Queue.push j todo else degree.(j) <- d-1)
        edges.(i);
      walk ()
  in
  for i = 0 to n-1 do if degree.(i) = 0 then Queue.push i todo done;
  walk ();
  let sccs = Array.make nb_nodes (-1) in
  Array.iteri (fun ordered node -> sccs.(ordered) <- scc node) ordered_to_node;
  (State_node.Hashtbl.find node_to_ordered, Array.get ordered_to_node, sccs)

module Serializable = struct

  module NodePair = Datatype.Pair_with_collections(State_node)(State_node)
      (struct let module_name = "node pair" end)
  module Vertices = Datatype.Array(State_node)
  module Edges = Datatype.Array(NodePair)
  include Datatype.Make(
    struct
      type t = Vertices.t * Edges.t
      let name = "Serializable node graph"
      include Datatype.Undefined
      let rehash = Datatype.identity
      let structural_descr = Structural_descr.t_sum
          [| [| Vertices.packed_descr; Edges.packed_descr |] |]
      let reprs = [([||], [||])]
      let mem_project = Datatype.never_any_project
    end
    )

  let of_graph g =
    let nv = NodeGraph.nb_vertex g in
    let ne = NodeGraph.nb_edges g in
    let dummy = List.hd State_node.reprs in
    let vertices = Array.make nv dummy in
    let edges = Array.make ne (dummy, dummy) in
    let iv = ref 0 in
    let ie = ref 0 in
    NodeGraph.iter_vertex (fun v -> vertices.(!iv) <- v; incr iv) g;
    NodeGraph.iter_edges (fun v1 v2 -> edges.(!ie) <- (v1,v2); incr ie) g;
    (vertices, edges)

  let to_graph (v, e) =
    let g = Builder.empty () in
    let add_node node =
      ignore (Builder.add_vertex g node)
    in
    Array.iter add_node v;
    let add_edge (node1, node2) =
      ignore (Builder.add_edge g node1 node2)
    in
    Array.iter add_edge e;
    g
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
