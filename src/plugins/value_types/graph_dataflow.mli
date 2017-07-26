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

module StartData(X: sig type t val size: int end) :
  NodeStartData with type data = X.t

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

(** Simple forward dataflow analysis on a result graph *)
module Forwards(T:ForwardsTransfer) : sig
  val compute: node list -> unit
end

(** Forward dataflow analysis on a result graph. Uses a topological
    sort to try to wait until all predecessors are visited before visiting
    a node. The topological sort is not tail-recursive and may cause a
    stack overflow for large graphs. *)
module ForwardsScc(T:ForwardsTransfer) : sig
  val compute: node list -> unit
end
(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
