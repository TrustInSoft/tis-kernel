(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)


include Graph.Sig.I with type t = Graph.Imperative.Digraph.ConcreteBidirectional(State_node).t
                     and type V.t = State_node.t

include Datatype.S with type t := Graph.Imperative.Digraph.ConcreteBidirectional(State_node).t

(** More compact pretty printing (hides memory state). *)
val pretty_no_state : Format.formatter -> t -> unit

module Builder: Graph.Builder.S with type G.t = t and type G.V.t = State_node.t

val get_ordered_nodes : t -> ( (State_node.t -> int) * (int -> State_node.t) * int array )

(** Less efficient, serializable representation. Conversions take linear time.
    Function to_graph maintains the node unicity invariants on deserialization. *)
module Serializable: sig
  type t
  val to_graph : t -> Graph.Imperative.Digraph.ConcreteBidirectional(State_node).t
  val of_graph : Graph.Imperative.Digraph.ConcreteBidirectional(State_node).t -> t
  include Datatype.S with type t := t
end
(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
