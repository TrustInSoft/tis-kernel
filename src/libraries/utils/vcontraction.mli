(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)


module type G = sig
  type t
  module V : Graph.Sig.COMPARABLE
  type vertex = V.t
  val is_directed : bool

  val empty : unit -> t
  val add_vertex : t -> V.t -> t
  val add_edge : t -> V.t -> V.t -> t

  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a

end

module Make
    (G: G) : sig

  val contract : (G.V.t -> bool) -> G.t -> G.t
  (** Performs vertex contraction on the graph [g]. Vertices for which the property [p] holds are contracted.
  *)

end
