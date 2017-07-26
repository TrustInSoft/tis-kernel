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



module Make (G: G) = struct

  module S = Set.Make(G.V)

  let contract (p : G.V.t -> bool) g =
    let rec iterate from v (memo,acc) =
      if S.mem v memo then (memo,acc)
      else
      if p v
      then
        G.fold_succ (iterate from) g v (S.add v memo,acc)
      else
        (memo,G.add_edge acc from v)
    in
    G.fold_vertex
      (fun v acc ->
         if p v then acc
         else begin
           snd (G.fold_succ (iterate v) g v (S.empty,(G.add_vertex acc v)))
         end)
      g (G.empty ())

end
