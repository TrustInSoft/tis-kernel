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

type 'a callback_result =
  | Normal of 'a
  | NormalStore of 'a * int
  | Reuse of int

type cacheable =
  | Cacheable
  | NoCache
  | NoCacheCallers

type c_value_result =
    StateOnly of Cvalue.V_Offsetmap.t option * Cvalue.Model.t
  | Node of Cvalue.V_Offsetmap.t option * State_node.t

let get_result_state = function
  | StateOnly (_,s) -> s
  | Node (_,n) -> State_node.state n

type from_result =
  | Froms of Function_Froms.froms
  | Closure of (Function_Froms.Memory.t -> Function_Froms.Deps.t list -> Function_Froms.Memory.t)

module FromResult = Datatype.Make(struct
    type t = from_result
    let name = "Callwise From result"
    include Datatype.Undefined
    let reprs = (List.map (fun f -> Froms f) Function_Froms.reprs) @
                (List.map (fun f -> Closure (fun _ _ -> f)) Function_Froms.Memory.reprs)
  end)

type call_result = {
  c_values: c_value_result list;
  c_clobbered: Base.SetLattice.t;
  c_cacheable: cacheable;
  c_from: from_result option;
  c_sureouts: Locations.Zone.t option;
}

type logic_dependencies = Locations.Zone.t Cil_datatype.Logic_label.Map.t

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
