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

(** Declarations that are useful for plugins written on top of the results
    of Value. *)

type 'a callback_result =
  | Normal of 'a
  | NormalStore of 'a * int
  | Reuse of int

type cacheable =
  | Cacheable (** Functions whose result can be safely cached *)
  | NoCache (** Functions whose result should not be cached, but for
                which the caller can still be cached. Typically, functions
                printing something during the analysis. *)
  | NoCacheCallers (** Functions for which neither the call, neither the
                       callers, can be cached *)
type c_value_result =
  | StateOnly of
      Cvalue.V_Offsetmap.t option
  (** the value returned (ie. what is after the 'return' C keyword). *)
      * Cvalue.Model.t
  (** the memory state after the function has been executed *)
  | Node of
      Cvalue.V_Offsetmap.t option
      * State_node.t
  (** Node corresponding to the return in the whole-program graph
      (contains the memory state). *)

(** Gets the memory state from a c_value_result *)
val get_result_state: c_value_result -> Cvalue.Model.t

type from_result =
  | Froms of Function_Froms.froms
  | Closure of (Function_Froms.Memory.t -> Function_Froms.Deps.t list -> Function_Froms.Memory.t)

module FromResult: Datatype.S with type t = from_result

(** Results of a a call to a function *)
type call_result = {
  c_values: c_value_result list;
  c_clobbered: Base.SetLattice.t
(** An over-approximation of the bases in which addresses of local
    variables might have been written *);

  c_cacheable: cacheable
(** Is it possible to cache the result of this call? *);

  c_from: from_result option
(** The froms of the function;
    i.e. the dependencies of the result, and the dependencies
    of each zone written to. *);
  c_sureouts: Locations.Zone.t option
(** If not None, the sure outputs of the function*);
}


(** Dependencies for the evaluation of a term or a predicate: for each
    program point involved, sets of zones that must be read *)
type logic_dependencies = Locations.Zone.t Cil_datatype.Logic_label.Map.t

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
