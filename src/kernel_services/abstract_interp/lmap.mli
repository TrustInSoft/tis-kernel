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

(** Maps from bases to memory maps. The memory maps are those of the
    [Offsetmap] module.
    @plugin development guide *)

module Make_LOffset
    (V: sig
       include module type of Offsetmap_lattice_with_isotropy
       include Lattice_type.With_Top with type t := t
       include Lattice_type.With_Narrow with type t := t
     end)
    (Offsetmap: module type of Offsetmap_sig
     with type v = V.t
      and type widen_hint = V.generic_widen_hint)
    (Default_offsetmap: sig
       val default_offsetmap : Base.t -> [`Bottom | `Map of Offsetmap.t]
       val uninitialized_offsetmap : Base.t -> [`Bottom | `Map of Offsetmap.t]
     end):
  module type of Lmap_sig
  with type v = V.t
   and type widen_hint_base = V.generic_widen_hint
   and type offsetmap = Offsetmap.t

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
