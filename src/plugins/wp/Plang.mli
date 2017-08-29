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
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** Lang Pretty-Printer *)

type scope = Qed.Engine.scope
module Env : Qed.Engine.Env with type term := Lang.F.term

type pool
val pool: unit -> pool
val xmark_e: pool -> (Lang.F.var -> unit) -> Lang.F.term -> unit
val xmark_p: pool -> (Lang.F.var -> unit) -> Lang.F.pred -> unit
val xmark: pool -> Lang.F.Vars.t

class engine :
  object
    inherit [ Z.t,
              Lang.ADT.t,
              Lang.Field.t,
              Lang.Fun.t,
              Lang.tau,
              Lang.F.var,
              Lang.F.term,
              Env.t ]
        Qed.Engine.engine
    method marks: Env.t * Lang.F.marks
    method pp_pred: Format.formatter -> Lang.F.pred -> unit
    method lookup: Lang.F.term -> scope
    (**/**)
    inherit Lang.idprinting
    method infoprover: 'a. 'a Lang.infoprover -> 'a
    method op_spaced: string -> bool
    (**/**)
  end
