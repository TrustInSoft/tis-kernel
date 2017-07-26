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

(* -------------------------------------------------------------------------- *)
(** Term implementation                                                       *)
(* -------------------------------------------------------------------------- *)

open Qed
open Logic

include Logic.Term

type env

val stdlib : env

val cc_tau : env -> Syntax.id list -> Syntax.t -> tau
val cc_sig : env -> Syntax.t list -> Syntax.t -> signature
val cc_def : env -> Syntax.arg list -> Syntax.t option -> Syntax.e -> signature * term
val cc_exp : env -> Syntax.e -> tau * term

val pp_tau : Format.formatter -> tau -> unit
val pp_term : Format.formatter -> term -> unit

val add_datatype : Syntax.id -> Syntax.id list -> env -> env
val add_typedef : Syntax.id -> tau -> env -> env
val add_symbol : Syntax.id -> category -> signature -> env -> env
val add_value : string -> tau -> term -> env -> env

val simplify : term -> term

val pp_env : env -> Format.formatter -> term -> unit
