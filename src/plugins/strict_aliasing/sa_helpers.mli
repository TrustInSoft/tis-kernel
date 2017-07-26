(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Locations
open Sa_types

(** {2 Shortcuts} *)

(** [lmap_find lmap zone] is a shortcut of [Lmap_pointer_aliasing.find] *)
val lmap_find : Lmap_pointer_aliasing.t -> Zone.t -> EffectiveType.t

(** [lmap_replace exact reducing lmap zone typ] replaces the value
    stored in [zone] by [typ]. *)
val lmap_replace :
  ?exact:bool -> ?reducing:bool -> Lmap_pointer_aliasing.t ->
  Zone.t -> EffectiveType.t -> Lmap_pointer_aliasing.t

(** [lmap_from_empty exact reducing zone typ] creates an empty lmap and binds
    [zone] to [typ]. *)
val lmap_from_empty :
  ?exact:bool -> ?reducing:bool -> Zone.t -> EffectiveType.t ->
  Lmap_pointer_aliasing.t

(** [lmap_replace_offset lmap src dest] replaces every offset of [dest] by
    offsets coming from [src]. *)
val lmap_replace_offset :
  Lmap_pointer_aliasing.t -> Zone.t -> Zone.t -> Lmap_pointer_aliasing.t


(** {2 Typing helpers} *)

(** [typeOf_expression ~keep_cast expr] is a wrapper of the already existing
    function [Cil.typeOf] but it removes any pointer cast if [keep_cast] is set
    to false. *)
val typeOf_expression : keep_cast:bool -> Cil_types.exp -> Cil_types.typ

(** [remove_pointer_casts expr] removes the cast when [expr] is a cast to a
    pointer type. *)
val remove_pointer_casts : Cil_types.exp -> Cil_types.exp

(** [complete_typing ?keep_cast state expr] finds the type of [expr], and
    evaluates the static effective type and the allocated_type.
    Its return is a tuple (type, effective_type). *)
val complete_typing :
  keep_cast:bool -> Db.Value.state -> Cil_types.exp ->
  (Cil_types.typ * EffectiveType.t)

(** [complete_typing_lval] is the same as [complete_typing] but for lvalue. *)
val complete_typing_lval :
  Db.Value.state -> Cil_types.lval -> (Cil_types.typ * EffectiveType.t)

(** [int_base_of_int i] creates an Int_Base from an integer. *)
val int_base_of_int : int -> Int_Base.t

(** {2 Expression } *)

val is_null : Cil_types.exp -> bool


(** {2 Zone evaluation} *)

module type EvalZone = sig
  type t
  val eval : for_writing:bool -> Db.Value.state -> t -> Zone.t
  val eval_static : for_writing:bool -> Db.Value.state -> t -> Zone.t
  val eval_allocated : for_writing:bool -> Db.Value.state -> t -> Zone.t
end

module ZoneLval : EvalZone with type t := Cil_datatype.Lval.t
module ZoneExpr : EvalZone with type t := Cil_datatype.Exp.t


(** {2 Others} *)

(** [extract_pointer_variable] extracts the lvalue if and only if its type is a
    pointer. *)
val extract_pointer_variable : Cil_types.exp -> Cil_types.lval option

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
