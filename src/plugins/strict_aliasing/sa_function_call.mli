(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Sa_types

(** Generic handler for a function call. *)
module type FunctionCallHandler = sig

  (** [handle lmap state lval_opt fname formals] is a function which
      evaluate the function call giving the current typing state [lmap],
      the value analysis state [state] and every informations given by
      the [Cil_types.Call] instruction. *)
  val handle :
    Lmap_pointer_aliasing.t -> Db.Value.state -> Cil_datatype.Lval.t option ->
    Cil_types.exp -> Cil_types.exp list -> Lmap_pointer_aliasing.t
end

module MemcpyHandler : FunctionCallHandler
module FreadHandler : FunctionCallHandler
module ReadHandler : FunctionCallHandler
module MemsetHandler : FunctionCallHandler

(** The module [NoActionHandler] is used when there is nothing to do during the
    call of the function. *)
module NoActionHandler : FunctionCallHandler

module Builtins : sig
  val is_show_each : string -> bool
  val is_dump_each : string -> bool
  val is_sa_builtin : string -> bool
  val is_sa_builtin_kf : Kernel_function.t -> bool
  val is_sa_builtin_exp : Cil_datatype.Exp.t -> bool

  module ShowEach : FunctionCallHandler
  module DumpEach : FunctionCallHandler
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
