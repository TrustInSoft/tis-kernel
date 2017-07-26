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


open Cil_datatype

(** State for the analysis of one function call *)
type from_state = {
  current_function: Kernel_function.t (** Function being analyzed *);
  value_initial_state: Db.Value.state (** State of Value at the beginning of
                                          the call *);
  table_for_calls: Function_Froms.t Kernel_function.Hashtbl.t Kinstr.Hashtbl.t
(** State of the From plugin for each statement containing a function call
    in the body of [current_function]. Updated incrementally each time
    Value analyses such a statement *);
}

(** The state of the callwise From analysis. Only the top of this callstack
    is accessed. New calls are pushed on the stack when Value starts the
    analysis of a function, and popped when the analysis finisheds. This
    stack is manually synchronized with Value's callstack. *)
let call_froms_stack : from_state list ref = ref []
