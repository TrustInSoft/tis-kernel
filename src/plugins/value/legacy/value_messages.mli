(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2017 TrustInSoft                                      *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** Warnings can either emit ACSL (Alarm), or do not emit ACSL
    (others). *)
type warning =
  | Alarm of
      Alarms.t *
      Cil_types.code_annotation *
      Property_status.emitted_status
  | Uncategorized of string

type precision_loss_message =
  | Exhausted_slevel
  | Garbled_mix_creation of
      Cil_types.exp (* Expression that creates the garbled mix. *)
  | Garbled_mix_propagation

type value_message =
  | Warning of warning
  | Property_evaluated of Property.t * Property_status.emitted_status
  | Precision_Loss of precision_loss_message
  | Lattice_message of Lattice_messages.emitter * Lattice_messages.t
  | Feedback of unit

module Value_Message_Callback: Hook.S
  with type param =
         value_message *
         Cil_types.kinstr *
         Value_callstack.callstack *
         Cvalue.Model.t
   and type result = unit

val new_alarm:
  Cil_types.kinstr
  -> Alarms.t
  -> Property_status.emitted_status
  -> Cil_types.code_annotation
  -> string
  -> unit

val new_status:
  Property.t
  -> Property_status.emitted_status
  -> Cvalue.Model.t
  -> unit

val warning: ('a, Format.formatter, unit) format -> 'a

val set_current_state: Cvalue.Model.t -> unit
