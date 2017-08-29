(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2017 TrustInSoft                                      *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

type memcpy_alarm_context = {
  (* The warn mode. *)
  memcpy_alarm_warn_mode: CilE.warn_mode;
  (* Function setting the syntactic context to the source. *)
  memcpy_alarm_set_syntactic_context_array_src: unit -> unit;
  (* Function setting the syntactic context to the destination. *)
  memcpy_alarm_set_syntactic_context_array_dst: unit -> unit
}

(** Alarm context used when necessary information about source and destination
    is hard to get. *)
val memcpy_alarm_context_none: memcpy_alarm_context

val abstract_memcpy:
  ?exact:bool
  -> character_bits:Integer.t
  -> emit_alarm:memcpy_alarm_context
  -> size:Ival.t (* Number of characters to copy. *)
  -> Locations.Location_Bytes.t
  -> Locations.Location_Bytes.t
  -> Cvalue.Model.t
  -> Cvalue.Model.t * Function_Froms.froms * Locations.Zone.t
