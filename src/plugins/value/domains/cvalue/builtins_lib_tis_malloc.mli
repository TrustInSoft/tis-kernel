(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** Dynamic allocation related builtins.
    Most functionality is exported as builtins. *)

val alloc_abstract_strong :
  Cil_types.location ->
  string -> Cvalue.V.t -> Cvalue.Model.t ->
  Locations.Location_Bytes.t * Cvalue.Model.t
