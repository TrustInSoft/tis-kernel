(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** This module contains some helper functions to by used by TiS builtins *)

val optionally_set_errno : Ival.t -> Cvalue.Model.t -> Cvalue.Model.t


type overlap_status_t = Overlap | Separated | MayOverlap

val dkey : Log.category

(** Function to register the builtin *)
val register_builtin : string -> Db.Value.builtin_sig -> unit

(** Returns the overlap status of two zones of the memory, each zone
    being defined by a Location_Bits and a size in bits (or bytes). *)
val overlap_status_loc_bits :
  ?size_in_bytes:bool ->
  Locations.Location_Bits.t -> Ival.t ->
  Locations.Location_Bits.t -> Ival.t ->
  overlap_status_t

(** Returns the overlap status of two zones of the memory, each zone
    being defined by a Location_Bytes and a size in bytes. *)
val overlap_status_loc_bytes :
  Locations.Location_Bytes.t -> Ival.t ->
  Locations.Location_Bytes.t -> Ival.t ->
  overlap_status_t

(** Returns the overlap status of two zones of the memory, each zone
    being defined by a Location. The locations need not have the same size *)
val overlap_status_loc :
  Locations.location -> Locations.location -> overlap_status_t

(** Returns a location of size sizeof_loc
    (the size is {Bit_utils.sizeofchar ()} by default).*)
val location_of_cvalue :
  ?sizeof_loc:Integer.t ->
  Locations.Location_Bytes.t -> Locations.location

(** This function takes a size and a triple (ptr_expression, ptr_addr, _) and
    emits a warning if the size can contain zero and the pointer address is not
    valid.
    The check is necessary as all functions in subclause 7.24 of
    ISO/IEC 9899:201x N1570 expect a valid pointer even if the size is zero
    (unless explicitly stated otherwise, see 7.24.1-2 *)
val additional_ptr_validity_check_for_size_zero :
  for_writing:bool ->
  size:Cvalue.V.t -> Cil_types.exp * Cvalue.V.t * 'a ->
  unit

module StringAndArrayUtilities : sig

  (* The pseudo-infinity length for string operations. *)
  val str_infinity : Integer.t

  (* Character Strings / Wide Character Strings *)
  type libc_character_kind =
    | Character
    | WideCharacter

  val get_character_bits : libc_character_kind -> Integer.t

  (* TODO: Required for abstract_strcpy and abstract_strcmp
           to avoid a forward reference. *)
  module Strlen : sig

    type strlen_alarm_context = {
      strlen_alarm_invalid_string : unit -> unit
    }

    type abstract_strlen_type =
      (character_bits:Integer.t ->
       max:Abstract_interp.Int.t ->
       emit_alarm:strlen_alarm_context ->
       Locations.Location_Bytes.t -> Cvalue.Model.t -> Ival.t)

    val abstract_strlen_ref : abstract_strlen_type ref

    val get_abstract_strlen : unit -> abstract_strlen_type

  end

end

(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
