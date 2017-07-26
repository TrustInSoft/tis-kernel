(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2015-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** Handling variadic arguments. *)

val add_variadic_arguments_to_state : Kernel_function.t -> Cil_types.kinstr ->
  (Cil_types.exp * Cvalue.V_Offsetmap.t) list ->
  Cvalue.Model.t -> Cvalue.Model.t
(** [add_variadic_arguments_to_state kf call_site actuals state] adds the
    variadic arguments for the function [kf] at call site [call_site] provided
    as [actuals] to the given abstract state [state]. Creates new varinfos for
    the arguments if necessary or reuses existing ones. Returns the new abstract
    state prepared this way. *)

val remove_variadic_arguments_from_state : Kernel_function.t ->
  Cvalue.Model.t -> Cvalue.Model.t
(** [remove_variadic_arguments_from_state kf state] removes all the variadic
    arguments of the function [kf] from the given abstract state [state].
    Returns the new abstract state prepared this way. *)

val get_variadic_arguments_varinfos : Kernel_function.t -> Cil_types.kinstr ->
  Cil_types.varinfo list
(** [get_variadic_arguments_varinfos kf call_site] returns a list of [varinfos]
    corresponding to the variadic arguments of function [kf] at the given call
    site [call_site]. *)

(** Handling [va_list] objects. *)

val create_structure_for_variadic_variables : Cil_types.varinfo list ->
  Cvalue.Model.t -> Cvalue.Model.t
(** [create_structure_for_variadic_variables varinfos state] creates the
    va_list object structure for each variable of type va_list in the provided
    [varinfos] list and sets the value of each such variable to a pointer to the
    corresponding structure. Returns the new abstract state prepared this way. *)

val map_varinfos_from_va_list : Cvalue.V.t -> Cvalue.Model.t ->
  (Cil_types.varinfo -> 'a) -> 'a list

val remove_structure_for_variadic_variables : Cil_types.varinfo list ->
  Cvalue.Model.t -> Cvalue.Model.t
(** [remove_structure_for_variadic_variables varinfos state] removes the
    va_list object structure for each variable of type va_list in the provided
    [varinfos] list and deinitializes each such variable. Returns the new
    abstract state prepared this way. *)

val check_variadic_variables : Kernel_function.t ->
  Cvalue.Model.t -> Cil_types.varinfo list -> bool
(** [check_variadic_variables kf state varinfos] checks if all the variables
    from the [varinfos] list that are of type va_list have been properly
    uninitialized. For each potentially uninitialized variable it prints a
    warning (the warning's text includes the name of the function [kf]). It
    returns [true] if among these variables there is none which is CERTAINLY
    initialized, [false] otherwise. *)

val arg_loc_of_va_list_loc :
  Cvalue.Model.t -> Locations.location -> Locations.location
(** [arg_loc_of_va_list_loc state va_list_loc] retrieves the location
    corresponding to the next variadic argument (i.e. the one that would be read
    next by the [va_arg] macro) of the va_list object given by location
    [va_list_loc] in the abstract state [state]. *)

(** Functions managing the variadic variables when entering / exiting a block. *)

val bind_block_variadic_locals: State_set.t -> Cil_types.block -> State_set.t
(** A direct add-on on [Value_util.bind_block_locals]. Creates the underlying
    structure for all the block's local va_list variables. *)
val bind_variadic_locals_state: Cvalue.Model.t -> Cil_types.block -> Cvalue.Model.t
(**Do the same for a single state*)

val uninitialize_blocks_variadic_locals : Cil_types.block list ->
  Cvalue.Model.t -> Cvalue.Model.t
(** A direct add-on on [Cvalue.Model.uninitialize_blocks_locals]. Removes the
    underlying structure for all the block's local va_list variables. *)


(** The implementation of variadic macros. *)

val va_start : with_alarms:CilE.warn_mode -> Kernel_function.t ->
  Cil_types.lval ->
  Cvalue.Model.t -> Cvalue.Model.t
(** The [va_start] macro implementation. *)

val va_end : with_alarms:CilE.warn_mode -> Kernel_function.t ->
  Cil_types.lval ->
  Cvalue.Model.t -> Cvalue.Model.t
(** The [va_end] macro implementation. *)

val va_copy : with_alarms:CilE.warn_mode -> Kernel_function.t ->
  Cil_types.lval * Cil_types.exp ->
  Cvalue.Model.t -> Cvalue.Model.t
(** The [va_copy] macro implementation. *)

val va_arg : with_alarms:CilE.warn_mode -> Kernel_function.t ->
  (Cil_types.lval * Cil_types.typ * Cil_types.lval) ->
  Cvalue.Model.t -> Cvalue.Model.t
(** The [va_arg] macro implementation. *)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
