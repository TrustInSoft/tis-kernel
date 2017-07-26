(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** {2 Types} *)

(* When implementing qualifiers usage, make it private again. *)
type qualifiers =
  | QConst
  | QRestrict
  | QVolatile

type integer_type = private
  | Bool
  | Short
  | UnsignedShort
  | Integer
  | UnsignedInteger
  | Long
  | UnsignedLong
  | LongLong
  | UnsignedLongLong
  | Int128
  | UnsignedInt128
  | Char
  | SignedChar
  | UnsignedChar

exception ImpreciseIndex of Ival.t

module rec SimpleType : sig
  type function_type = private {
    return_type : simple_type;
    parameters : simple_type list;
  }

  and simple_type = private
    | Structure of Cil_types.compinfo
    | StructureField of Cil_types.fieldinfo * simple_type
    | Union of Cil_types.compinfo * SimpleTypeSet.t
    | Enum of Cil_types.enuminfo
    | Array of simple_type * Cil_types.exp option
    | ArrayElement of simple_type * Cil_types.exp option * Ival.t
    | IntegerType of integer_type
    | FloatType of Cil_types.fkind
    | Function of function_type
    | VariadicFunction of function_type
    | PointerType of simple_type
    | FirstAccessType
    | VoidType
    | MayAlias
    | VaList

  type t = simple_type

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val is_compatible : t -> t -> bool
  val is_compatible_with_set : t -> SimpleTypeSet.t -> bool
  val pretty : Format.formatter -> t -> unit
  val pretty_set : Format.formatter -> SimpleTypeSet.t -> unit
end
and SimpleTypeSet : Set.S with type elt = SimpleType.t


module EffectiveType : sig
  type t =
    | Top
    | EffectiveType of SimpleTypeSet.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val is_compatible : t -> t -> bool
  val hash : t -> int
  val pretty : Format.formatter -> t -> unit
end

module EffectiveTypeData : Datatype.S with type t := EffectiveType.t

module Lmap_pointer_aliasing :
  Lmap_bitwise.Location_map_bitwise with type v:= EffectiveType.t


(** {2 Shortcuts} **)

val top : EffectiveType.t
val bottom : EffectiveType.t
val may_alias : EffectiveType.t
val st_bool : SimpleType.simple_type
val st_short : SimpleType.simple_type
val st_int : SimpleType.simple_type
val st_long : SimpleType.simple_type
val st_longlong : SimpleType.simple_type
val st_float : SimpleType.simple_type
val st_double : SimpleType.simple_type
val st_longdouble : SimpleType.simple_type
val st_char : SimpleType.simple_type
val st_schar : SimpleType.simple_type
val st_uchar : SimpleType.simple_type
val st_void : SimpleType.simple_type
val st_ptr_char : SimpleType.simple_type
val st_va_list : SimpleType.simple_type
val et_bool : EffectiveType.t
val et_short : EffectiveType.t
val et_int : EffectiveType.t
val et_long : EffectiveType.t
val et_longlong : EffectiveType.t
val et_float : EffectiveType.t
val et_double : EffectiveType.t
val et_longdouble : EffectiveType.t
val et_char : EffectiveType.t
val et_schar : EffectiveType.t
val et_uchar : EffectiveType.t
val et_void : EffectiveType.t
val et_first_access : EffectiveType.t
val et_va_list : EffectiveType.t

(** {2 Translators} *)

(** [effective_type_of_cil_type typ] translates [typ] to an effective type. *)
val effective_type_of_cil_type :
  ?array_as_ptr:bool -> Cil_types.typ -> EffectiveType.t

(** [effective_type_of_lvalue state lval] translates the type of [lval] to an
    effective type using the current [state]. *)
val effective_type_of_lvalue :
  ?array_as_ptr: bool -> Db.Value.state -> Cil_types.lval -> EffectiveType.t

(** [effective_type_of_expression ?keep_cast state expr] translates the type of
    [expr] to an effective type. If [keep_cast] is set to false and if [expr] is
    a cast to a pointer type, it ignores the cast. *)
val effective_type_of_expression :
  keep_cast:bool -> Db.Value.state -> Cil_types.exp -> EffectiveType.t

(** [effective_type_of_location location] translates the [location] to
    an effective type according to its bases and ivals. *)
val effective_type_of_location : Locations.location -> EffectiveType.t

(** [pointed_type ety] translates the effective type [ety] into the pointed type
    of [ety]. If [ety] is [Top], it's returned as is; every types into [ety]
    must have a pointer type. *)
val pointed_type : EffectiveType.t -> EffectiveType.t

(** [pointer_of ety] translates the effective type [ety] into a pointer to
    [ety]. If [ety] is [Top], it's returned as is; if [ety] contains
    multiple types, then those types are translated into pointers. *)
val pointer_of : EffectiveType.t -> EffectiveType.t



(** {2 Checker} *)

val is_bottom : EffectiveType.t -> bool

val is_may_alias : EffectiveType.t -> bool

(** [is_character_type ety] checks if [ety] is either a char, an unsigned char
    or a signed char. *)
val is_character_type : EffectiveType.t -> bool

val is_void_type : EffectiveType.t -> bool

val is_pointer_to_character : EffectiveType.t -> bool

val is_first_access_type : EffectiveType.t -> bool

(** [is_effective_type ety] checks if [ety] exists and is not Top. *)
val is_effective_type : EffectiveType.t -> bool

(** [has_first_access_type ety] checks if the effective type contains
    the simple type [FirstAccessType]. *)
val has_first_access_type : EffectiveType.t -> bool

(** [replace_first_access_type from to] removes the FirstAccessType of
    the [from] effective type by [to].
    If [from] does not contains [FirstAccesType] or is top, nothing
    happens. *)
val replace_first_access_type :
  EffectiveType.t -> EffectiveType.t -> EffectiveType.t

(** {2 Pretty printers} *)

val pretty_integer_type : Format.formatter -> integer_type -> unit



(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
