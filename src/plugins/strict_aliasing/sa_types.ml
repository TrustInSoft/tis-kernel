(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype
open Cil_types

type qualifiers =
  | QConst
  | QRestrict
  | QVolatile

type integer_type =
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

let pretty_integer_type fmt t =
  Format.fprintf fmt
    (match t with
     | Bool -> "bool"
     | Short -> "short"
     | UnsignedShort -> "unsigned short"
     | Integer -> "int"
     | UnsignedInteger -> "unsigned int"
     | Long -> "long"
     | UnsignedLong -> "unsigned long"
     | LongLong -> "long long"
     | UnsignedLongLong -> "unsigned long long"
     | Int128 -> "__int128"
     | UnsignedInt128 -> "unsigned __int128"
     | Char -> "char"
     | SignedChar -> "signed char"
     | UnsignedChar -> "unsigned char")

exception ImpreciseIndex of Ival.t

module rec SimpleType : sig
  type function_type = {
    return_type : simple_type;
    parameters : simple_type list;
  }

  and simple_type =
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
end = struct
  type function_type = {
    return_type : simple_type;
    parameters : simple_type list;
  }

  and simple_type =
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

  let int_of_simple_type = function
    | StructureField _ -> 1
    | Union _ -> 2
    | IntegerType _ -> 3
    | FloatType _ -> 4
    | PointerType _ -> 5
    | MayAlias -> 7
    | VoidType -> 8
    | Enum _ -> 9
    | Function _ -> 10
    | Structure _ -> 11
    | FirstAccessType -> 12
    | VariadicFunction _ -> 13
    | Array _ -> 14
    | ArrayElement _ -> 15
    | VaList -> 16

  let compare_integer_type_no_sign i1 i2 =
    match i1, i2 with
    | Short, UnsignedShort | UnsignedShort, Short
    | Integer, UnsignedInteger | UnsignedInteger, Integer
    | Long, UnsignedLong | UnsignedLong, Long
    | LongLong, UnsignedLongLong | UnsignedLongLong, LongLong -> 0
    | _ -> Pervasives.compare i1 i2

  let rec compare t1 t2 =
    match t1, t2 with
    | StructureField (c1, set1),
      StructureField (c2, set2) ->
      let res = Fieldinfo.compare c1 c2 in
      if res = 0 then compare set1 set2
      else res
    | Union (u1, set1), Union (u2, set2) ->
      let res = Compinfo.compare u1 u2 in
      if res = 0 then SimpleTypeSet.compare set1 set2 else res
    | IntegerType i1, IntegerType i2 ->
      compare_integer_type_no_sign i1 i2
    | FloatType f1, FloatType f2 ->
      Pervasives.compare f1 f2
    | PointerType (IntegerType i1), PointerType (IntegerType i2) ->
      Pervasives.compare i1 i2
    | PointerType t1, PointerType t2 -> compare t1 t2
    | Enum e1, Enum e2 -> Enuminfo.compare e1 e2
    | VariadicFunction fptr, VariadicFunction fptr2
    | Function fptr, Function fptr2 ->
      let res = compare fptr.return_type fptr2.return_type in
      if res = 0 then compare_list fptr.parameters fptr2.parameters
      else res
    | Structure s1, Structure s2 -> Compinfo.compare s1 s2
    | Array (s1, e1), Array (s2, e2) ->
      let res = compare s1 s2 in
      if res = 0 then Extlib.opt_compare Exp.compare e1 e2
      else res
    | ArrayElement (s1, e1, i1), ArrayElement (s2, e2, i2) ->
      let res = compare s1 s2 in
      if res = 0 then
        let res' = Extlib.opt_compare Exp.compare e1 e2 in
        if res' = 0 then Ival.compare i1 i2
        else res'
      else res
    | _ -> int_of_simple_type t1 - int_of_simple_type t2
  and compare_list ts1 ts2 =
    match ts1, ts2 with
    | [], [] -> 0
    | x :: xs, y :: ys ->
      let res = compare x y in
      if res = 0 then compare_list xs ys else res
    | _, [] -> -1
    | [], _ -> 1

  let equal st1 st2 = compare st1 st2 = 0

  let rec pretty fmt t =
    let rec pretty_aux ~ptr_depth t =
      let stars = String.make ptr_depth '*' in
      let space = if ptr_depth = 0 then "" else " " in
      match t with
      | Structure cinfo ->
        Format.fprintf fmt "struct %s%s%s" cinfo.cname space stars
      | StructureField (finfo, ety) ->
        Format.fprintf fmt "(struct %s).%s[%a]%s"
          finfo.fcomp.cname
          finfo.fname
          pretty ety
          stars
      | Union (compinfo, ens_type) ->
        Format.fprintf fmt "(union %s%s%s)[%a]"
          compinfo.cname space stars pretty_set ens_type
      | Enum enuminfo ->
        Format.fprintf fmt "enum %s%s%s" enuminfo.eorig_name space stars
      | IntegerType it ->
        Format.fprintf fmt "%a%s%s" pretty_integer_type it space stars
      | FloatType ft ->
        Format.fprintf fmt "%a%s%s" Cil_printer.pp_fkind ft space stars
      | VariadicFunction { return_type; parameters } ->
        Format.fprintf fmt "(%a %s(%a, ...))"
          pretty return_type
          (if stars = "" then "" else Printf.sprintf "(%s)" stars)
          pretty_simple_type_list parameters
      | Function { return_type; parameters } ->
        Format.fprintf fmt "(%a %s(%a))"
          pretty return_type
          (if stars = "" then "" else Printf.sprintf "(%s)" stars)
          pretty_simple_type_list parameters
      | FirstAccessType -> Format.fprintf fmt "<first access type>"
      | PointerType st -> pretty_aux ~ptr_depth:(ptr_depth + 1) st
      | VoidType -> Format.fprintf fmt "void%s%s" space stars
      | MayAlias -> Format.fprintf fmt "__may_alias__"
      | Array (sty, e) -> pretty_array_deep fmt space stars sty [e]
      | ArrayElement (sty, e, ival) ->
        pretty_array_element_deep fmt space stars sty [e] [ival]
      | VaList -> Format.fprintf fmt "va_list"
    and pretty_array_deep fmt space stars sty e =
      match sty with
      | Array (sty, e') -> pretty_array_deep fmt space stars sty (e' :: e)
      | sty ->
        Format.fprintf fmt "%a%a%s%s"
          pretty sty
          (Pretty_utils.pp_list ~pre:"[" ~suf:"]" ~sep:"]["
             (Pretty_utils.pp_opt Exp.pretty))
          (List.rev e)
          space stars
    and pretty_array_element_deep fmt space stars sty e ivals =
      match sty with
      | ArrayElement (sty, e', ival') ->
        pretty_array_element_deep fmt space stars sty (e' :: e) (ival' :: ivals)
      | sty ->
        let pp fmt = Pretty_utils.pp_list ~pre:"[" ~suf:"]" ~sep:"][" fmt in
        Format.fprintf fmt "(%a%a)@@%a%s%s"
          pretty sty
          (pp (Pretty_utils.pp_opt Exp.pretty)) (List.rev e)
          (pp Ival.pretty) (List.rev ivals)
          space stars
    in
    pretty_aux ~ptr_depth:0 t

  and pretty_simple_type_list stys =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
      pretty stys

  and pretty_set fmt set =
    let singleton = SimpleTypeSet.cardinal set = 1 in
    Pretty_utils.pp_iter
      ~pre:(if singleton then "" else "@[{")
      ~sep:", "
      ~suf:(if singleton then "" else "}@]")
      SimpleTypeSet.iter pretty fmt set

  let hash = int_of_simple_type

  let is_included set t =
    SimpleTypeSet.exists (equal t) set

  let rec check_simple_type_precision t =
    match t with
    | ArrayElement (sty, _, ival) ->
      if not (Ival.is_singleton_int ival) then raise (ImpreciseIndex ival);
      check_simple_type_precision sty
    | StructureField (_, sty) -> check_simple_type_precision sty
    | Array (sty, _) -> check_simple_type_precision sty
    | PointerType sty -> check_simple_type_precision sty
    | _ -> ()

  let rec is_compatible tleft tright =
    check_simple_type_precision tleft;
    check_simple_type_precision tright;
    match tleft, tright with
    | Union _, Union _ ->
      equal tleft tright
    | t, Union (_, uset') | Union (_, uset'), t
      when not (Sa_parameters.StrictUnion.get ()) ->
      is_included uset' t
    | StructureField _, StructureField _ ->
      equal tleft tright
    | _, StructureField (_, ety)
      when not (Sa_parameters.StrictStruct.get ()) ->
      is_compatible tleft ety
    | StructureField (_, ety), _
      when not (Sa_parameters.StrictStruct.get ()) ->
      is_compatible ety tright
    | PointerType pt' , PointerType (StructureField _ as pt)
    | PointerType (StructureField _ as pt), PointerType pt'
      when not (Sa_parameters.StrictStruct.get ()) ->
      is_compatible pt' pt
    | PointerType pt', PointerType (Union _ as pt) ->
      is_compatible pt' pt
    | _, Array (sty, _) | _, ArrayElement (sty, _, _) ->
      is_compatible tleft sty
    | Array (sty, _), _ | ArrayElement (sty, _, _), _ ->
      is_compatible sty tright
    | _ ->
      equal tleft tright

  (** [is_compatible_with_set ety ety_set] checks if the effective type is
      compatible with all the elements from the set of effective type [ety_set].

      It's mainly used to support the compatibility between a pointer to T, and
      an effective type of two pointers to two differents fields of type T. *)
  let is_compatible_with_set ety ety_set =
    SimpleTypeSet.for_all
      (fun t ->
         match t with
         | StructureField (_, field_ety) -> is_compatible ety field_ety
         | ArrayElement (sty, _, _) -> is_compatible ety sty
         | _ -> equal ety t)
      ety_set

end
and SimpleTypeSet : Set.S with type elt = SimpleType.t
  = Set.Make(SimpleType)


module EffectiveType = struct
  include Datatype.Undefined

  type t =
    | Top
    | EffectiveType of SimpleTypeSet.t

  let name = "Pointer_aliasing_datatype.effective_type"
  let rehash = Datatype.identity
  let structural_descr = Structural_descr.t_abstract
  let reprs = [ Top ]

  let pretty fmt t =
    match t with
    | Top -> Format.fprintf fmt "#TOP"
    | EffectiveType ens_type ->
      if SimpleTypeSet.is_empty ens_type
      then Format.fprintf fmt  "%s" (Unicode.emptyset_string ())
      else Format.fprintf fmt "%a" SimpleType.pretty_set ens_type

  let mem_project = Datatype.never_any_project

  let compare t1 t2 =
    match t1, t2 with
    | Top, Top -> 0
    | Top, _ -> -1
    | _, Top -> 1
    | EffectiveType set1, EffectiveType set2 -> SimpleTypeSet.compare set1 set2

  let equal t1 t2 = compare t1 t2 = 0

  let is_compatible tleft tright =
    match tleft, tright with
    | _, Top -> true
    | Top, _ -> false
    | EffectiveType s1, EffectiveType s2
      when SimpleTypeSet.cardinal s1 = 1 && SimpleTypeSet.cardinal s2 = 1 ->
      let elt_s1 = SimpleTypeSet.min_elt s1 in
      let elt_s2 = SimpleTypeSet.min_elt s2 in
      SimpleType.is_compatible elt_s2 elt_s1
    | EffectiveType s1, EffectiveType s2
      when SimpleTypeSet.cardinal s1 = 1 ->
      let elt_s1 = SimpleTypeSet.min_elt s1 in
      SimpleType.is_compatible_with_set elt_s1 s2
    | EffectiveType s2, EffectiveType s1
      when SimpleTypeSet.cardinal s1 = 1 ->
      let elt_s1 = SimpleTypeSet.min_elt s1 in
      SimpleType.is_compatible_with_set elt_s1 s2
    | _ -> false

  let hash = function
    | Top -> 3
    | EffectiveType set ->
      SimpleTypeSet.fold (fun elt acc -> SimpleType.hash elt + acc) set 0
end

open SimpleType


(* DEFAULT VALUES *)

let top = EffectiveType.Top
let bottom = EffectiveType.EffectiveType SimpleTypeSet.empty
let may_alias = EffectiveType.EffectiveType (SimpleTypeSet.singleton MayAlias)
let st_bool = IntegerType Bool
let st_short = IntegerType Short
let st_ushort = IntegerType UnsignedShort
let st_int = IntegerType Integer
let st_uint = IntegerType UnsignedInteger
let st_long = IntegerType Long
let st_ulong = IntegerType UnsignedLong
let st_longlong = IntegerType LongLong
let st_ulonglong = IntegerType UnsignedLongLong
let st_int128 = IntegerType Int128
let st_uint128 = IntegerType UnsignedInt128
let st_char = IntegerType Char
let st_schar = IntegerType SignedChar
let st_uchar = IntegerType UnsignedChar
let st_float = FloatType Cil_types.FFloat
let st_double = FloatType Cil_types.FDouble
let st_longdouble = FloatType Cil_types.FLongDouble
let st_void = VoidType
let st_ptr_char = PointerType st_char
let st_first_access = FirstAccessType
let st_va_list = VaList
let et_bool = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_bool)
let et_short = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_short)
let et_int = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_int)
let et_long = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_long)
let et_longlong =
  EffectiveType.EffectiveType (SimpleTypeSet.singleton st_longlong)
let et_float = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_float)
let et_double = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_double)
let et_longdouble =
  EffectiveType.EffectiveType (SimpleTypeSet.singleton st_longdouble)
let et_char = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_char)
let et_schar = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_schar)
let et_uchar = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_uchar)
let et_void = EffectiveType.EffectiveType (SimpleTypeSet.singleton st_void)
let et_ptr_char =
  EffectiveType.EffectiveType (SimpleTypeSet.singleton st_ptr_char)
let et_first_access =
  EffectiveType.EffectiveType (SimpleTypeSet.singleton st_first_access)
let et_va_list =
  EffectiveType.EffectiveType (SimpleTypeSet.singleton st_va_list)

(* HELPERS *)

let is_bottom ety =
  match ety with
  | EffectiveType.Top -> false
  | EffectiveType.EffectiveType set -> SimpleTypeSet.is_empty set

let is_may_alias = EffectiveType.equal may_alias
let is_void_type = EffectiveType.equal et_void
let is_pointer_to_character = EffectiveType.equal et_ptr_char

let is_character_type ety =
  EffectiveType.equal et_char ety ||
  EffectiveType.equal et_schar ety ||
  EffectiveType.equal et_uchar ety

let is_effective_type = function
  | EffectiveType.EffectiveType _ -> true
  | EffectiveType.Top -> false

let is_first_access_type = EffectiveType.equal et_first_access

let has_first_access_type = function
  | EffectiveType.EffectiveType set ->
    SimpleTypeSet.exists (SimpleType.equal st_first_access) set
  | EffectiveType.Top -> false

let replace_first_access_type et1 et2 =
  match et1, et2 with
  | EffectiveType.Top, _ -> EffectiveType.Top
  | _, EffectiveType.Top -> et1
  | EffectiveType.EffectiveType _, _ when not (has_first_access_type et1) -> et1
  | EffectiveType.EffectiveType set1, EffectiveType.EffectiveType set2 ->
    let set' =
      SimpleTypeSet.filter
        (fun s -> not (SimpleType.equal st_first_access s))
        set1
    in
    EffectiveType.EffectiveType (SimpleTypeSet.union set' set2)



(* TRANSLATORS *)

let pointer_of = function
  | EffectiveType.EffectiveType set1 ->
    let set' =
      SimpleTypeSet.fold
        (fun elt acc -> SimpleTypeSet.add (PointerType elt) acc)
        set1
        SimpleTypeSet.empty
    in
    EffectiveType.EffectiveType set'
  | ety -> ety

let pointed_type ety =
  let rec pointed_simple_type sty =
    match sty with
    | PointerType st -> st
    | StructureField (x, ssty) ->
      StructureField (x, pointed_simple_type ssty)
    | Array (sty, _)
    | ArrayElement (sty, _, _) -> sty
    | MayAlias -> MayAlias
    | _ ->
      Sa_parameters.abort "assert is_pointer_type(%a).@\n"
        SimpleType.pretty sty
  and pointed_set_type set =
    SimpleTypeSet.fold
      (fun elt acc -> SimpleTypeSet.add (pointed_simple_type elt) acc)
      set
      SimpleTypeSet.empty
  in
  match ety with
  | EffectiveType.Top -> top
  | EffectiveType.EffectiveType set ->
    EffectiveType.EffectiveType (pointed_set_type set)


let rec function_type is_variadic ret_type parameters =
  let extract (_, t, _) = simple_type_of_cil_type t in
  let return_type = simple_type_of_cil_type ret_type in
  let parameters = List.map extract parameters in
  let ftype = { return_type; parameters } in
  if is_variadic then VariadicFunction ftype
  else Function ftype

and union_type fcomp =
  assert (not fcomp.cstruct);
  let types =
    List.fold_left
      (fun acc elt ->
         let sty = simple_type_of_cil_type elt.ftype in
         SimpleTypeSet.add sty acc)
      SimpleTypeSet.empty
      fcomp.cfields
  in
  Union (fcomp, types)

and simple_type_of_cil_type ?(array_as_ptr = false) ty =
  match Cil.unrollType ty with
  | TVoid _ -> st_void
  | TInt (IBool, _) -> st_bool
  | TInt (IShort, _) -> st_short
  | TInt (IUShort, _) -> st_ushort
  | TInt (IInt, _) -> st_int
  | TInt (IUInt, _) -> st_uint
  | TInt (ILong, _) -> st_long
  | TInt (IULong, _) -> st_ulong
  | TInt (ILongLong, _) -> st_longlong
  | TInt (IULongLong, _) -> st_ulonglong
  | TInt (IInt128, _) -> st_int128
  | TInt (IUInt128, _) -> st_uint128
  | TInt (IChar, _) -> st_char
  | TInt (ISChar, _) -> st_schar
  | TInt (IUChar, _) -> st_uchar
  | TFloat (FFloat, _) -> st_float
  | TFloat (FDouble, _) -> st_double
  | TFloat (FLongDouble, _) -> st_longdouble
  | TFun (return_type, parameters, variadic, _) ->
    function_type variadic return_type (Cil.argsToList parameters)
  | TArray (ty, exp, _, _) ->
    let sty = simple_type_of_cil_type ty in
    if array_as_ptr then PointerType sty else Array (sty, exp)
  | TPtr (ty, _) ->
    PointerType (simple_type_of_cil_type ty)
  | TEnum (enuminfo, _) ->
    if Sa_parameters.StrictEnum.get () then Enum enuminfo
    else simple_type_of_cil_type (TInt (enuminfo.ekind, []))
  | TComp (cinfo, _, _) when cinfo.cstruct -> Structure cinfo
  | TComp (cinfo, _, _) -> union_type cinfo
  | TBuiltin_va_list _ -> st_va_list
  | TNamed _->
    Sa_parameters.fatal
      "simple_type_of_cil_type: Type `%a' is not handled.@\n"
      Typ.pretty ty

and effective_type_of_cil_type ?(array_as_ptr = false) ty =
  let sty = simple_type_of_cil_type ~array_as_ptr ty in
  EffectiveType.EffectiveType (SimpleTypeSet.singleton sty)

let ival_of_expr state expr =
  let bytes = !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state expr in
  try Locations.Location_Bytes.find Base.null bytes
  with Not_found -> Ival.bottom

let rec simple_type_of_offset state cil_type offset =
  match offset, cil_type with
  | NoOffset, t ->
    simple_type_of_cil_type t
  | Index (exp, offset'), TArray (cil_type', size, _, _) ->
    let ival = ival_of_expr state exp in
    ArrayElement (simple_type_of_offset state cil_type' offset', size, ival)
  | Field (finfo, offset'), _ ->
    if finfo.fcomp.cstruct
    then StructureField (finfo, simple_type_of_offset state finfo.ftype offset')
    else union_type finfo.fcomp
  | _ -> assert false (* Index without array type ?? *)

let effective_type_of_offset state last_type offset =
  let simple_type = simple_type_of_offset state last_type offset in
  EffectiveType.EffectiveType (SimpleTypeSet.singleton simple_type)

let has_may_alias ty =
  match Cil.unrollType ty with
  | TPtr (TNamed ({ ttype; _ }, _), _) -> Cil.typeHasAttribute "may_alias" ttype
  | _ -> false

let effective_type_of_lvalue ?(array_as_ptr = false) state lval =
  if has_may_alias (Cil.typeOfLval lval) then may_alias
  else
    match lval with
    | _, NoOffset ->
      effective_type_of_cil_type ~array_as_ptr (Cil.typeOfLval lval)
    | Var vinfo, offset -> effective_type_of_offset state vinfo.vtype offset
    | Mem e, offset ->
      let mem_lval = (Mem e, NoOffset) in
      effective_type_of_offset state (Cil.typeOfLval mem_lval) offset

let rec effective_type_of_expression ~keep_cast state e =
  match e.enode with
  | Lval l ->
    effective_type_of_lvalue state l
  | StartOf l ->
    effective_type_of_lvalue ~array_as_ptr:true state l
  | AddrOf l ->
    let add_to st acc = SimpleTypeSet.add (PointerType st) acc in
    begin match effective_type_of_lvalue state l with
      | EffectiveType.Top -> top
      | EffectiveType.EffectiveType set1 ->
        EffectiveType.EffectiveType SimpleTypeSet.(fold add_to set1 empty)
    end
  | CastE (typ, expr)
    when not keep_cast && Cil.(isPointerType (unrollType typ)) ->
    effective_type_of_expression state ~keep_cast expr
  | _ ->
    effective_type_of_cil_type Cil.(typeOf e)


let rec evaluate_type size ival typ =
  match Cil.unrollType typ with
  | TComp ({ cstruct = true; _ } as cinfo, _, _) ->
    List.fold_left
      (fun acc field ->
         let start, fsize = Cil.bitsOffset typ (Field (field, NoOffset)) in
         if Ival.overlap ival size start fsize then
           let t = simple_type_of_cil_type field.ftype in
           let ety = StructureField (field, t) in
           SimpleTypeSet.add ety acc
         else acc)
      SimpleTypeSet.empty
      cinfo.cfields
  | TArray (ty2, _, _, _) ->
    let ty_size = Cil.bitsSizeOf ty2 in
    let ival = Ival.c_rem ival (Ival.of_int ty_size) in
    evaluate_type size ival ty2
  | ty ->
    SimpleTypeSet.singleton (simple_type_of_cil_type ty)

let simple_type_of_base_and_ival size base ival =
  match base with
  | Base.Var (vinfo, _) -> evaluate_type size ival vinfo.vtype
  | Base.Null | Base.String _ -> SimpleTypeSet.singleton st_ptr_char
  | _ -> SimpleTypeSet.empty

let effective_type_of_location locations =
  let size =
    match locations.Locations.size with
    | Int_Base.Top -> assert false
    | Int_Base.Value v -> v
  in
  let effective_types =
    Locations.Location_Bits.fold_i
      (fun base ival acc ->
         let set = simple_type_of_base_and_ival size base ival in
         SimpleTypeSet.union acc set)
      locations.Locations.loc
      SimpleTypeSet.empty
  in
  EffectiveType.EffectiveType effective_types

module EffectiveTypeData = Datatype.Make(EffectiveType)

module Lmap_bitwise_input = struct
  include EffectiveTypeData

  let bottom = bottom
  let top = top

  let default base =
    match base with
    | Base.Allocated (v, _) when Cil.isPointerType v.vtype -> et_ptr_char
    | Base.Null | Base.String _ -> et_ptr_char
    |  _ -> bottom

  let join tleft tright =
    match tleft, tright with
    | EffectiveType.Top, _ | _, EffectiveType.Top -> top
    | EffectiveType.EffectiveType set1, EffectiveType.EffectiveType set2 ->
      EffectiveType.EffectiveType (SimpleTypeSet.union set1 set2)

  let is_included = EffectiveType.is_compatible

  let join_and_is_included tleft tright =
    (join tleft tright, is_included tleft tright)

  let narrow = Datatype.undefined
end

module Lmap_pointer_aliasing = Lmap_bitwise.Make_bitwise(Lmap_bitwise_input)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
