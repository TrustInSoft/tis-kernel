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

(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(** Mutually independent helper functions of Cabs2cil. *)

(*
  DEPENDS ON: Nothing.
*)

(*
  ENVIRONMENT:
  + Parameters.typeForInsertedVar
  + Parameters.typeForInsertedCast
  + Parameters.doTransformWhile
  + Parameters.doAlternateConditional
*)

(*
  EXPORTS:
  + Parameters.typeForInsertedVar        as typeForInsertedVar
  + Parameters.typeForInsertedCast       as typeForInsertedCast
  + Parameters.setDoTransformWhile       as setDoTransformWhile
  + Parameters.setDoAlternateConditional as setDoAlternateConditional
*)

open Cil_types

module Parameters = struct

  (** A hook into the code that creates temporary local vars. By default this
      is the identity function, but you can overwrite it if you need to change
      the types of cabs2cil-introduced temp variables. *)
  let typeForInsertedVar: (Cil_types.typ -> Cil_types.typ) ref = ref (fun t -> t)

  (** Like [typeForInsertedVar], but for casts.
      Casts in the source code are exempt from this hook. *)
  let typeForInsertedCast = ref (fun _ _ t -> t)

  (** To avoid generating backward gotos, we treat while loops as non-while
      ones, adding a marker for continue. (useful for Jessie) *)
  let doTransformWhile = ref false

  (** If called, sets a flag so that [continue] in while loops get transformed
      into forward gotos, like it is already done in do-while and for loops. *)
  let setDoTransformWhile () = doTransformWhile := true

  (** To avoid generating forward ingoing gotos, we translate conditionals in
      an alternate way. (useful for Jessie) *)
  let doAlternateConditional = ref false
  let setDoAlternateConditional () = doAlternateConditional := true

end

let dummy_function = Cil.emptyFunction "@dummy@"

module StripParen : sig
  val file : Cabs.file -> Cabs.file
end = struct

  module A = Cabs
  module V = Cabsvisit
  open Cabs
  open Cil

  let rec stripParenLocal e = match e.expr_node with
    | A.PAREN e2 -> stripParenLocal e2
    | _ -> e

  class stripParenClass : V.cabsVisitor = object
    inherit V.nopCabsVisitor

    method! vexpr e = match e.expr_node with
      | A.PAREN e2 -> ChangeDoChildrenPost (stripParenLocal e2,stripParenLocal)
      | _ -> DoChildren
  end

  let stripParenFile file = V.visitCabsFile (new stripParenClass) file

  let file = stripParenFile

end

let rec dropCasts e = match e.enode with
  | CastE (_, e) -> dropCasts e
  | _ -> e

(* C99 6.3.2.1:2: l-values used as r-values lose their qualifier. By default,
   we drop qualifiers, and recover them for the few operators that are
   exceptions, also listed in 6.3.2.1:2 *)
let dropQualifiers = Cil.type_remove_qualifier_attributes

let integerArrayLength (leno: exp option) : int =
  match leno with
  | None -> max_int
  | Some len ->
    try Cil.lenOfArray leno
    with Cil.LenOfArray ->
      Kernel.abort ~current:true
        "invalid non-constant length of array '%a'"
        Cil_printer.pp_exp len

module CanDrop : sig
  val statement : stmt -> bool
end = struct

  open Cil

  (* A simple visitor that searchs a statement for labels *)
  class canDropStmtClass pRes = object
    inherit nopCilVisitor

    method! vstmt s =
      if s.labels != [] then
        (pRes := false; SkipChildren)
      else
      if !pRes then DoChildren else SkipChildren

    method! vinst _ = SkipChildren
    method! vexpr _ = SkipChildren

  end

  let canDropStatement (s: stmt) : bool =
    let pRes = ref true in
    let vis = new canDropStmtClass pRes in
    ignore (visitCilStmt vis s);
    !pRes

  let statement = canDropStatement

end

let canDropStatement = CanDrop.statement

(* Evaluate constants to CTrue (non-zero) or CFalse (zero) *)
let rec isConstTrueFalse c : [ `CTrue | `CFalse ] =
  match c with
  | CInt64 (n,_,_) ->
    if Integer.equal n Integer.zero then `CFalse else `CTrue
  | CChr c ->
    if Char.code c = 0 then `CFalse else `CTrue
  | CStr _ | CWStr _ -> `CTrue
  | CReal(f, _, _) ->
    if f = 0.0 then `CFalse else `CTrue
  | CEnum { eival = e; _ } ->
    match isExpTrueFalse e with
    | `CTrue | `CFalse as r -> r
    | `CUnknown -> Kernel.fatal ~current:true "Non-constant enum"

(* Evaluate expressions to `CTrue, `CFalse or `CUnknown *)
and isExpTrueFalse e : [ `CTrue | `CFalse | `CUnknown ] =
  match e.enode with
  | Const c -> (isConstTrueFalse c :> [ `CTrue | `CFalse | `CUnknown ])
  | CastE _ ->
    (* Do not ignore the cast, because of possible overflows.
       However, calling constFoldToInt might make some UB disappear... *)
    begin match Cil.constFoldToInt e with
      | None -> `CUnknown
      | Some i -> if Integer.(equal zero i) then `CFalse else `CTrue
    end
  | _ -> `CUnknown


let rec isCabsZeroExp e =
  let open Cabs in
  match e.expr_node with
  | CAST (_, ie) ->
    begin match ie with
      | SINGLE_INIT e -> isCabsZeroExp e
      | NO_INIT | COMPOUND_INIT _ -> false
    end
  | CONSTANT (CONST_INT i) ->
    Integer.is_zero (Cil.parseInt i)
  | _ -> false

(** Applies [mkAddrOf] after marking variable whose address is taken. *)
let mkAddrOfAndMark loc ((b, off) as lval) : exp =
  (* Mark the vaddrof flag if b is a variable *)
  begin match Cil.lastOffset off with
    | NoOffset ->
      (match b with
       | Var vi ->
         (* Do not mark arrays as having their address taken. *)
         if not (Cil.isArrayType vi.vtype) then
           vi.vaddrof <- true
       | _ -> ())
    | Index _ -> ()
    | Field(fi,_) -> fi.faddrof <- true
  end;
  Cil.mkAddrOf ~loc lval

(* Call only on arrays *)
let mkStartOfAndMark loc ((_b, _off) as lval) : exp =
  (* Mark the vaddrof flag if b is a variable *)
  (* Do not mark arrays as having their address taken.
     (match b with
     | Var vi -> vi.vaddrof <- true
     | _ -> ());
  *)
  let res = Cil.new_exp ~loc (StartOf lval) in
  res

let isOldStyleVarArgName n =
  if Cil.msvcMode () then n = "va_alist"
  else n = "__builtin_va_alist"

let isOldStyleVarArgTypeName n =
  if Cil.msvcMode () then n = "va_list"  || n = "__ccured_va_list"
  else n = "__builtin_va_alist_t"

(* Check if a type is a transparent union, and return the first field if it
   is. *)
let isTransparentUnion (t: typ) : fieldinfo option =
  match Cil.unrollType t with
  | TComp (comp, _, _) when not comp.cstruct ->
    (* Turn transparent unions into the type of their first field *)
    if Cil.typeHasAttribute "transparent_union" t then begin
      match comp.cfields with
      | [] ->
        Kernel.abort ~current:true
          "Empty transparent union: %s" (Cil.compFullName comp)
      | f :: _ -> Some f
    end else
      None
  | _ -> None

let cabslu s =
  let open Lexing in
  {Lexing.dummy_pos with pos_fname="Cabs2cil_start"^s},
  {Lexing.dummy_pos with pos_fname="Cabs2cil_end"^s}

let cabs_exp loc node =
  let open Cabs in
  { expr_loc = loc; expr_node = node }

module CabsAttributes : sig
  val cabsAddAttributes : attribute list -> attributes -> attributes
  val cabsTypeAddAttributes : attributes -> typ -> typ
end = struct

  open Cil

  (* We have our own version of addAttributes that does not allow duplicates *)
  let cabsAddAttributes al0 (al: attributes) : attributes =
    if al0 == [] then al else
      List.fold_left
        (fun acc (Attr(an, _) | AttrAnnot an as a) ->
           (* See if the attribute is already in there *)
           match filterAttributes an acc with
           | [] -> addAttribute a acc (* Nothing with that name *)
           | a' :: _ ->
             if Cil_datatype.Attribute.equal a a' then
               acc (* Already in *)
             else begin
               Kernel.debug ~level:3
                 "Duplicate attribute %a along with %a"
                 Cil_printer.pp_attribute a Cil_printer.pp_attribute a' ;
               (* let acc' = dropAttribute an acc in *)
               (** Keep both attributes *)
               addAttribute a acc
             end)
        al
        al0

  (* BY: nothing cabs here, plus seems to duplicate most
         of Cil.typeAddAttributes *)
  let rec cabsTypeAddAttributes a0 t =
    begin
      match a0 with
      | [] ->
        (* no attributes, keep same type *)
        t
      | _ ->
        (* anything else: add a0 to existing attributes *)
        let add (a: attributes) = cabsAddAttributes a0 a in
        match t with
        | TVoid a -> TVoid (add a)
        | TInt (ik, a) ->
          (* Here we have to watch for the mode attribute
             sm: This stuff is to handle a GCC extension where you can
             request integer of specific widths using the "mode" attribute
             syntax; for example:
               typedef int int8_t __attribute__ ((__mode__ (  __QI__ ))) ;
             The cryptic "__QI__" defines int8_t to be 8 bits wide, instead
             of the 32 bits you'd guess if you didn't know about "mode". The
             relevant testcase is test/small2/mode_sizes.c, and it was
             inspired by my /usr/include/sys/types.h. A consequence of this
             handling is that we throw away the mode attribute, which we used
             to go out of our way to avoid printing anyway. *)
          let ik', a0' =
            (* Go over the list of new attributes and come back with a
               filtered list and a new integer kind *)
            List.fold_left
              (fun (ik', a0') a0one ->
                 match a0one with
                 | Attr("mode", [ACons(mode,[])]) -> begin
                     match ik' with
                     | IInt | IUInt ->
                       begin
                         try
                           (* GCC allows a pair of underscores on both sides of
                              the argument, but not mixed combinations *)
                           let stripUnderscoreOnBothSides n =
                             let l = String.length n in
                             if l > 4 && String.sub n 0 2 = "__"
                                && String.sub n (l - 2) 2 = "__"
                             then String.sub n 2 (l - 4)
                             else n
                           in
                           let width =
                             match stripUnderscoreOnBothSides mode with
                             | "byte"
                             | "QI" -> theMachine.theMachine.char_bit
                             | "HI" -> theMachine.theMachine.sizeof_short
                             | "word"
                             | "SI" -> theMachine.theMachine.sizeof_int
                             | "pointer" -> theMachine.theMachine.sizeof_ptr
                             | "DI" -> theMachine.theMachine.sizeof_longlong
                             | "TI" -> theMachine.theMachine.sizeof_int128
                             | _ -> raise Not_found
                           in
                           let unsigned = ik' = IUInt in
                           (intKindForSize width unsigned, a0')
                         with Not_found ->
                           begin
                             Kernel.error ~once:true ~current:true
                               "unknown GCC width mode %s"
                               mode;
                             (ik', a0one :: a0')
                           end
                       end
                     | _ ->
                       Kernel.error ~once:true ~current:true
                         "GCC width mode %s applied to unexpected type"
                         mode;
                       (ik', a0one :: a0')
                   end
                 | _ -> (ik', a0one :: a0'))
              (ik, [])
              a0
          in
          TInt (ik', cabsAddAttributes a0' a)

        | TFloat (fk, a) -> TFloat (fk, add a)
        | TEnum (enum, a) -> TEnum (enum, add a)
        | TPtr (t, a) -> TPtr (t, add a)
        | TFun (t, args, isva, a) -> TFun(t, args, isva, add a)
        | TComp (comp, s, a) -> TComp (comp, s, add a)
        | TNamed (t, a) -> TNamed (t, add a)
        | TBuiltin_va_list a -> TBuiltin_va_list (add a)
        | TArray (t, l, s, a) ->
          let att_elt, att_typ = Cil.splitArrayAttributes a0 in
          TArray (cabsArrayPushAttributes att_elt t, l, s,
                  cabsAddAttributes att_typ a)
    end
  and cabsArrayPushAttributes al = function
    | TArray (bt, l, s, a) ->
      TArray (cabsArrayPushAttributes al bt, l, s, a)
    | t -> cabsTypeAddAttributes al t

end

let cabsAddAttributes     = CabsAttributes.cabsAddAttributes
let cabsTypeAddAttributes = CabsAttributes.cabsTypeAddAttributes

module A = Cabs

let rec replaceLastInList (lst: 'a list) (how: 'a -> 'a) : 'a list =
  match lst with
  | [] -> []
  | [e] -> [how e]
  | h :: t -> h :: replaceLastInList t how

let convBinOp (bop: A.binary_operator) : binop =
  match bop with
  | A.ADD -> PlusA
  | A.SUB -> MinusA
  | A.MUL -> Mult
  | A.DIV -> Div
  | A.MOD -> Mod
  | A.BAND -> BAnd
  | A.BOR -> BOr
  | A.XOR -> BXor
  | A.SHL -> Shiftlt
  | A.SHR -> Shiftrt
  | A.EQ -> Eq
  | A.NE -> Ne
  | A.LT -> Lt
  | A.LE -> Le
  | A.GT -> Gt
  | A.GE -> Ge
  | _ -> Kernel.fatal ~current:true "convBinOp"

module Is_dangerous : sig
  val exp : exp -> bool
end = struct

  let rec is_dangerous_offset t = function
      NoOffset -> false
    | Field (fi, o) ->
      let t_offset = Cil.unrollType (Cil.typeOffset t (Field (fi, NoOffset))) in
      Cil.typeHasAttribute "volatile" t_offset ||
      is_dangerous_offset t_offset o
    | Index _ -> true

  let rec is_dangerous_exp e = match e.enode with
    | Lval lv | AddrOf lv | StartOf lv -> is_dangerous_lval lv
    | UnOp (_,e,_) | CastE(_,e) | Info(e,_) -> is_dangerous_exp e
    | BinOp(_,e1,e2,_) -> is_dangerous_exp e1 || is_dangerous_exp e2
    | Const _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
      false

  and is_dangerous_lval = function
    | Var v,_ when
        (not v.vglob && not v.vformal && not v.vtemp)
        || Cil.hasAttribute "volatile" v.vattr
        || Cil.typeHasAttribute "volatile" (Cil.unrollType v.vtype)
      -> true
    (* Local might be uninitialized, which will trigger UB,
       but we assume that the variables we generate are correctly initialized.
    *)
    | Var v, o -> is_dangerous_offset (Cil.unrollType v.vtype) o
    | Mem _,_ -> true

  let exp = is_dangerous_exp

end

module Check_no_locals : sig
  val in_initializer : init -> unit
end = struct

  open Cil

  class check_no_locals = object
    inherit Cil.nopCilVisitor
    method! vlval (h,_) =
      (match h with
       | Var v ->
         if not v.vglob then
           Kernel.error ~once:true ~current:true
             "Forbidden access to local variable %a in static initializer"
             Cil_printer.pp_varinfo v
       | _ -> ());
      DoChildren
  end

  let rec check_no_locals_in_initializer i =
    match i with
    | SingleInit e ->
      ignore (visitCilExpr (new check_no_locals) e)
    | CompoundInit (ct, initl) ->
      foldLeftCompound ~implicit:false
        ~doinit:(fun _off' i' _ () -> check_no_locals_in_initializer i')
        ~ct:ct
        ~initl:initl
        ~acc:()

  let in_initializer = check_no_locals_in_initializer

end

module ConstFold : sig
  val constFoldType : typ -> typ
end = struct

  open Cil

  let constFoldTypeVisitor = object
    inherit nopCilVisitor
    method! vtype t: typ visitAction =
      match t with
      | TArray(bt, Some len, _, a) ->
        let len' = constFold true len in
        ChangeDoChildrenPost (
          TArray(bt, Some len', empty_size_cache (), a),
          (fun x -> x)
        )
      | _ -> DoChildren
  end

  (* Const-fold any expressions that appear as array lengths in this type *)
  let constFoldType (t:typ) : typ =
    visitCilType constFoldTypeVisitor t

end

let constFoldType = ConstFold.constFoldType

let namedMembersCount (nglist : Cabs.field_group list) =
  let open Cabs in
  let is_named ((name, _, _, _), _) = name <> Cil.missingFieldName in
  let count_field field = if is_named field then 1 else 0 in
  List.fold_left (* sum the number of named fields of each field group *)
    (fun sum field_group ->
       match field_group with
       | FIELD (_, field_list) ->
         (* count the named fields in this field group *)
         List.fold_left
           (fun sum field -> sum + count_field field)
           sum field_list
       | TYPE_ANNOT _ -> sum)
    0 nglist

let rec comp_used_in_typ comp t =
  match Cil.unrollType t with
  | TArray (t',_,_,_) -> comp_used_in_typ comp t'
  | TComp (comp',_,_) ->
    if Cil_datatype.Compinfo.equal comp comp' then true
    else List.exists (fun fi -> comp_used_in_typ comp fi.ftype) comp'.cfields
  | _ -> false


let isVoidLval e t =
  match e.enode with
  | Lval _ when Cil.isVoidType t -> true
  | _                        -> false

let checkVoidLval e t =
  if isVoidLval e t then
    Kernel.abort ~current:true
      "lvalue of type void: %a@\n" Cil_printer.pp_exp e

let isPointedTypeSizeZero t =
  match Cil.typeOf_pointed t with
  | TVoid _ -> true
  | TArray (_, None , _, _) -> false (* VLA cannot have length zero. *)
  | TArray (_, array_size_opt , _, _) -> integerArrayLength array_size_opt == 0
  | TComp (compinfo, _, _) -> compinfo.cfields = []
  | _ -> false

(* The way formals are handled now might generate incorrect types, in the
   sense that they refer to a varinfo (in the case of VLA depending on a
   previously declared formal) that exists only during the call to do_type.
   We replace them here with the definitive version of the formals' varinfos.
   A global refactoring of cabs2cil would be welcome, though.
*)
let fixFormalsType formals =
  let table = Hashtbl.create 5 in
  let vis =
    object
      inherit Cil.nopCilVisitor
      method! vvrbl v =
        if v.vformal then begin
          try
            Cil.ChangeTo (Hashtbl.find table v.vname)
          with Not_found ->
            Kernel.fatal "Formal %a not tied to a varinfo"
              Cil_printer.pp_varinfo v;
        end else Cil.SkipChildren
    end
  in
  let treat_one_formal v =
    v.vtype <- Cil.visitCilType vis v.vtype;
    Hashtbl.add table v.vname v;
  in
  List.iter treat_one_formal formals
