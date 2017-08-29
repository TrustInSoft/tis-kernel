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

(** Cabs2cil some cast-related helpers. *)

(*
  DEPENDS ON:
  + Utility.isTransparentUnion
  + Utility.Parameters.typeForInsertedCast
*)

(*
  ENVIRONMENT: None.
*)

(*
  EXPORTS: Nothing.
*)

open Cil_types
open Cil

let isTransparentUnion = Cabs2cil_Utility.isTransparentUnion
let typeForInsertedCast = Cabs2cil_Utility.Parameters.typeForInsertedCast

let category_cast = Kernel.register_category "cabs2cil:cast"

(* true if the expression is known to be a boolean result, i.e. 0 or 1. *)
let rec is_boolean_result e =
  match e.enode with
  | Const _ ->
    (match Cil.isInteger e with
     | Some i ->
       Integer.equal i Integer.zero || Integer.equal i Integer.one
     | None -> false)
  | CastE (_,e) -> is_boolean_result e
  | BinOp((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr),_,_,_) -> true
  | BinOp((PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP | Mult
          | Div | Mod | Shiftlt | Shiftrt | BAnd | BXor | BOr),_,_,_) -> false
  | UnOp(LNot,_,_) -> true
  | UnOp ((Neg | BNot),_,_) -> false
  | Lval _ | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _
  | AlignOfE _ | AddrOf _ | StartOf _ | Info _ -> false

(* Specify whether the cast is from the source code *)
let rec castTo ?(fromsource=false)
    (ot : typ) (nt : typ) (e : exp) : (typ * exp ) =
  Kernel.debug ~dkey:category_cast "@[%t: castTo:%s %a->%a@\n@]"
    Cil.pp_thisloc (if fromsource then "(source)" else "")
    Cil_printer.pp_typ ot Cil_printer.pp_typ nt;
  let ot' = unrollType ot in
  let nt' = unrollType nt in
  if not fromsource && not (need_cast ot' nt') then begin
    (* Do not put the cast if it is not necessary, unless it is from the
     * source. *)
    Kernel.debug ~dkey:category_cast "no cast to perform";
    (ot, e)
  end else begin
    let nt' = if fromsource then nt' else !typeForInsertedCast e ot' nt' in
    let result =
      (nt',
       if theMachine.insertImplicitCasts || fromsource
       then Cil.mkCastT ~force:true ~e ~oldt:ot ~newt:nt'
       else e)
    in
    let error s =
      (if fromsource then Kernel.abort else Kernel.fatal) ~current:true s
    in
    (*  [BM] uncomment the following line to enable attributes static typing
        ignore (check_strict_attributes true ot nt &&
                check_strict_attributes false nt ot);*)
    Kernel.debug ~dkey:category_cast
      "@[castTo: ot=%a nt=%a\n  result is %a@\n@]"
      Cil_printer.pp_typ ot Cil_printer.pp_typ nt'
      Cil_printer.pp_exp (snd result);
    (* Now see if we can have a cast here *)
    match ot', nt' with
    | TNamed _, _
    | _, TNamed _ -> Kernel.fatal ~current:true "unrollType failed in castTo"
    | _, TInt (IBool, _) ->
      if is_boolean_result e then result
      else
        nt,
        Cil.mkCastT
          ?force:None
          ~e:(mkBinOp ~loc:e.eloc Ne e (Cil.zero ~loc:e.eloc))
          ~oldt:ot
          ~newt:nt'

    | TInt(_,_), TInt(_,_) ->
      (* We used to ignore attributes on integer-integer casts. Not anymore *)
      (* if ikindo = ikindn then (nt, e) else *)
      result
    | TPtr (_, _), TPtr(_, _) -> result

    | TInt _, TPtr _ -> result

    | TPtr _, TInt _ -> result

    | TArray _, TPtr _ -> result

    | TArray(t1,_,_,_), TArray(t2,None,_,_)
      when Cil_datatype.Typ.equal t1 t2 -> (nt', e)

    | TPtr _, TArray(_,_,_,_) ->
      error "Cast over a non-scalar type %a" Cil_printer.pp_typ nt';

    | TEnum _, TInt _ -> result
    | TFloat _, (TInt _|TEnum _) -> result
    | (TInt _|TEnum _), TFloat _ -> result
    | TFloat _, TFloat _ -> result
    | TInt (ik,_), TEnum (ei,_) ->
      begin match e.enode with
        | Const (CEnum { eihost = ei'; _ })
          when ei.ename = ei'.ename &&
               not fromsource &&
               Cil.bytesSizeOfInt ik = Cil.bytesSizeOfInt ei'.ekind ->
          nt', e
        | _ -> result
      end
    | TEnum _, TEnum _ -> result

    | TEnum _, TPtr _ -> result
    | TBuiltin_va_list _, (TInt _ | TPtr _) ->
      result

    | TBuiltin_va_list _, TBuiltin_va_list _ ->
      result

    | _, TBuiltin_va_list _ ->
      Kernel.abort ~current:true
        "Casting %a to __builtin_va_list"
        Cil_printer.pp_typ ot

    | TBuiltin_va_list _, _ ->
      Kernel.abort ~current:true
        "Casting __builtin_va_list to %a"
        Cil_printer.pp_typ ot

    | TPtr _, TEnum _ ->
      Kernel.debug ~dkey:category_cast
        "Casting a pointer into an enumeration type" ;
      result

    (* The expression is evaluated for its effects *)
    | _ , TVoid _ ->
      Kernel.debug ~level:3
        "Casting a value into void: expr is evaluated for side effects";
      result

    (* Even casts between structs are allowed when we are only
     * modifying some attributes *)
    | TComp (comp1, _, _), TComp (comp2, _, _) when comp1.ckey = comp2.ckey ->
      result

    (** If we try to pass a transparent union value to a function
     * expecting a transparent union argument, the argument type would
     * have been changed to the type of the first argument, and we'll
     * see a cast from a union to the type of the first argument. Turn
     * that into a field access *)
    | TComp(_, _, _), _ -> begin
        match isTransparentUnion ot with
        | None ->
          Kernel.abort ~current:true
            "casting '%a' to incompatible type '%a'"
            Cil_printer.pp_typ ot Cil_printer.pp_typ nt'
        | Some fstfield -> begin
            (* We do it now only if the expression is an lval *)
            let e' =
              match e.enode with
              | Lval lv ->
                new_exp ~loc:e.eloc
                  (Lval (addOffsetLval (Field(fstfield, NoOffset)) lv))
              | _ ->
                Kernel.fatal ~current:true
                  "castTo: transparent union expression is not an lval: %a\n"
                  Cil_printer.pp_exp e
            in
            (* Continue casting *)
            castTo ~fromsource:fromsource fstfield.ftype nt' e'
          end
      end
    | _ ->
      Kernel.abort ~current:true
        "cannot cast from %a to %a@\n"
        Cil_printer.pp_typ ot Cil_printer.pp_typ nt'
  end

(* Like Cil.mkCastT, but it calls typeForInsertedCast *)
let makeCastT ~(e: exp) ~(oldt: typ) ~(newt: typ) =
  if need_cast oldt newt then
    Cil.mkCastT ?force:None ~e ~oldt ~newt:(!typeForInsertedCast e oldt newt)
  else e

let makeCast ~(e: exp) ~(newt: typ) =
  makeCastT ~e ~oldt:(typeOf e) ~newt

(* A cast that is used for conditional expressions. Pointers are Ok.
   Abort if invalid *)
let checkBool (ot : typ) (_ : exp) =
  match unrollType ot with
  | TInt _
  | TPtr _
  | TEnum _
  | TFloat _ -> ()
  |  _ -> Kernel.fatal ~current:true "castToBool %a" Cil_printer.pp_typ ot

let default_argument_promotion idx exp =
  let name = "x_" ^ string_of_int idx in
  let arg_type = Cil.typeOf exp in
  let typ =
    match Cil.unrollType arg_type with
    | TVoid _ -> voidType
    | TInt(k,_) when Cil.rank k < Cil.rank IInt ->
      if intTypeIncluded k IInt then intType
      else (* This may happen when char or short have the same size as int *)
        uintType
    | TInt(k,_) -> TInt(k,[])
    | TFloat(FFloat,_) -> doubleType
    | TFloat(k,_) -> TFloat(k,[])
    | TPtr(t,_) | TArray(t,_,_,_) -> TPtr(t,[])
    | (TFun _) as t -> TPtr(t,[])
    | TComp(ci,_,_) -> TComp(ci,{ scache = Not_Computed },[])
    | TEnum(ei,_) -> TEnum(ei,[])
    | TBuiltin_va_list _ -> TBuiltin_va_list []
    | TNamed _ -> assert false (* unrollType *)
  in
  (* if we make a promotion, take it explicitly
     into account in the argument itself *)
  let (_,e) = castTo arg_type typ exp in
  (name,typ,[]), e

(* Promote variadic arguments with standard argument promotions.*)
let promote_variadic_arguments (chunk,args) =
  let args =
    Extlib.mapi
      (fun i arg -> snd (default_argument_promotion i arg))
      args
  in
  (chunk,args)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
