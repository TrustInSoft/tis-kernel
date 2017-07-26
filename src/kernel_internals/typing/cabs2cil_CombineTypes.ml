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

(** Cabs2cil combining types and conditional conversion. *)

(*
  DEPENDS ON:
  + Utility.cabsAddAttributes
  + Utility.cabsTypeAddAttributes
*)

(*
  ENVIRONMENT:
  + isomorphicStructs
*)

(*
  EXPORTS:
  + compatibleTypes
  + compatibleTypesp
  + logicConditionalConversion
*)

open Cil_types
open Cil
open Cil_datatype

module H = Hashtbl
let cabsAddAttributes = Cabs2cil_Utility.cabsAddAttributes
let cabsTypeAddAttributes = Cabs2cil_Utility.cabsTypeAddAttributes

(* Combine the types. Raises the Failure exception with an error message.
   isdef says whether the new type is for a definition *)

type combineWhat =
    CombineFundef of bool
  (** The new definition is for a function definition. The old is for
      a prototype. (The argument is [true] for an old-style declaration. *)
  | CombineFunarg of bool
  (** Comparing a function argument type with an old prototype argument.
      (The argument is [true] for an old-style declaration, which triggers some
      ad'hoc treatment in GCC mode. *)
  | CombineFunret (** Comparing the return of a function with that from an old
                      prototype. *)
  | CombineOther

(* We sometimes want to succeed in combining two structure types that are
   identical except for the names of the structs. We keep a list of types
   that are known to be equal *)
let isomorphicStructs : (string * string, bool) H.t = H.create 15

let rec combineTypes (what: combineWhat) (oldt: typ) (t: typ) : typ =
  match oldt, t with
  | TVoid olda, TVoid a -> TVoid (cabsAddAttributes olda a)
  | TInt (oldik, olda), TInt (ik, a) ->
    let combineIK oldk k =
      if oldk = k then oldk else
        (match what with
         | CombineFunarg b when
             Cil.gccMode () && oldk = IInt
             && bytesSizeOf t <= (bytesSizeOfInt IInt) && b ->
           (* GCC allows a function definition to have a more precise integer
              type than a prototype that says "int" *)
           k
         | _ ->
           raise (Failure "different integer types"))
    in
    TInt (combineIK oldik ik, cabsAddAttributes olda a)
  | TFloat (oldfk, olda), TFloat (fk, a) ->
    let combineFK oldk k =
      if oldk = k then oldk else
        ( match what with
          | CombineFunarg b when
              Cil.gccMode () && oldk = FDouble && k = FFloat && b ->
            (* GCC allows a function definition to have a more precise float
               type than a prototype that says "double" *)
            k
          | _ ->
            raise (Failure "different floating point types"))
    in
    TFloat (combineFK oldfk fk, cabsAddAttributes olda a)
  | TEnum (_, olda), TEnum (ei, a) ->
    TEnum (ei, cabsAddAttributes olda a)

  | TEnum ({ ekind; _ } as enum, olda), TInt (ikind, a)
  | TInt (ikind, a), TEnum ({ ekind; _ } as enum, olda) when ekind = ikind ->
    TEnum (enum, cabsAddAttributes olda a)

  | TComp (oldci, _, olda) , TComp (ci, _, a) ->
    if oldci.cstruct <> ci.cstruct then
      raise (Failure "different struct/union types");
    let comb_a = cabsAddAttributes olda a in
    if oldci.cname = ci.cname then
      TComp (oldci, empty_size_cache (), comb_a)
    else
      (* Now maybe they are actually the same *)
    if H.mem isomorphicStructs (oldci.cname, ci.cname) then
      (* We know they are the same *)
      TComp (oldci, empty_size_cache (), comb_a)
    else begin
      (* If one has 0 fields (undefined) while the other has some fields
         we accept it *)
      let oldci_nrfields = List.length oldci.cfields in
      let ci_nrfields = List.length ci.cfields in
      if oldci_nrfields = 0 then
        TComp (ci, empty_size_cache (), comb_a)
      else if ci_nrfields = 0 then
        TComp (oldci, empty_size_cache (), comb_a)
      else begin
        (* Make sure that at least they have the same number of fields *)
        if  oldci_nrfields <> ci_nrfields then begin
(*
          ignore (E.log "different number of fields: %s had %d and %s had %d\n"
                    oldci.cname oldci_nrfields
                    ci.cname ci_nrfields);
*)
          raise (Failure "different structs(number of fields)");
        end;
        (* Assume they are the same *)
        H.add isomorphicStructs (oldci.cname, ci.cname) true;
        H.add isomorphicStructs (ci.cname, oldci.cname) true;
        (* Check that the fields are isomorphic and watch for Failure *)
        (try
           List.iter2 (fun oldf f ->
               if oldf.fbitfield <> f.fbitfield then
                 raise (Failure "different structs(bitfield info)");
               if oldf.fattr <> f.fattr then
                 raise (Failure "different structs(field attributes)");
               (* Make sure the types are compatible *)
               ignore (combineTypes CombineOther oldf.ftype f.ftype);
             ) oldci.cfields ci.cfields
         with Failure _ as e -> begin
             (* Our assumption was wrong. Forget the isomorphism *)
             Kernel.debug ~level:2
               "Failed in our assumption that %s and %s are isomorphic"
               oldci.cname ci.cname ;
             H.remove isomorphicStructs (oldci.cname, ci.cname);
             H.remove isomorphicStructs (ci.cname, oldci.cname);
             raise e
           end);
        (* We get here if we succeeded *)
        TComp (oldci, empty_size_cache (), comb_a)
      end
    end

  | TArray (oldbt, oldsz, _, olda), TArray (bt, sz, _, a) ->
    let newbt = combineTypes CombineOther oldbt bt in
    let newsz =
      match oldsz, sz with
      | None, Some _ -> sz
      | Some _, None -> oldsz
      | None, None -> sz
      | Some oldsz', Some sz' ->
        (* They are not structurally equal. But perhaps they are equal if
           we evaluate them. Check first machine independent comparison  *)
        let checkEqualSize (machdep: bool) =
          ExpStructEq.equal
            (constFold machdep oldsz')
            (constFold machdep sz')
        in
        if checkEqualSize false then
          oldsz
        else if checkEqualSize true then begin
          Kernel.warning ~current:true
            "Array type comparison succeeds only based on machine-dependent \
             constant evaluation: %a and %a\n"
            Cil_printer.pp_exp oldsz' Cil_printer.pp_exp sz' ;
          oldsz
        end else
          raise (Failure "different array lengths")

    in
    TArray (newbt, newsz, empty_size_cache (), cabsAddAttributes olda a)

  | TPtr (oldbt, olda), TPtr (bt, a) ->
    TPtr (combineTypes CombineOther oldbt bt, cabsAddAttributes olda a)

  | TFun (oldrt, oldargs, oldva, olda), TFun (rt, args, va, a) ->
    let rt_what =
      match what with
      | CombineFundef _ -> CombineFunret
      | _ -> CombineOther
    in
    let newrt = combineTypes rt_what oldrt rt in
    if oldva != va then
      raise (Failure "different vararg specifiers");
    (* If one does not have arguments, believe the one with the
       arguments *)
    let newargs, olda' =
      if oldargs = None then args, olda else
      if args = None then oldargs, olda else
        let oldargslist = argsToList oldargs in
        let argslist = argsToList args in
        if List.length oldargslist <> List.length argslist then
          raise (Failure "different number of arguments")
        else begin
          (* Construct a mapping between old and new argument names. *)
          let map = H.create 5 in
          List.iter2
            (fun (on, _, _) (an, _, _) -> H.replace map on an)
            oldargslist argslist;
          (* Go over the arguments and update the old ones with the
             adjusted types *)
          (* Format.printf "new type is %a@." Cil_printer.pp_typ t; *)
          let what =
            match what with
            | CombineFundef b -> CombineFunarg b
            | _ -> CombineOther
          in
          Some
            (List.map2
               (fun (on, ot, oa) (an, at, aa) ->
                  (* Update the names. Always prefer the new name. This is
                     very important if the prototype uses different names than
                     the function definition. *)
                  let n = if an <> "" then an else on in
                  let t = combineTypes what ot at in
                  let a = addAttributes oa aa in
                  (n, t, a))
               oldargslist argslist),
          olda
        end
    in
    (* Drop missingproto as soon as one of the type is a properly declared one*)
    let olda =
      if not (Cil.hasAttribute "missingproto" a) then
        Cil.dropAttribute "missingproto" olda'
      else olda'
    in
    let a =
      if not (Cil.hasAttribute "missingproto" olda') then
        Cil.dropAttribute "missingproto" a
      else a
    in
    TFun (newrt, newargs, oldva, cabsAddAttributes olda a)

  | TNamed (oldt, olda), TNamed (t, a) when oldt.tname = t.tname ->
    TNamed (oldt, cabsAddAttributes olda a)

  | TBuiltin_va_list olda, TBuiltin_va_list a ->
    TBuiltin_va_list (cabsAddAttributes olda a)

  (* Unroll first the new type *)
  | _, TNamed (t, a) ->
    let res = combineTypes what oldt t.ttype in
    cabsTypeAddAttributes a res

  (* And unroll the old type as well if necessary *)
  | TNamed (oldt, a), _ ->
    let res = combineTypes what oldt.ttype t in
    cabsTypeAddAttributes a res

  | _ -> raise (Failure "different type constructors")

let cleanup_isomorphicStructs () = H.clear isomorphicStructs

let clear_env = cleanup_isomorphicStructs

let compatibleTypes t1 t2 =
  try
    let r = combineTypes CombineOther t1 t2 in
    cleanup_isomorphicStructs ();
    r
  with Failure _ as e ->
    cleanup_isomorphicStructs ();
    raise e

let compatibleTypesp t1 t2 =
  try
    ignore (combineTypes CombineOther t1 t2);
    cleanup_isomorphicStructs ();
    true
  with Failure _ ->
    cleanup_isomorphicStructs ();
    false

let conditionalConversion (t2: typ) (t3: typ) : typ =
  let tresult =  (* ISO 6.5.15 *)
    match unrollType t2, unrollType t3 with
    | (TInt _ | TEnum _ | TFloat _), (TInt _ | TEnum _ | TFloat _) ->
      arithmeticConversion t2 t3
    | TComp (comp2,_,_), TComp (comp3,_,_)
      when comp2.ckey = comp3.ckey -> t2
    | TVoid _, TVoid _ -> t2
    | TPtr(_, _), TPtr(TVoid _, _) -> t2
    | TPtr(TVoid _, _), TPtr(_, _) -> t3
    | TPtr _, TPtr _ when Cil_datatype.Typ.equal t2 t3 -> t2
    | TPtr _, TInt _  -> t2 (* most likely comparison with 0 *)
    | TInt _, TPtr _ -> t3 (* most likely comparison with 0 *)

    (* When we compare two pointers of diffent type, we combine them
       using the same algorithm when combining multiple declarations of
       a global *)
    | (TPtr _) as t2', (TPtr _ as t3') -> begin
        try combineTypes CombineOther t2' t3'
        with Failure msg -> begin
            Kernel.warning ~current:true "A.QUESTION: %a does not match %a (%s)"
              Cil_printer.pp_typ (unrollType t2)
              Cil_printer.pp_typ (unrollType t3) msg;
            t2 (* Just pick one *)
          end
      end
    | _, _ ->
      Kernel.abort ~current:true "invalid implicit conversion from %a to %a"
        Cil_printer.pp_typ t2 Cil_printer.pp_typ t3
  in
  tresult

let logicConditionalConversion t1 t2 =
  match unrollType t1, unrollType t2 with
  | TPtr _ , TInt _ | TInt _, TPtr _ ->
    Kernel.abort ~current:true "invalid implicit conversion from %a to %a"
      Cil_printer.pp_typ t2 Cil_printer.pp_typ t1
  | _ -> conditionalConversion t1 t2
