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

(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Logic Path and Regions                                             --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic
open Lang
open Vset

type path = offset list
and offset =
  | Oindex of Lang.F.term
  | Ofield of field

let rec access e = function
  | [] -> e
  | Oindex k :: path -> access (Lang.F.e_get e k) path
  | Ofield f :: path -> access (Lang.F.e_getfield e f) path

let rec update e path v =
  match path with
  | [] -> v
  | Oindex k :: tail ->
    let e_k = update (Lang.F.e_get e k) tail v in
    Lang.F.e_set e k e_k
  | Ofield f :: tail ->
    let e_f = update (Lang.F.e_getfield e f) tail v in
    Lang.F.e_setfield e f e_f

(* -------------------------------------------------------------------------- *)
(* --- Region                                                             --- *)
(* -------------------------------------------------------------------------- *)

type rpath = roffset list
and roffset =
  | Rindex of set
  | Rfield of field

type region =
  | Empty
  | Full
  | Fields of (field * region) list (* SORTED, DEFAULT : empty *)
  | Indices of set * ( set * region ) list
  (* Indices for FULL region.
      Then indices for non-FULL and non-EMPTY regions *)

let empty = Empty
let full = Full

let rec path = function
  | [] -> Full
  | Oindex k :: tail ->
    let r = path tail in
    let s = Vset.singleton k in
    begin
      match r with (* never Empty *)
      | Full -> Indices(s,[])
      | _ -> Indices(Vset.empty,[s,r])
    end
  | Ofield f :: tail ->
    Fields [f,path tail]

let rec rpath = function
  | [] -> Full
  | Rindex s :: tail ->
    let r = rpath tail in
    begin
      match r with (* never Empty *)
      | Full -> Indices(s,[])
      | _ -> Indices(Vset.empty,[s,r])
    end
  | Rfield f :: tail ->
    Fields [f,rpath tail]

let rec merge a b =
  match a , b with
  | Full , _ | _ , Full -> Full
  | Empty , c | c , Empty -> c
  | Fields fxs , Fields gys -> Fields (merge_fields fxs gys)
  | Indices(s1,kxs) , Indices(s2,kys) ->
    Indices(Vset.union s1 s2,kxs @ kys)
  | Fields _ , Indices _
  | Indices _ , Fields _ -> assert false

and merge_fields fxs gys =
  match fxs , gys with
  | [] , w | w , [] -> w
  | (f,x)::fxstail , (g,y)::gystail ->
    let c = Field.compare f g in
    if c < 0 then (f,x)::merge_fields fxstail gys else
    if c > 0 then (g,y)::merge_fields fxs gystail else
      (f,merge x y) :: merge_fields fxstail gystail

(* -------------------------------------------------------------------------- *)
(* --- Disjonction                                                        --- *)
(* -------------------------------------------------------------------------- *)

let rec disjoint a b =
  match a , b with
  | Empty , _ | _ , Empty -> Lang.F.p_true
  | Full , _ | _ , Full -> Lang.F.p_false

  | Fields fxs , Fields gys ->
    Lang.F.p_conj (disjoint_fields fxs gys)

  | Indices(s,xs) , Indices(t,ts) ->
    Lang.F.p_conj (disjoint_indices [Vset.disjoint s t] xs ts)

  | Fields _ , Indices _
  | Indices _ , Fields _ -> assert false

and disjoint_fields frs grs =
  match frs , grs with
  | [] , _ | _ , [] -> []
  | (f,r)::ftail , (g,s)::gtail ->
    let c = Field.compare f g in
    if c < 0 then disjoint_fields ftail grs else
    if c > 0 then disjoint_fields frs gtail else
      disjoint r s :: disjoint_fields ftail gtail

and disjoint_indices w sr1 sr2 =
  List.fold_left
    (fun w (s1,r1) ->
       List.fold_left
         (fun w (s2,r2) ->
            (Lang.F.p_or (Vset.disjoint s1 s2) (disjoint r1 r2)) :: w
         ) w sr2
    ) w sr1

(* -------------------------------------------------------------------------- *)
(* --- Region Inclusion                                                   --- *)
(* -------------------------------------------------------------------------- *)

let rec subset r1 r2 =
  match r1 , r2 with
  | _ , Full -> Lang.F.p_true
  | Empty , _ -> Lang.F.p_true
  | _ , Empty -> Lang.F.p_false
  | Full , _ -> Lang.F.p_false
  | Fields frs , Fields grs -> subset_fields frs grs
  | Indices(s1,ks1) , Indices(s2,ks2) ->
    Lang.F.p_and
      (Vset.subset s1 s2) (* because FULL never appears in ks2 *)
      (Lang.F.p_all (fun (s1,r1) -> subset_indices s1 r1 ks2) ks1)
  | Fields _ , Indices _
  | Indices _ , Fields _ -> assert false

and subset_fields frs grs =
  match frs , grs with
  | [] , _ -> Lang.F.p_true
  | _ , [] -> Lang.F.p_false
  | (f,r)::ftail , (g,s)::gtail ->
    let c = Field.compare f g in
    if c < 0 then Lang.F.p_false (* only f is present *) else
    if c > 0 then subset_fields frs gtail (* g is not present *)
    else (* f=g *)
      Lang.F.p_and (subset r s) (subset_fields ftail gtail)

(* All path (k,p) in (s1,r1) are in ks2
   = AND (k in s1 -> p in r1 -> (k,p) in ks2
   = AND (k in s1 -> p in r1 -> (OR (k in s2 and p in r2) for (s2,r2) in r2)
   = AND (k in s1 -> OR (k in s2 and r1 in r2) for (s2,r2) in r2)
   = AND (k in s1 -> subset_index k r1 ks2)
*)
and subset_indices s1 r1 ks2 =
  Lang.F.p_all (fun w ->
      let xs,e,p = Vset.descr w in
      Lang.F.p_forall xs
        (Lang.F.p_imply p (subset_index e r1 ks2))
    ) s1

(* OR (k in s2 and r1 in r2) for (s2,r2) in r2) *)
and subset_index e r1 ks2 =
  Lang.F.p_any (fun (s2,r2) ->
      Lang.F.p_and (Vset.member e s2) (subset r1 r2)
    ) ks2

(* -------------------------------------------------------------------------- *)
(* --- Equality outside a Region                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec equal_but t r a b =
  match t , r with
  | _ , Full -> Lang.F.p_true
  | _ , Empty -> Lang.F.p_equal a b
  | _ , Fields grs ->
    let fs = List.sort Field.compare (fields_of_tau t) in
    Lang.F.p_conj (equal_but_fields a b fs grs)
  | Array(ta,tb) , Indices(s,krs) ->
    let x = freshvar ta in
    let k = Lang.F.e_var x in
    let a_k = Lang.F.e_get a k in
    let b_k = Lang.F.e_get b k in
    Lang.F.p_forall [x] (Lang.F.p_conj (equal_but_index tb k a_k b_k s krs))
  | _ -> assert false

and equal_but_fields a b fts grs =
  match fts , grs with
  | [] , _ -> []
  | _ , [] ->
    List.map
      (fun f -> Lang.F.p_equal (Lang.F.e_getfield a f) (Lang.F.e_getfield b f))
      fts
  | f::ftail , (g,r)::gtail ->
    let c = Field.compare f g in
    if c < 0 then
      let eqf =
        Lang.F.p_equal (Lang.F.e_getfield a f) (Lang.F.e_getfield b f)
      in
      eqf :: equal_but_fields a b ftail grs
    else
    if c > 0 then
      (* field g does not appear *)
      equal_but_fields a b fts gtail
    else
      let tf = tau_of_field f in
      let eqf =
        equal_but tf r (Lang.F.e_getfield a f) (Lang.F.e_getfield b f)
      in
      eqf :: equal_but_fields a b ftail gtail

and equal_but_index tb k a_k b_k s krs =
  List.map
    (fun (s,r) -> Lang.F.p_or (Vset.member k s) (equal_but tb r a_k b_k))
    ((s,Full)::krs)

(* -------------------------------------------------------------------------- *)
(* --- Utils                                                              --- *)
(* -------------------------------------------------------------------------- *)

let rec occurs x = function
  | Empty | Full -> false
  | Fields frs -> List.exists (fun (_,r) -> occurs x r) frs
  | Indices(s,srs) -> Vset.occurs x s || List.exists (occurs_idx x) srs

and occurs_idx x (s,r) = Vset.occurs x s || occurs x r

let rec vars = function
  | Empty | Full -> Lang.F.Vars.empty
  | Fields frs ->
    List.fold_left
      (fun xs (_,r) -> Lang.F.Vars.union xs (vars r))
      Lang.F.Vars.empty frs
  | Indices(s,srs) ->
    List.fold_left
      (fun xs (s,r) ->
         Lang.F.Vars.union xs (Lang.F.Vars.union (Vset.vars s) (vars r)))
      (Vset.vars s) srs

(* -------------------------------------------------------------------------- *)
(* --- Pretty                                                             --- *)
(* -------------------------------------------------------------------------- *)

let pretty fmt = function
  | Empty -> Format.fprintf fmt "empty"
  | Full -> Format.fprintf fmt "full"
  | Fields _ -> Format.fprintf fmt "fields" (*TODO*)
  | Indices _ -> Format.fprintf fmt "indices" (*TODO*)
