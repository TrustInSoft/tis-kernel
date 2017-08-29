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

open Qed
open Lang

(* -------------------------------------------------------------------------- *)
(* --- Logical Sets                                                       --- *)
(* -------------------------------------------------------------------------- *)

type set = vset list
and vset =
  | Set of tau * Lang.F.term
  | Singleton of Lang.F.term
  | Range of Lang.F.term option * Lang.F.term option
  | Descr of Lang.F.var list * Lang.F.term * Lang.F.pred

let occurs_opt x = function
  | None -> false
  | Some t -> Lang.F.occurs x t

let occurs_vset x = function
  | Set(_,t)
  | Singleton t -> Lang.F.occurs x t
  | Range(a,b) -> occurs_opt x a || occurs_opt x b
  | Descr(xs,t,p) ->
    if List.exists (Lang.F.Var.equal x) xs then false
    else (Lang.F.occurs x t || Lang.F.occursp x p)

let occurs x = List.exists (occurs_vset x)

let vars_opt = function None -> Lang.F.Vars.empty | Some e -> F.vars e

let vars_vset = function
  | Set(_,t) -> F.vars t
  | Singleton t -> F.vars t
  | Range(a,b) -> Lang.F.Vars.union (vars_opt a) (vars_opt b)
  | Descr(xs,t,p) ->
    List.fold_left
      (fun xs x -> Lang.F.Vars.remove x xs)
      (Lang.F.Vars.union (F.vars t) (F.varsp p)) xs

let vars vset = List.fold_left
    (fun xs s -> Lang.F.Vars.union xs (vars_vset s))
    Lang.F.Vars.empty vset

(* -------------------------------------------------------------------------- *)
(* --- Pretty                                                             --- *)
(* -------------------------------------------------------------------------- *)

let pp_bound fmt = function
  | None -> ()
  | Some e -> F.pp_term fmt e

let pp_vset fmt = function
  | Set(_,t) -> F.pp_term fmt t
  | Singleton x -> Format.fprintf fmt "@[<hov 2>{ %a }@]" F.pp_term x
  | Range(None,None) -> Format.pp_print_string fmt "[..]"
  | Range(a,b) -> Format.fprintf fmt "@[<hov 2>[%a@,..%a]@]" pp_bound a pp_bound b
  | Descr _ -> Format.fprintf fmt "{ <comprehension> }"

let pretty fmt = function
  | [] -> Format.pp_print_string fmt "{}"
  | [v] -> pp_vset fmt v
  | v::vs ->
    Format.fprintf fmt "@[<hov 2>(%a" pp_vset v ;
    List.iter (fun v -> Format.fprintf fmt "@ + %a" pp_vset v) vs ;
    Format.fprintf fmt ")@]"

(* -------------------------------------------------------------------------- *)
(* --- Set Operations                                                     --- *)
(* -------------------------------------------------------------------------- *)

let library = "vset"

let adt_set = Lang.datatype ~library "set"
let tau_of_set te = Logic.Data( adt_set , [te] )
let p_member = Lang.extern_p ~library ~bool:"member_bool" ~prop:"member" ()
let f_empty = Lang.extern_f ~library "empty"
let f_union = Lang.extern_f ~library "union"
let f_inter = Lang.extern_f ~library "inter"
let f_range = Lang.extern_f ~library "range"
let f_range_sup = Lang.extern_f ~library "range_sup"
let f_range_inf = Lang.extern_f ~library "range_inf"
let f_range_all = Lang.extern_f ~library "range_all"
let f_singleton = Lang.extern_f ~library "singleton"

let single a b = match a,b with
  | Some x , Some y when F.equal x y -> a
  | _ -> None

let test_range x y a b =
  let p_inf = match a with Some a -> Lang.F.p_leq a x | None -> Lang.F.p_true in
  let p_sup = match b with Some b -> Lang.F.p_leq y b | None -> Lang.F.p_true in
  Lang.F.p_and p_inf p_sup

let sub_range x y a b =
  match single a b with
  | Some z -> Lang.F.p_and (Lang.F.p_equal x z) (Lang.F.p_equal y z)
  | None -> test_range x y a b

let in_size x n =
  Lang.F.p_and (Lang.F.p_leq Lang.F.e_zero x) (Lang.F.p_lt x (Lang.F.e_int n))

let in_range x a b =
  match single a b with
  | Some y -> Lang.F.p_equal x y
  | None -> test_range x x a b

let ordered ~limit ~strict a b =
  match a , b with
  | Some x , Some y -> if strict then Lang.F.p_lt x y else Lang.F.p_leq x y
  | _ -> if limit then Lang.F.p_true else Lang.F.p_false

let member x xs = Lang.F.p_any
    (function
      | Set(_,s) -> Lang.F.p_call p_member [x;s]
      | Singleton e -> Lang.F.p_equal x e
      | Range(a,b) -> in_range x a b
      | Descr(xs,t,p) ->
        Lang.F.p_exists xs (Lang.F.p_and (Lang.F.p_equal x t) p)
    ) xs

let empty = []
let singleton x = [Singleton x]
let range a b = [Range(a,b)]

let union xs ys = (xs @ ys)

let descr = function
  | Set(t,s) ->
    let x = Lang.freshvar t in
    let e = Lang.F.e_var x in
    [x] , e , Lang.F.p_call p_member [e;s]
  | Singleton e -> ( [] , e , Lang.F.p_true )
  | Range(a,b) ->
    let x = Lang.freshvar ~basename:"k" Logic.Int in
    let e = Lang.F.e_var x in
    [x] , e , in_range e a b
  | Descr(xs,t,p) ->
    xs, t, p

(* -------------------------------------------------------------------------- *)
(* --- Concretize                                                         --- *)
(* -------------------------------------------------------------------------- *)

let concretize_vset = function
  | Set(_,s) -> s
  | Singleton e -> Lang.F.e_fun f_singleton [e]
  | Range(None,None) -> Lang.F.e_fun f_range_all []
  | Range(None,Some b) -> Lang.F.e_fun f_range_inf [b]
  | Range(Some a,None) -> Lang.F.e_fun f_range_sup [a]
  | Range(Some a,Some b) -> Lang.F.e_fun f_range [a;b]
  | Descr _ ->
    Warning.error "Concretization for comprehension sets not implemented yet"

let concretize = function
  | [] -> Lang.F.e_fun f_empty []
  | x::xs ->
    List.fold_left
      (fun w x -> Lang.F.e_fun f_union [w;concretize_vset x])
      (concretize_vset x) xs

let inter xs ys = Lang.F.e_fun f_inter [xs;ys]

(* -------------------------------------------------------------------------- *)
(* --- Inclusion                                                          --- *)
(* -------------------------------------------------------------------------- *)

let subrange a b = function
  | [Range(c,d)] ->
    Lang.F.p_and
      (match c,a with
       | None,_ -> Lang.F.p_true
       | Some _,None -> Lang.F.p_false
       | Some c,Some a -> Lang.F.p_leq c a)
      (match b,d with
       | _,None -> Lang.F.p_true
       | None,Some _ -> Lang.F.p_false
       | Some b,Some d -> Lang.F.p_leq b d)
  | ys ->
    let x = Lang.freshvar ~basename:"k" Logic.Int in
    let k = Lang.F.e_var x in
    Lang.F.p_forall [x] (Lang.F.p_imply (in_range k a b) (member k ys))

let subset xs ys =
  Lang.F.p_all (function
      | Set(t,s) ->
        let x = Lang.freshvar t in
        let e = Lang.F.e_var x in
        Lang.F.p_forall [x]
          (Lang.F.p_imply (Lang.F.p_call p_member [e;s]) (member e ys))
      | Singleton e -> member e ys
      | Descr(xs,t,p) ->
        Lang.F.p_forall xs (Lang.F.p_imply p (member t ys))
      | Range(a,b) ->
        subrange a b ys
    ) xs

(* -------------------------------------------------------------------------- *)
(* --- Equality                                                           --- *)
(* -------------------------------------------------------------------------- *)

let equal xs ys =
  Lang.F.p_and (subset xs ys) (subset ys xs)

(* -------------------------------------------------------------------------- *)
(* --- Separation                                                         --- *)
(* -------------------------------------------------------------------------- *)

let empty_range a b =
  match a,b with
  | None,_ | _,None -> Lang.F.p_false
  | Some x , Some y -> Lang.F.p_lt y x

let disjoint_bounds left right =
  match left , right with
  | None,_ | _,None -> Lang.F.p_false
  | Some x , Some y -> Lang.F.p_lt x y

let disjoint_vset x y =
  match x , y with

  | Singleton x , Singleton y ->
    Lang.F.p_neq x y

  | Singleton e , Range(a,b)
  | Range(a,b) , Singleton e ->
    Lang.F.p_not (in_range e a b)

  | Range(a,b) , Range(c,d) ->
    Lang.F.p_disj [
      empty_range a b ;
      empty_range c d ;
      disjoint_bounds b c ;
      disjoint_bounds d a ;
    ]

  | Singleton x , Descr(xs,t,p)
  | Descr(xs,t,p) , Singleton x ->
    Lang.F.p_forall xs (Lang.F.p_imply p (Lang.F.p_neq x t))

  | Range(a,b) , Descr(xs,t,p)
  | Descr(xs,t,p) , Range(a,b) ->
    Lang.F.p_forall xs (Lang.F.p_imply p (Lang.F.p_not (in_range t a b)))

  | Descr(xs,ta,pa) , Descr(ys,tb,pb) ->
    Lang.F.p_forall xs
      (Lang.F.p_forall ys
         (Lang.F.p_hyps [pa;pb] (Lang.F.p_neq ta tb)))

  | Singleton e , Set(_,s)
  | Set(_,s) , Singleton e ->
    Lang.F.p_not (Lang.F.p_call p_member [e;s])

  | Set _ , Set _ ->
    let xs,a,p = descr x in
    let ys,b,q = descr y in
    Lang.F.p_forall (xs @ ys) (Lang.F.p_hyps [p;q] (Lang.F.p_neq a b))

  | Set(_,s) , w | w , Set(_,s) ->
    let xs,t,p = descr w in
    let t_in_s = Lang.F.p_call p_member [t;s] in
    Lang.F.p_forall xs (Lang.F.p_not (Lang.F.p_and p t_in_s))

let disjoint xs ys =
  let ws =
    List.fold_left
      (fun w x ->
         List.fold_left
           (fun w y -> disjoint_vset x y :: w) w ys
      ) [] xs
  in Lang.F.p_conj ws

(* -------------------------------------------------------------------------- *)
(* --- Lifting & Maping                                                   --- *)
(* -------------------------------------------------------------------------- *)

let cartesian f xs ys =
  let zs =
    List.fold_left
      (fun w x ->
         List.fold_left (fun w y -> f x y :: w) w ys
      ) [] xs
  in List.rev zs

let map_vset f x = let xs,t,p = descr x in Descr(xs,f t,p)

let map f xs = List.map
    (function Singleton x -> Singleton (f x) | u -> map_vset f u) xs

let map_opt f = function None -> None | Some x -> Some (f x)

let map_opp xs = List.map
    (function
      | Singleton x -> Singleton (Lang.F.e_opp x)
      | Range(a,b) -> Range(map_opt Lang.F.e_opp b,map_opt Lang.F.e_opp a)
      | Descr(xs,t,p) -> Descr(xs,Lang.F.e_opp t,p)
      | (Set _) as w -> let xs,t,p = descr w in Descr(xs,Lang.F.e_opp t,p)
    ) xs

let lift_vset f x y =
  let xs,ta,pa = descr x in
  let ys,tb,pb = descr y in
  Descr (xs @ ys , f ta tb , Lang.F.p_and pa pb)

let lift f xs ys =
  cartesian
    (fun x y ->
       match x , y with
       | Singleton a , Singleton b -> Singleton (f a b)
       | _ -> lift_vset f x y
    ) xs ys

let bound_shift a k =
  match a with
  | None -> None
  | Some x -> Some (Lang.F.e_add x k)

let bound_add a b =
  match a,b with
  | None,_ | _,None -> None
  | Some x , Some y -> Some (Lang.F.e_add x y)

let bound_sub a b =
  match a,b with
  | None,_ | _,None -> None
  | Some x , Some y -> Some (Lang.F.e_sub x y)

let lift_add xs ys =
  cartesian
    (fun x y ->
       match x , y with
       | Singleton a , Singleton b -> Singleton(Lang.F.e_add a b)
       | Singleton u , Range(a,b) | Range(a,b) , Singleton u ->
         Range(map_opt (Lang.F.e_add u) a, map_opt (Lang.F.e_add u) b)
       | Range(a,b) , Range(c,d) ->
         Range(bound_add a c,bound_add b d)
       | _ -> lift_vset Lang.F.e_add x y
    ) xs ys

let lift_sub xs ys =
  cartesian
    (fun x y ->
       match x , y with
       | Singleton a , Singleton b -> Singleton(Lang.F.e_sub a b)
       | Singleton u , Range(a,b) ->
         Range(bound_sub (Some u) b , bound_sub (Some u) a)
       | Range(a,b) , Singleton u ->
         Range(bound_sub a (Some u) , bound_sub b (Some u))
       | Range(a,b) , Range(c,d) ->
         Range(bound_sub a d , bound_sub b c)
       | _ -> lift_vset Lang.F.e_sub x y
    ) xs ys

(* -------------------------------------------------------------------------- *)
