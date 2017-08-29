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
(* --- Letification of Goals                                              --- *)
(* -------------------------------------------------------------------------- *)

open Qed.Logic

let vmem x a = Lang.F.Vars.mem x (Lang.F.vars a)
let occurs xs a = Lang.F.Vars.intersect xs (Lang.F.vars a)

(* -------------------------------------------------------------------------- *)
(* --- Generalized Substitution                                           --- *)
(* -------------------------------------------------------------------------- *)

module Sigma :
sig
  type t
  val equal : t -> t -> bool
  val pretty : string -> Format.formatter -> t -> unit

  val empty : t
  val add : Lang.F.var -> Lang.F.term -> t -> t
  val mem : Lang.F.var -> t -> bool
  val find : Lang.F.var -> t -> Lang.F.term
  val e_apply : t -> Lang.F.term -> Lang.F.term
  val p_apply : t -> Lang.F.pred -> Lang.F.pred

  val assume : t -> Lang.F.pred -> t

  val iter : (Lang.F.var -> Lang.F.term -> unit) -> t -> unit
  val class_of : t -> Lang.F.var -> Lang.F.var list
  val domain : t -> Lang.F.Vars.t
  val codomain : t -> Lang.F.Vars.t
end =
struct

  module Ceq = Qed.Partition.Make(Lang.F.Var)

  type t = {
    dvar : Lang.F.Vars.t ; (* Domain of def *)
    dcod : Lang.F.Vars.t ; (* Codomain of def *)
    dall : Lang.F.Vars.t ; (* Domain of cst and def *)
    def : Lang.F.term Lang.F.Vmap.t ; (* Definitions *)
    ceq : Ceq.t ; (* Variable Classes *)
    cst : Lang.F.term Lang.F.Tmap.t ; (* Constants *)
    mutable mem : Lang.F.term Lang.F.Tmap.t array ; (* Memoization *)
  }

  let empty = {
    dcod = Lang.F.Vars.empty ;
    dvar = Lang.F.Vars.empty ;
    dall = Lang.F.Vars.empty ;
    ceq = Ceq.empty ;
    def = Lang.F.Vmap.empty ;
    cst = Lang.F.Tmap.empty ;
    mem = Array.make 5 Lang.F.Tmap.empty ;
  }

  let equal s1 s2 =
    Lang.F.Vmap.equal Lang.F.equal s1.def s2.def &&
    Lang.F.Tmap.equal Lang.F.equal s1.cst s2.cst

  let mem x sigma = Lang.F.Vmap.mem x sigma.def
  let find x sigma = Lang.F.Vmap.find x sigma.def
  let iter f sigma = Lang.F.Vmap.iter f sigma.def

  let rec m_apply sigma n (e:Lang.F.term) =
    match Lang.F.repr e with
    | Fvar x ->
      begin
        try Lang.F.Vmap.find x sigma.def
        with Not_found -> e
      end
    | _ ->
      let ys = Lang.F.vars e in
      if not (Lang.F.Vars.is_empty ys || Lang.F.Vars.intersect ys sigma.dall)
      then e (* no subst *)
      else if n < 5 then
        begin
          (* memoization *)
          try Lang.F.Tmap.find e sigma.mem.(n)
          with Not_found ->
            let r =
              try
                if n > 0 then raise Not_found ;
                Lang.F.Tmap.find e sigma.cst
              with Not_found ->
                Lang.F.f_map (m_apply sigma) n e
            in
            sigma.mem.(n) <- Lang.F.Tmap.add e r sigma.mem.(n) ; r
        end
      else Lang.F.f_map (m_apply sigma) n e

  let e_apply sigma e = m_apply sigma 0 e
  let p_apply sigma p = Lang.F.p_bool (e_apply sigma (Lang.F.e_prop p))

  (* Returns true if [x:=a] applied to [y:=b] raises a circularity *)
  let occur_check sigma x a =
    try
      if vmem x a then raise Exit ;
      Lang.F.Vmap.iter
        (fun y b -> if vmem x b && vmem y a then raise Exit)
        sigma.def ;
      false
    with Exit -> true

  let add_ceq x e ceq =
    match Lang.F.repr e with
    | Fvar y -> Ceq.join x y ceq
    | _ -> ceq

  let single x e =
    let sx = Lang.F.Vars.singleton x in
    {
      dvar = sx ; dall = sx ; dcod = Lang.F.vars e ;
      def = Lang.F.Vmap.add x e Lang.F.Vmap.empty ;
      ceq = add_ceq x e Ceq.empty ;
      cst = Lang.F.Tmap.empty ;
      mem = [| Lang.F.Tmap.empty |] ;
    }

  let add x e sigma =
    let e = e_apply sigma e in
    if Lang.F.Vmap.mem x sigma.def then sigma
    else
    if occur_check sigma x e then sigma
    else
      let sx = single x e in
      let def =
        Lang.F.Vmap.add x e
          (Lang.F.Vmap.map (fun _ d -> e_apply sx d) sigma.def)
      in
      let cst0 = Lang.F.Tmap.filter (fun e _c -> not (vmem x e)) sigma.cst in
      let cst1 = Lang.F.Tmap.fold
          (fun e c cst ->
             if vmem x e then Lang.F.Tmap.add (e_apply sx e) c cst else cst)
          cst0 sigma.cst in
      let cache = Array.make (Array.length sigma.mem) Lang.F.Tmap.empty in
      cache.(0) <- cst1 ;
      {
        mem = cache ;
        cst = cst1 ;
        def = def ;
        ceq = add_ceq x e sigma.ceq ;
        dvar = Lang.F.Vars.add x sigma.dvar ;
        dall = Lang.F.Vars.add x sigma.dall ;
        dcod = Lang.F.Vars.union (Lang.F.vars e) sigma.dcod ;
      }

  let domain sigma = sigma.dvar
  let codomain sigma = sigma.dcod
  let class_of sigma x = Ceq.members sigma.ceq x

  (* --- Constants --- *)

  (* c must be closed *)
  let add_cst e c sigma =
    try
      let c0 = Lang.F.Tmap.find e sigma.cst in
      if compare c c0 < 0 then raise Not_found else sigma
    with Not_found ->
      let cst = Lang.F.Tmap.add e c sigma.cst in
      let all = Lang.F.Vars.union (Lang.F.vars e) sigma.dall in
      let cache = Array.make (Array.length sigma.mem) Lang.F.Tmap.empty in
      cache.(0) <- cst ;
      {
        mem = cache ;
        cst = cst ;
        dall = all ;
        dvar = sigma.dvar ;
        dcod = sigma.dcod ;
        def = sigma.def ;
        ceq = sigma.ceq ;
      }

  let mem_lit l sigma =
    try Lang.F.Tmap.find l sigma.mem.(0) == Lang.F.e_true
    with Not_found -> false

  let add_lit l sigma =
    add_cst l Lang.F.e_true (add_cst (Lang.F.e_not l) Lang.F.e_false sigma)


  (** look for the shape:
          \forall x:integer. (csta <= x /\ x <= cstb) => t1=t2
      and return [Some(csta,cstb)]

          < on integer are always normalized to <=
  *)
  let extract_forall_equality fb =
    begin match Lang.F.repr (Lang.F.lc_repr fb) with
      | Imply ([la;lb],c) ->
        begin match Lang.F.repr c with
          | Eq _ ->
            let order = 0 in (** todo get the order from term *)
            begin match Lang.F.repr la, Lang.F.repr lb with
              | Leq(a,b), Leq(c,d) ->
                begin
                  match
                    Lang.F.repr a, Lang.F.repr b, Lang.F.repr c, Lang.F.repr d
                  with
                  | Bvar(o1,Int), Kint cstb, Kint csta, Bvar(o2,Int) when
                      o1 = order && o2 = order -> Some(csta,cstb)
                  | Kint csta, Bvar(o1,Int), Bvar(o2,Int), Kint cstb when
                      o1 = order && o2 = order -> Some(csta,cstb)
                  | _ -> None
                end
              | _ -> None
            end
          | _ -> None
        end
      | _ -> None
    end

  let is_kint e = match Lang.F.repr e with Qed.Logic.Kint _ -> true | _ -> false

  let rec add_pred sigma p = match Lang.F.repr p with
    | And ps -> List.fold_left add_pred sigma ps
    | Eq(a,b) ->
      begin
        match Lang.F.repr a , Lang.F.repr b with
        | Fvar x , _ when not (Lang.F.occurs x b) -> add x b sigma
        | _ , Fvar x when not (Lang.F.occurs x a) -> add x a sigma
        | _ ->
          match Lang.F.is_closed a , Lang.F.is_closed b with
          | true , false -> add_cst b a sigma
          | false , true -> add_cst a b sigma
          | _ -> add_lit p sigma
      end
    | Leq(a,b) ->
      if mem_lit (Lang.F.e_leq b a) sigma
      then add_pred sigma (Lang.F.e_eq a b)
      else add_lit p sigma
    | Lt(a,b) ->
      let sigma =
        if is_kint b then
          add_pred sigma (Lang.F.e_leq a (Lang.F.e_add b Lang.F.e_one))
        else sigma
      in
      let sigma =
        if is_kint a then
          add_pred sigma (Lang.F.e_leq (Lang.F.e_sub a Lang.F.e_one) b)
        else sigma
      in
      add_lit p (add_lit (Lang.F.e_leq a b) (add_lit (Lang.F.e_neq a b) sigma))
    | Neq _ | Fun _ | Not _ -> add_lit p sigma
    | Bind (Forall,Int,fb) ->
      let bound = Integer.of_int (Wp_parameters.BoundForallUnfolding.get ()) in
      begin match extract_forall_equality fb with
        | Some (csta,cstb) when
            Integer.le csta cstb &&
            Integer.le (Integer.sub cstb csta) bound ->
          let rec aux sigma i =
            if Integer.lt cstb i then sigma
            else begin
              let eq = Lang.F.lc_open_term (Lang.F.e_zint i) fb in
              (** qed should be able to simplify it directly *)
              let sigma = add_pred sigma eq in
              aux sigma (Integer.succ i)
            end
          in
          aux sigma csta
        | _ -> sigma
      end
    | _ -> sigma

  let assume sigma p = add_pred sigma (Lang.F.e_prop p)

  (* --- Pretty --- *)

  module Xmap = FCMap.Make(Lang.F.Var)

  let pretty title fmt sigma =
    let def = Lang.F.Vmap.fold Xmap.add sigma.def Xmap.empty in
    begin
      Format.fprintf fmt "@[<hv 0>@[<hv 2>%s {" title ;
      Format.fprintf fmt "@ @[vars: %a;@]" Lang.F.pp_vars sigma.dall ;
      Xmap.iter
        (fun x e ->
           Format.fprintf fmt "@ @[%a := %a ;@]"
             Lang.F.pp_term (Lang.F.e_var x) Lang.F.pp_term e
        ) def ;
      Array.iteri
        (fun i w ->
           Lang.F.Tmap.iter
             (fun e m ->
                Format.fprintf fmt "@ C%d: @[%a := %a ;@]" i
                  Lang.F.pp_term e Lang.F.pp_term m
             ) w
        ) sigma.mem ;
      Format.fprintf fmt "@ @]}@]" ;
    end

end

(* -------------------------------------------------------------------------- *)
(* --- Definition Extractions                                             --- *)
(* -------------------------------------------------------------------------- *)

module Defs =
struct
  type t = Lang.F.Tset.t Lang.F.Vmap.t

  let empty = Lang.F.Vmap.empty
  let merge = Lang.F.Vmap.union (fun _ -> Lang.F.Tset.union)

  let add_def (w : t ref) x e =
    let es = try Lang.F.Vmap.find x !w with Not_found -> Lang.F.Tset.empty in
    w := Lang.F.Vmap.add x (Lang.F.Tset.add e es) !w

  let rec diff s y = function
    | [] -> s
    | e::es ->
      match Lang.F.repr e with
      | Fvar x when x==y -> diff s y es
      | _ -> diff (Lang.F.e_opp e :: s) y es

  let add_linear w x pos neg =
    add_def w x (Lang.F.e_sum (diff pos x neg))

  let terms e = match Lang.F.repr e with Add es -> es | _ -> [e]
  let rec atoms = function
    | [] -> []
    | e::es ->
      match Lang.F.repr e with
      | Fvar x -> x :: atoms es
      | _ -> atoms es

  let rec defs w p =
    match Lang.F.repr p with
    | And ps -> List.iter (defs w) ps
    | Eq(a,b) ->
      begin
        match Lang.F.congruence_eq a b with
        | None -> defs_eq w a b
        | Some eqs -> List.iter (fun (a,b) -> defs_eq w a b) eqs
      end
    | Not p ->
      begin
        match Lang.F.repr p with
        | Fvar x -> add_def w x Lang.F.e_false
        | _ -> ()
      end
    | Fvar x -> add_def w x Lang.F.e_true
    | _ -> ()

  and defs_affine w a b =
    let ta = terms a in
    let tb = terms b in
    let xa = atoms ta in
    let yb = atoms tb in
    begin
      List.iter (fun x -> add_linear w x tb ta) xa ;
      List.iter (fun y -> add_linear w y ta tb) yb ;
    end

  and defs_eq w a b =
    match Lang.F.repr a , Lang.F.repr b with
    | Add _ , _ | _ , Add _ -> defs_affine w a b
    | Fvar x , Fvar y -> add_def w x b ; add_def w y a
    | Fvar x , _ -> add_def w x b
    | _ , Fvar y -> add_def w y a
    | _ -> ()

  let extract p =
    let w = ref empty in
    defs w (Lang.F.e_prop p) ; !w

  let add w p = defs w (Lang.F.e_prop p)

  let domain d =
    Lang.F.Vmap.fold (fun x _ xs -> Lang.F.Vars.add x xs) d Lang.F.Vars.empty

end

(* -------------------------------------------------------------------------- *)
(* --- Substitution Extraction                                            --- *)
(* -------------------------------------------------------------------------- *)

module XS = FCSet.Make(Lang.F.Var)

let elements xs = Lang.F.Vars.fold XS.add xs XS.empty
let iter f xs = XS.iter f (elements xs)

let rec extract defs sref cycle x =
  if not (Lang.F.Vars.mem x cycle) && not (Sigma.mem x !sref) then
    try
      let cycle = Lang.F.Vars.add x cycle in
      let ds = Lang.F.Vmap.find x defs in (* if no defs, exit early *)
      let ys = ref [] in (* variables equal to x *)
      let es = ref [] in (* possible definitions *)
      let rs = ref [] in (* sigma definitions *)
      Lang.F.Tset.iter
        (fun e ->
           if not (occurs cycle e) then
             match Lang.F.repr e with
             | Fvar y ->
               begin
                 try let d = Sigma.find y !sref in rs := d :: !rs
                 with Not_found -> ys := y :: !ys
               end
             | _ -> es := e :: !es
        ) ds ;
      (* Now choose the represent of x and the dependencies *)
      let select d = sref := Sigma.add x d !sref ; d , Lang.F.vars d in
      let ceq , depends =
        match List.sort Lang.F.compare !rs with
        | r :: _ -> select r
        | [] -> match List.sort Lang.F.compare !es with
          | e :: _ -> select e
          | [] -> Lang.F.e_var x , Lang.F.Vars.empty
      in
      List.iter (fun y -> sref := Sigma.add y ceq !sref) !ys ;
      iter (extract defs sref cycle) depends
    with Not_found -> ()

let bind sigma defs xs =
  let sref = ref sigma in
  iter (extract defs sref Lang.F.Vars.empty) xs ;
  !sref

let get_class sigma xs x =
  List.sort Lang.F.Var.compare
    (List.filter (fun y -> Lang.F.Vars.mem y xs) (Sigma.class_of sigma x))

let rec add_eq ps y = function
  | z::zs ->
    add_eq (Lang.F.p_equal (Lang.F.e_var y) (Lang.F.e_var z) :: ps) y zs
  | [] -> ps

let add_equals ys ps =
  match ys with [] -> ps | y::ys -> add_eq ps y ys

let add_definitions sigma defs xs ps =
  let xs = Lang.F.Vars.filter (fun x -> Lang.F.Vmap.mem x defs) xs in
  Lang.F.Vars.fold
    (fun x ps ->
       let ps = add_equals (get_class sigma xs x) ps in
       try Lang.F.p_equal (Lang.F.e_var x) (Sigma.find x sigma) :: ps
       with Not_found -> ps
    ) xs ps

(* -------------------------------------------------------------------------- *)
(* --- Split-Cases                                                        --- *)
(* -------------------------------------------------------------------------- *)

module Split =
struct

  type occur = int Lang.F.Tmap.t ref

  let create () = ref Lang.F.Tmap.empty

  let literal m p =
    try
      let n = Lang.F.Tmap.find p !m in
      m := Lang.F.Tmap.add p (succ n) !m
    with Not_found ->
      m := Lang.F.Tmap.add p 1 !m

  let rec occur m p =
    match Lang.F.repr p with
    | And ps | Or ps -> List.iter (occur m) ps
    | Imply(hs,p) -> List.iter (occur m) (p::hs)
    | Not p -> occur m p
    | If(p,a,b) -> occur m p ; occur m a ; occur m b
    | Eq(a,b) when Lang.F.is_closed a || Lang.F.is_closed b -> literal m p
    | Neq(a,b) when Lang.F.is_closed a || Lang.F.is_closed b ->
      literal m (Lang.F.e_not p)
    | Fun _ | Leq _ -> literal m p
    | Lt _ -> literal m (Lang.F.e_not p)
    | _ -> ()

  let add m p = occur m (Lang.F.e_prop p)

  let select m =
    let compare (c1,n1) (c2,n2) =
      (* most often first *)
      if n1 < n2 then 1 else
      if n1 > n2 then (-1) else
        Lang.F.comparep c1 c2
    in
    List.sort
      compare
      (Lang.F.Tmap.fold (fun c n s -> (Lang.F.p_bool c,n)::s) !m [])

end

(* -------------------------------------------------------------------------- *)
