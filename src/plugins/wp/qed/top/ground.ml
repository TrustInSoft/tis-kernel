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

(* -------------------------------------------------------------------------- *)
(* --- Ground Terms for Toplevel                                          --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Logic

module Id(A : sig type t val id : t -> string end) : Symbol with type t = A.t =
struct

  type t = A.t
  let id = A.id
  let hash a = Hashtbl.hash (A.id a)
  let equal a b = A.id a = A.id b
  let compare a b = String.compare (A.id a) (A.id b)
  let pretty fmt a = Format.pp_print_string fmt (A.id a)

end

module Gadt =
struct
  include Id(struct type t = string let id x = x end)
  let basename a = String.sub a 0 1
end

module Gfield =
struct
  type fd = string * sort
  include Id(struct type t = fd let id = fst end)
  let sort = snd
  let field (_,f) s = (f,s)
  let lookup (_,f) (g,_) = f = g
end

module Gfun =
struct
  type f = {
    name : string ;
    category : category ;
    signature : (Gfield.t,Gadt.t) funtype ;
  }
  include Id (struct type t = f let id a = a.name end)
  let category f = f.category
  let sort f = Kind.of_tau f.signature.result
end

module Gtau =
struct
  type t = {
    name : string ;
    degree : int ;
    definition : (Gfield.t,Gadt.t) datatype option ;
  }
end

module Idmap = Map.Make(String)

module Term = Term.Make(Gadt)(Gfield)(Gfun)
module CC = Compiler.Make(Term)
module PP = Pretty.Make(Term)

module TermPP =
struct
  include Term
  let pretty = PP.pp_term PP.closed
end

include Term

let pp_tau = PP.pp_tau
let pp_term = PP.pp_term PP.closed

(** Global lookup functions *)

let datatype typ =
  let rec tvars k n = if k <= n then Tvar k :: tvars (succ k) n else [] in
  Data(typ.Gtau.name,tvars 1 typ.Gtau.degree)

type env = {
  types : Gtau.t Idmap.t ;
  symbols : CC.symbol Idmap.t ;
  pretty : PP.env ;
}

let lookup_typedef env (loc,x) =
  try datatype (Idmap.find x env.types)
  with Not_found -> Input.error_at loc "Unknown type '%s'" x

let lookup_datatype env a =
  try (Idmap.find a env.types).Gtau.definition
  with Not_found -> None

let lookup_symbol env (loc,x) =
  try Idmap.find x env.symbols
  with Not_found -> Input.error_at loc "Unknown symbol '%s'" x

let lookup env = {
  CC.make_field = Gfield.field ;
  CC.lookup_field = Gfield.lookup ;
  CC.lookup_datatype = lookup_datatype env ;
  CC.lookup_typedef = lookup_typedef env ;
  CC.lookup_symbol = lookup_symbol env ;
}

let cc_tau env = CC.cc_tau (lookup env)
let cc_sig env = CC.cc_sig (lookup env)
let cc_def env = CC.cc_def (lookup env)
let cc_exp env = CC.cc_exp (lookup env)

let add_datatype (_,x) xs env =
  let tdef = {
    Gtau.name = x ;
    Gtau.degree = List.length xs ;
    Gtau.definition=None ;
  } in
  { env with types = Idmap.add x tdef env.types }

let add_typedef (_,x) t env =
  let tdef = {
    Gtau.name = x ;
    Gtau.degree = Kind.degree_of_tau t ;
    Gtau.definition = Some t ;
  } in
  { env with types = Idmap.add x tdef env.types }

let add_symbol (_,x) c s env =
  let fdef = {
    Gfun.name = x ;
    Gfun.category = c ;
    Gfun.signature = s ;
  } in
  { env with symbols = Idmap.add x (CC.Fun(s,fdef)) env.symbols }

let add_value x t v env =
  {
    env with
    symbols = Idmap.add x (CC.Val(t,v)) env.symbols ;
    pretty = PP.bind x v env.pretty ;
  }

let stdlib = { types = Idmap.empty ; symbols = Idmap.empty ; pretty = PP.closed }

module S = Simplifier.Make(TermPP)
module C = Scomp.Make(TermPP)
module R = Srange.Make(TermPP)

let simplify e = S.simplify (S.make [R.make()]) e

let pp_env env t = PP.pp_term env.pretty t
