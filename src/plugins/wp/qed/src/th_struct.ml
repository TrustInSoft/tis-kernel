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
(* --- Structures Theory (Arrays and Records)                             --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Simplifier

module Make(T : Term) =
struct

  open T

  let is_struct e = match T.repr e with Rdef _ | Aset _ -> true | _ -> false

  (* [e] should be a Rdef or Aset *)
  let add k e domain =
    let es = try Tmap.find k domain with Not_found -> Tset.empty in
    Tmap.add k (Tset.add e es) domain

  let merge eqs domain =
    let d = ref Tmap.empty in
    Tmap.iter
      (fun _ es ->
         Tset.iter
           (fun e ->
              let k = eqs#class_of e in
              let s = T.e_map eqs#class_of e in
              if is_struct s then d := add k s !d
           ) es
      ) domain ; !d

  let array_get eqs k s es =
    match T.repr s with
    | Aset(a0,k0,v0) ->
        begin
          match eqs#equal k k0 with
          | Yes -> v0 :: es
          | No -> T.e_get a0 k :: es
          | Maybe -> es
        end
    | _ -> es

  let record_get f s es =
    match T.repr s with
    | Rdef _ -> T.e_getfield s f :: es
    | _ -> es

  let compute domain (eqs:term system) e =
    try
      match T.repr e with
      | Aget(a,k) -> Tset.fold (array_get eqs k) (Tmap.find a domain) []
      | Rget(r,f) -> Tset.fold (record_get f) (Tmap.find r domain) []
      | _ -> []
    with Not_found -> []

  class struct_theory structures =
    object(_ : 'a)
      constraint 'a = term #theory

      val mutable domain : Tset.t Tmap.t = structures ;

      method copy = new struct_theory domain
      method merge eqs = domain <- merge eqs domain
      method assume _ _ = ()
      method compute = compute domain

    end

  let theory () = new struct_theory Tmap.empty

end
