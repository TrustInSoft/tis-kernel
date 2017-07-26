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
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Cil_types

type call_site = kernel_function * kinstr

module Callsite = struct
  include Datatype.Pair_with_collections(Kernel_function)(Cil_datatype.Kinstr)
      (struct let module_name = "Value_callbacks.Callpoint" end)

  let pretty fmt (kf, ki) =
    Format.fprintf fmt "%a@@%t" Kernel_function.pretty kf
      (fun fmt ->
         match ki with
         | Kglobal -> Format.pp_print_string fmt "<main>"
         | Kstmt stmt -> Format.pp_print_int fmt stmt.sid
      )
end


type callstack = call_site list

module Callstack = Datatype.With_collections
    (Datatype.List(Callsite))
    (struct let module_name = "Value_callstack.Callstack" end)

module Shared_callstack : sig
  type t
  val empty : t
  val push : call_site -> t -> t
  val pop : t -> t
  val get : t -> callstack
  val put : callstack -> t
end = struct
  (* An abstract callstack is either:
     - [Nil] for the empty callstack, or
     - [Cons (tag, cs, t)] for a non-empty callstack, where
       - [tag] is the unique tag of this callstack,
       - [cs] is the associated concrete callstack,
       - [t] is the tail of this callstack.
     See the functions below for details about these fields.
  *)
  type t = Nil | Cons of int * callstack * t

  let empty = Nil

  (* [tag t] returns the unique tag of [t]. *)
  let tag = function | Nil -> 0 | Cons (x, _, _) -> x

  (* [get t] returns the concrete callstack represented by [t].
     It has the same length as [t].
     In particular, when [t] is [Cons], [get t] is [::].
  *)
  let get = function | Nil -> [] | Cons (_, x, _) -> x

  let pop = function | Nil -> assert false | Cons (_, _, x) -> x

  let rehash_ref = ref (fun _ -> assert false)

  module T = Datatype.Make(struct
      type tt = t
      type t = tt
      let name = "Value_callstack.Shared_callstack.T.t"
      let rehash x = !rehash_ref x
      let r = Structural_descr.Recursive.create ()
      let structural_descr =
        let open Structural_descr in
        t_sum [| [| p_int; Callstack.packed_descr; recursive_pack r; |] |]
      let () = Structural_descr.Recursive.update r structural_descr
      let reprs = [ Nil ]
      let equal = (==)
      let compare x y = compare (tag x) (tag y)
      let hash = tag
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let pretty fmt x = Callstack.pretty fmt (get x)
      let varname = Datatype.undefined
      let mem_project = Datatype.never_any_project
    end)

  module K = Datatype.Pair_with_collections(Callsite)(T)(struct
      let module_name = "Value_callstack.Shared_callstack.K"
    end)

  module H = State_builder.Hashtbl(K.Hashtbl)(T)(struct
      let name = "Value_callstack.Shared_callstack.H"
      let dependencies = [ Ast.self ]
      let size = 17
    end)
  let () = Ast.add_monotonic_state H.self

  let push =
    let tag = ref 0 in
    fun c t ->
      let k = c, t in
      try H.find k
      with Not_found ->
        incr tag;
        let v = Cons (!tag, c :: get t, t) in
        H.add k v;
        v

  let put cs =
    let rec aux t = function
      | [] -> t
      | c :: cs -> aux (push c t) cs
    in
    aux Nil (List.rev cs)

  let () =
    let rehash = function
      | Nil -> Nil
      | Cons (_, [], _) -> assert false
      | Cons (_, c :: _, t) -> push c t
    in
    rehash_ref := rehash
end
