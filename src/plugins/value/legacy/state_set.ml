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

(* We store model -> trace information as a simple association
   list. This suffices because we do not need the find operation. *)
type t = Cvalue.Model.t list

let fold = List.fold_left

let of_list l = l

let iter = List.iter
let map = List.map

let empty = []

let is_empty t = t = empty

let exists = List.exists

let length = List.length

exception Unchanged
let pretty fmt s =
  List.iter
    (fun state ->
       Format.fprintf fmt "set contains %a@\n"
         Cvalue.Model.pretty state)
    s

let add_to_list v s =
  if not (Cvalue.Model.is_reachable v) then raise Unchanged;
  let remain =
    ( match s with
      | [ _ ] | [ _; _ ] | [ _; _; _ ] | [ _; _; _; _ ] ->
        if List.exists
            (Cvalue.Model.is_included v)
            s
        then raise Unchanged;
        List.filter
          (fun e -> not (Cvalue.Model.is_included e v))
          s
      | _ -> s)
  in
  v :: remain

let add_exn p s = add_to_list p s

let merge_into sa ~into:sb =
  let unchanged = ref true in
  let f acc e =
    try
      let r = add_exn  e acc in
      unchanged := false;
      r
    with Unchanged ->
      acc
  in
  let result = List.fold_left f sb sa in
  if !unchanged then raise Unchanged;
  result

let merge sa sb =
  try merge_into sa ~into:sb
  with Unchanged -> sb
;;

let add p s =
  try
    add_exn p s
  with Unchanged -> s

let singleton p = add p empty ;;

let join s =
  List.fold_left
    Cvalue.Model.join
    Cvalue.Model.bottom
    s

(* Computes a greatest lower bound of two disjoint unions of states.
   This computation is not as precise as computing each pairwise
   narrow between states, but it avoids a quadratic increase in the number of
   computations. *)
let narrow st1 st2 =
  let us1 = join st1 in
  let us2 = join st2 in
  let unmerged =
    List.map (fun s1 -> Cvalue.Model.narrow s1 us1) st2 @
    List.map (fun s1 -> Cvalue.Model.narrow s1 us2) st1
  in
  (* Remove eventual duplicates *)
  List.fold_right add unmerged []

(* Computes [narrow] with all the state sets in [stl].
   [stl] must not be empty.
   Note: defining this function inside State_set avoids list boxing/unboxing. *)
let narrow_list stl =
  let s = List.map join stl in
  let snarrow =
    List.fold_left
      Cvalue.Model.narrow
      Cvalue.Model.top
      s
  in
  List.fold_left
    (fun acc st ->
       let narrowed_st =
         List.map
           (fun s -> Cvalue.Model.narrow s snarrow)
           st
       in
       merge narrowed_st acc)
    []
    stl

let to_list l = l

let reorder l = List.rev l

;;

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
