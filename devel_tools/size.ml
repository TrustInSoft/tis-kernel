(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: size.ml,v 1.1 2007-11-28 12:52:04 uid568 Exp $ i*)

(*s Pointers already visited are stored in a hash-table, where
    comparisons are done using physical equality. *)

external address_of_value: 'a -> int = "address_of_value"

module H = Hashtbl.Make(
  struct
    type t = Obj.t
    let equal = (==)
    let hash = address_of_value
  end)

let size_of_double = Obj.size (Obj.repr 1.0)

module Multi(Arg : sig type t end) : sig
  val measure : Arg.t list -> (Arg.t list * int) list
end = struct

  type a = Arg.t

  (* Ensure [roots] contains distinct blocks. *)
  let valid (roots : a list) =
    let table = H.create 1 in
    let check root =
      let root = Obj.repr root in
      if not (Obj.is_block root) then raise Exit;
      if H.mem table root then raise Exit;
      H.add table root ()
    in
    try List.iter check roots; true with Exit -> false

  (* Shared ordered lists representing sets. *)
  module L = struct
    type t = {
      data : a list;
      mutable next : (a * t) list; (* [cons] results *)
      mutable size : int;
    }
    let nil = { data = []; next = []; size = 0; }
    let cons x s =
      (* Invariant: [cons] is called in [roots] order.
         This is why we only need to match the last result. *)
      match s.next with
      | (y, ys) :: _ when x == y -> ys
      | _ ->
        let xs = { data = x :: s.data; next = []; size = 0; } in
        s.next <- (x, xs) :: s.next;
        xs
  end

  (* Traverse [root] using [first] to decide visited nodes. *)
  let traverse first root =
    let push, pop =
      let stack = Stack.create () in
      let push x = Stack.push x stack in
      let pop () = Stack.pop stack in
      push, pop
    in
    let push_fields x =
      if Obj.tag x < Obj.no_scan_tag then
        for i = Obj.size x - 1 downto 0 do
          let y = Obj.field x i in
          if Obj.is_block y then push y
        done
    in
    push (Obj.repr root);
    try
      while true do
        let x = pop () in
        if first x then push_fields x
      done
    with Stack.Empty -> ()

  let measure (roots : a list) =
    (* Force a minor and disable compaction. *)
    Gc.minor ();
    let old_gc = Gc.get () in
    Gc.set { old_gc with Gc.max_overhead = 1000000; };
    (* Compute result (before restoring compaction). *)
    let result =
      if valid roots then
        (* Phase 1: Visit all roots in order. *)
        let visited = H.create 257 in
        let first root = fun x ->
          let old = try H.find visited x with Not_found -> L.nil in
          match old.L.data with
          | a :: _ when a == root -> false
          | _ -> H.replace visited x (L.cons root old); true
        in
        List.iter (fun root -> traverse (first root) root) roots;
        (* Phase 2: Compute block sizes. *)
        let add x set =
          let n = Obj.size x in
          let t = Obj.tag x in
          let s =
            match () with
            | () when t < Obj.no_scan_tag -> 1 + n
            | () when t = Obj.string_tag -> 1 + n
            | () when t = Obj.double_tag -> size_of_double
            | () when t = Obj.double_array_tag -> 1 + size_of_double * n
            | () -> 1
          in
          set.L.size <- set.L.size + s
        in
        H.iter add visited;
        (* Phase 3: Format the result as an association list. *)
        let rec dump acc set =
          let acc =
            if set.L.size = 0 then acc else (set.L.data, set.L.size) :: acc
          in
          List.fold_left (fun acc (_, set) -> dump acc set) acc set.L.next
        in
        dump [] L.nil
      else []
    in
    (* Restore compaction. *)
    Gc.set old_gc;
    result

end

let measure (type a) (roots : a list) =
  let module M = Multi(struct type t = a end) in
  M.measure roots

let _test () =
  let rec permutation = function
    | [] -> [[]]
    | x :: xs ->
      let xss = permutation xs in
      xss @ List.map (fun xs -> x :: xs) xss
  in
  let rec number n r = function
    | [] -> r
    | x :: xs -> number (n + 1) ((n, x) :: r) xs
  in
  let test pp input expected =
    let result = measure input in
    let actual = List.map (fun (x, s) -> List.map pp x, s) result in
    assert (actual = expected)
  in
  let rec make n r = if n < 0 then r else make (n - 1) (n :: r) in
  let m n = make n [] in
  let start = Unix.gettimeofday () in
  let a, b, c = m 1000, m 1000, m 1000 in
  let same = 1, a in
  (* Errors *)
  test fst [] [];
  test (fun x -> x) [1; 2; 3] [];
  test fst [2, b; same; 3, c; same] [];
  (* Successes *)
  test fst [1, a] [([1], 3006)];
  test fst [1, a; 2, a] [([2; 1], 3003); ([1], 3); ([2], 3)];
  test fst [1, a; 2, 0 :: a] [([2; 1], 3003); ([1], 3); ([2], 6)];
  test fst [1, a; 2, b] [([1], 3006); ([2], 3006)];
  test fst [1, [a]; 2, [b]] [([1], 3009); ([2], 3009)];
  test fst [0, []; 1, [a]; 2, [b]; 3, [a; a]]
    [([0], 3); ([3; 1], 3003); ([1], 6); ([2], 3009); ([3], 9)];
  test fst [0, []; 1, [a]; 2, [b]; 3, [a; b]]
    [([0], 3); ([3; 1], 3003); ([1], 6); ([3; 2], 3003); ([2], 6); ([3], 9)];
  test fst [1, a; 2, b; 3, c]
    [([1], 3006); ([2], 3006); ([3], 3006)];
  test fst [0, []; 1, [a]; 2, [b]; 3, [c]; 4, [a; b]]
    [([0], 3); ([4; 1], 3003); ([1], 6); ([4; 2], 3003); ([2], 6); ([3], 3009); ([4], 9)];
  test fst (number 0 [] (permutation [a; b; c]))
    [ ([4; 5; 6; 7], 3003); ([2; 3; 6; 7], 3003); ([1; 3; 5; 7], 3006);
      ([3; 7], 3); ([7], 6); ([2; 6], 3); ([6], 6); ([5], 6); ([4], 6);
      ([3], 3); ([2], 3); ([1], 3); ([0], 3) ];
  Format.printf "All tests passed in %f seconds.@."
    (Unix.gettimeofday () -. start)
;;

(*i*)
open Obj
(*i*)

let node_table = (H.create 257 : unit H.t)

let in_table o = try H.find node_table o; true with Not_found -> false

let add_in_table o = H.add node_table o ()

let reset_table () = H.clear node_table

(*s Objects are traversed recursively, as soon as their tags are less than
    [no_scan_tag]. [count] records the numbers of words already visited. *)

let count = ref 0

let rec traverse t =
  if not (in_table t) then begin
    add_in_table t;
    if is_block t then begin
      let n = size t in
      let tag = tag t in
      if tag < no_scan_tag then begin
	count := !count + 1 + n;
	for i = 0 to n - 1 do
      	  let f = field t i in
	  if is_block f then traverse f
	done
      end else if tag = string_tag then
	count := !count + 1 + n
      else if tag = double_tag then
	count := !count + size_of_double
      else if tag = double_array_tag then
	count := !count + 1 + size_of_double * n
      else
	incr count
    end
  end

(*s Sizes of objects in words and in bytes. The size in bytes is computed
    system-independently according to [Sys.word_size]. *)

let res () =
  let r = !count in
  reset_table ();
  r

let size_w ?except o =
  reset_table ();
  (match except with
     | None -> ()
     | Some except -> traverse (repr except)
  );
  count := 0;
  traverse (repr o);
  res ()


let size_b ?except o = (size_w ?except o) * (Sys.word_size / 8)

let size_kb ?except o = (size_w ?except o) / (8192 / Sys.word_size)
