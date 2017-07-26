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

let memory_footprint_var_name = "FRAMA_C_MEMORY_FOOTPRINT"

let default_value = 2

let memory_footprint =
  try
    let i = int_of_string (Sys.getenv memory_footprint_var_name) in
    if i land (lnot 15) <> 0
    then failwith memory_footprint_var_name
    else i
  with
  | Not_found -> default_value
  | Failure _ ->
    Cmdline.Kernel_log.error
      "@[Bad value@ for environment variable@ %s.@ Expected: \
       integer@ between 0 and 15.@ Using@ default of %d.@]"
      memory_footprint_var_name
      default_value;
    default_value


let cache_size = 1 lsl (8 + memory_footprint)

let cleared_marker = Obj.repr memory_footprint_var_name

(** The caches of this module have a "cleared" marker.

    - Caches must be cleared as soon as some states change, in order to remain
    coherent (for example, when the current project changes). When setting
    multiple command-line options, the caches may be cleared after each option.
    When caches are big, this becomes very time-consuming.
*)

module type Cacheable =
sig
  type t
  val hash : t -> int
  val sentinel : t
  val equal : t -> t -> bool
end

module type Result =
sig
  type t
  val sentinel : t
end

type elt1 = A | B of int

module Array_2 =
struct
  type ('a, 'b) t

  let marker_index = 1

  let really_clear (t : elt1 array) a b =
    let size2 = Obj.size (Obj.repr t) in
    let i = ref 0 in
    let a : elt1 = Obj.magic a in
    let b : elt1 = Obj.magic b in
    while (!i < size2)
    do
      let base = !i in
      t.(base)   <- a;
      t.(base+1) <- b;
      i := base + 2;
    done;
    t.(marker_index) <- Obj.obj cleared_marker

  let get_marker t =
    let t : elt1 array = Obj.magic t in
    Obj.repr t.(marker_index)

  let clear t a b =
    let t : elt1 array = Obj.magic t in
    if get_marker t != cleared_marker
    then really_clear t a b

  let (make : int -> 'a -> 'b -> ('a, 'b) t)
    = fun size a b ->
      let size2 = 2 * size in
      let t = Obj.obj (Obj.new_block 0 size2) in
      really_clear t a b;
      t

  let (set : ('a, 'b) t -> int -> 'a -> 'b -> unit)
    = fun t i a b ->
      let t : elt1 array = Obj.magic t in
      if get_marker t == cleared_marker
      then t.(marker_index) <- t.(2*marker_index+1);
      let base = 2 * i in
      let a : elt1 = Obj.magic a in
      let b : elt1 = Obj.magic b in
      let t : elt1 array = Obj.magic t in
      t.(base)   <- a;
      t.(base+1) <- b

  let (get0 :
         ('a, 'b) t -> int -> 'a)
    = fun t i ->
      let t : elt1 array = Obj.magic t in
      let base = 2 * i in
      Obj.magic t.(base)

  let (get1 : ('a, 'b) t -> int -> 'b)
    = fun t i ->
      let t : elt1 array = Obj.magic t in
      let base = 2 * i in
      Obj.magic t.(base+1)
end

module Array_3 =
struct
  type ('a, 'b, 'c) t

  let marker_index = 2

  let really_clear t a b c =
    let size3 = Obj.size t in
    let i = ref 0 in
    let a : elt1 = Obj.magic a in
    let b : elt1 = Obj.magic b in
    let c : elt1 = Obj.magic c in
    let t : elt1 array = Obj.magic t in
    while (!i < size3)
    do
      let base = !i in
      t.(base)   <- a;
      t.(base+1) <- b;
      t.(base+2) <- c;
      i := base + 3;
    done;
    t.(marker_index) <- Obj.obj cleared_marker

  let get_marker t =
    let t : elt1 array = Obj.magic t in
    Obj.repr t.(marker_index)

  let (clear : ('a, 'b, 'c) t ->
       'a -> 'b -> 'c -> unit)
    = fun t a b c ->
      let t = Obj.repr t in
      if get_marker t != cleared_marker
      then really_clear t a b c

  let (make : int -> 'a -> 'b -> 'c -> ('a, 'b, 'c) t)
    = fun size a b c ->
      let size3 = 3 * size in
      let t  = Obj.obj (Obj.new_block 0 size3) in
      really_clear t a b c;
      t

  let (set : ('a, 'b, 'c) t -> int -> 'a -> 'b -> 'c -> unit)
    = fun t i a b c ->
      let a : elt1 = Obj.magic a in
      let b : elt1 = Obj.magic b in
      let c : elt1 = Obj.magic c in
      let t : elt1 array = Obj.magic t in
      if get_marker t == cleared_marker
      then t.(marker_index) <- t.(2*marker_index+1);
      let base = 3 * i in
      t.(base)   <- a;
      t.(base+1) <- b;
      t.(base+2) <- c

  let (get2 : ('a, 'b, 'c) t -> int -> 'c)
    = fun t i ->
      let t : elt1 array = Obj.magic t in
      let base = 3 * i in
      Obj.magic t.(base+2)

  let (check2 : ('a, 'b, 'c) t -> int ->
       ('a -> 'a -> bool) -> 'a -> ('b -> 'b -> bool) -> 'b -> bool)
    = fun t i eq0 a0 eq1 a1 ->
      let t : elt1 array = Obj.magic t in
      let base = 3 * i in
      eq0 (Obj.magic t.(base)) a0
      && eq1 (Obj.magic t.(base+1)) a1
end

module Symmetric_Binary (H: Cacheable) (R: Result) =
struct
  let size = cache_size
  let cache = Array_3.make size H.sentinel H.sentinel R.sentinel

  let mask = pred size

  let clear () =
    Array_3.clear cache H.sentinel H.sentinel R.sentinel

  let hash = H.hash

  let merge f a0 a1 =
    let a0', a1', h0, h1 =
      let h0 = hash a0 in
      let h1 = hash a1 in
      if h0 < h1
      then a0, a1, h0, h1
      else a1, a0, h1, h0
    in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in
    if Array_3.check2 cache has H.equal a0 H.equal a1
    then begin
      (*      Format.printf "Cache O@.";  *)
      Array_3.get2 cache has
    end
    else
      let result = f a0 a1 in
      (*      Format.printf "Cache N@."; *)
      Array_3.set cache has a0' a1' result;
      result
end

module Arity_One (H: Cacheable) (R: Result) =
struct
  let size = cache_size
  let cache = Array_2.make size H.sentinel R.sentinel

  let mask = pred size

  let clear () =
    Array_2.clear cache H.sentinel R.sentinel

  let merge f a0 =
    let h0 = H.hash a0 in
    let has = h0 land mask in
    if H.equal (Array_2.get0 cache has) a0
    then begin
      (*      Format.printf "Cache O@.";  *)
      Array_2.get1 cache has
    end
    else
      let result = f a0 in
      (*      Format.printf "Cache N@."; *)
      Array_2.set cache has a0 result;
      result
end

module Arity_Two (H0: Cacheable) (H1: Cacheable) (R: Result) =
struct

  let size = cache_size
  let cache = Array_3.make size H0.sentinel H1.sentinel R.sentinel
  let mask = pred size

  let clear () =
    Array_3.clear cache H0.sentinel H1.sentinel R.sentinel

  let merge f a0 a1 =
    let h0 = H0.hash a0 in
    let h1 = H1.hash a1 in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in
    if Array_3.check2 cache has H0.equal a0 H1.equal a1
    then begin
      (*      Format.printf "Cache O@.";  *)
      Array_3.get2 cache has
    end
    else
      let result = f a0 a1 in
      (*      Format.printf "Cache N@."; *)
      Array_3.set cache has a0 a1 result;
      result
end

module Array_Bit =
struct
  let make size =
    let size = (size + 7) lsr 3 in
    String.make size (char_of_int 0)

  let get s i =
    let c = i lsr 3 in
    let b = 1 lsl (i land 7) in
    (Char.code s.[c]) land b <> 0

  let set s i v =
    let c = i lsr 3 in
    let b = 1 lsl (i land 7) in
    let oldcontents = Char.code s.[c] in
    let newcontents =
      if v
      then b lor oldcontents
      else
        let mask = lnot b in
        oldcontents land mask
    in
    Bytes.set s c (Char.chr newcontents)
end

module Binary_Predicate (H0: Cacheable) (H1: Cacheable) =
struct
  let size = cache_size
  let cache = Array_2.make size H0.sentinel H1.sentinel
  let result = Array_Bit.make size
  let mask = pred size

  let clear () =
    Array_2.clear cache H0.sentinel H1.sentinel

  let merge f a0 a1 =
    let has =
      let h0 = H0.hash a0 in
      let h1 = H1.hash a1 in
      599 * h0 + h1
    in
    let has = has land mask in
    if H0.equal (Array_2.get0 cache has) a0
    && H1.equal (Array_2.get1 cache has) a1
    then begin
      (*      Format.printf "Cache O@.";  *)
      Array_Bit.get result has
    end
    else
      let r = f a0 a1 in
      (*      Format.printf "Cache N@."; *)
      Array_2.set cache has a0 a1;
      Array_Bit.set result has r;
      r
end

module Symmetric_Binary_Predicate (H0: Cacheable) =
struct
  let size = cache_size
  let cache = Array_2.make size H0.sentinel H0.sentinel
  let result = Array_Bit.make size
  let mask = pred size

  let clear () =
    Array_2.clear cache H0.sentinel H0.sentinel

  let hash = H0.hash

  let merge f a0 a1 =
    let a0, a1, h0, h1 =
      let h0 = hash a0 in
      let h1 = hash a1 in
      if h0 < h1
      then a0, a1, h0, h1
      else a1, a0, h1, h0
    in
    let has = h1 lsl 5 - h1 + h0
    in
    let has = has land mask in
    if H0.equal (Array_2.get0 cache has) a0
    && H0.equal (Array_2.get1 cache has) a1
    then begin
      (*      Format.printf "Cache O@.";  *)
      Array_Bit.get result has
    end
    else
      let r = f a0 a1 in
      (*      Format.printf "Cache N@."; *)
      Array_2.set cache has a0 a1;
      Array_Bit.set result has r;
      r
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
