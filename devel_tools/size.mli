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

(*i $Id: size.mli,v 1.1 2007-11-28 12:52:04 uid568 Exp $ i*)

(* Sizes of ocaml values (in their memory representation).
   Sizes are given in words ([size_w]), bytes ([size_b]) or kilobytes
   ([size_kb]), in a system-independent way. *)

val size_w : ?except:'a -> 'b -> int

val size_b : ?except:'a -> 'b -> int

val size_kb : ?except:'a -> 'b -> int

(* [measure l] returns for each set of elements in [l] the memory size
   shared by these elements and only these elements. Since there is an
   exponential number of sets, we only return the memory size of a set
   when its memory size is non-null.

   The empty list is returned in case of errors or when [l] is empty.
*)
val measure : 'a list -> ('a list * int) list
