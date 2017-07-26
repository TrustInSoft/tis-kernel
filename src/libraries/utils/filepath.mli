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

(** Functions manipulating filepaths. *)

(** returns an absolute path leading to the given file.
    @modified Magnesium-20151001+dev optional base. *)
val normalize: ?base:string -> string -> string

(** returns an absolute path or a (strict) relative path of file to base.
    The default base is working directory.
    @since Magnesium-20151001+dev *)
val relativize: ?base:string -> string -> string

(** returns true if the file is strictly relative to base.
    @since Magnesium-20151001+dev *)
val is_relative: ?base:string -> string -> bool

(** Normalize a filename: make it relative if it is "close" to the
    current working directory and results in a shorter path and replace
    known prefixes by symbolic names. Note that the result of this function
    does not necessarily represent a valid file name. Use
    {!Sysutil.absolutize_filename} if you want to obtain the absolute path
    of a given file.

    @since Neon-20140301
*)
val pretty: string -> string

(** [add_symbolic_dir name dir] indicates that the (absolute) path [dir] must
    be replaced by [name] in the normalized version. *)
val add_symbolic_dir: string -> string -> unit

(*
  Local Variables:
  compile-command: "make -C ../../.."
  End:
*)
