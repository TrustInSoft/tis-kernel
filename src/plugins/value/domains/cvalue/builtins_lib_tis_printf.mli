(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** builtins for printing. *)

(** Nothing is exported, all the builtins are registered through
    {Builtins.register_builtin} *)

(** Set the formatters to use to dump the content of printing function.
    If a formatter is not given, the builtin default way to print the result
    is used. *)
val set_output_formatter: ?stderr:Format.formatter -> ?stdout:Format.formatter
  -> unit -> unit


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
