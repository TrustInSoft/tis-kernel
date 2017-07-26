(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

(** Option -sa *)
module StrictAliasing : Parameter_sig.Bool

(** Option -sa-strict-union *)
module StrictUnion : Parameter_sig.Bool

(** Option -sa-strict-enum *)
module StrictEnum : Parameter_sig.Bool

(** Option -sa-strict-struct *)
module StrictStruct : Parameter_sig.Bool

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
