(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2017 TrustInSoft                                      *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

val should_memorize_function: Cil_types.kernel_function -> bool
val no_memoization_enabled: unit -> bool
