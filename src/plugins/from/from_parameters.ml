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

include Plugin.Register
    (struct
      let name = "from analysis"
      let shortname = "from"
      let help = "functional dependencies"
    end)

module ForceDeps =
  WithOutput
    (struct
      let option_name = "-deps"
      let help = "force dependencies display"
      let output_by_default = true
    end)

module ForceCallDeps =
  WithOutput
    (struct
      let option_name = "-calldeps"
      let help = "force callsite-wise dependencies"
      let output_by_default = true
    end)

module GraphDeps =
  False
    (struct
      let option_name = "-graphdeps"
      let help = "compute callwise dependencies from the Value result graph"
    end)

module WholeProgramDeps =
  WithOutput
    (struct
      let option_name = "-wholeprogramdeps"
      let help = "Compute whole program dependencies. Incompatible with memexec."
      let output_by_default = true
    end)

module ShowIndirectDeps =
  False
    (struct
      let option_name = "-show-indirect-deps"
      let help = "experimental"
    end)

module VerifyAssigns =
  False
    (struct
      let option_name = "-from-verify-assigns"
      let help = "verification of assigns/from clauses for functions with \
                  bodies. Implies -calldeps"
    end)
let () =
  VerifyAssigns.add_set_hook
    (fun _ new_ ->
       if new_ then ForceCallDeps.set true)

module PathDeps =
  WithOutput
    (struct
      let option_name = "-experimental-path-deps"
      let help = "experimental"
      let output_by_default = true
    end)

module MemDeps =
  WithOutput
    (struct
      let option_name = "-experimental-mem-deps"
      let help = "experimental"
      let output_by_default = true
    end)

module DepsShowProgress =
  True
    (struct
      let option_name = "-deps-show-progress"
      let help = "Show progression messages during analysis"
    end)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
