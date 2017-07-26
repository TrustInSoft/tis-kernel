(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
    (struct
      let name = "strict aliasing"
      let shortname = "sa"
      let help = "strict aliasing violation detection"
    end)

module StrictAliasing =
  False
    (struct
      let option_name = "-sa"
      let help = "Activate the analysis to detect strict aliasing violation."
    end)

module StrictUnion =
  True
    (struct
      let option_name = "-sa-strict-union"
      let help = "Access to an object marked by an union effective type has to \
                  be made through the exact same union type."
    end)

module StrictEnum =
  False
    (struct
      let option_name = "-sa-strict-enum"
      let help = "Consider enum types as enumeration, not as their integer \
                  representation. Access to an enum cell requires the same \
                  enum type and not the same integer representation."
    end)

module StrictStruct =
  False
    (struct
      let option_name = "-sa-strict-struct"
      let help = "Access to an object marked by a structure effective type \
                  has to be made throught the exact same structure type."
    end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
