(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(* Builtin for printing allocated variables. *)

open Cvalue


let tis_show_allocated state args =
  match args with
  | [] ->
    ( match state with
      | Model.Bottom | Model.Top -> assert false
      | Model.Map m ->
        Value_parameters.result ~current:true ~once:false
          "remaining allocated variables:@\n  @[%a@]"
          (fun fmt ->
             let first = ref true in
             Model.iter
               (fun base _ ->
                  match base with
                  | Base.Allocated (v,_) ->
                    if !first
                    then first := false
                    else Format.fprintf fmt ",@ ";
                    Format.fprintf fmt "%a" Printer.pp_varinfo v;
                  | _ -> ()))
          m);
    { Value_types.c_values = [ Value_types.StateOnly(None, state) ];
      c_clobbered = Base.SetLattice.bottom;
      c_cacheable = Value_types.Cacheable;
      c_sureouts = None;
      c_from = None; (* TODO?*)
    }
  | _ ->
    Value_parameters.error ~current:true
      "tis_show_allocated() expects void%t"
      Value_util.pp_callstack;
    raise Db.Value.Aborted



let () =
  Builtins.register_builtin "tis_show_allocated" tis_show_allocated;


(*
  Local Variables:
  compile-command: "make -C ../../../../.."
  End:
*)
