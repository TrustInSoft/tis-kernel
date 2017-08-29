(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp
open Value_util

module Aux = Builtins_lib_tis_aux
let register_builtin = Aux.register_builtin

exception Not_same_base

(** This builtin checks if ptr is between start_addr and end_addr.
 *  RETURN VALUE:
 *   - TRUE (1) if ptr, start_addr and end_addr are pointers to the
 *  same object and start_addr <= ptr < end_addr
 *   - FALSE (0) otherwise
 *  *)
let tis_ptr_is_within state actuals =
  match actuals with
  | [ (_, ptr, _) ; (_, start_addr, _) ; (_, end_addr, _) ] -> (
      try
        if Value_parameters.ValShowProgress.get () then
          Value_parameters.feedback ~current:true
            "Call to builtin tis_ptr_is_within(%a)%t"
            pretty_actuals actuals Value_util.pp_callstack;
        let ptr_base,ptr_offsets  = Cvalue.V.find_lonely_key ptr in
        let start_addr_base, start_addr_offsets =
          Cvalue.V.find_lonely_key start_addr
        in
        let end_addr_base,end_addr_offsets =
          Cvalue.V.find_lonely_key end_addr
        in
        if not (Base.equal start_addr_base end_addr_base) then
          raise Not_same_base;

        let result =
          if Base.equal ptr_base start_addr_base then begin
            let get_min_max ival = match Ival.min_and_max ival with
              | Some min, Some max -> min,max
              | _ -> raise Not_found
            in

            let min_ptr,max_ptr = get_min_max ptr_offsets in
            let _min_start,max_start =get_min_max start_addr_offsets in
            let min_end,_max_end = get_min_max end_addr_offsets in

            if Int.le max_start min_ptr && Int.lt max_ptr min_end then
              Ival.one
            else
              Ival.zero
          end
          else
            Ival.zero
        in
        { Value_types.c_values =
            [ Value_types.StateOnly
                (Eval_op.wrap_int (Cvalue.V.inject_ival result), state) ];
          c_clobbered = Base.SetLattice.bottom;
          c_cacheable = Value_types.Cacheable;
          c_from = None; (* TODO?*)
          c_sureouts = None;
        }
      with
      | Not_found -> (* from find_lonely_key *)
        Value_parameters.warning ~current:true
          "assert(single base adress for each pointer)";

        { Value_types.c_values =
            [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
          c_clobbered = Base.SetLattice.bottom;
          c_cacheable = Value_types.Cacheable;
          c_from = None; (* TODO?*)
          c_sureouts = None;
        }
      | Not_same_base ->
        Value_parameters.warning ~current:true
          "assert(%a %a point to the same object)."
          Cvalue.V.pretty start_addr
          Cvalue.V.pretty end_addr;

        { Value_types.c_values =
            [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
          c_clobbered = Base.SetLattice.bottom;
          c_cacheable = Value_types.Cacheable;
          c_from = None; (* TODO?*)
          c_sureouts = None;
        }
    )
  | _ ->
    raise Db.Value.Aborted



(** This builtin allows to compare two pointers ptr1 and ptr2.
 *  The order defined is total and persistent. *)
let tis_ptr_is_less_than state actuals =
  match actuals with
  | [ (_, ptr1, _) ; (_, ptr2, _) ] -> (
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.feedback ~current:true
          "Call to builtin tis_ptr_is_less_than(%a)%t"
          pretty_actuals actuals Value_util.pp_callstack;

      let precise_and_valid, result =
        let op = Abstract_interp.Comp.Lt in
        Cvalue.V.ptr_compare op ptr1 ptr2
      in
      begin
        if not precise_and_valid then
          let problem =
            if Cvalue.V.is_bottom result then "invalid" else "imprecise"
          in
          Value_parameters.warning ~current:true
            "Pointers %a and/or %a are %s and an ordering couldn't be \
             determined by builtin tis_ptr_is_less_than."
            Cvalue.V.pretty ptr1 Cvalue.V.pretty ptr2 problem
      end;

      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_int result, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    )
  | _ ->
    raise Db.Value.Aborted

let () = register_builtin "tis_ptr_is_within" tis_ptr_is_within
let () = register_builtin "tis_ptr_is_less_than" tis_ptr_is_less_than




 (*
  Local Variables:
    compile-command: "make -C ../../../../.."
  End:
 *)
