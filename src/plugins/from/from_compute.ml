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

open Callwise_types
open Cil_types
open Cil
open Cil_datatype
open Abstract_interp
open Locations

exception Call_did_not_take_place

module type To_Use =
sig
  val get_value_state : stmt -> Db.Value.state
end

module Kf_FromResults = Kernel_function.Hashtbl.Make(Value_types.FromResult)

let rec find_deps_no_transitivity state expr =
  (* The value of the expression [expr], just before executing the statement
     [instr], is a function of the values of the returned zones. *)
  match expr.enode with
  | Info (e, _) -> find_deps_no_transitivity state e
  | AlignOfE _| AlignOf _| SizeOfStr _ |SizeOfE _| SizeOf _ | Const _
    -> Function_Froms.Deps.bottom
  | AddrOf lv  | StartOf lv ->
    let deps, _ = !Db.Value.lval_to_loc_with_deps_state (* loc ignored *)
        state
        ~deps:Zone.bottom
        lv
    in
    Function_Froms.Deps.from_data_deps deps
  | CastE (_, e)|UnOp (_, e, _) ->
    find_deps_no_transitivity state e
  | BinOp (_, e1, e2, _) ->
    Function_Froms.Deps.join
      (find_deps_no_transitivity state e1)
      (find_deps_no_transitivity state e2)
  | Lval v ->
    find_deps_lval_no_transitivity state v

and find_deps_lval_no_transitivity state lv =
  let ind_deps, direct_deps, _exact =
    !Db.Value.lval_to_zone_with_deps_state
      state ~for_writing:false ~deps:(Some Zone.bottom) lv
  in
  From_parameters.debug "find_deps_lval_no_trs:@\n deps:%a@\n direct_deps:%a"
    Zone.pretty ind_deps Zone.pretty direct_deps;
  { Function_Froms.Deps.data = direct_deps; indirect = ind_deps }

let compute_using_prototype_for_state state kf =
  let varinfo = Kernel_function.get_vi kf in
  let behaviors = !Db.Value.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  let return_deps,deps =
    match assigns with
    | WritesAny ->
      From_parameters.warning "no assigns clauses@ for function %a.@ \
                               Results@ will be@ imprecise."
        Kernel_function.pretty kf;
      Function_Froms.Memory.(top_return, top)
    | Writes assigns ->
      let (rt_typ,_,_,_) = splitFunctionTypeVI varinfo in
      let input_zone out ins =
        (* Technically out is unused, but there is a signature problem *)
        !Db.Value.assigns_inputs_to_zone state (Writes [out, ins])
      in
      let treat_assign acc (out, ins) =
        try
          let (output_loc_under, output_loc_over, _deps) =
            !Db.Properties.Interp.loc_to_loc_under_over
              ~result:None state out.it_content
          in
          let input_zone = input_zone out ins in
          (* assign clauses do not let us specify address
             dependencies for now, so we assume it is all data
             dependencies *)
          let input_deps =
            Function_Froms.Deps.from_data_deps input_zone
          in
          (* Weak update of the over-approximation of the zones assigned *)
          let acc = Function_Froms.Memory.add_binding_loc ~exact:false
              acc output_loc_over input_deps in
          let output_loc_under_zone = Locations.enumerate_valid_bits_under
              ~for_writing:true output_loc_under in
          (* Now, perform a strong update on the zones that are guaranteed
             to be assigned (under-approximation) AND that do not depend
             on themselves.
             Note: here we remove an overapproximation from an
             underapproximation to get an underapproximation, which is not
             the usual direction. It works here because diff on non-top
             zones is an exact operation. *)
          let sure_out_zone =
            Zone.(if equal top input_zone then bottom
                  else diff output_loc_under_zone input_zone)
          in
          let acc = Function_Froms.Memory.add_binding ~exact:true
              acc sure_out_zone input_deps in
          acc
        with Db.Properties.Interp.No_conversion ->
          From_parameters.result
            ~once:true ~current:true "Unable to extract assigns in %a"
            Kernel_function.pretty kf;
          acc
      in
      let treat_ret_assign acc (out, from) =
        let zone_from = input_zone out from in
        (* assign clauses do not let us specify address dependencies for
           now, so we assume it is all data dependencies *)
        let inputs_deps = Function_Froms.Deps.from_data_deps zone_from in
        try
          let coffs =
            !Db.Properties.Interp.loc_to_offset ~result:None out.it_content
          in
          List.fold_left
            (fun acc coff ->
               let (base,width) = bitsOffset rt_typ coff in
               let size = Int_Base.inject (Int.of_int width) in
               Function_Froms.Memory.(add_to_return
                                        ~start:base ~size ~m:acc inputs_deps)
            )
            acc coffs
        with Db.Properties.Interp.No_conversion | SizeOfError _ ->
          From_parameters.result  ~once:true ~current:true
            "Unable to extract a proper offset. \
             Using FROM for the whole \\result";
          let size = Bit_utils.sizeof rt_typ in
          Function_Froms.(Memory.add_to_return ~size ~m:acc inputs_deps)
      in
      let return_assigns, other_assigns =
        List.fold_left
          (fun (ra,oa) (loc,_ as a) ->
             if Logic_utils.is_result loc.it_content
             then a::ra,oa else ra,a::oa)
          ([],[]) assigns
      in
      let return_assigns =
        match return_assigns with
        | [] when Cil.isVoidType rt_typ ->
          Function_Froms.Memory.default_return
        | [] -> (* \from unspecified. *)
          let size = Bit_utils.sizeof rt_typ in
          Function_Froms.Memory.top_return_size size
        | _ ->
          List.fold_left treat_ret_assign
            Function_Froms.Memory.default_return return_assigns
      in
      return_assigns,
      List.fold_left
        treat_assign Function_Froms.Memory.empty other_assigns
  in
  { deps_return = return_deps; Function_Froms.deps_table = deps }

module ZoneStmtMap = struct
  include
    Hptmap.Make(Stmt_Id)(Zone)(Hptmap.Comp_unused)
      (struct let v = [[]] end)
      (struct let l = [Ast.self] end)
  let () = Ast.add_monotonic_state self

  let join =
    let decide _k z1 z2 = Zone.join z1 z2 in
    join ~cache:(Hptmap_sig.PersistentCache "From_compute.ZoneStmtMap.join")
      ~symmetric:true ~idempotent:true ~decide
end

module Domain = struct

  type t' =
    {
      additional_deps_table : ZoneStmtMap.t;
      (** Additional control dependencies to add to all modified variables,
          coming from the control statements encountered so far (If, Switch).
          The statement information is used to remove the dependencies that
          are no longer useful, when we reach a statement that post-dominates
          the statement that gave rise to the dependency. *)
      additional_deps : Zone.t;
      (** Union of the sets in {!additional_deps_table} *)
      deps_table : Function_Froms.Memory.t;
      (** dependency table *)
    }

  let empty_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.empty }

  let bottom_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.bottom }

  module FromsData = Datatype.Make(struct
      type t = t'
      let name = "Whole-program Froms data"
      let reprs = [ empty_from; bottom_from ]
      include Datatype.Serializable_undefined
      let structural_descr = Structural_descr.t_record
          [| ZoneStmtMap.packed_descr; Zone.packed_descr; Function_Froms.Memory.packed_descr |]
    end
    )

  include FromsData
  (** Recreate the [additional_deps] field from [additional_deps_table] *)
  let rebuild_additional_deps map =
    ZoneStmtMap.fold (fun _ z accz -> Zone.join z accz) map Zone.bottom

  (** given a [Function_Froms.Deps.t], apply [f] on both components and merge
      the result:
      depending directly on an indirect dependency -> indirect,
      depending indirectly on a direct dependency  -> indirect *)
  let merge_deps f deps =
    let open Function_Froms.Deps in
    let ind = f deps.indirect in
    let data = f deps.data in
    let ind = Zone.join data.indirect (to_zone ind) in
    let data = data.data in
    { data = data; indirect = ind }

  let join_and_is_included new_ old =
    let additional_map, additional_zone, included =
      let mold = old.additional_deps_table in
      let mnew = new_.additional_deps_table in
      let zold = old.additional_deps in
      let m = ZoneStmtMap.join mnew mold in
      if ZoneStmtMap.equal m mold then
        mold, zold, true
      else
        let new_z = Zone.join old.additional_deps new_.additional_deps in
        m, new_z, false
    in
    let map, included' =
      Function_Froms.Memory.join_and_is_included
        new_.deps_table old.deps_table
    in
    { deps_table = map;
      additional_deps_table = additional_map;
      additional_deps = additional_zone;},
    included && included'

  (** Bind all the variables of [b] to [Assigned \from \nothing]. This function
      is always called on local variables, which are semantically assigned to
      "uninitialized". *)
  let bind_locals m b =
    let aux_local acc vi =
      Cil.CurrentLoc.set vi.vdecl;
      (* Consider that local are initialized to a constant value *)
      Function_Froms.Memory.bind_var vi Function_Froms.Deps.bottom acc
    in
    let loc = Cil.CurrentLoc.get () in

    let r = List.fold_left aux_local m b.blocals in
    Cil.CurrentLoc.set loc;
    r

  let unbind_locals m b =
    let aux_local acc vi =
      Function_Froms.Memory.unbind_var vi acc
    in
    List.fold_left aux_local m b.blocals

  let find state deps_tbl expr =
    let pre_trans = find_deps_no_transitivity state expr in
    merge_deps
      (fun d -> Function_Froms.Memory.find_precise deps_tbl d) pre_trans

  let find = State_node.with_state find

  let lval_to_zone_with_deps state ~for_writing lv =
    !Db.Value.lval_to_zone_with_deps_state
      state ~deps:(Some Zone.bottom) ~for_writing lv

  let lval_to_zone_with_deps = State_node.with_state lval_to_zone_with_deps

  let lval_to_precise_loc_with_deps state ~for_writing lv =
    let deps, loc =
      !Db.Value.lval_to_precise_loc_with_deps_state
        state ~deps:(Some Zone.bottom) lv
    in
    let exact = Precise_locs.valid_cardinal_zero_or_one ~for_writing loc in
    deps, loc, exact

  let lval_to_precise_loc_with_deps = State_node.with_state lval_to_precise_loc_with_deps
end


module GMake (X:sig
    val graph : Nodegraph.t
    val initial_nodes : State_node.Set.t
  end)
= struct

  include Domain

  let call_stack : kernel_function Stack.t = Stack.create ()
  (** Stack of function being processed *)

  module Computer = struct
    type t = {
      mutable to_propagate : State_node.Set.t
      (** nodes to propagate next *)
    }

    (*let bottom = bottom_from;; *)
    let callwise_states_with_formals = Stmt.Hashtbl.create 7
    let propagated = State_node.Hashtbl.create 97

    let stmt_to_nodes = Stmt.Hashtbl.create 7

    let current_table : t' State_node.Hashtbl.t = State_node.Hashtbl.create 97

    let node_state n =
      try State_node.Hashtbl.find current_table n
      with Not_found -> empty_from

    let add_node_to_stmt node stmt =
      if Stmt.Hashtbl.mem stmt_to_nodes stmt
      then
        let set = Stmt.Hashtbl.find stmt_to_nodes stmt in
        Stmt.Hashtbl.replace stmt_to_nodes stmt (State_node.Set.add node set)
      else Stmt.Hashtbl.add stmt_to_nodes stmt (State_node.Set.singleton node)

    let add_to_result stmt _ node data =
      if Stmt.Hashtbl.mem callwise_states_with_formals stmt
      then State_node.Hashtbl.replace
          (Stmt.Hashtbl.find callwise_states_with_formals stmt) node data
      else begin
        let new_tbl = State_node.Hashtbl.create 7 in
        State_node.Hashtbl.add new_tbl node data;
        Stmt.Hashtbl.add callwise_states_with_formals stmt new_tbl
      end

    let add_to_result = State_node.with_stmt_state_node add_to_result
    let return_is_reachable = ref false
    let return_zone = ref Zone.bottom

    let substitute call_site_froms extra_loc deps =
      let subst_deps = Function_Froms.Memory.substitute call_site_froms deps in
      Function_Froms.Deps.add_indirect_dep subst_deps extra_loc

    let display_one_from fmt v =
      Function_Froms.Memory.pretty fmt v.deps_table;
      Format.fprintf fmt "Additional Variable Map : %a@\n"
        ZoneStmtMap.pretty v.additional_deps_table;
      Format.fprintf fmt
        "Additional Variable Map Set : %a@\n"
        Zone.pretty
        v.additional_deps

    let pretty_stmt_state fmt stmt =
      let nodes = Stmt.Hashtbl.find stmt_to_nodes stmt in
      State_node.Set.iter (fun node -> display_one_from fmt (node_state node)) nodes

    let pretty fmt (v: t) =
      Format.fprintf fmt "Propagating nodes %a@\n"
        (fun fmt s -> State_node.Set.iter (fun v -> State_node.pretty fmt v) s)
        v.to_propagate;
      try
        let stmt = State_node.stmt (State_node.Set.choose v.to_propagate) in
        pretty_stmt_state fmt stmt
      with Not_found -> ()

    let update_and_is_included n data =
      try
        let old = State_node.Hashtbl.find current_table n in
        let join, inc = join_and_is_included data old in
        State_node.Hashtbl.replace current_table n join;
        inc
      with Not_found ->
        State_node.Hashtbl.replace current_table n data;
        false

    let aggregated_stmt_state stmt =
      let nodes = Stmt.Hashtbl.find stmt_to_nodes stmt in
      State_node.Set.fold
        (fun node s ->
           let data = node_state node in
           fst (join_and_is_included s data))
        nodes
        bottom_from

    let transfer_conditional_exp node exp state =
      let additional = find node state.deps_table exp in
      let additional = Function_Froms.Deps.to_zone additional in
      {state with
       additional_deps_table =
         ZoneStmtMap.add (State_node.stmt node) additional state.additional_deps_table;
       additional_deps =
         Zone.join additional state.additional_deps }

    (** Handle an assignement [lv = ...], the dependencies of the right-hand
        side being stored in [deps_right]. *)
    let transfer_assign node lv deps_right state =
      (* The assigned location is [loc], whose address is computed from
         [deps]. *)
      let deps, loc, exact =
        lval_to_precise_loc_with_deps node ~for_writing:true lv
      in
      let deps_of_deps = Function_Froms.Memory.find state.deps_table deps in
      let all_indirect = Zone.join state.additional_deps deps_of_deps in
      let deps = Function_Froms.Deps.add_indirect_dep deps_right all_indirect in
      { state with deps_table =
                     Function_Froms.Memory.add_binding_precise_loc
                       ~exact state.deps_table loc deps }

    let transfer_instr stmt value_state node (i: instr) (state: t') =
      !Db.progress ();
      match i with
      | Set (lv, exp, _) ->
        let comp_vars = find node state.deps_table exp in
        transfer_assign node lv comp_vars state

      (* Variadic functions handling implemented in the GMake module using the
         infamous programming methods of Dr Copy and Mr Paste. *)

      (* >>> COPY-PASTE BEGINS <<< *)
      (** Variadic: handling the va_arg macro. *)
      | Call
          (None,
           { enode = Lval (Var { vname= "__builtin_va_arg"; _ }, NoOffset);
             _},
           [ { enode =
                 Lval ((Var _va_list_varinfo, _va_list_offset) as va_list_lv);
               _ } as _va_list_exp;
             { enode = SizeOf _expected_arg_typ; _ } as _size_exp;
             { enode = CastE (voidPtrType, { enode = AddrOf dest_lv; _});
               _ } as _dest_exp ],
           _loc)
        when Cil.isVoidPtrType voidPtrType ->
        (* Set the destination lvalue dependencies to the dependencies of the
           next variadic argument. *)
        (* 1. Get the next variadic argument's dependencies. *)
        let variadic_argument_deps : Function_Froms.Deps.deps =
          (* let value_state = To_Use.get_value_state stmt in *)
          let arg_zone : Locations.Zone.t =
            let arg_loc : Locations.location =
              let va_list_loc =
                !Db.Value.lval_to_loc_state value_state va_list_lv
              in
              Value.Value_variadic.arg_loc_of_va_list_loc
                value_state va_list_loc
            in
            Locations.enumerate_valid_bits ~for_writing:false arg_loc
          in
          Function_Froms.Memory.find_precise state.deps_table arg_zone
        in
        (* 2. Set the destination lvalue dependencies. *)
        transfer_assign node dest_lv variadic_argument_deps state
      (* >>> COPY-PASTE ENDS <<< *)
      (* REPLACED "stmt" by "node" and
         REMOVED "let value_state = To_Use.get_value_state stmt in"
         (as "value_state" is just given in the "transfer_instr" arguments) *)

      | Call (lvaloption,funcexp,argl,_) ->
        !Db.progress ();
        let funcexp_deps, called_vinfos =
          match funcexp.enode with
          | Lval (Var _, NoOffset) ->
            Zone.bottom, Value.Value_results.callees stmt
          | _ -> (* Function pointers *)
            !Db.Value.expr_to_kernel_function_state
              value_state ~deps:(Some Zone.bottom) funcexp
        in
        (* dependencies for the evaluation of [funcexp] *)
        let funcexp_deps =
          Function_Froms.Memory.find state.deps_table funcexp_deps
        in
        let additional_deps =
          Zone.join
            state.additional_deps
            funcexp_deps
        in
        let args_froms =
          List.map
            (fun arg ->
               (* TODO : dependencies on subfields for structs *)
               find node state.deps_table arg)
            argl
        in
        let states_with_formals = ref [] in
        let do_on kf =
          let called_vinfo = Kernel_function.get_vi kf in
          if Ast_info.is_cea_function called_vinfo.vname then
            state
          else
            let froms_call =
              let { table_for_calls; _ } = List.hd !call_froms_stack in
              try Kinstr.Hashtbl.find table_for_calls (Kstmt stmt)
              with Not_found -> raise Call_did_not_take_place
            in
            let froms_call =
              try Kernel_function.Hashtbl.find froms_call kf
              with Not_found ->
                raise Call_did_not_take_place
            in
            let froms_call_table = froms_call.Function_Froms.deps_table in
            if Function_Froms.Memory.is_bottom froms_call_table then
              bottom_from
            else
              let formal_args = Kernel_function.get_formals kf in

              (* Dr Copy and Mr Paste send their regards... *)

              (* >>> COPY-PASTE BEGINS <<< *)
              let state_with_formals : Function_Froms.Memory.t =

                (* Binding the function arguments to dependencies:
                   - each matching argument + deps pair is handled by the
                     [bind_arg_varinfo_to_deps] function;
                   - first we bind all the formal arguents to the
                     corresponding dependencies in the
                     [bind_formals_to_depss] function;
                   - then, if there are still depss left and the function:
                     - is variadic,
                     - is not a builtin,
                     - and has a definition,
                     we bind the variadic arguments to the remaining
                     dependencies in the [bind_variadics_to_depss] function.
                *)

                let bind_arg_varinfo_to_deps state arg_varinfo arg_deps =
                  Function_Froms.Memory.bind_var arg_varinfo arg_deps state
                in

                let rec bind_variadics_to_depss
                    state variadic_args args_depss =
                  match variadic_args, args_depss with
                  | [], [] -> state
                  | variadic_arg :: remaining_variadic_args,
                    arg_deps     :: remaining_args_depss ->
                    (* A matching pair: variadic argument varinfo + deps. *)
                    let state' =
                      bind_arg_varinfo_to_deps state variadic_arg arg_deps
                    in
                    (* The remaining variadic arguments and the remaining
                       dependencies. *)
                    bind_variadics_to_depss state'
                      remaining_variadic_args remaining_args_depss
                  | _variadic_args, [] ->
                    (* More variadic arguments than depss.
                       NOTE: This case should never happen. *)
                    From_parameters.fatal ~current:true
                      "more variadic arguments than depss"
                  | [], _depss ->
                    (* More depss than variadic arguments.
                       NOTE: This case should never happen. *)
                    From_parameters.fatal ~current:true
                      "more depss than variadic arguments"
                in

                let rec bind_formals_to_depss state formal_args args_depss =
                  match formal_args, args_depss with
                  | [], [] -> state
                  | formal_arg :: remaining_formal_args,
                    arg_deps   :: remaining_args_depss ->
                    (* A matching pair: formal argument varinfo and deps. *)
                    let state' =
                      bind_arg_varinfo_to_deps state formal_arg arg_deps
                    in
                    (* The remaining formal arguments and the remaining
                       dependencies. *)
                    bind_formals_to_depss state'
                      remaining_formal_args remaining_args_depss
                  | _formal_args, [] ->
                    (* Not enough dependencies for formal arguments.
                       NOTE: This case should never happen. *)
                    From_parameters.fatal ~current:true
                      "not enough deps for formal arguments of a function"
                  | [], _depss
                    when not (Value.Value_util.is_function_variadic kf) ->
                    (* More deps than formal arguments, but the function
                       is not variadic. *)
                    let arguments_count =
                      min (List.length formal_args) (List.length args_froms)
                    in
                    From_parameters.warning ~once:true ~current:true
                      "Too many arguments for function. Using only %d \
                       argument%s."
                      arguments_count
                      (if arguments_count = 1 then "" else "s");
                    state
                  | [], _depss
                    when Value.Builtins.is_function_a_builtin kf ->
                    (* More deps than formal arguments, the function is
                       officially variadic, but it is a built-in. *)
                    state
                  | [], _depss
                    when not (Kernel_function.is_definition kf) ->
                    (* More deps than formal arguments, the function is
                       viariadic, but it has no definition. *)
                    From_parameters.debug ~current:true
                      "Variadic call for function %a without definition."
                      Kernel_function.pretty kf;
                    (* TODO: Can we handle this case better? *)
                    state
                  | [], depss ->
                    (* The variadic case: we bind the remaining froms to the
                       variables corresponding to the variadic arguments. *)
                    let variadic_varinfos =
                      let call_site = Kstmt stmt in
                      Value.Value_variadic.get_variadic_arguments_varinfos
                        kf call_site
                    in
                    bind_variadics_to_depss state variadic_varinfos depss
                in

                bind_formals_to_depss
                  state.deps_table formal_args args_froms
              in
              (* >>> COPY-PASTE ENDS <<< *)
              (* (ABSOLUTELY NO MODIFICATIONS MADE) *)

              if not (Db.From.Record_From_Graph_Callbacks.is_empty ())
              then
                states_with_formals :=
                  (kf, state_with_formals) :: !states_with_formals;
              let subst_before_call =
                substitute state_with_formals additional_deps
              in
              (* From state just after the call,
                 but before the result assigment *)
              let deps_after_call =
                let before_call = state.deps_table in
                let open Function_Froms in
                let subst d = DepsOrUnassigned.subst subst_before_call d in
                let call_substituted = Memory.map subst froms_call_table in
                Memory.compose call_substituted before_call
              in
              let state = {state with deps_table = deps_after_call } in
              (* Treatement for the possible assignement
                 of the call result *)
              match lvaloption with
              | None -> state
              | Some lv ->
                let return_from = froms_call.Function_Froms.deps_return in
                let deps_ret = subst_before_call return_from in
                transfer_assign node lv deps_ret state
        in
        let f f acc =
          let p = do_on f in
          match acc with
          | None -> Some p
          | Some acc_memory ->
            Some
              {state with
               deps_table = Function_Froms.Memory.join
                   p.deps_table
                   acc_memory.deps_table}
        in
        let result =
          try
            (match Kernel_function.Hptset.fold f called_vinfos None with
             | None -> state
             | Some s -> s);
          with Call_did_not_take_place -> state
        in
        if not (Db.From.Record_From_Graph_Callbacks.is_empty ())
        then
          add_to_result
            node
            !states_with_formals;
        result
      | _ -> state

    let transfer_instr = State_node.with_stmt_state_node transfer_instr

    (* Eliminate additional variables originating from a control-flow branching
       statement closing at [s]. *)
    let eliminate_additional s data =
      let kf = Stack.top call_stack in
      let map = data.additional_deps_table in
      let map' =
        ZoneStmtMap.fold
          (fun k  _v acc_map ->
             if !Db.Postdominators.is_postdominator kf ~opening:k ~closing:s
             then ZoneStmtMap.remove k acc_map
             else acc_map
          ) map map
      in
      if not (map == map') then
        { data with
          additional_deps_table = map';
          additional_deps = rebuild_additional_deps map';
        }
      else data

    let filter_edge s succ d =
      let dt = d.deps_table in
      let opened = Kernel_function.blocks_opened_by_edge s succ in
      let closed = Kernel_function.blocks_closed_by_edge s succ in
      let dt = List.fold_left bind_locals dt opened in
      let dt = List.fold_left unbind_locals dt closed in
      { d with deps_table = dt }

    let doStmt s nodes =
      let to_propagate = nodes.to_propagate in
      let process_node s' state node t =
        let data = node_state node in
        (* Filter out unreachable values *)
        if Function_Froms.Memory.is_bottom data.deps_table
        then t (* stop propagation *)
        else begin
          let data = eliminate_additional s data in
          let propagate_to_succs new_ t =
            let succs = Nodegraph.succ X.graph node in
            let add_to_list stmt v map =
              let data =
                try v :: (Stmt.Map.find stmt map)
                with Not_found -> [v]
              in
              Stmt.Map.add stmt data map
            in
            let map =
              List.fold_left
                (fun acc v -> add_to_list (State_node.stmt v) v acc)
                Stmt.Map.empty
                succs
            in
            Stmt.Map.fold
              (fun stmt succs t ->
                 let newdata = filter_edge s stmt new_ in
                 List.fold_left
                   (fun t succ ->
                      let inc = update_and_is_included succ newdata in
                      if inc && (State_node.Hashtbl.mem propagated succ)
                      then t
                      else State_node.Set.add succ t)
                   t
                   succs)
              map
              t
          in
          assert (Stmt.equal s s');
          if not (Db.Value.is_reachable state)
          then t
          else begin
            if not (State_node.Hashtbl.mem propagated node) then
              begin
                State_node.Hashtbl.add propagated node ();
                add_node_to_stmt node s
              end;
            match s.skind with
            | Instr i ->
              let new_ = transfer_instr node i data in
              propagate_to_succs new_ t
            | If (exp,_,_,_) | Switch (exp,_,_,_) ->
              let new_ = transfer_conditional_exp node exp data in
              propagate_to_succs new_ t
            | Return (Some { enode = Lval v; _ },_) ->
              let deps, target, _exact =
                lval_to_zone_with_deps node ~for_writing:false v
              in
              let z = Zone.join target deps in
              return_zone := Zone.join !return_zone z;
              return_is_reachable := true;
              t
            | Return _ -> return_is_reachable := true; t
            | Throw _ -> t
            | UnspecifiedSequence _ | Loop _ | Block _
            | Goto _ | Break _ | Continue _
            | TryExcept _ | TryFinally _ | TryCatch _ ->
              propagate_to_succs data t
          end
        end
      in
      nodes.to_propagate <- State_node.Set.empty;
      let succnodes =
        State_node.Set.fold
          (State_node.with_stmt_state_node process_node)
          to_propagate
          State_node.Set.empty
      in
      if State_node.Set.is_empty succnodes then Dataflow2.SDone
      else Dataflow2.SUse { to_propagate = succnodes }

    let doInstr _ _ _ t = t (*transfer done in doStmt*)

    let doEdge _s succ t =
      { to_propagate = State_node.Set.filter
            (fun node -> Stmt.equal succ (State_node.stmt node))
            t.to_propagate }

    let doGuard _ _ _ = Dataflow2.GDefault, Dataflow2.GDefault
    (* filtering successors states to the right instruction is done by doStmt and doEdge *)

    let computeFirstPredecessor _ t = t

    let combinePredecessors _ ~old new_ =
      let nodes = State_node.Set.union
          old.to_propagate
          new_.to_propagate in
      if State_node.Set.is_empty nodes then None
      else Some { to_propagate = nodes }
  end

  (* Remove all local variables and formals from table *)
  let externalize return return_zone kf state =
    let deps_return =
      (match return.skind with
       | Return (Some ({ enode = Lval v; _ }),_) ->
         let z = return_zone in
         let deps = Function_Froms.Memory.find_precise state.deps_table z in
         let size = Bit_utils.sizeof (Cil.typeOfLval v) in
         Function_Froms.(Memory.add_to_return ~size deps)
       | Return (None,_) ->
         Function_Froms.Memory.default_return
       | _ -> assert false)
    in
    let accept base =
      let fundec = Kernel_function.get_definition kf in
      not (Base.is_formal_or_local base fundec)
    in
    let deps_table =
      Function_Froms.Memory.filter_base accept state.deps_table
    in
    { deps_return = deps_return;
      Function_Froms.deps_table = deps_table }

  let compute_using_cfg kf =
    match kf.fundec with
    | Declaration _ -> assert false
    | Definition (f,_) ->
      if !Db.Value.no_results kf then Function_Froms.top
      else
        try
          Stack.iter
            (fun g ->
               if kf == g then begin
                 if Db.Value.ignored_recursive_call kf then
                   From_parameters.error
                     "during dependencies computations for %a, \
                      ignoring probable recursive"
                     Kernel_function.pretty kf;
                 raise Exit
               end)
            call_stack;
          Stack.push kf call_stack;
          let module Fenv =
            (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV)
          in
          let module Dataflow_arg = struct
            include Computer
            module StmtStartData = Dataflow2.StartData (struct type t = Computer.t let size = 7 end)
            let name = "from analysis"
            let debug = false
            let copy t = t
          end
          in
          let module Compute = Dataflow2.Forwards(Dataflow_arg) in
          let start = Kernel_function.find_first_stmt kf in
          Dataflow_arg.StmtStartData.add start { Dataflow_arg.to_propagate = X.initial_nodes };
          let state =  { empty_from with
                         deps_table = bind_locals empty_from.deps_table f.sbody }
          in
          State_node.Set.iter
            (fun n ->
               assert (Stmt.equal start (State_node.stmt n));
               ignore (Dataflow_arg.update_and_is_included n state))
            X.initial_nodes;
          Compute.compute [start];
          let ret_id = Kernel_function.find_return kf in
          if not (Db.From.Record_From_Graph_Callbacks.is_empty ())
          then begin
            if From_parameters.DepsShowProgress.get()
            then From_parameters.feedback "Now calling From callbacks";
            let states =
              Stmt.Hashtbl.create Fenv.nb_stmts
            in
            Stmt.Hashtbl.iter
              (fun k _ ->
                 Stmt.Hashtbl.add
                   states
                   k
                   (Dataflow_arg.aggregated_stmt_state k).deps_table)
              Dataflow_arg.stmt_to_nodes;
            Db.From.Record_From_Graph_Callbacks.apply
              (call_stack, states, Dataflow_arg.stmt_to_nodes,
               Dataflow_arg.callwise_states_with_formals)
          end;
          let _poped = Stack.pop call_stack in
          let last_from =
            try
              if !Dataflow_arg.return_is_reachable
              then
                externalize
                  ret_id
                  !Dataflow_arg.return_zone
                  kf
                  (Dataflow_arg.aggregated_stmt_state ret_id)
              else
                raise Not_found
            with Not_found -> begin
                From_parameters.result
                  "Non-terminating function %a (no dependencies)"
                  Kernel_function.pretty kf;
                { Function_Froms.deps_return =
                    Function_Froms.Memory.default_return;
                  deps_table = Function_Froms.Memory.bottom }
              end
          in
          last_from

        with Exit (* Recursive call *) ->
          { Function_Froms.deps_return = Function_Froms.Memory.default_return;
            deps_table = Function_Froms.Memory.empty }

  let compute_using_prototype kf =
    let state = Db.Value.get_initial_state kf in
    compute_using_prototype_for_state state kf

  let compute_and_return kf =
    let call_site_loc = CurrentLoc.get () in
    if From_parameters.DepsShowProgress.get()
    then From_parameters.feedback
        "Computing for function %a%s"
        Kernel_function.pretty kf
        (let s = ref "" in
         Stack.iter
           (fun kf ->
              s := !s^" <-"^(Pretty_utils.sfprintf "%a" Kernel_function.pretty kf))
           call_stack;
         !s);
    !Db.progress ();
    let result =
      if !Db.Value.use_spec_instead_of_definition kf
      then (compute_using_prototype kf)
      else (compute_using_cfg kf)
    in
    if From_parameters.DepsShowProgress.get()
    then
      From_parameters.feedback
        "Done for function %a" Kernel_function.pretty kf;
    !Db.progress ();
    CurrentLoc.set call_site_loc;
    result

  let compute kf =
    !Db.Value.compute ();
    ignore (compute_and_return kf)

end

module WholeProgramDomain = struct

  type t' =
    {
      additional_deps_table : ZoneStmtMap.t;
      (** Additional control dependencies to add to all modified variables,
          coming from the control statements encountered so far (If, Switch).
          The statement information is used to remove the dependencies that
          are no longer useful, when we reach a statement that post-dominates
          the statement that gave rise to the dependency. *)
      additional_deps : Zone.t;
      (** Union of the sets in {!additional_deps_table} *)
      path_deps_table : ZoneStmtMap.t;
      path_deps : Zone.t;
      deps_table : Function_Froms.Memory.t;
      (** dependency table *)
    }

  let empty_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      path_deps_table = ZoneStmtMap.empty;
      path_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.empty }

  let bottom_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      path_deps_table = ZoneStmtMap.empty;
      path_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.bottom }

  module FromsData = Datatype.Make(struct
      type t = t'
      let name = "Froms data"
      let reprs = [ empty_from; bottom_from ]
      include Datatype.Serializable_undefined
      let structural_descr = Structural_descr.t_record
          [| ZoneStmtMap.packed_descr; Zone.packed_descr; Function_Froms.Memory.packed_descr |]
    end
    )

  include FromsData
  (** Recreate the [additional_deps] field from [additional_deps_table] *)
  let rebuild_additional_deps map =
    ZoneStmtMap.fold (fun _ z accz -> Zone.join z accz) map Zone.bottom

  (** given a [Function_Froms.Deps.t], apply [f] on both components and merge
      the result:
      depending directly on an indirect dependency -> indirect,
      depending indirectly on a direct dependency  -> indirect *)
  let merge_deps f deps =
    let open Function_Froms.Deps in
    let ind = f deps.indirect in
    let data = f deps.data in
    let ind = Zone.join data.indirect (to_zone ind) in
    let data = data.data in
    { data = data; indirect = ind }

  let join_and_is_included new_ old =
    let additional_map, additional_zone, included =
      let mold = old.additional_deps_table in
      let mnew = new_.additional_deps_table in
      let zold = old.additional_deps in
      let m = ZoneStmtMap.join mnew mold in
      if ZoneStmtMap.equal m mold then
        mold, zold, true
      else
        let new_z = Zone.join old.additional_deps new_.additional_deps in
        m, new_z, false
    in
    let map, included' =
      Function_Froms.Memory.join_and_is_included
        new_.deps_table old.deps_table
    in
    let path_map, path_zone, included'' =
      let mold = old.path_deps_table in
      let mnew = new_.path_deps_table in
      let zold = old.path_deps in
      let m = ZoneStmtMap.join mnew mold in
      if ZoneStmtMap.equal m mold then
        mold, zold, true
      else let new_z = Zone.join zold new_.path_deps in
        m, new_z, false
    in
    {
      deps_table = map;
      additional_deps_table = additional_map;
      additional_deps = additional_zone;
      path_deps_table = path_map;
      path_deps = path_zone;
    },
    included && included' && included''

  let join new_ old = fst (join_and_is_included new_ old)

  (** Bind all the variables of [b] to [Assigned \from \nothing]. This function
      is always called on local variables, which are semantically assigned to
      "uninitialized". *)
  let bind_locals m b =
    let aux_local acc vi =
      Cil.CurrentLoc.set vi.vdecl;
      (* Consider that local are initialized to a constant value *)
      Function_Froms.Memory.bind_var vi Function_Froms.Deps.bottom acc
    in
    let loc = Cil.CurrentLoc.get () in

    let r = List.fold_left aux_local m b.blocals in
    Cil.CurrentLoc.set loc;
    r

  let unbind_locals m b =
    let aux_local acc vi =
      Function_Froms.Memory.unbind_var vi acc
    in
    List.fold_left aux_local m b.blocals

  let find state deps_tbl expr =
    let pre_trans = find_deps_no_transitivity state expr in
    merge_deps
      (fun d -> Function_Froms.Memory.find_precise deps_tbl d) pre_trans

  let find = State_node.with_state find

  let lval_to_zone_with_deps state ~for_writing lv =
    !Db.Value.lval_to_zone_with_deps_state
      state ~deps:(Some Zone.bottom) ~for_writing lv

  let lval_to_zone_with_deps = State_node.with_state lval_to_zone_with_deps

  let lval_to_precise_loc_with_deps state ~for_writing lv =
    let deps, loc =
      !Db.Value.lval_to_precise_loc_with_deps_state
        state ~deps:(Some Zone.bottom) lv
    in
    let exact = Precise_locs.valid_cardinal_zero_or_one ~for_writing loc in
    deps, loc, exact

  let lval_to_precise_loc_with_deps = State_node.with_state lval_to_precise_loc_with_deps
end

module WholeProgram = struct

  let compute_whole_program () =
    From_parameters.WholeProgramDeps.get ()

  let contract_graph =
    let module Contraction = Vcontraction.Make (struct
        include Nodegraph
        include Nodegraph.Builder
      end) in
    Contraction.contract (State_node.with_action
                            (function State_node.PostAnnot -> false | _ -> true))

  let find_nodes iter graph nodes =
    let rec aux p v acc =
      if p v then State_node.Set.add v acc
      else iter (aux p) graph v acc
    in
    State_node.Set.fold
      (aux (fun v -> State_node.action v  = State_node.PostAnnot))
      nodes State_node.Set.empty

  let prev_nodes = find_nodes Nodegraph.fold_pred
  let next_nodes = find_nodes Nodegraph.fold_succ

  module CachedCalls = State_builder.Hashtbl(State_node.Hashtbl)(Kf_FromResults)
      (struct
        let name = "Calls using the specification"
        let size = 97
        let dependencies = [ Ast.self; Db.Value.self ]
      end)
  let () = Ast.add_monotonic_state CachedCalls.self

  let store_spec_call node kf froms =
    let nodes =
      prev_nodes
        (Db.Value.whole_program_graph ())
        (State_node.Set.singleton node)
    in
    State_node.Set.iter (fun node ->
        let node_tbl =
          try CachedCalls.find node
          with Not_found ->
            let tbl = Kernel_function.Hashtbl.create 7 in
            CachedCalls.add node tbl;
            tbl
        in
        Kernel_function.Hashtbl.replace node_tbl kf froms)
      nodes

  let store_call_froms (call_type, value_init_state, call_stack, call_node) =
    if compute_whole_program () then
      let cur_kf, call_kinstr = List.hd call_stack in
      match call_node with
      | None -> assert (Kinstr.equal call_kinstr Kglobal)
      | Some call_node ->
        match call_type with
        | `Def -> () (*no need to store anything, it's in the graph*)
        | `Memexec ->
          assert false (*this analysis isn't compatible with memexec yet*)
        | `Builtin { Value_types.c_from = Some result; _ } ->
          store_spec_call call_node cur_kf result
        | `Spec | `Builtin { Value_types.c_from = None; _ } ->
          let froms = compute_using_prototype_for_state value_init_state cur_kf in
          store_spec_call call_node cur_kf (Value_types.Froms froms)

  include WholeProgramDomain

  module ReturnZone = State_builder.Hashtbl(State_node.Hashtbl)(Zone)
      (struct
        let name = "Return zones"
        let size = 97
        let dependencies = [ Ast.self; Db.Value.self ]
      end)
  let () = Ast.add_monotonic_state ReturnZone.self

  let return_zone node =
    try ReturnZone.find node
    with Not_found -> Zone.bottom

  let join_return_zone node zone =
    ReturnZone.replace node (Zone.join zone (return_zone node))

  module CallwiseStatesWithFormals = State_builder.Hashtbl(Stmt.Hashtbl)
      (State_node.Hashtbl.Make
         (Kernel_function.Hashtbl.Make(Function_Froms.Memory)))
      (struct
        let name = "Callwise states with formals"
        let size = 97
        let dependencies = [ Ast.self; Db.Value.self ]
      end)

  let () = Ast.add_monotonic_state CallwiseStatesWithFormals.self

  let add_to_result stmt _ node data =
    if CallwiseStatesWithFormals.mem stmt
    then State_node.Hashtbl.replace (CallwiseStatesWithFormals.find stmt) node data
    else begin
      let new_tbl = State_node.Hashtbl.create 7 in
      State_node.Hashtbl.add new_tbl node data;
      CallwiseStatesWithFormals.replace stmt new_tbl
    end
  let add_to_result = State_node.with_stmt_state_node add_to_result

  module Computer (X: sig val graph: Nodegraph.t end) = struct

    type t = t'

    let substitute call_site_froms extra_loc deps =
      let subst_deps = Function_Froms.Memory.substitute call_site_froms deps in
      Function_Froms.Deps.add_indirect_dep subst_deps extra_loc

    let pretty_path_deps fmt v =
      Format.fprintf fmt "Path dependencies map : %a@\n"
        ZoneStmtMap.pretty v.path_deps_table;
      Format.fprintf fmt
        "Path dependencies union : %a@\n"
        Zone.pretty
        v.path_deps

    (** Prints the bytes of the first tis-mkfs file that are not in the
        additional dependencies, which means they can be generalized while
        preserving the reachability oof the current statement.
        The last byte printed is after the last dependency, which means that any bytes
        after it can also be generalized. *)
    let pretty_file_generalizable_bytes fmt v =
      let z = v.additional_deps in
      let z =
        Zone.filter_base
          (fun b ->
             try
               (Base.to_varinfo b).vname = "fc_file_contents_1_array"
             with Base.Not_a_C_variable -> false)
          z
      in
      let ok = ref false in
      let charbits = Integer.to_int (Bit_utils.sizeofchar ()) in
      Format.fprintf fmt "GENERALIZABLE BYTES@.";
      Zone.fold_i
        (fun _ itvs _ ->
           assert (not !ok);
           let curr =
             Int_Intervals.fold
               (fun (a,b) curr ->
                  let a,b = Abstract_interp.Int.to_int a, Abstract_interp.Int.to_int b in
                  assert (curr mod charbits= 0);
                  assert (a mod charbits = 0);
                  for i = (curr/charbits) to (a/charbits)-1 do
                    Format.fprintf fmt "%d," i
                  done;
                  ok:=true;
                  b+1)
               itvs
               0
           in
           assert (curr mod charbits = 0);
           Format.fprintf fmt "\n%d@." (curr/charbits);
           ())
        z
        ();
      Format.fprintf fmt "END@."

    let pretty_additional fmt v =
      Format.fprintf fmt "Additional Variable Map : %a@\n"
        ZoneStmtMap.pretty v.additional_deps_table;
      Format.fprintf fmt
        "Additional Variable Map Set : %a@\n"
        Zone.pretty v.additional_deps

    let pretty fmt v =
      Function_Froms.Memory.pretty_ind_data fmt v.deps_table

    let transfer_conditional_exp node exp state =
      let additional = find node state.deps_table exp in
      let additional = Function_Froms.Deps.to_zone additional in
      assert (
        if (Zone.equal additional Zone.top)
        then
          let stmt = State_node.stmt node in
          let callstack = State_node.callstack node in
          ignore (From_parameters.error "Transfer cond: %a anything at %a %a@."
                    pretty state
                    Stmt.pretty stmt
                    Value_callstack.Callstack.pretty callstack);
          false
        else true);
      {state with
       additional_deps_table =
         ZoneStmtMap.add (State_node.stmt node) additional state.additional_deps_table;
       additional_deps =
         Zone.join additional state.additional_deps;
       path_deps_table =
         ZoneStmtMap.add (State_node.stmt node) additional state.path_deps_table;
       path_deps =
         Zone.join additional state.path_deps;
      }

    (** Handle an assignement [lv = ...], the dependencies of the right-hand
        side being stored in [deps_right]. *)
    let transfer_assign node lv deps_right state =
      (* The assigned location is [loc], whose address is computed from
         [deps]. *)
      let deps, loc, exact =
        lval_to_precise_loc_with_deps node ~for_writing:true lv
      in
      let deps_of_deps = Function_Froms.Memory.find state.deps_table deps in
      let all_indirect = Zone.join state.additional_deps deps_of_deps in
      let deps = Function_Froms.Deps.add_indirect_dep deps_right all_indirect in
      { state with deps_table =
                     Function_Froms.Memory.add_binding_precise_loc
                       ~exact state.deps_table loc deps }

    let transfer_instr value_state node (i: instr) (state: t') =
      !Db.progress ();
      let succs = Nodegraph.succ X.graph node in
      match i with
      | Set (lv, exp, _) ->
        let comp_vars = find node state.deps_table exp in
        assert (
          if (Function_Froms.Deps.equal comp_vars Function_Froms.Deps.top)
          then
            let stmt = State_node.stmt node in
            let callstack = State_node.callstack node in
            ignore (From_parameters.error "assign : anything at %a %a@."
                      Stmt.pretty stmt
                      Value_callstack.Callstack.pretty callstack);
            true
          else true);
        let froms = transfer_assign node lv comp_vars state in
        List.map (fun succ -> succ,froms) succs
      | Call (lvaloption,funcexp,argl,_) ->
        !Db.progress ();
        let funcexp_deps, called_vinfos =
          match funcexp.enode with
          | Lval (Var _, NoOffset) ->
            Zone.bottom, Value.Value_results.callees (State_node.stmt node)
          | _ -> (* Function pointers *)
            !Db.Value.expr_to_kernel_function_state
              value_state ~deps:(Some Zone.bottom) funcexp
        in
        (* dependencies for the evaluation of [funcexp] *)
        let funcexp_deps =
          Function_Froms.Memory.find state.deps_table funcexp_deps
        in
        let additional_deps = Zone.join state.additional_deps funcexp_deps
        in
        let path_deps = Zone.join state.path_deps funcexp_deps in
        let args_froms =
          List.map
            (fun arg ->
               (* TODO : dependencies on subfields for structs *)
               find node state.deps_table arg)
            argl
        in
        let states_with_formals = Kernel_function.Hashtbl.create 7 in
        let do_on kf =
          (*TODO change this*)
          let called_vinfo_name = (Kernel_function.get_vi kf).vname in
          if ("tis_deps_show_deps" = called_vinfo_name)
          then begin
            From_parameters.feedback "Dependencies at node %a : %a\n"
              State_node.pretty_no_state node
              pretty state;
            List.map (fun succ -> succ, state) succs
          end
          else if ("tis_deps_show_open_pathdeps" = called_vinfo_name)
          then begin
            From_parameters.feedback
              "Open path dependencies at node %a : %a\n"
              State_node.pretty_no_state node
              pretty_additional state;
            List.map (fun succ -> succ, state) succs
          end
          else if ("tis_deps_show_pathdeps" = called_vinfo_name)
          then begin
            From_parameters.feedback
              "Path dependencies at node %a : %a\n"
              State_node.pretty_no_state node
              pretty_path_deps state;
            List.map (fun succ -> succ, state) succs
          end
          else if ("tis_deps_show_file_generalizable_bytes" =
                   called_vinfo_name)
          then begin
            From_parameters.feedback
              "File generalizable bytes at node %a : %a\n"
              State_node.pretty_no_state node
              pretty_file_generalizable_bytes state;
            List.map (fun succ -> succ, state) succs
          end
          else if Ast_info.is_cea_function called_vinfo_name then
            List.map (fun succ -> succ,state) succs
          else
            let formal_args = Kernel_function.get_formals kf in
            let state_with_formals = ref state.deps_table in
            begin try
                List.iter2
                  (fun vi from ->
                     state_with_formals :=
                       Function_Froms.Memory.bind_var
                         vi from !state_with_formals;
                  ) formal_args args_froms;
              with Invalid_argument _ ->
                From_parameters.warning ~once:true ~current:true
                  "variadic call detected. Using only %d argument(s) for %a."
                  (min
                     (List.length formal_args)
                     (List.length args_froms))
                  Kernel_function.pretty kf
            end;
            begin
              if not (Db.From.Record_From_Graph_Callbacks.is_empty ())
              then
                Kernel_function.Hashtbl.replace states_with_formals
                  kf !state_with_formals
            end;
            let aux succ =
              let succ_cs = State_node.callstack succ in
              if (Value_callstack.Callstack.equal
                    succ_cs
                    (State_node.callstack node)
                 )
              then
                (*Call not using body : fetch cached result*)
                (* let _ = From_parameters.feedback
                     "Fetching cached call to kf %a at node %a\n"
                     Kernel_function.pretty kf
                     State_node.pretty_no_state node in*)
                let froms_call =
                  Kernel_function.Hashtbl.find (CachedCalls.find node) kf in
                match froms_call with
                | Value_types.Froms froms_call ->
                  let froms_call_table = froms_call.Function_Froms.deps_table in
                  if Function_Froms.Memory.is_bottom froms_call_table then
                    bottom_from
                  else
                    let subst_before_call =
                      substitute !state_with_formals additional_deps
                    in
                    (* From state just after the call,
                       but before the result assigment *)
                    let deps_after_call =
                      let before_call = state.deps_table in
                      let open Function_Froms in
                      let subst d = DepsOrUnassigned.subst subst_before_call d in
                      let call_substituted = Memory.map subst froms_call_table in
                      Memory.compose call_substituted before_call
                    in
                    let state = {state with deps_table = deps_after_call} in
                    (* Treatement for the possible assignement
                       of the call result *)
                    begin match lvaloption with
                      | None -> state
                      | Some lv ->
                        let return_from = froms_call.Function_Froms.deps_return in
                        let deps_ret = subst_before_call return_from in
                        transfer_assign node lv deps_ret state
                    end
                | Value_types.Closure f ->
                  {state with deps_table = f state.deps_table args_froms}
              else
                (*Call uses body*)
              if Kernel_function.equal kf (fst (List.hd succ_cs))
              then (* Function call is relevant to the successor node*)
                begin

                  let callstate= { deps_table = !state_with_formals;
                                   additional_deps_table =
                                     ZoneStmtMap.add (State_node.stmt node)
                                       additional_deps state.additional_deps_table;
                                   additional_deps = additional_deps;
                                   path_deps_table =
                                     ZoneStmtMap.add (State_node.stmt node)
                                       additional_deps state.path_deps_table;
                                   path_deps = path_deps
                                 } (*TODO check this*)
                  in
                  (*From_parameters.feedback "Calling %a with state %a\n"
                    Value_callstack.Callstack.pretty succ_cs
                    pretty callstate;*)
                  callstate
                end
              else (* Irrelevant function call for that particular successor *)
                bottom_from
            in
            List.map (fun succ -> (succ, aux succ)) succs
        in
        let f kf acc =
          List.map2
            (fun (v, st) (v', st2) ->
               assert (State_node.equal v v');
               (v, join st st2))
            acc
            (do_on kf)
        in
        let result = Kernel_function.Hptset.fold f called_vinfos
            (List.map (fun v -> v, bottom_from) succs)
        in
        if not (Db.From.Record_From_Graph_Callbacks.is_empty ())
        then
          add_to_result
            node
            states_with_formals;
        result
      | _ -> List.map (fun v -> v, state) succs

    let transfer_instr = State_node.with_state_node transfer_instr

    (* Eliminate additional variables originating from a control-flow branching
       statement closing at [s]. *)
    let eliminate_additional kf node data =
      let s = State_node.stmt node in
      let map = data.additional_deps_table in
      let map' =
        ZoneStmtMap.fold
          (fun k  _v acc_map ->
             if (Kernel_function.equal kf (Kernel_function.find_englobing_kf k)) &&
                (!Db.Postdominators.is_postdominator kf ~opening:k ~closing:s)
             then ZoneStmtMap.remove k acc_map
             else acc_map
          ) map map
      in
      if not (map == map') then
        { data with
          additional_deps_table = map';
          additional_deps = rebuild_additional_deps map';
        }
      else data

    let filter_edge node succnode d =
      if Value_callstack.Callstack.equal (State_node.callstack node) (State_node.callstack succnode)
      then
        let s = State_node.stmt node in
        let succ = State_node.stmt succnode in
        let dt = d.deps_table in
        let opened = Kernel_function.blocks_opened_by_edge s succ in
        let closed = Kernel_function.blocks_closed_by_edge s succ in
        let dt = List.fold_left bind_locals dt opened in
        let dt = List.fold_left unbind_locals dt closed in
        { d with deps_table = dt }
      else
        (*This is a cross-function edge, the work is done in doInstr or externalize *)
        d

    let doNode node data =
      let kf = fst (List.hd (State_node.callstack node)) in
      let s = State_node.stmt node in
      (* Filter out unreachable values *)
      if Function_Froms.Memory.is_bottom data.deps_table
      then [] (* stop propagation *)
      else begin
        let data = eliminate_additional kf node data in
        let propagate_to_succs new_ =
          let do_succ acc (succ, new_) =
            let new_ = filter_edge node succ new_ in (* factor by statement ?*)
            (succ, new_)::acc
          in
          List.fold_left do_succ [] new_
        in
        if not (Db.Value.is_reachable (State_node.state node))
        then []
        else begin
          let succs = Nodegraph.succ X.graph node in
          match s.skind with
          | Instr i ->
            propagate_to_succs (transfer_instr node i data)
          | If (exp,_,_,_) | Switch (exp,_,_,_) ->
            let t' = transfer_conditional_exp node exp data in
            propagate_to_succs (List.map (fun v -> v,t') succs)
          | Return (Some ({ enode = Lval v; _ }),_) ->
            let deps, target, _exact =
              lval_to_zone_with_deps node ~for_writing:false v
            in
            let z = Zone.join target deps in
            join_return_zone node z;
            propagate_to_succs (List.map (fun v -> v,data) succs)
          | Return _ ->
            propagate_to_succs (List.map (fun v -> v,data) succs)
          | Throw _ -> []
          | UnspecifiedSequence _ | Loop _ | Block _
          | Goto _ | Break _ | Continue _
          | TryExcept _ | TryFinally _ | TryCatch _ ->
            propagate_to_succs (List.map (fun v -> v,data) succs)
        end
      end

    (* Remove all local variables and formals from table *)
    let externalize return return_zone kf state =
      let deps_return =
        (match return.skind with
         | Return (Some ({ enode = Lval v; _ }),_) ->
           let z = return_zone in
           let deps = Function_Froms.Memory.find_precise state.deps_table z in
           let size = Bit_utils.sizeof (Cil.typeOfLval v) in
           Function_Froms.(Memory.add_to_return ~size deps)
         | Return (None,_) ->
           Function_Froms.Memory.default_return
         | _ -> assert false)
      in
      let accept base =
        let fundec = Kernel_function.get_definition kf in
        not (Base.is_formal_or_local base fundec)
      in
      let deps_table =
        Function_Froms.Memory.filter_base accept state.deps_table
      in
      { deps_return = deps_return;
        Function_Froms.deps_table = deps_table }

    let doEdge node succ t =
      let stmt = State_node.stmt node in
      match stmt.skind with
      | Return _ ->
        let (kf, kinstr) = List.hd (State_node.callstack node) in
        let return_froms = externalize stmt (return_zone node) kf t in
        let t = { t with deps_table = return_froms.Function_Froms.deps_table } in
        begin match kinstr with
          | Kglobal -> t
          (* this is a constructor, probably *)
          (* TODO what to do with retval in entry point ?*)
          | Kstmt s ->
            begin match s.skind with
              | Instr (Call (lvalopt,_,_,_)) ->
                begin match lvalopt with
                  | None -> t
                  | Some lval ->
                    transfer_assign succ lval (return_froms.Function_Froms.deps_return) t
                end
              | _ -> assert false
            end
        end
      | _ -> t

    let computeFirstPredecessor _ t = t
    let combinePredecessors _node ~old new_ =
      let joined, inc = join_and_is_included new_ old in
      if inc then None else Some joined

  end

  let compute () =
    if compute_whole_program () then
      begin
        From_parameters.feedback "====== COMPUTING WHOLE-PROGRAM DEPENDENCIES ======@\n";
        let graph = Db.Value.whole_program_graph () in
        let initial_nodes = next_nodes graph (Db.Value.InitialNodes.get ()) in
        let graph = contract_graph graph in
        let module Computer = Computer(struct let graph = graph end) in
        let module DataflowArg = struct
          include Computer
          let graph = graph
          let name = "Whole-program From analysis"
          let debug = true
          let copy t = t
          module NodeStartData = Graph_dataflow.StartData
              (struct type t = Computer.t let size = 97 end)
        end
        in
        let module Compute = Graph_dataflow.Forwards(DataflowArg) in
        State_node.Set.iter
          (fun node -> DataflowArg.NodeStartData.add node empty_from) initial_nodes;
        Compute.compute (State_node.Set.elements initial_nodes);
        From_parameters.feedback "====== END OF WHOLE-PROGRAM DEPENDENCIES ======@\n"
      end
  let () =
    Db.Value.Call_Type_Value_Callbacks.extend store_call_froms;
    Db.Main.extend compute
end



module Make (To_Use: To_Use) =
struct
  type t' =
    { additional_deps_table : ZoneStmtMap.t;
      (** Additional control dependencies to add to all modified variables,
          coming from the control statements encountered so far (If, Switch).
          The statement information is used to remove the dependencies that
          are no longer useful, when we reach a statement that post-dominates
          the statement that gave rise to the dependency. *)
      additional_deps : Zone.t;
      (** Union of the sets in {!additional_deps_table} *)
      deps_table : Function_Froms.Memory.t
      (** dependency table *)
    }

  let call_stack : kernel_function Stack.t = Stack.create ()
  (** Stack of function being processed *)

  (** Recreate the [additional_deps] field from [additional_deps_table] *)
  let rebuild_additional_deps map =
    ZoneStmtMap.fold (fun _ z accz -> Zone.join z accz) map Zone.bottom

  (** given a [Function_Froms.Deps.t], apply [f] on both components and merge
      the result:
        depending directly on an indirect dependency -> indirect,
        depending indirectly on a direct dependency  -> indirect *)
  let merge_deps f deps =
    let open Function_Froms.Deps in
    let ind = f deps.indirect in
    let data = f deps.data in
    let ind = Zone.join data.indirect (to_zone ind) in
    let data = data.data in
    { data = data; indirect = ind }


  (** Bind all the variables of [b] to [Assigned \from \nothing]. This function
      is always called on local variables, which are semantically assigned to
      "uninitialized". *)
  let bind_locals m b =
    let aux_local acc vi =
      Cil.CurrentLoc.set vi.vdecl;
      (* Consider that local are initialized to a constant value *)
      Function_Froms.Memory.bind_var vi Function_Froms.Deps.bottom acc
    in
    let loc = Cil.CurrentLoc.get () in

    let r = List.fold_left aux_local m b.blocals in
    Cil.CurrentLoc.set loc;
    r

  let unbind_locals m b =
    let aux_local acc vi =
      Function_Froms.Memory.unbind_var vi acc
    in
    List.fold_left aux_local m b.blocals


  let find stmt deps_tbl expr =
    let state = To_Use.get_value_state stmt in
    let pre_trans = find_deps_no_transitivity state expr in
    merge_deps
      (fun d -> Function_Froms.Memory.find_precise deps_tbl d) pre_trans

  let lval_to_zone_with_deps stmt ~for_writing lv =
    let state = To_Use.get_value_state stmt in
    !Db.Value.lval_to_zone_with_deps_state
      state ~deps:(Some Zone.bottom) ~for_writing lv

  let lval_to_precise_loc_with_deps stmt ~for_writing lv =
    let state = To_Use.get_value_state stmt in
    let deps, loc =
      !Db.Value.lval_to_precise_loc_with_deps_state
        state ~deps:(Some Zone.bottom) lv
    in
    let exact = Precise_locs.valid_cardinal_zero_or_one ~for_writing loc in
    deps, loc, exact

  let empty_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.empty }

  let bottom_from =
    { additional_deps_table = ZoneStmtMap.empty;
      additional_deps = Zone.bottom;
      deps_table = Function_Froms.Memory.bottom }

  module Computer = struct

    type t = t'
    let bottom = bottom_from;;

    let callwise_states_with_formals = Stmt.Hashtbl.create 7

    let substitute call_site_froms extra_loc deps =
      let subst_deps = Function_Froms.Memory.substitute call_site_froms deps in
      Function_Froms.Deps.add_indirect_dep subst_deps extra_loc

    let display_one_from fmt v =
      Function_Froms.Memory.pretty fmt v.deps_table;
      Format.fprintf fmt "Additional Variable Map : %a@\n"
        ZoneStmtMap.pretty v.additional_deps_table;
      Format.fprintf fmt
        "Additional Variable Map Set : %a@\n"
        Zone.pretty
        v.additional_deps

    let pretty fmt (v: t) =
      display_one_from fmt v

    let transfer_conditional_exp s exp state =
      let additional = find s state.deps_table exp in
      let additional = Function_Froms.Deps.to_zone additional in
      {state with
       additional_deps_table =
         ZoneStmtMap.add s additional state.additional_deps_table;
       additional_deps =
         Zone.join additional state.additional_deps }


    let join_and_is_included new_ old =
      let additional_map, additional_zone, included =
        let mold = old.additional_deps_table in
        let mnew = new_.additional_deps_table in
        let zold = old.additional_deps in
        let m = ZoneStmtMap.join mnew mold in
        if ZoneStmtMap.equal m mold then
          mold, zold, true
        else
          let new_z = Zone.join old.additional_deps new_.additional_deps in
          m, new_z, false
      in
      let map, included' =
        Function_Froms.Memory.join_and_is_included
          new_.deps_table old.deps_table
      in
      { deps_table = map;
        additional_deps_table = additional_map;
        additional_deps = additional_zone; },
      included && included'

    let join old new_ = fst (join_and_is_included old new_)
    let is_included old new_ = snd (join_and_is_included old new_)

    (** Handle an assignement [lv = ...], the dependencies of the right-hand
        side being stored in [deps_right]. *)
    let transfer_assign stmt lv deps_right state =
      (* The assigned location is [loc], whose address is computed from
         [deps]. *)
      let deps, loc, exact =
        lval_to_precise_loc_with_deps stmt ~for_writing:true lv
      in
      let deps_of_deps = Function_Froms.Memory.find state.deps_table deps in
      let all_indirect = Zone.join state.additional_deps deps_of_deps in
      let deps = Function_Froms.Deps.add_indirect_dep deps_right all_indirect in
      { state with deps_table =
                     Function_Froms.Memory.add_binding_precise_loc
                       ~exact state.deps_table loc deps }

    let transfer_instr stmt (i: instr) (state: t) =
      !Db.progress ();
      match i with
      | Set (lv, exp, _) ->
        let comp_vars = find stmt state.deps_table exp in
        transfer_assign stmt lv comp_vars state

      (** Variadic: handling the va_arg macro. *)
      | Call
          (None,
           { enode = Lval (Var { vname = "__builtin_va_arg"; _ }, NoOffset);
             _ },
           [ { enode =
                 Lval ((Var _va_list_varinfo, _va_list_offset) as va_list_lv);
               _ } as _va_list_exp;
             { enode = SizeOf _expected_arg_typ; _} as _size_exp;
             { enode = CastE (voidPtrType, { enode = AddrOf dest_lv; _ });
               _ } as _dest_exp ],
           _loc)
        when Cil.isVoidPtrType voidPtrType ->
        (* Set the destination lvalue dependencies to the dependencies of the
           next variadic argument. *)
        (* 1. Get the next variadic argument's dependencies. *)
        let variadic_argument_deps : Function_Froms.Deps.deps =
          let value_state = To_Use.get_value_state stmt in
          let arg_zone : Locations.Zone.t =
            let arg_loc : Locations.location =
              let va_list_loc =
                !Db.Value.lval_to_loc_state value_state va_list_lv
              in
              Value.Value_variadic.arg_loc_of_va_list_loc
                value_state va_list_loc
            in
            Locations.enumerate_valid_bits ~for_writing:false arg_loc
          in
          Function_Froms.Memory.find_precise state.deps_table arg_zone
        in
        (* 2. Set the destination lvalue dependencies. *)
        transfer_assign stmt dest_lv variadic_argument_deps state

      | Call (lvaloption,funcexp,argl,_) ->
        !Db.progress ();
        let value_state = To_Use.get_value_state stmt in
        let funcexp_deps, called_vinfos =
          match funcexp.enode with
          | Lval (Var _, NoOffset) -> (* Normal function calls. *)
            Zone.bottom, Value.Value_results.callees stmt
          | _ -> (* Function pointers. *)
            !Db.Value.expr_to_kernel_function_state
              value_state ~deps:(Some Zone.bottom) funcexp
        in
        (* dependencies for the evaluation of [funcexp] *)
        let funcexp_deps =
          Function_Froms.Memory.find state.deps_table funcexp_deps
        in
        let additional_deps =
          Zone.join
            state.additional_deps
            funcexp_deps
        in
        let args_froms =
          List.map
            (fun arg ->
               (* TODO : dependencies on subfields for structs *)
               find stmt state.deps_table arg)
            argl
        in
        let states_with_formals = ref [] in
        let do_on kf =
          let called_vinfo = Kernel_function.get_vi kf in
          if Ast_info.is_cea_function called_vinfo.vname then
            state
          else
            let froms_call =
              let { table_for_calls; _ } = List.hd !call_froms_stack in
              try Kinstr.Hashtbl.find table_for_calls (Kstmt stmt)
              with Not_found -> raise Call_did_not_take_place
            in
            let froms_call =
              try Kernel_function.Hashtbl.find froms_call kf
              with Not_found ->
                raise Call_did_not_take_place
            in
            let froms_call_table = froms_call.Function_Froms.deps_table in
            if Function_Froms.Memory.is_bottom froms_call_table then
              bottom_from
            else
              let formal_args = Kernel_function.get_formals kf in

              let state_with_formals : Function_Froms.Memory.t =

                (* Binding the function arguments to dependencies:
                   - each matching argument + deps pair is handled by the
                     [bind_arg_varinfo_to_deps] function;
                   - first we bind all the formal arguents to the
                     corresponding dependencies in the
                     [bind_formals_to_depss] function;
                   - then, if there are still depss left and the function:
                     - is variadic,
                     - is not a builtin,
                     - and has a definition,
                     we bind the variadic arguments to the remaining
                     dependencies in the [bind_variadics_to_depss] function.
                *)

                let bind_arg_varinfo_to_deps state arg_varinfo arg_deps =
                  Function_Froms.Memory.bind_var arg_varinfo arg_deps state
                in

                let rec bind_variadics_to_depss
                    state variadic_args args_depss =
                  match variadic_args, args_depss with
                  | [], [] -> state
                  | variadic_arg :: remaining_variadic_args,
                    arg_deps     :: remaining_args_depss ->
                    (* A matching pair: variadic argument varinfo + deps. *)
                    let state' =
                      bind_arg_varinfo_to_deps state variadic_arg arg_deps
                    in
                    (* The remaining variadic arguments and the remaining
                       dependencies. *)
                    bind_variadics_to_depss state'
                      remaining_variadic_args remaining_args_depss
                  | _variadic_args, [] ->
                    (* More variadic arguments than depss.
                       NOTE: This case should never happen. *)
                    From_parameters.fatal ~current:true
                      "more variadic arguments than depss"
                  | [], _depss ->
                    (* More depss than variadic arguments.
                       NOTE: This case should never happen. *)
                    From_parameters.fatal ~current:true
                      "more depss than variadic arguments"
                in

                let rec bind_formals_to_depss state formal_args args_depss =
                  match formal_args, args_depss with
                  | [], [] -> state
                  | formal_arg :: remaining_formal_args,
                    arg_deps   :: remaining_args_depss ->
                    (* A matching pair: formal argument varinfo and deps. *)
                    let state' =
                      bind_arg_varinfo_to_deps state formal_arg arg_deps
                    in
                    (* The remaining formal arguments and the remaining
                       dependencies. *)
                    bind_formals_to_depss state'
                      remaining_formal_args remaining_args_depss
                  | _formal_args, [] ->
                    (* Not enough dependencies for formal arguments.
                       NOTE: This case should never happen. *)
                    From_parameters.fatal ~current:true
                      "not enough deps for formal arguments of a function"
                  | [], _depss
                    when not (Value.Value_util.is_function_variadic kf) ->
                    (* More deps than formal arguments, but the function
                       is not variadic. *)
                    let arguments_count =
                      min (List.length formal_args) (List.length args_froms)
                    in
                    From_parameters.warning ~once:true ~current:true
                      "Too many arguments for function. Using only %d \
                       argument%s."
                      arguments_count
                      (if arguments_count = 1 then "" else "s");
                    state
                  | [], _depss
                    when Value.Builtins.is_function_a_builtin kf ->
                    (* More deps than formal arguments, the function is
                       officially variadic, but it is a built-in. *)
                    state
                  | [], _depss
                    when not (Kernel_function.is_definition kf) ->
                    (* More deps than formal arguments, the function is
                       viariadic, but it has no definition. *)
                    From_parameters.debug ~current:true
                      "Variadic call for function %a without definition."
                      Kernel_function.pretty kf;
                    (* TODO: Can we handle this case better? *)
                    state
                  | [], depss ->
                    (* The variadic case: we bind the remaining froms to the
                       variables corresponding to the variadic arguments. *)
                    let variadic_varinfos =
                      let call_site = Kstmt stmt in
                      Value.Value_variadic.get_variadic_arguments_varinfos
                        kf call_site
                    in
                    bind_variadics_to_depss state variadic_varinfos depss
                in

                bind_formals_to_depss
                  state.deps_table formal_args args_froms
              in

              if not (Db.From.Record_From_Callbacks.is_empty ()) then
                states_with_formals :=
                  (kf, state_with_formals) :: !states_with_formals;
              let subst_before_call =
                substitute state_with_formals additional_deps
              in
              (* From state just after the call,
                 but before the result assigment *)
              let deps_after_call =
                let before_call = state.deps_table in
                let open Function_Froms in
                let subst d = DepsOrUnassigned.subst subst_before_call d in
                let call_substituted = Memory.map subst froms_call_table in
                Memory.compose call_substituted before_call
              in
              let state = {state with deps_table = deps_after_call } in
              (* Treatement for the possible assignement
                 of the call result *)
              match lvaloption with
              | None -> state
              | Some lv ->
                let return_from = froms_call.Function_Froms.deps_return in
                let deps_ret = subst_before_call return_from in
                transfer_assign stmt lv deps_ret state
        in
        let f f acc =
          let p = do_on f in
          match acc with
          | None -> Some p
          | Some acc_memory ->
            Some
              {state with
               deps_table = Function_Froms.Memory.join
                   p.deps_table
                   acc_memory.deps_table}
        in
        let result =
          try
            (match Kernel_function.Hptset.fold f called_vinfos None with
             | None -> state
             | Some s -> s);
          with Call_did_not_take_place -> state
        in
        if not (Db.From.Record_From_Callbacks.is_empty ())
        then
          Stmt.Hashtbl.replace
            callwise_states_with_formals
            stmt
            !states_with_formals;
        result
      | _ -> state


    let transfer_guard s e d =
      let value_state = To_Use.get_value_state s in
      let interpreted_e =
        !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode value_state e
      in
      let t1 = unrollType (typeOf e) in
      let do_then, do_else =
        if isIntegralType t1 || isPointerType t1
        then Cvalue.V.contains_non_zero interpreted_e,
             Cvalue.V.contains_zero interpreted_e
        else true, true (* TODO: a float condition is true iff != 0.0 *)
      in
      (if do_then then d else bottom),
      (if do_else then d else bottom)
    ;;

    (* Eliminate additional variables originating from a control-flow branching
       statement closing at [s]. *)
    let eliminate_additional s data =
      let kf = Stack.top call_stack in
      let map = data.additional_deps_table in
      let map' =
        ZoneStmtMap.fold
          (fun k _v acc_map ->
             if !Db.Postdominators.is_postdominator kf ~opening:k ~closing:s
             then ZoneStmtMap.remove k acc_map
             else acc_map
          ) map map
      in
      if not (map == map') then
        { data with
          additional_deps_table = map';
          additional_deps = rebuild_additional_deps map';
        }
      else data

    let transfer_stmt s data =
      let data = eliminate_additional s data in
      let map_on_all_succs new_data =
        List.map (fun x -> (x,new_data)) s.succs
      in
      match s.skind with
      | Instr i -> map_on_all_succs (transfer_instr s i data)

      | If(exp,_,_,_) ->
        let data = transfer_conditional_exp s exp data in
        Dataflows.transfer_if_from_guard transfer_guard s data
      | Switch(exp,_,_,_) ->
        let data = transfer_conditional_exp s exp data in
        Dataflows.transfer_switch_from_guard transfer_guard s data

      | Return _ | Throw _ -> []

      | UnspecifiedSequence _ | Loop _ | Block _
      | Goto _ | Break _ | Continue _
      | TryExcept _ | TryFinally _ | TryCatch _ ->
        map_on_all_succs data
    ;;

    (* Filter out unreachable values. *)
    let transfer_stmt s d =
      if Db.Value.is_reachable (To_Use.get_value_state s) &&
         not (Function_Froms.Memory.is_bottom d.deps_table)
      then transfer_stmt s d
      else []

    let doEdge s succ d =
      if Db.Value.is_reachable (To_Use.get_value_state succ)
      then
        let dt = d.deps_table in
        let opened = Kernel_function.blocks_opened_by_edge s succ in
        let closed = Kernel_function.blocks_closed_by_edge s succ in
        let dt = List.fold_left bind_locals dt opened in
        let dt = List.fold_left unbind_locals dt closed in
        { d with deps_table = dt }
      else
        bottom_from

    (* Filter the outgoing data using doEdge. *)
    let transfer_stmt s d =
      let ds = transfer_stmt s d in
      List.map (fun (succ, d) -> (succ, doEdge s succ d)) ds
    ;;

  end


  (* Remove all local variables and formals from table *)
  let externalize return kf state =
    let deps_return =
      (match return.skind with
       | Return (Some ({ enode = Lval v; _ }),_) ->
         let deps, target, _exact =
           lval_to_zone_with_deps ~for_writing:false return v
         in
         let z = Zone.join target deps in
         let deps = Function_Froms.Memory.find_precise state.deps_table z in
         let size = Bit_utils.sizeof (Cil.typeOfLval v) in
         Function_Froms.(Memory.add_to_return ~size deps)
       | Return (None,_) ->
         Function_Froms.Memory.default_return
       | _ -> assert false)
    in
    let accept base =
      let fundec = Kernel_function.get_definition kf in
      not (Base.is_formal_or_local base fundec)
    in
    let deps_table =
      Function_Froms.Memory.filter_base accept state.deps_table
    in
    { deps_return = deps_return;
      Function_Froms.deps_table = deps_table }

  let compute_using_cfg kf =
    match kf.fundec with
    | Declaration _ -> assert false
    | Definition (f,_) ->
      if !Db.Value.no_results kf then Function_Froms.top
      else
        try
          Stack.iter
            (fun g ->
               if kf == g then begin
                 if Db.Value.ignored_recursive_call kf then
                   From_parameters.error
                     "during dependencies computations for %a, \
                      ignoring probable recursive"
                     Kernel_function.pretty kf;
                 raise Exit
               end)
            call_stack;
          Stack.push kf call_stack;
          let state =
            { empty_from with
              deps_table = bind_locals empty_from.deps_table f.sbody }
          in
          let module Fenv =
            (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV)
          in
          let module Dataflow_arg = struct
            include Computer
            let init = [(Kernel_function.find_first_stmt kf, state)]
          end
          in
          let module Compute = Dataflows.Simple_forward(Fenv)(Dataflow_arg) in
          let return_stmt = Kernel_function.find_return kf in
          if not (Db.From.Record_From_Callbacks.is_empty ())
          then begin
            if From_parameters.DepsShowProgress.get()
            then From_parameters.feedback "Now calling From callbacks";
            let states =
              Stmt.Hashtbl.create Fenv.nb_stmts
            in
            Compute.iter_on_result (fun k record ->
                Stmt.Hashtbl.add states k record.deps_table);
            Db.From.Record_From_Callbacks.apply
              (call_stack, states,
               Dataflow_arg.callwise_states_with_formals,
               To_Use.get_value_state)
          end;
          let _popped = Stack.pop call_stack in
          let last_from =
            try
              if Db.Value.is_reachable (To_Use.get_value_state return_stmt)
              then
                externalize
                  return_stmt
                  kf
                  Compute.before.(Fenv.to_ordered return_stmt)
              else
                raise Not_found
            with Not_found -> begin
                From_parameters.result
                  "Non-terminating function %a (no dependencies)"
                  Kernel_function.pretty kf;
                { Function_Froms.deps_return =
                    Function_Froms.Memory.default_return;
                  deps_table = Function_Froms.Memory.bottom }
              end
          in
          last_from

        with Exit (* Recursive call *) ->
          { Function_Froms.deps_return = Function_Froms.Memory.default_return;
            deps_table = Function_Froms.Memory.empty }

  let compute_using_prototype kf =
    let state = Db.Value.get_initial_state kf in
    compute_using_prototype_for_state state kf

  let compute_and_return kf =
    let call_site_loc = CurrentLoc.get () in
    if From_parameters.DepsShowProgress.get()
    then From_parameters.feedback
        "Computing for function %a%s"
        Kernel_function.pretty kf
        (let s = ref "" in
         Stack.iter
           (fun kf ->
              s := !s^" <-"^(Pretty_utils.sfprintf "%a" Kernel_function.pretty kf))
           call_stack;
         !s);
    !Db.progress ();
    let result =
      if !Db.Value.use_spec_instead_of_definition kf
      then compute_using_prototype kf
      else (compute_using_cfg kf )
    in
    if From_parameters.DepsShowProgress.get()
    then
      From_parameters.feedback
        "Done for function %a" Kernel_function.pretty kf;
    !Db.progress ();
    CurrentLoc.set call_site_loc;
    result

  let compute kf =
    !Db.Value.compute ();
    ignore (compute_and_return kf)

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
