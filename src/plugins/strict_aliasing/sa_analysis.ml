(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype
open Cil_types
open Sa_helpers
open Sa_types
open State_node

type access = Read | Write
(** Describe how the memory is accessed. *)

module type AnalysisInformation = sig
  val state : Db.Value.state
  val callstack : Value_callstack.callstack
end

(* Main module of the Strict Aliasing analysis. *)

module type StrictAliasingAnalysisS = sig
  val check_expression : access:access -> Lmap_pointer_aliasing.t -> exp -> unit
  val check_instruction : Lmap_pointer_aliasing.t -> instr -> unit
  val handle_instruction :
    Lmap_pointer_aliasing.t -> instr -> Lmap_pointer_aliasing.t
end

module StrictAliasingAnalysis(Info : AnalysisInformation)
  : StrictAliasingAnalysisS =
struct

  let compare_effective_type ?(allow_eq=false) ~warning_emitter et1 et2 =
    let open EffectiveType in
    match et1, et2 with
    | Top, _ | _, Top ->
      Sa_parameters.warning ~current:true
        "compare_effective_type: (%a, %a) results in TOP.@."
        EffectiveType.pretty et1 EffectiveType.pretty et2
    | _ when allow_eq && EffectiveType.equal et1 et2 ->
      Sa_parameters.log ~verbose:3 ~current:true
        "compare_effective_type: allow_eq set to true and equality detected.@.\
         et1: %a@.\
         et2: %a@."
        EffectiveType.pretty et1 EffectiveType.pretty et2;
      ()
    | x, y when is_may_alias x || is_may_alias y || is_void_type x ||
                is_void_type y || is_character_type x || is_character_type y ||
                is_bottom x || is_bottom y || is_first_access_type x ||
                is_first_access_type y ->
      ()
    | _ ->
      try
        let compatible = EffectiveType.is_compatible et1 et2
        (* EffectiveType.is_compatible et2 et1 *)
        in
        if not compatible then warning_emitter ()
      with ImpreciseIndex idx ->
        Sa_parameters.warning ~current:true ~once:true
          "@[Effective type contains an imprecise index array: %a@.@]\
           et1: %a@.\
           et2: %a@.\
           Callstack: %a@."
          Ival.pretty idx
          EffectiveType.pretty et1
          EffectiveType.pretty et2
          Value.Value_util.pretty_call_stack Info.callstack

  let invalid_pointer_type expr ety () =
    let keep_cast = true in
    Sa_parameters.warning ~current:true ~once:true
      "@[The@ pointer@ %a@ has@ type@ %a.@ It@ violates@ strict@ aliasing@ \
       rules@ by@ accessing@ a@ cell@ with@ effective@ type@ %a.@]@\n\
       Callstack: %a@\n"
      Exp.pretty expr
      EffectiveType.pretty (snd (complete_typing ~keep_cast Info.state expr))
      EffectiveType.pretty ety
      Value.Value_util.pretty_call_stack Info.callstack

  let invalid_lvalue_access ~access lval ety () =
    Sa_parameters.warning ~current:true ~once:true
      "@[%s@ a@ cell@ with@ effective@ type@ %a through the lvalue %a \
       of type %a.@\nCallstack: %a@\n@]"
      (if access = Write then "Writing" else "Reading")
      EffectiveType.pretty ety
      Lval.pretty lval
      EffectiveType.pretty (snd (complete_typing_lval Info.state lval))
      Value.Value_util.pretty_call_stack Info.callstack

  let check_memory_access ~access lmap lval =
    let for_writing = access = Write in
    let check_ptr_zone exp exp_ty ptr_zone =
      let ptr_ety = lmap_find lmap ptr_zone in
      let ety = pointed_type ptr_ety in
      let base_ety =
        let location = !Db.Value.lval_to_loc_state Info.state lval in
        effective_type_of_location location
      in
      let pointed_exp_ty = pointed_type exp_ty in
      let warning_emitter = invalid_pointer_type exp base_ety in
      compare_effective_type ~allow_eq:true ~warning_emitter base_ety ety;
      compare_effective_type ~warning_emitter base_ety pointed_exp_ty
    in
    let check_lval_alloc lval_alloc =
      if access = Read then
        let _, cell_ety = complete_typing_lval Info.state lval in
        let expected_ety = lmap_find lmap lval_alloc in
        let warning_emitter = invalid_lvalue_access ~access lval expected_ety in
        compare_effective_type ~warning_emitter expected_ety cell_ety
    in
    match lval with
    | Var _, NoOffset
    | Var _, Field _
    | Var _, Index _ -> ()
    | Mem mem_expr, NoOffset ->
      let _, ptr_ty = complete_typing ~keep_cast:true Info.state mem_expr in
      if not (is_pointer_to_character ptr_ty) then begin
        let ptr_zone = match mem_expr.enode with
          | Lval lval -> ZoneLval.eval ~for_writing Info.state lval
          | _ -> ZoneExpr.eval ~for_writing Info.state mem_expr
        in
        let lval_alloc = ZoneLval.eval_allocated ~for_writing Info.state lval in
        check_ptr_zone mem_expr ptr_ty ptr_zone;
        check_lval_alloc lval_alloc;
      end
    | Mem _, Field ({ fcomp = { cstruct; _ }; _ }, _) ->
      let _, ety = complete_typing_lval Info.state lval in
      let zone =
        if access = Write then ZoneLval.eval_static ~for_writing Info.state lval
        else ZoneLval.eval ~for_writing Info.state lval
      in
      let lval_ety = lmap_find lmap zone in
      let use_ety, warning_emitter =
        if not cstruct then
          let base_ety =
            let location = !Db.Value.lval_to_loc_state Info.state lval in
            effective_type_of_location location
          in
          base_ety, invalid_lvalue_access ~access lval base_ety
        else begin
          let ety =
            if is_bottom lval_ety then
              let location = !Db.Value.lval_to_loc_state Info.state lval in
              effective_type_of_location location
            else lval_ety
          in
          ety, invalid_lvalue_access ~access lval ety
        end
      in
      compare_effective_type ~warning_emitter use_ety ety
    | _ -> ()

  let rec check_expression ~access lmap expression =
    match expression.enode with
    | Lval lval -> check_memory_access ~access lmap lval
    | SizeOfE e | AlignOfE e | UnOp (_, e, _) | CastE (_, e) | Info (e, _) ->
      check_expression ~access lmap e
    | BinOp (_, e1, e2, _) ->
      check_expression ~access lmap e1;
      check_expression ~access lmap e2
    | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ | AddrOf _ | StartOf _ -> ()

  let check_instruction lmap instruction =
    match instruction with
    | Set (lval, expr, _) ->
      check_memory_access ~access:Write lmap lval;
      check_expression ~access:Read lmap expr
    | Call (olval, fname, formals,_)
      when not (Sa_function_call.Builtins.is_sa_builtin_exp fname) ->
      Extlib.may (check_memory_access ~access:Write lmap) olval;
      check_expression ~access:Read lmap fname;
      List.iter (check_expression ~access:Read lmap) formals
    | Asm _ | Skip _ | Code_annot _ | Call _ -> ()

  let reduce_ety ety =
    let open SimpleType in
    let rec keep_sty = function
      | StructureField _ | Function _ | MayAlias | ArrayElement _ | Array _
      | Structure _ -> true
      | PointerType ty -> keep_sty ty
      | _ -> false
    in
    match ety with
    | EffectiveType.Top -> EffectiveType.Top
    | EffectiveType.EffectiveType set ->
      let set' = SimpleTypeSet.filter keep_sty set in
      EffectiveType.EffectiveType set'

  let set_lval_var_ptr lmap lval expr =
    match extract_pointer_variable expr with
    | Some ptr ->
      let rvalue = ZoneLval.eval ~for_writing:false Info.state ptr in
      let lvalue = ZoneLval.eval ~for_writing:true Info.state lval in
      let effective_type = lmap_find lmap rvalue in
      lmap_replace lmap lvalue effective_type
    | None ->
      let zone = ZoneExpr.eval ~for_writing:false Info.state expr in
      let expr_ety = Lmap_pointer_aliasing.find lmap zone in
      let write_ety ety =
        let lval_zone = ZoneLval.eval ~for_writing:true Info.state lval in
        lmap_replace lmap lval_zone ety
      in
      if is_bottom expr_ety
      then begin
        let _, ety = complete_typing ~keep_cast:false Info.state expr in
        let ety = reduce_ety ety in
        if is_bottom ety then lmap (* Static type will be used
                                      if this variable is accessed again. *)
        else write_ety ety
      end else write_ety expr_ety

  let set_lval_mem_nooffset lmap lval expr _ =
    let evaluate zone =
      let ety = lmap_find lmap zone in
      if is_bottom ety then (* We have no information about [expr]: static
                               type is used. *)
        match expr.enode with
        | Lval lval' ->
          let location = !Db.Value.lval_to_loc_state Info.state lval' in
          effective_type_of_location location
        | _ ->
          snd (complete_typing ~keep_cast:true Info.state expr)
      else ety
    in
    let allocated = ZoneLval.eval_allocated ~for_writing:true Info.state lval in
    let zone = ZoneExpr.eval ~for_writing:false Info.state expr in
    lmap_replace lmap allocated (evaluate zone)

  let generic_field_or_index lmap lval _expr =
    let zone = ZoneLval.eval_allocated ~for_writing:true Info.state lval in
    let _, ety = complete_typing_lval Info.state lval in
    lmap_replace lmap zone ety

  let instruction_set lmap lval expr =
    let expr = remove_pointer_casts expr in
    match lval with
    | Var _, _ when Cil.(isPointerType (typeOfLval lval)) ->
      set_lval_var_ptr lmap lval expr
    | Var _, NoOffset -> lmap
    | Mem mem_expr, NoOffset -> set_lval_mem_nooffset lmap lval expr mem_expr
    | _, (Field _ | Index _) -> generic_field_or_index lmap lval expr

  let update_formals lmap fname formals =
    let update_formal lmap vinfo formal =
      if Cil.isPointerType vinfo.vtype
      then instruction_set lmap (Cil.var vinfo) formal
      else lmap
    in
    let update_kf kf lmap =
      let kf_formals = Kernel_function.get_formals kf in
      let kf_formals =
        if Value.Value_util.is_function_variadic kf
        then
          let _, kinstr = List.hd Info.callstack in
          kf_formals @
          Value.Value_variadic.get_variadic_arguments_varinfos kf kinstr
        else kf_formals
      in
      let rec apply_updates lmap = function
        | [], _ -> lmap
        | _, [] -> assert false
        | x :: xs, y :: ys -> apply_updates (update_formal lmap x y) (xs, ys)
      in
      apply_updates lmap (kf_formals, formals)
    in
    match Kernel_function.get_called fname with
    | None ->
      let _, kf_set =
        !Db.Value.expr_to_kernel_function_state Info.state ~deps:None fname
      in
      Kernel_function.Hptset.fold update_kf kf_set lmap
    | Some kf -> update_kf kf lmap

  let is_memcpy_family fname =
    fname = "memcpy" || fname = "memmove" || fname = "mempcpy" ||
    fname = "wmemcpy" || fname = "wmemmove"

  let is_memset_family fname =
    fname = "memset" || fname = "wmemset"

  let instruction_call lmap olval fname formals  =
    let open Sa_function_call in
    let lmap = update_formals lmap fname formals in
    let handler : (module FunctionCallHandler) =
      match fname.enode with
      | Lval (Var { vname; _ }, _) ->
        begin match vname with
          | _ when is_memcpy_family vname -> (module MemcpyHandler)
          | _ when is_memset_family vname -> (module MemsetHandler)
          | _ when Builtins.is_dump_each vname -> (module Builtins.DumpEach)
          | _ when Builtins.is_show_each vname -> (module Builtins.ShowEach)
          | "fread" -> (module FreadHandler)
          | "read" -> (module ReadHandler)
          | "memset" -> (module MemsetHandler)
          | _ -> (module NoActionHandler)
        end
      | _ ->
        (module NoActionHandler)
    in
    let module Handler = (val handler) in
    Handler.handle lmap Info.state olval fname formals

  let handle_instruction lmap instruction =
    match instruction with
    | Set (lval, expr, _) when not (is_null expr) ->
      instruction_set lmap lval expr
    | Call (olval, f, formals, _) -> instruction_call lmap olval f formals
    | Asm _ | Skip _ | Code_annot _ | Set _ -> lmap

end

let make_analyzer state callstack : (module StrictAliasingAnalysisS) =
  let module Info : AnalysisInformation = struct
    let state = state
    let callstack = callstack
  end in
  (module StrictAliasingAnalysis(Info))



(* Dataflow *)

(* Keep information on computed nodes.
   Useful to find the fixpoint. *)
module ComputedNodes : sig
  val is_computed : State_node.t -> bool
  val mark_as_computed : State_node.t -> unit
end = struct
  module S = State_node.Set
  let computed_nodes = ref S.empty
  let is_computed node = S.mem node !computed_nodes
  let mark_as_computed node = computed_nodes := S.add node !computed_nodes
end


(* Association between key and data calculated during the analysis. *)
module Data : sig
  type key = State_node.t
  val get_state : key -> Lmap_pointer_aliasing.t
  val merge_state : key -> Lmap_pointer_aliasing.t -> unit
  val replace_state : key -> Lmap_pointer_aliasing.t -> unit
  val merge_state_included : key -> Lmap_pointer_aliasing.t -> bool
end = struct
  type key = State_node.t
  module S = State_node.Hashtbl

  let table = S.create 13

  let get_state key =
    try S.find table key with Not_found -> Lmap_pointer_aliasing.empty

  let merge_state key lmap =
    let old_lmap = get_state key in
    let new_lmap = Lmap_pointer_aliasing.join old_lmap lmap in
    S.replace table key new_lmap

  let replace_state key lmap =
    S.replace table key lmap

  let merge_state_included key lmap =
    let old_lmap = get_state key in
    let (new_lmap, included) =
      Lmap_pointer_aliasing.join_and_is_included old_lmap lmap
    in
    replace_state key new_lmap;
    included
end


(* Keep information of living nodes during the dataflow analysis.
   The storing process is done by the callback StoreLiveNodes. *)
module LivingNodes : sig
  val store_nodes : (Value_callstack.Callstack.t * Db.Value.storing_stage) -> unit
  val get_living_nodes : Value_callstack.Callstack.t -> State_node.Set.t
end = struct
  let living_nodes = ref State_node.Set.empty

  let merge_from_predecessors predecessor nodes =
    let lmap = Data.get_state predecessor in
    State_node.Set.iter (fun n -> Data.merge_state n lmap) nodes

  let store_nodes (_callstack, stage) = match stage with
    | Db.Value.StartingFunction (caller, nodes) ->
      begin match caller with
        | None -> ()
        | Some node -> merge_from_predecessors node nodes
      end;
      living_nodes := nodes;
    | Db.Value.ReturnOfFunction nodes ->
      State_node.Set.iter
        (fun n -> match State_node.action n with
           | Return (_, node) ->
             let node_set = State_node.Set.singleton n in
             merge_from_predecessors node node_set
           | _ -> assert false)
        nodes;
      living_nodes := nodes

  let get_living_nodes _callstack = !living_nodes
end


(** [to_postannot_and_merge ?lmap graph nodes] propagates the state [lmap] to
    successors of every [nodes] until reaching a postannot node for each path.*)
let to_postannot_and_merge ?lmap graph nodes =
  (* A queue is needed here to avoid stack overflow when there are too many
     nodes and states to propagate. *)
  let queue = Queue.create () in
  let rec run result =
    if Queue.is_empty queue then result
    else
      let vertex = Queue.pop queue in
      if State_node.action vertex = State_node.PostAnnot
      then run (State_node.Set.add vertex result)
      else begin
        if Nodegraph.mem_vertex graph vertex then begin
          let data =
            match lmap with
            | None -> Data.get_state vertex
            | Some lmap -> lmap
          in
          Nodegraph.iter_succ
            (fun vertex' ->
               Data.merge_state vertex' data;
               Queue.add vertex' queue)
            graph
            vertex
        end;
        run result
      end
  in
  State_node.Set.iter (fun n -> Queue.add n queue) nodes;
  run State_node.Set.empty


(* Dataflow2 functor parameter. *)
module Dataflow2_parameter(G: sig
    val graph : Nodegraph.t
    val callstack : Value_callstack.callstack
  end) =
struct
  type propagation = { mutable to_propagate : State_node.Set.t }

  let pretty fmt t =
    Format.fprintf fmt "to_propagate: @[%a@]@\n"
      State_node.Set.pretty t.to_propagate

  let is_not_return stmt = match stmt.skind with
    | Cil_types.Return _ -> false
    | _ -> true

  let propagate_to_successors node lmap t =
    let successors = Nodegraph.succ G.graph node in
    let node_set = State_node.Set.of_list successors in
    let successors = to_postannot_and_merge ~lmap G.graph node_set in
    let successors =
      State_node.Set.filter
        (fun n ->
           let lmap' = Data.get_state n in
           let fixpoint =
             Lmap_pointer_aliasing.is_included lmap lmap' &&
             ComputedNodes.is_computed n &&
             is_not_return (State_node.stmt n)
           in
           not fixpoint)
        successors
    in
    State_node.Set.union successors t

  let process_stmt_node_aux stmt lmap node t stmt' state =
    assert (Stmt.equal stmt stmt');
    if not (Db.Value.is_reachable state) then t
    else begin
      ComputedNodes.mark_as_computed node;
      let module Analyzer = (val make_analyzer state G.callstack) in
      let access = Read in
      let lmap' =
        match stmt.skind with
        | Instr instr ->
          Analyzer.check_instruction lmap instr;
          Analyzer.handle_instruction lmap instr
        | If (cond, _, _,_) ->
          Analyzer.check_expression ~access lmap cond;
          lmap
        | Switch (expr, _, _, _) ->
          Analyzer.check_expression ~access lmap expr;
          lmap
        | Goto _ | Break _ | Continue _ | Block _ | UnspecifiedSequence _
        | TryCatch _ | TryFinally _ | TryExcept _ | Loop _ | Throw _ ->
          lmap
        | Cil_types.Return (oe, _) ->
          Extlib.may_map
            (fun e ->
               Analyzer.check_expression ~access lmap e;
               lmap)
            ~dft:lmap
            oe
      in
      Data.replace_state node lmap';
      propagate_to_successors node lmap' t
    end

  let process_stmt_node stmt lmap node t =
    with_stmt_state (process_stmt_node_aux stmt lmap node t) node

  let doStmt stmt nodes =
    let to_propagate = nodes.to_propagate in
    nodes.to_propagate <- State_node.Set.empty;
    let to_propagate =
      State_node.Set.fold
        (fun n acc ->
           let lmap = Data.get_state n in
           process_stmt_node stmt lmap n acc)
        to_propagate
        State_node.Set.empty
    in
    if State_node.Set.is_empty to_propagate then Dataflow2.SDone
    else Dataflow2.SUse { to_propagate }

  let doInstr _ _ _ t = t

  let doEdge _stmt successor t =
    let stmt_eq n = Stmt.equal successor (State_node.stmt n) in
    { to_propagate = State_node.Set.filter stmt_eq t.to_propagate }

  let doGuard _ _ _ = Dataflow2.GDefault, Dataflow2.GDefault

  let computeFirstPredecessor _ t = t

  let combinePredecessors _ ~old new_ =
    let nodes = State_node.Set.union old.to_propagate new_.to_propagate in
    if State_node.Set.is_empty nodes then None
    else Some { to_propagate = nodes }
end


let initialize_globals node =
  let state = State_node.state node in
  let rec globals varinfo initinfo lmap =
    if Cil.isPointerType varinfo.vtype then
      let lvalue = Cil.var varinfo in
      match initinfo.init with
      | None -> lmap (* do something here ? *)
      | Some init_value -> init_globals lvalue varinfo.vdecl init_value lmap
    else lmap
  and init_globals lvalue locs init lmap = match init with
    | SingleInit e ->
      (* Empty callstack should not give any problem when initializing
         globals. *)
      let module Analyzer = (val make_analyzer state []) in
      let instr = Set (lvalue, e, locs) in
      Analyzer.handle_instruction lmap instr
    | CompoundInit (ct, initl) ->
      let doinit o i _ l = init_globals (Cil.addOffsetLval o lvalue) locs i l in
      Cil.foldLeftCompound ~implicit:true ~doinit ~ct ~initl ~acc:lmap
  in
  let lmap = Globals.Vars.fold globals Lmap_pointer_aliasing.empty in
  Data.merge_state node lmap


(* Called at every start of function or after a return statement. *)
let progress (callstack, graph) =
  let (kf, _kinstr) = List.hd callstack in
  let to_propagate = LivingNodes.get_living_nodes callstack in
  let to_propagate = to_postannot_and_merge graph to_propagate in
  match kf.fundec with
  | Declaration _ -> assert false
  | Definition _ when State_node.Set.cardinal to_propagate <> 0 ->
    let module Dataflow2_arg = struct
      include Dataflow2_parameter(struct
          let graph = graph
          let callstack = callstack
        end)
      type t = propagation
      module StmtStartData = Dataflow2.StartData(struct
          type t = propagation
          let size = 13
        end)
      let name = "strict_aliasing_dataflow"
      let debug = false
      let copy t = t
    end in
    let module Dataflow_compute = Dataflow2.Forwards(Dataflow2_arg) in
    let start_stmts =
      State_node.Set.fold
        (fun elt acc ->
           let stmt = State_node.stmt elt in
           Dataflow2_arg.(StmtStartData.add stmt { to_propagate });
           stmt :: acc)
        to_propagate
        []
    in
    Dataflow_compute.compute start_stmts
  | Definition _ -> ()



(* Entry points of the analysis *)

let register_builtins () =
  Cil.add_special_builtin_family Sa_function_call.Builtins.is_sa_builtin

let register_callbacks () =
  if Sa_parameters.StrictAliasing.get () then begin
    Db.Value.Record_Value_Globals_Init.extend_once initialize_globals;
    Db.Value.Record_Value_Graph_Callbacks_StoreLiveNodes.extend_once
      LivingNodes.store_nodes;
    Db.Value.Record_Value_Graph_Callbacks_Progress.extend_once progress
  end

let strict_aliasing_main () =
  if Sa_parameters.StrictAliasing.get () then
    !Db.Value.compute ()

let () =
  Cmdline.run_after_configuring_stage register_callbacks;
  register_builtins ();
  Db.Main.extend strict_aliasing_main


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
