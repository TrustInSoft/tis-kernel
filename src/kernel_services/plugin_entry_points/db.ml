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

open Cil_types
open Cil_datatype
open Extlib

let memo_dkey = Kernel.register_category "tis:memo"

let assert_false _ = assert false

type 'a how_to_journalize =
  | Journalize of string * 'a Type.t
  | Journalization_not_required
  | Journalization_must_not_happen of string

let register how_to_journalize r f =
  match how_to_journalize with
  | Journalize (name, ty) -> r := Journal.register ("!Db." ^ name) ty f
  | Journalization_not_required -> r := f
  | Journalization_must_not_happen name ->
    r := Journal.never_write ("!Db." ^ name) f

let register_compute name deps r f =
  let name = "!Db." ^ name in
  let f = Journal.register name (Datatype.func Datatype.unit Datatype.unit) f in
  let compute, self = State_builder.apply_once name deps f in
  r := compute;
  self

let register_guarded_compute name is_computed r f =
  let name = "!Db." ^ name in
  let f = Journal.register name (Datatype.func Datatype.unit Datatype.unit) f in
  let compute () = if not (is_computed ()) then f () in
  r := compute

module Main = struct
  include Hook.Make(struct end)
  let play = ref assert_false
end

module Toplevel = struct

  let run = ref (fun f -> f ())

end

(* ************************************************************************* *)
(** {2 Inouts} *)
(* ************************************************************************* *)

module type INOUTKF = sig
  type t
  val self_internal: State.t ref
  val self_external: State.t ref
  val compute : (kernel_function -> unit) ref

  val get_internal : (kernel_function -> t) ref
  val get_external : (kernel_function -> t) ref

  val display : (Format.formatter -> kernel_function -> unit) ref
  val pretty : Format.formatter -> t -> unit
end
module type INOUT = sig
  include INOUTKF
  val statement : (stmt -> t) ref
  val kinstr : kinstr -> t option
end

(** State_builder.of outputs
    - over-approximation of zones written by each function. *)
module Outputs = struct
  type t = Locations.Zone.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let compute = ref assert_false
  let display = ref assert_false
  let display_external = ref assert_false
  let get_internal = ref assert_false
  let get_external = ref assert_false
  let statement = ref assert_false
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end

(** State_builder.of read inputs
    - over-approximation of locations read by each function. *)
module Inputs = struct
  (*       What about [Inputs.statement] ? *)
  type t = Locations.Zone.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let self_with_formals = ref State.dummy
  let compute = ref assert_false
  let display = ref assert_false
  let display_with_formals = ref assert_false
  let get_internal = ref assert_false
  let get_external = ref assert_false
  let get_with_formals = ref assert_false
  let statement = ref assert_false
  let expr = ref assert_false
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end

(** State_builder.of operational inputs
    - over-approximation of zones whose input values are read by each function,
    State_builder.of sure outputs
    - under-approximation of zones written by each function. *)
module Operational_inputs = struct
  type t = Inout_type.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let compute = ref assert_false
  let display = ref assert_false
  let get_internal = ref assert_false
  let get_internal_precise = ref (fun ?stmt:_ _ ->
      failwith ("Db.Operational_inputs.get_internal_precise not implemented"))
  let get_external = ref assert_false

  module Record_Inout_Callbacks =
    Hook.Build (struct type t = Value_callstack.callstack * Inout_type.t end)

  let pretty fmt x =
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "@[<v 2>Operational inputs:@ @[<hov>%a@]@]@ "
      Locations.Zone.pretty (x.Inout_type.over_inputs);
    Format.fprintf fmt "@[<v 2>Operational inputs on termination:@ @[<hov>%a@]@]@ "
      Locations.Zone.pretty (x.Inout_type.over_inputs_if_termination);
    Format.fprintf fmt "@[<v 2>Sure outputs:@ @[<hov>%a@]@]"
      Locations.Zone.pretty (x.Inout_type.under_outputs_if_termination);
    Format.fprintf fmt "@]";

end

(** Derefs computations *)
module Derefs = struct
  type t = Locations.Zone.t
  let self_internal = ref State.dummy
  let self_external = ref State.dummy
  let compute = ref assert_false
  let display = ref assert_false
  let get_internal = ref assert_false
  let get_external = ref assert_false
  let statement = ref assert_false
  let kinstr ki = match ki with
    | Kstmt s -> Some (!statement s)
    | Kglobal -> None

  let pretty = Locations.Zone.pretty
end


(* ************************************************************************* *)
(** {2 Values} *)
(* ************************************************************************* *)

module Value = struct
  type state = Cvalue.Model.t
  type t = Cvalue.V.t

  (* This function is responsible for clearing completely Value's state
     when the user-supplied initial state or main arguments are changed.
     It is set deep inside Value  for technical reasons *)
  let initial_state_changed = ref assert_false

  (* Arguments of the root function of the value analysis *)
  module ListArgs = Datatype.List(Cvalue.V)
  module FunArgs =
    State_builder.Option_ref
      (ListArgs)
      (struct
        let name = "Db.Value.fun_args"
        let dependencies =
          [ Ast.self; Kernel.LibEntry.self; Kernel.MainFunction.self]
      end)
  let () = Ast.add_monotonic_state FunArgs.self


  exception Incorrect_number_of_arguments

  let fun_get_args () = FunArgs.get_option ()

  (* This function is *not* journalized *)
  let fun_set_args =
    let module L = Datatype.List(Cvalue.V) in
    Journal.register "(failwith \"Function cannot be journalized: \
                      Db.Value.fun_set_args\" : _ -> unit)"
      (Datatype.func L.ty Datatype.unit)
      (fun l ->
         if
           not
             (Extlib.opt_equal ListArgs.equal (Some l) (FunArgs.get_option ()))
         then begin
           !initial_state_changed ();
           FunArgs.set l
         end)


  let fun_use_default_args =
    Journal.register "Db.Value.fun_use_default_args"
      (Datatype.func Datatype.unit Datatype.unit)
      (fun () ->
         if FunArgs.get_option () <> None then
           (!initial_state_changed (); FunArgs.clear ()))


  (* Initial memory state of the value analysis *)
  module VGlobals =
    State_builder.Option_ref
      (Cvalue.Model)
      (struct
        let name = "Db.Value.Vglobals"
        let dependencies = [Ast.self]
      end)
  let () = Ast.add_monotonic_state VGlobals.self

  (* This function is *not* journalized *)
  let globals_set_initial_state =
    Journal.register "(failwith \"Function cannot be journalized: \
                      Db.Value.globals_set_initial_state\" : _ -> unit)"
      (Datatype.func Cvalue.Model.ty Datatype.unit)
      (fun state ->
         if not (Extlib.opt_equal Cvalue.Model.equal
                   (Some state)
                   (VGlobals.get_option ()))
         then begin
           !initial_state_changed ();
           VGlobals.set state
         end)


  let globals_use_default_initial_state =
    Journal.register
      "Db.Value.globals_use_default_initial_state"
      (Datatype.func Datatype.unit Datatype.unit)
      (fun () -> if VGlobals.get_option () <> None then
          (!initial_state_changed (); VGlobals.clear ()))

  let initial_state_only_globals = ref assert_false

  let globals_state () =
    match VGlobals.get_option () with
    | Some v -> v
    | None -> !initial_state_only_globals ()

  let globals_use_supplied_state () = not (VGlobals.get_option () = None)

  let size = 1789

  module States_by_callstack =
    Value_callstack.Callstack.Hashtbl.Make(Cvalue.Model)

  type before_after_state = state * state option
  module Table_By_Slevel =
    Cil_state_builder.Stmt_hashtbl
      (Datatype.Pair
         (Datatype.List
            (Datatype.Triple
               (Datatype.Array
                  (Datatype.Pair(Cvalue.Model)(Datatype.Option(Cvalue.Model))))
               (Datatype.Int)
               (Value_callstack.Callstack)))
         (Value_callstack.Callstack.Hashtbl.Make(Datatype.Int)))
      (struct
        let name = "Value analysis results by slevel"
        let size = size
        let dependencies =
          (* Do NOT add dependencies to Kernel parameters here, but at
             the top of Value/Value_parameters *)
          [ Ast.self;
            Alarms.self;
            Annotations.code_annot_state;
            FunArgs.self;
            VGlobals.self ]
      end)
  let () = Ast.add_monotonic_state Table_By_Slevel.self

  module Table_By_Callstack =
    Cil_state_builder.Stmt_hashtbl(States_by_callstack)
      (struct
        let name = "Value analysis results by callstack"
        let size = size
        let dependencies = [ Table_By_Slevel.self ]
      end)
  let () = Ast.add_monotonic_state Table_By_Callstack.self

  module Table =
    Cil_state_builder.Stmt_hashtbl(Cvalue.Model)
      (struct
        let name = "Value analysis results"
        let size = size
        let dependencies = [ Table_By_Callstack.self ]
      end)
  let () = Ast.add_monotonic_state Table.self

  module WholeProgramGraph =
    State_builder.Ref(Nodegraph)
      (struct
        let name = "Whole-program graph"
        let dependencies = [ Table_By_Slevel.self ]
        let default () = Nodegraph.Builder.empty ()
      end)
  let () = Ast.add_monotonic_state WholeProgramGraph.self

  module SerializableWholeProgramGraph = State_builder.Ref(Nodegraph.Serializable)
      (struct
        let name = "Serializable whole-program graph"
        let dependencies = [ Table_By_Slevel.self ]
        let default () = List.hd Nodegraph.Serializable.reprs
      end)
  let () = Ast.add_monotonic_state SerializableWholeProgramGraph.self

  let () =
    Project.register_before_save_hook
      (fun _ ->
         let g = WholeProgramGraph.get() in
         SerializableWholeProgramGraph.set(Nodegraph.Serializable.of_graph g);
      )
    ;
    Project.register_after_load_hook
      (fun _ ->
         let g = SerializableWholeProgramGraph.get () in
         let g = Nodegraph.Serializable.to_graph g in
         WholeProgramGraph.set g;
         SerializableWholeProgramGraph.clear ()
      )
  module InitialNodes =
    State_builder.Ref(State_node.Set)(struct
      let name = "Whole-program graph initial nodes"
      let dependencies = [ Table_By_Slevel.self ]
      let default () = State_node.Set.empty
    end)
  let () = Ast.add_monotonic_state InitialNodes.self

  let whole_program_graph = WholeProgramGraph.get

  let add_graph_vertex v = ignore (Nodegraph.Builder.add_vertex (whole_program_graph ()) v)
  let add_graph_edge v1 v2 = ignore (Nodegraph.Builder.add_edge (whole_program_graph ()) v1 v2)

  (* Clear Value's various caches each time [Db.Value.is_computed] is updated,
     including when it is set, reset, or during project change. Some operations
     of Value depend on -ilevel, -plevel, etc, so clearing those caches when
     Value ends ensures that those options will have an effect between two runs
     of Value. *)
  let () = Table_By_Callstack.add_hook_on_update
      (fun _ ->
         Cvalue.V_Offsetmap.clear_caches ();
         Cvalue.Model.clear_caches ();
         Locations.Location_Bytes.clear_caches ();
         Locations.Zone.clear_caches ();
         Function_Froms.Memory.clear_caches ();
      )


  module AfterTable_By_Callstack =
    Cil_state_builder.Stmt_hashtbl(States_by_callstack)
      (struct
        let name = "Value analysis results after states by callstack"
        let size = size
        let dependencies = [ Table_By_Slevel.self ]
      end)
  let () = Ast.add_monotonic_state AfterTable_By_Callstack.self

  module AfterTable =
    Cil_state_builder.Stmt_hashtbl(Cvalue.Model)
      (struct
        let name = "Value analysis after states"
        let dependencies = [AfterTable_By_Callstack.self]
        let size = size
      end)
  let () = Ast.add_monotonic_state AfterTable.self


  let self = Table_By_Slevel.self
  let only_self = [ self ]

  let mark_as_computed =
    Journal.register "Db.Value.mark_as_computed"
      (Datatype.func Datatype.unit Datatype.unit)
      (Table_By_Callstack.mark_as_computed ?project:None)

  let is_computed () = Table_By_Callstack.is_computed ()

  module Conditions_table =
    Cil_state_builder.Stmt_hashtbl
      (Datatype.Int)
      (struct
        let name = "Conditions statuses"
        let size = 101
        let dependencies = only_self
      end)
  let () = Ast.add_monotonic_state Conditions_table.self

  let merge_conditions h =
    Cil_datatype.Stmt.Hashtbl.iter
      (fun stmt v ->
         try
           let old = Conditions_table.find stmt in
           Conditions_table.replace stmt (old lor v)
         with Not_found ->
           Conditions_table.add stmt v)
      h

  let mask_then = 1
  let mask_else = 2

  let condition_truth_value s =
    try
      let i = Conditions_table.find s in
      ((i land mask_then) <> 0, (i land mask_else) <> 0)
    with Not_found -> false, false

  module RecursiveCallsFound =
    State_builder.Set_ref
      (Kernel_function.Set)
      (struct
        let name = "Db.Value.RecursiveCallsFound"
        let dependencies = only_self
      end)
  let () = Ast.add_monotonic_state RecursiveCallsFound.self

  let ignored_recursive_call kf =
    RecursiveCallsFound.mem kf

  let recursive_call_occurred kf =
    RecursiveCallsFound.add kf

  (* We do not store the order of callstacks but just the relative
     order of callranks with respect to their callstack.
     If this is wanted, a modification similar to Table_By_Slevel
     needs to be done.
  *)
  module Called_Functions_By_Slevel =
    State_builder.Hashtbl(Kernel_function.Hashtbl)
      (Value_callstack.Callstack.Hashtbl.Make(Datatype.List(Cvalue.Model)))
      (struct
        let name = "called_functions_by_slevel"
        let size = 11
        let dependencies = only_self
      end)
  let () = Ast.add_monotonic_state Called_Functions_By_Slevel.self

  module Called_Functions_By_Callstack_Memo =
    State_builder.Hashtbl(Kernel_function.Hashtbl)
      (Datatype.Option(States_by_callstack))
      (struct
        let name = "called_functions_by_callstack_memo"
        let size = 11
        let dependencies = [ Called_Functions_By_Slevel.self ]
      end)
  let () = Ast.add_monotonic_state Called_Functions_By_Callstack_Memo.self

  module Called_Functions_Memo =
    State_builder.Hashtbl(Kernel_function.Hashtbl)
      (Cvalue.Model)
      (struct
        let name = "called_functions_memo"
        let size = 11
        let dependencies = [ Called_Functions_By_Callstack_Memo.self ]
      end)
  let () = Ast.add_monotonic_state Called_Functions_Memo.self
(*
  let pretty_table () =
   Table.iter
      (fun k v ->
         Kernel.log ~kind:Log.Debug
           "GLOBAL TABLE at %a: %a@\n"
           Kinstr.pretty k
           Cvalue.Model.pretty v)

  let pretty_table_raw () =
    Kinstr.Hashtbl.iter
      (fun k v ->
         Kernel.log ~kind:Log.Debug
           "GLOBAL TABLE at %a: %a@\n"
           Kinstr.pretty k
           Cvalue.Model.pretty v)
*)

  type callstack = (kernel_function * kinstr) list

  let inside_callback = ref false
  let is_inside_callback () = !inside_callback

  module Hook_Build(P:sig type t end) =
  struct
    include Hook.Build(P)
    let apply x =
      inside_callback := true;
      apply x;
      inside_callback := false;
  end

  module Record_Value_Globals_Init =
    Hook_Build
      (struct
        type t = State_node.t
      end)

  module Record_Value_Callbacks =
    Hook_Build
      (struct
        type t =
          (kernel_function * kinstr) list * (state Stmt.Hashtbl.t) Lazy.t
      end)

  module Record_Value_Callbacks_New =
    Hook_Build
      (struct
        type t =
          (kernel_function * kinstr) list *
          ((state Stmt.Hashtbl.t) Lazy.t  * (state Stmt.Hashtbl.t) Lazy.t)
            Value_types.callback_result
      end)

  module Record_Value_Slevel_Callbacks =
    Hook_Build
      (struct
        type t =
          (kernel_function * kinstr) list
          * (before_after_state array * int) Stmt.Hashtbl.t Value_types.callback_result
      end)

  module Record_Value_After_Callbacks =
    Hook_Build
      (struct
        type t = (kernel_function * kinstr) list * (state Stmt.Hashtbl.t) Lazy.t
      end)

  module Record_Value_Superposition_Callbacks =
    Hook_Build
      (struct
        type t = (kernel_function * kinstr) list * (state list Stmt.Hashtbl.t) Lazy.t
      end)

  module Record_Value_Graph_Callbacks =
    Hook_Build
      (struct
        type t = (kernel_function * kinstr) list * (Nodegraph.t Value_types.callback_result)
      end)

  type storing_stage =
    | StartingFunction of State_node.t option * State_node.Set.t
    | ReturnOfFunction of State_node.Set.t

  module Record_Value_Graph_Callbacks_StoreLiveNodes =
    Hook_Build
      (struct
        type t =  (kernel_function * kinstr) list * storing_stage
      end)

  module Record_Value_Graph_Callbacks_Progress =
    Hook_Build
      (struct
        type t =  (kernel_function * kinstr) list * Nodegraph.t
      end)

  module Record_Post_Value_Callbacks =
    Hook_Build
      (struct
        type t = unit
      end)

  module Call_Value_Callbacks =
    Hook_Build
      (struct type t = state * (kernel_function * kinstr) list end)

  module Call_Type_Value_Callbacks =
    Hook_Build(struct
      type t = [`Builtin of Value_types.call_result | `Spec | `Def | `Memexec]
               * state * (kernel_function * kinstr) list * State_node.t option end)

  module Call_Value_Post_Spec_Callbacks =
    Hook_Build
      (struct type t = state * state * (kernel_function * kinstr) list end)

  module Compute_Statement_Callbacks =
    Hook_Build
      (struct type t = stmt * callstack * state list end)

  let no_results = ref assert_false

  let update_slevel_table stmt cs (array, widen) =
    let module H = Value_callstack.Callstack.Hashtbl in
    let module T = Table_By_Slevel in
    (* Maybe use amortized array. *)
    let old, htbl =
      try T.find stmt
      with Not_found ->
        [], H.create 7
    in
    H.replace htbl cs (try H.find htbl cs + 1 with Not_found -> 1);
    T.replace stmt ((array, widen, cs) :: old, htbl)

  let update_callstack_table ~after stmt callstack v =
    let open Value_callstack in
    let find,add =
      if after
      then AfterTable_By_Callstack.find, AfterTable_By_Callstack.add
      else Table_By_Callstack.find, Table_By_Callstack.add
    in
    try
      let by_callstack = find stmt in
      begin try
          let o = Callstack.Hashtbl.find by_callstack callstack in
          Callstack.Hashtbl.replace by_callstack callstack(Cvalue.Model.join o v)
        with Not_found ->
          Callstack.Hashtbl.add by_callstack callstack v
      end;
    with Not_found ->
      let r = Callstack.Hashtbl.create 7 in
      Callstack.Hashtbl.add r callstack v;
      add stmt r

  let merge_initial_state cs state =
    let open Value_callstack in
    let kf = match cs with (kf, _) :: _ -> kf | _ -> assert false in
    let by_callstack =
      try Called_Functions_By_Slevel.find kf
      with Not_found ->
        let h = Callstack.Hashtbl.create 7 in
        Called_Functions_By_Slevel.add kf h;
        h
    in
    try
      let old = Callstack.Hashtbl.find by_callstack cs in
      Callstack.Hashtbl.replace by_callstack cs (state :: old)
    with Not_found ->
      Callstack.Hashtbl.add by_callstack cs [state]

  let get_initial_state_callstack kf =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    try Called_Functions_By_Callstack_Memo.find kf
    with Not_found ->
      let result =
        try
          let open Value_callstack in
          let input = Called_Functions_By_Slevel.find kf in
          let output = Callstack.Hashtbl.create 7 in
          (* Couldn't find a Callstack.Hashtbl.map function. *)
          Callstack.Hashtbl.iter (fun cs states -> Callstack.Hashtbl.add output cs
                                     (List.fold_left Cvalue.Model.join Cvalue.Model.bottom states)) input;
          Some output
        with Not_found -> None
      in
      Called_Functions_By_Callstack_Memo.add kf result;
      result

  let get_initial_state kf =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    try Called_Functions_Memo.find kf
    with Not_found ->
      let state =
        match get_initial_state_callstack kf with
        | Some by_callstack ->
          Value_callstack.Callstack.Hashtbl.fold
            (fun _cs state acc -> Cvalue.Model.join acc state)
            by_callstack Cvalue.Model.bottom
        | None -> Cvalue.Model.bottom
      in
      Called_Functions_Memo.add kf state;
      state

  let valid_behaviors = ref assert_false

  let add_formals_to_state = ref assert_false

  let noassert_get_stmt_state s =
    if !no_results (Kernel_function.find_englobing_kf s)
    then Cvalue.Model.top
    else
      try Table.find s
      with Not_found ->
        Kernel.result ~dkey:memo_dkey "Table.find failed for stmt %d" s.sid;
        let ho = try Some (Table_By_Callstack.find s) with Not_found -> None in
        let state =
          match ho with
          | None -> Cvalue.Model.bottom
          | Some h ->
            Value_callstack.Callstack.Hashtbl.fold (fun _cs state acc ->
                Cvalue.Model.join acc state
              ) h Cvalue.Model.bottom
        in
        Table.add s state;
        state

  let noassert_get_state k =
    match k with
    | Kglobal -> globals_state ()
    | Kstmt s -> noassert_get_stmt_state s

  let get_stmt_state s =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    noassert_get_stmt_state s

  let get_state k =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    noassert_get_state k

  let get_stmt_state_callstack ~after stmt =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    try
      Some (if after then AfterTable_By_Callstack.find stmt else
              Table_By_Callstack.find stmt)
    with Not_found -> None

  let fold_stmt_state_callstack f acc ~after stmt =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    match get_stmt_state_callstack ~after stmt with
    | None -> acc
    | Some h -> Value_callstack.Callstack.Hashtbl.fold (fun _ -> f) h acc

  let fold_state_callstack f acc ~after ki =
    assert (is_computed ()); (* this assertion fails during value analysis *)
    match ki with
    | Kglobal -> f (globals_state ()) acc
    | Kstmt stmt -> fold_stmt_state_callstack f acc ~after stmt

  let is_reachable = Cvalue.Model.is_reachable

  exception Is_reachable
  let is_reachable_stmt stmt =
    if !no_results (Kernel_function.find_englobing_kf stmt)
    then true
    else
      let ho = try Some (Table_By_Callstack.find stmt) with Not_found -> None in
      match ho with
      | None -> false
      | Some h ->
        try
          Value_callstack.Callstack.Hashtbl.iter
            (fun _cs state ->
               if Cvalue.Model.is_reachable state
               then raise Is_reachable) h;
          false
        with Is_reachable -> true

  let is_accessible ki =
    match ki with
    | Kglobal -> Cvalue.Model.is_reachable (globals_state ())
    | Kstmt stmt -> is_reachable_stmt stmt

  let is_called = ref assert_false
  let callers = ref assert_false

  let access_location = ref assert_false

  let find state loc = snd (Cvalue.Model.find state loc)

  let access =  ref assert_false
  let access_expr =  ref assert_false

  (** Type for a Value builtin function *)

  type builtin_sig =
    state ->
    (Cil_types.exp * Cvalue.V.t * Cvalue.V_Offsetmap.t) list ->
    Value_types.call_result

  exception Outside_builtin_possibilities
  let register_builtin = ref assert_false
  let registered_builtins = ref assert_false
  let mem_builtin = ref assert_false

  let use_spec_instead_of_definition =
    ref assert_false

  let eval_lval =
    ref (fun ~with_alarms:_ _ -> mk_labeled_fun "Value.eval_lval")
  let eval_expr =
    ref (fun ~with_alarms:_ _ -> mk_labeled_fun "Value.eval_expr")

  let eval_expr_with_state =
    ref (fun ~with_alarms:_ _ -> mk_labeled_fun "Value.eval_expr_with_state")

  let find_lv_plus = ref assert_false

  let pretty_state = Cvalue.Model.pretty

  let pretty = Cvalue.V.pretty

  let compute = ref assert_false

  let memoize = ref assert_false
  let expr_to_kernel_function = ref assert_false
  let expr_to_kernel_function_state =
    ref assert_false

  exception Not_a_call

  let call_to_kernel_function call_stmt = match call_stmt.skind with
    | Instr (Call (_, fexp, _, _)) ->
      let _, called_functions =
        !expr_to_kernel_function
          ~with_alarms:CilE.warn_none_mode ~deps:None
          (Kstmt call_stmt) fexp
      in called_functions
    | _ -> raise Not_a_call


  let lval_to_loc_with_deps = ref assert_false
  let lval_to_loc_with_deps_state = ref assert_false
  let lval_to_loc = ref assert_false
  let lval_to_offsetmap = ref assert_false
  let lval_to_offsetmap_state = ref assert_false
  let lval_to_loc_state = ref assert_false
  let lval_to_zone = ref assert_false
  let lval_to_zone_state = ref assert_false
  let lval_to_zone_with_deps_state = ref assert_false
  let lval_to_precise_loc_with_deps_state =
    ref assert_false
  let assigns_inputs_to_zone = ref assert_false
  let assigns_outputs_to_zone = ref assert_false
  let assigns_outputs_to_locations = ref assert_false
  let verify_assigns_froms = ref assert_false

  module Logic = struct
    let eval_predicate =
      ref (fun ~pre:_ ~here:_ _ ->
          raise
            (Extlib.Unregistered_function
               "Function 'Value.Logic.eval_predicate' not registered yet"))

  end

  exception Void_Function

  let find_return_loc kf =
    try
      let ki = Kernel_function.find_return kf in
      let lval = match ki with
        | { skind =
              Return (Some ({ enode = Lval ((_ , offset) as lval); _ }), _);
            _ } ->
          assert (offset = NoOffset) ;
          lval
        | { skind = Return (None, _); _ } -> raise Void_Function
        | _ -> assert false
      in
      !lval_to_loc (Kstmt ki) ~with_alarms:CilE.warn_none_mode lval
    with Kernel_function.No_Statement ->
      (* [JS 2011/05/17] should be better to have another name for this
         exception or another one since it is possible to have no return without
         returning void (the case when the kf corresponds to a declaration *)
      raise Void_Function

  exception Aborted

  let display = ref assert_false

  let emitter = ref Emitter.dummy

end

module From = struct

  exception Not_lval

  let access = ref assert_false
  let find_deps_no_transitivity = ref assert_false
  let find_deps_no_transitivity_state =
    ref assert_false
  let find_deps_term_no_transitivity_state =
    ref assert_false
  let compute = ref assert_false
  let compute_all = ref assert_false
  let compute_all_calldeps = ref assert_false
  let is_computed = ref assert_false
  let pretty = ref assert_false
  let get = ref assert_false
  let self = ref State.dummy
  let display = ref assert_false

  module Record_From_Callbacks =
    Hook.Build
      (struct
        type t =
          (Kernel_function.t Stack.t) *
          Function_Froms.Memory.t Stmt.Hashtbl.t *
          (Kernel_function.t * Function_Froms.Memory.t) list
            Stmt.Hashtbl.t *
          (stmt -> Value.state)
      end)

  module Record_From_Graph_Callbacks =
    Hook.Build
      (struct
        type t =
          (Kernel_function.t Stack.t) *
          Function_Froms.Memory.t Stmt.Hashtbl.t *
          State_node.Set.t Stmt.Hashtbl.t *
          (Kernel_function.t * Function_Froms.Memory.t) list
            State_node.Hashtbl.t Stmt.Hashtbl.t
      end)

  module Callwise = struct
    let iter = ref assert_false
    let find = ref assert_false
  end

  module PathDeps = struct
    let iter = ref assert_false
    let find = ref assert_false
  end

  module MemDeps = struct
    let iter = ref assert_false
    let find = ref assert_false
  end
end

module Users = struct
  let get = ref assert_false
end

(* ************************************************************************* *)
(** {2 PDG} *)
(* ************************************************************************* *)

module Pdg = struct
  type t = PdgTypes.Pdg.t

  type t_nodes_and_undef =
    ((PdgTypes.Node.t * Locations.Zone.t option) list * Locations.Zone.t option)

  exception Top = PdgTypes.Pdg.Top
  exception Bottom = PdgTypes.Pdg.Bottom

  let self = ref State.dummy

  let get = ref assert_false

  let from_same_fun pdg1 pdg2 =
    let kf1 =  PdgTypes.Pdg.get_kf pdg1 in
    let kf2 =  PdgTypes.Pdg.get_kf pdg2 in
    Kernel_function.equal kf1 kf2

  let node_key = ref assert_false

  let find_decl_var_node = ref assert_false
  let find_input_node = ref assert_false
  let find_ret_output_node = ref assert_false
  let find_output_nodes = ref assert_false
  let find_all_inputs_nodes = ref assert_false
  let find_stmt_and_blocks_nodes = ref assert_false
  let find_simple_stmt_nodes = ref assert_false
  let find_stmt_node = ref assert_false
  let find_label_node = ref assert_false
  let find_entry_point_node = ref assert_false
  let find_top_input_node = ref assert_false
  let find_call_ctrl_node = ref assert_false
  let find_location_nodes_at_stmt = ref assert_false
  let find_location_nodes_at_end = ref assert_false
  let find_location_nodes_at_begin = ref assert_false
  let find_call_input_node = ref assert_false
  let find_call_output_node = ref assert_false
  let find_code_annot_nodes = ref assert_false
  let find_fun_precond_nodes = ref assert_false
  let find_fun_postcond_nodes = ref assert_false
  let find_fun_variant_nodes = ref assert_false

  let find_call_out_nodes_to_select = ref assert_false
  let find_in_nodes_to_select_for_this_call =
    ref assert_false

  let direct_dpds = ref assert_false
  let direct_ctrl_dpds = ref assert_false
  let direct_data_dpds = ref assert_false
  let direct_addr_dpds = ref assert_false

  let all_dpds = ref assert_false
  let all_ctrl_dpds = ref assert_false
  let all_data_dpds = ref assert_false
  let all_addr_dpds = ref assert_false

  let direct_uses = ref assert_false
  let direct_ctrl_uses = ref assert_false
  let direct_data_uses = ref assert_false
  let direct_addr_uses = ref assert_false

  let all_uses = ref assert_false

  let custom_related_nodes = ref assert_false

  let find_call_stmts = ref assert_false

  let iter_nodes = ref assert_false

  let extract = ref assert_false
  let pretty = ref (fun ?bw:_ _ _ -> mk_labeled_fun "Pdg.pretty")
  let pretty_node = ref assert_false
  let pretty_key = ref assert_false

end

(* ************************************************************************* *)
(** {2 Scope} *)
(* ************************************************************************* *)

(** Interface for the Scope plugin *)
module Scope = struct
  let get_data_scope_at_stmt = ref assert_false
  let get_prop_scope_at_stmt = ref assert_false
  let check_asserts = ref assert_false
  let rm_asserts = ref assert_false
  let get_defs = ref assert_false
  let get_defs_with_type = ref assert_false

  type t_zones = Locations.Zone.t Stmt.Hashtbl.t
  let build_zones = ref assert_false
  let pretty_zones = ref assert_false
  let get_zones = ref assert_false
end

(* ************************************************************************* *)
(** {2 Spare Code} *)
(* ************************************************************************* *)

(** Detection of the unused code of an application. *)
module Sparecode = struct
  let get =
    ref (fun ~select_annot:_  -> mk_labeled_fun "Sparecode.run")
  let rm_unused_globals =
    ref (fun ?new_proj_name:_ -> mk_labeled_fun "Sparecode.rm_unused_globals")
end

(* ************************************************************************* *)
(** {2 Slicing} *)
(* ************************************************************************* *)

(** Interface for the slicing tool. *)
module Slicing = struct

  exception No_Project
  exception Existing_Project

  let self = ref State.dummy

  let set_modes =
    ref (fun ?calls:_ ?callers:_ ?sliceUndef:_ ?keepAnnotations:_
          ?print:_ _ -> mk_labeled_fun "Slicing.set_modes")

  (* TODO: merge with frama-c projects (?) *)
  module Project = struct
    type t = SlicingTypes.sl_project
    let dyn_t = SlicingTypes.Sl_project.ty

    let default_slice_names = ref assert_false
    let extract = ref assert_false
    let pretty = ref assert_false
    let print_extracted_project =
      ref (fun ?fmt:_ ~extracted_prj:_ ->
          mk_labeled_fun "Slicing.Project.print_extracted_project")
    let print_dot =
      ref (fun ~filename:_ ~title:_ _ ->
          mk_labeled_fun "Slicing.Project.print_dot")

    let get_all = ref assert_false
    let get_project = ref assert_false
    let set_project = ref assert_false
    let mk_project = ref assert_false
    let from_unique_name = ref assert_false
    let get_name = ref assert_false

    let is_directly_called_internal =
      ref assert_false
    let is_called = ref assert_false
    let has_persistent_selection =
      ref assert_false
    let change_slicing_level =
      ref assert_false
  end

  module Mark = struct
    type t = SlicingTypes.sl_mark
    let dyn_t = SlicingTypes.dyn_sl_mark
    let compare = ref assert_false
    let pretty = ref assert_false
    let make =
      ref (fun ~data:_ ~addr:_ ~ctrl:_ -> mk_labeled_fun "Slicing.Mark.make")
    let is_bottom = ref assert_false
    let is_spare = ref assert_false
    let is_ctrl = ref assert_false
    let is_data = ref assert_false
    let is_addr = ref assert_false
    let get_from_src_func  = ref assert_false
  end

  module Select = struct
    type t = SlicingTypes.sl_select
    let dyn_t = SlicingTypes.Sl_select.ty
    type set = SlicingTypes.Fct_user_crit.t Cil_datatype.Varinfo.Map.t
    module S = Cil_datatype.Varinfo.Map.Make(SlicingTypes.Fct_user_crit)
    let dyn_set = S.ty

    let get_function = ref assert_false
    let select_stmt = ref assert_false
    let select_stmt_ctrl = ref assert_false
    let select_stmt_lval_rw = ref assert_false
    let select_stmt_lval = ref assert_false
    let select_stmt_zone = ref assert_false
    let select_stmt_annots = ref assert_false
    let select_stmt_annot = ref assert_false
    let select_stmt_pred = ref assert_false
    let select_stmt_term = ref assert_false
    let select_func_return = ref assert_false
    let select_func_calls_to = ref assert_false
    let select_func_calls_into = ref assert_false
    let select_func_lval_rw = ref assert_false
    let select_func_lval = ref assert_false
    let select_func_zone = ref assert_false
    let select_func_annots = ref assert_false
    let select_stmt_internal = ref assert_false
    let select_label_internal = ref assert_false
    let empty_selects =
      Journal.register
        "Db.Slicing.Select.empty_selects"
        dyn_set
        Cil_datatype.Varinfo.Map.empty
    let add_to_selects_internal =
      ref assert_false
    let iter_selects_internal =
      ref assert_false
    (* didn't manage to put this polymorphic function as a ref... *)
    let fold_selects_internal f acc selections =
      let r = ref acc in
      let dof select = r := f !r select in
      !iter_selects_internal dof selections; !r
    let merge_internal =
      ref assert_false
    let select_min_call_internal =
      ref assert_false
    let select_stmt_ctrl_internal =
      ref assert_false
    let select_pdg_nodes =
      ref assert_false
    let select_entry_point_internal =
      ref assert_false
    let select_return_internal =
      ref assert_false
    let select_decl_var_internal =
      ref assert_false
    let select_pdg_nodes_internal =
      ref assert_false
    let select_stmt_zone_internal =
      ref assert_false
    let select_zone_at_entry_point_internal =
      ref assert_false
    let select_modified_output_zone_internal =
      ref assert_false
    let select_zone_at_end_internal =
      ref assert_false
    let pretty = ref assert_false
  end

  module Slice = struct
    type t = SlicingTypes.sl_fct_slice
    let dyn_t = SlicingTypes.dyn_sl_fct_slice
    let create = ref assert_false
    let remove = ref assert_false
    let remove_uncalled = ref assert_false
    let get_all = ref assert_false
    let get_callers = ref assert_false
    let get_called_slice = ref assert_false
    let get_called_funcs = ref assert_false
    let get_function = ref assert_false
    let pretty = ref assert_false
    let get_mark_from_stmt = ref assert_false
    let get_mark_from_local_var =
      ref assert_false
    let get_mark_from_formal = ref assert_false
    let get_mark_from_label = ref assert_false
    let get_user_mark_from_inputs =
      ref assert_false
    let get_num_id =
      ref assert_false
    let from_num_id =
      ref assert_false
  end

  module Request = struct
    let add_selection = ref assert_false
    let add_persistent_selection = ref assert_false
    let add_persistent_cmdline = ref assert_false
    let is_already_selected_internal =
      ref assert_false
    let add_slice_selection_internal =
      ref assert_false
    let add_selection_internal =
      ref assert_false
    let add_call_slice = ref assert_false
    let add_call_fun = ref assert_false
    let add_call_min_fun = ref assert_false
    let merge_slices = ref assert_false
    let copy_slice = ref assert_false
    let split_slice = ref assert_false
    let propagate_user_marks = ref assert_false
    let apply_all = ref assert_false
    let apply_all_internal = ref assert_false
    let apply_next_internal = ref assert_false
    let is_request_empty_internal = ref assert_false
    let pretty = ref assert_false
  end

end

(* ************************************************************************* *)
(** {2 Properties} *)
(* ************************************************************************* *)

module Properties = struct

  let mk_resultfun s =
    ref (fun ~result:_ -> failwith (Printf.sprintf "Function '%s' not registered yet" s))

  module Interp = struct

    exception No_conversion

    (** Interpretation and conversions of of formulas *)
    let code_annot = ref assert_false
    let term_lval = ref assert_false
    let term = ref assert_false
    let predicate = ref assert_false
    let term_lval_to_lval = mk_resultfun "Properties.Interp.term_lval_to_lval"
    let term_to_exp = mk_resultfun "Properties.Interp.term_to_exp"
    let term_to_lval = mk_resultfun "Properties.Interp.term_to_lval"
    let loc_to_lval = mk_resultfun "Properties.Interp.loc_to_lval"
    (* loc_to_loc and loc_to_locs are defined in Value/Eval_logic, not
       in Logic_interp *)
    let loc_to_loc = mk_resultfun "Properties.Interp.loc_to_loc"
    let loc_to_loc_under_over = mk_resultfun "Properties.Interp.loc_to_loc_with_deps"
    let loc_to_offset = mk_resultfun "Properties.Interp.loc_to_offset"
    let loc_to_exp = mk_resultfun "Properties.Interp.loc_to_exp"
    let term_offset_to_offset =
      mk_resultfun "Properties.Interp.term_offset_to_offset"

    module To_zone = struct
      type t_ctx =
        { state_opt: bool option;
          ki_opt: (stmt * bool) option;
          kf:Kernel_function.t }
      let mk_ctx_func_contrat:
        (kernel_function -> state_opt:bool option -> t_ctx) ref
        = ref assert_false
      let mk_ctx_stmt_contrat:
        (kernel_function -> stmt -> state_opt:bool option -> t_ctx) ref
        = ref assert_false
      let mk_ctx_stmt_annot: (kernel_function -> stmt -> t_ctx) ref =
        ref assert_false
      type t = {before:bool ; ki:stmt ; zone:Locations.Zone.t}
      type t_zone_info = (t list) option
      type t_decl =
        { var: Varinfo.Set.t;
          lbl: Logic_label.Set.t }
      type t_pragmas =
        { ctrl: Stmt.Set.t;
          stmt: Stmt.Set.t }
      let from_term: (term -> t_ctx -> t_zone_info * t_decl) ref =
        ref assert_false
      let from_terms: (term list -> t_ctx -> t_zone_info * t_decl) ref =
        ref assert_false
      let from_pred: (predicate named -> t_ctx -> t_zone_info * t_decl) ref =
        ref assert_false
      let from_preds:
        (predicate named list -> t_ctx -> t_zone_info * t_decl) ref
        = ref assert_false
      let from_zone: (identified_term -> t_ctx -> t_zone_info * t_decl) ref =
        ref assert_false
      let from_stmt_annot:
        (code_annotation -> stmt * kernel_function
         -> (t_zone_info * t_decl) * t_pragmas) ref
        = ref assert_false
      let from_stmt_annots:
        ((code_annotation -> bool) option ->
         stmt * kernel_function -> (t_zone_info * t_decl) * t_pragmas) ref
        = ref assert_false
      let from_func_annots:
        (((stmt -> unit) -> kernel_function -> unit) ->
         (code_annotation -> bool) option ->
         kernel_function -> (t_zone_info * t_decl) * t_pragmas) ref
        = ref assert_false
      let code_annot_filter:
        (code_annotation ->
         threat:bool -> user_assert:bool -> slicing_pragma:bool ->
         loop_inv:bool -> loop_var:bool -> others:bool -> bool) ref
        = ref assert_false
    end

    let to_result_from_pred = ref assert_false
  end

  let add_assert emitter kf kinstr prop =
    Kernel.deprecated "Db.Properties.add_assert" ~now:"ACSL_importer plug-in"
      (fun () ->
         let interp_prop = !Interp.code_annot kf kinstr prop in
         Annotations.add_code_annot emitter kinstr interp_prop)
      ()

end

(* ************************************************************************* *)
(** {2 Others plugins} *)
(* ************************************************************************* *)

module ACSL_importer = struct
  let emitter = ref None
end

module Impact = struct
  let compute_pragmas = ref assert_false
  let from_stmt = ref assert_false
  let from_nodes = ref assert_false
end

module Security = struct
  let run_whole_analysis = ref assert_false
  let run_ai_analysis = ref assert_false
  let run_slicing_analysis = ref assert_false
  let self = ref State.dummy
end

module Occurrence = struct
  type t = (kernel_function option * kinstr * lval) list
  let get = ref assert_false
  let get_last_result = ref assert_false
  let print_all = ref assert_false
  let self = ref State.dummy
end

module RteGen = struct
  type status_accessor =
    string * (kernel_function -> bool -> unit) * (kernel_function -> bool)
  let compute = ref assert_false
  let annotate_kf = ref assert_false
  let self = ref State.dummy
  let do_precond = ref assert_false
  let do_all_rte = ref assert_false
  let do_rte = ref assert_false
  let get_all_status = ref assert_false
  let get_precond_status = ref assert_false
  let get_signedOv_status = ref assert_false
  let get_divMod_status = ref assert_false
  let get_signed_downCast_status = ref assert_false
  let get_memAccess_status = ref assert_false
  let get_unsignedOv_status = ref assert_false
  let get_unsignedDownCast_status = ref assert_false
end

module Report = struct
  let print = ref assert_false
end

module Constant_Propagation = struct
  let get = ref assert_false
  let compute = ref assert_false
end

module PostdominatorsTypes = struct
  exception Top

  module type Sig = sig
    val compute: (kernel_function -> unit) ref
    val stmt_postdominators:
      (kernel_function -> stmt -> Stmt.Hptset.t) ref
    val is_postdominator:
      (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
    val display: (unit -> unit) ref
    val print_dot : (string -> kernel_function -> unit) ref
  end
end


module Postdominators = struct
  let compute = ref assert_false
  let is_postdominator
    : (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
    = ref assert_false
  let stmt_postdominators = ref assert_false
  let display = ref assert_false
  let print_dot = ref assert_false
end

module PostdominatorsValue = struct
  let compute = ref assert_false
  let is_postdominator
    : (kernel_function -> opening:stmt -> closing:stmt -> bool) ref
    = ref assert_false
  let stmt_postdominators = ref assert_false
  let display = ref assert_false
  let print_dot = ref assert_false
end

(* ************************************************************************* *)
(** {2 GUI} *)
(* ************************************************************************* *)

let progress = ref (fun () -> ())

exception Cancel

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
