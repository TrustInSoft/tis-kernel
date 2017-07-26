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
open Value_callstack

type node = int * Callstack.t * Stmt.t * last_action * Cvalue.Model.t

and return_kind =
  | BuiltinOrContract
  | NormalCall

and last_action =
  | Init
  | Statement
  | InterpAnnot of string
  | Join of string
  | MergeIncludedStates
  | Widen
  | MergeAfterLoop
  | Edge
  | Guard of Exp.t
  | ComputeFirstPredecessor
  | CombinePredecessors
  | OpenBlock
  | CloseBlock
  | PostAnnot
  | Return of (return_kind * node)
  | Externalize
  | InterpCall
  | Reduce of last_action

let node_repr =
  let stmt_repr = List.hd Stmt.reprs in
  let callstack_repr = List.hd Callstack.reprs in
  (0, callstack_repr, stmt_repr, Statement, List.hd Cvalue.Model.reprs)

let compare_node (i1,_,_,_,_) (i2,_,_,_,_) = Pervasives.compare i1 i2

let hash_node (i,_,_,_,_) = i

let pretty_return_kind fmt = function
  | BuiltinOrContract -> Format.fprintf fmt "BuiltinOrContract"
  | NormalCall -> Format.fprintf fmt "NormalCall"

let rec pretty_action fmt a =
  match a with
  | Guard exp -> Format.fprintf fmt "Guard %a" Printer.pp_exp exp
  | Return (k, (id, _, _, _, _)) ->
    Format.fprintf fmt "Return (%a, %d)" pretty_return_kind k id
  | Reduce a -> Format.fprintf fmt "Reduce %a" pretty_action a
  | _ ->
    Format.fprintf fmt "%s" (match a with
        | Init -> "Init"
        | Statement -> "Statement"
        | InterpAnnot s -> "InterpAnnot "^s
        | Join s -> "Join "^s
        | MergeIncludedStates -> "MergeIncludedStates"
        | Widen -> "Widen"
        | MergeAfterLoop -> "MergeAfterLoop"
        | Edge -> "Edge"
        | ComputeFirstPredecessor -> "ComputeFirstPredecessor"
        | CombinePredecessors -> "CombinePredecessors"
        | OpenBlock -> "OpenBlock"
        | CloseBlock -> "CloseBlock"
        | PostAnnot -> "PostAnnot"
        | InterpCall -> "InterpCall"
        | Guard _ -> assert false
        | Return _ -> assert false
        | Externalize -> "Externalize"
        | Reduce _ -> assert false)

module Action = Datatype.Make(
  struct
    type t = last_action
    let name = "action"
    let rehash = Datatype.identity
    let structural_descr = Structural_descr.t_abstract
    let reprs =
      [ Statement; InterpAnnot ""; Widen; MergeAfterLoop; Join "";
        MergeIncludedStates; Init; Edge ; CombinePredecessors;
        ComputeFirstPredecessor; OpenBlock; CloseBlock; PostAnnot;
        Guard (Exp.dummy); Return (NormalCall, node_repr); Externalize;
        InterpCall; Reduce (Statement) ]
    let rec compare a1 a2 =
      match a1, a2 with
      | Guard e1, Guard e2 -> Exp.compare e1 e2
      | Return (k1, r1), Return (k2, r2) ->
        let r = Pervasives.compare k1 k2 in
        if r = 0 then compare_node r1 r2 else r
      | Reduce a1, Reduce a2 -> compare a1 a2
      | Reduce _, _ | Guard _, _ | Return _, _ -> -1
      | _, Reduce _ | _, Guard _ | _, Return _ -> 1
      | _ -> Pervasives.compare a1 a2
    let equal = Datatype.from_compare
    let rec hash = function
      | Guard e -> Exp.hash e
      | Return (k, r) -> Hashtbl.hash k + hash_node r
      | Reduce a -> 17 * hash a + 3
      | a -> Hashtbl.hash a
    let copy = Datatype.identity
    let internal_pretty_code = Datatype.undefined
    let pretty = pretty_action
    let varname = Datatype.undefined
    let mem_project = Datatype.never_any_project
  end)

module Data =
  Datatype.Quadruple_with_collections
    (Callstack)(Stmt)(Action)(Cvalue.Model)
    (struct let module_name = "state_with_stmt" end)

module CurrId = State_builder.Counter(struct let name = "State node counter" end)

let create_new cs stmt action state =
  let id = CurrId.next() in
  (id, cs, stmt, action, state)

let get table cs stmt action state =
  let key = cs, stmt, action, state in
  try
    Data.Hashtbl.find table key
  with Not_found ->
    let node = create_new cs stmt action state in
    Data.Hashtbl.add table key node;
    node

include Datatype.Make_with_collections(
  struct
    type t = node
    let name = "state_node"
    let rehash = Datatype.identity
    let structural_descr =
      Structural_descr.t_tuple
        [| Structural_descr.p_int; Callstack.packed_descr; Stmt.packed_descr;
           Action.packed_descr; Cvalue.Model.packed_descr |]
    let reprs = [node_repr]
    let compare = compare_node
    let equal = Pervasives.( == )
    let hash = hash_node
    let copy = Datatype.undefined
    let internal_pretty_code = Datatype.undefined
    let pretty fmt (i,cs,s,a,m) =
      Format.fprintf fmt "@[@[%a@],@[%a@],@[%a@] @[%a@],@[%a@]@]"
        Datatype.Int.pretty i Callstack.pretty cs Stmt.pretty s
        pretty_action a Cvalue.Model.pretty m
    let varname = Datatype.undefined
    let mem_project = Datatype.never_any_project
  end)

let pretty_no_state fmt (i,cs, s,a,_) =
  Format.fprintf fmt "@[%a, %a, @[%a at %a@], %a@]"
    Datatype.Int.pretty i
    Callstack.pretty cs
    Printer.pp_stmt s
    Printer.pp_location (Cil_datatype.Stmt.loc s)
    Action.pretty a

let id (i,_,_,_,_) = i
let callstack (_,cs,_,_,_) = cs
let stmt (_,_,s,_,_) = s
let action (_,_,_,a,_) = a
let state (_,_,_,_,m) = m

let with_state f (_,_,_,_,state) = f state
let with_state_node f ((_,_,_,_,state) as node) = f state node
let with_stmt_state f (_,_,stmt,_,state) = f stmt state
let with_stmt_state_node f ((_,_,stmt,_,state) as node) = f stmt state node
let with_action f (_,_,_,act,_) = f act
let with_fields f (_,_,stmt,act,state) = f stmt act state
let with_all f (id,cs,stmt,act,state) = f id cs stmt act state
(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
