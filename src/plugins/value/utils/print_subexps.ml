(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

(* We are interested structural equality of expressions. *)
module Exp = ExpStructEq


(* Extracting information. *)

(** Extract all sub-expressions from an expression. *)
let rec subexps_of_exp ?(include_top_exp=true) (exp : exp) : Exp.Set.t =
  let subexps_of_exp = subexps_of_exp ~include_top_exp:true
  in
  (* This expression. *)
  let this_exp : Exp.Set.t =
    if include_top_exp
    then Exp.Set.singleton exp
    else Exp.Set.empty
  in
  (* Sub-expressions. *)
  let sub_exps : Exp.Set.t =
    (match (exp.enode : exp_node) with
     | Const      _const                      -> Exp.Set.empty
     | Lval       lval                        -> subexps_of_lval lval
     | SizeOf     _typ                        -> Exp.Set.empty
     | SizeOfE    exp'                        -> subexps_of_exp exp'
     | SizeOfStr  _str                        -> Exp.Set.empty
     | AlignOf    _typ                        -> Exp.Set.empty
     | AlignOfE   exp'                        -> subexps_of_exp exp'
     | UnOp      (_unop, exp', _typ)          -> subexps_of_exp exp'
     | BinOp     (_binop, exp_l, exp_r, _typ) -> Exp.Set.union
                                                   (subexps_of_exp exp_l)
                                                   (subexps_of_exp exp_r)
     | CastE     (_typ, exp')                 -> subexps_of_exp exp'
     | AddrOf     lval                        -> subexps_of_lval lval
     | StartOf    lval                        -> subexps_of_lval lval
     | Info      (exp', _exp_info)            -> subexps_of_exp exp')
  in
  Exp.Set.union this_exp sub_exps

(** Extract all expressions from a lvalue. *)
and subexps_of_lval (lval : lval) : Exp.Set.t =
  let subexps_of_exp = subexps_of_exp ~include_top_exp:true in
  match lval with
  | (Var _varinfo, offset) -> subexps_of_offset offset
  | (Mem  exp,     offset) -> Exp.Set.union
                                (subexps_of_exp exp)
                                (subexps_of_offset offset)

(** Extract all expressions from an offset. *)
and subexps_of_offset (offset : offset) : Exp.Set.t =
  let subexps_of_exp = subexps_of_exp ~include_top_exp:true in
  match offset with
  | NoOffset                       -> Exp.Set.empty
  | Field    (_fieldinfo, offset') -> subexps_of_offset offset'
  | Index    (exp,        offset') -> Exp.Set.union
                                        (subexps_of_exp exp)
                                        (subexps_of_offset offset')


(* Filter out the sub-expressions which are not interesting to show. *)

let filter_interesting_exps (exps : Exp.Set.t) : Exp.Set.t =
  (* Note: Usually we don't want to see constants printed out, but sometimes
     in fact this may be helpful, e.g. when the contant value is provided using
     a macro. Hence, at least for now, we just show everything. *)
  let is_interesting _exp =
    (* not (Cil.isConstant exp) *)
    true
  in
  Exp.Set.filter is_interesting exps


(* Pretty printers. *)

(* Pretty printer that prints an expression and its value. *)
let pp_exp_with_value fmt ((exp, value) : exp * Cvalue.V.t) =
  let typ = Cil.typeOf exp in
  (* Pretty printer that prints the expression's value. *)
  let pp_value fmt value =
    match typ with
    | TComp(compinfo, _bitsSizeofTypCache, _attributes) ->
      (* Don't print details of composite data types (i.e. struct and union). *)
      Format.fprintf fmt "= <composite data type: %s>"
        (Cil.compFullName compinfo)
    | _ ->
      (* Print the value of any other data type. *)
      Format.fprintf fmt "%s @[%a@]"
        (Unicode.inset_string ()) (* the \in symbol *)
        (Cvalue.V.pretty_typ (Some typ)) value
  in
  Format.fprintf
    fmt "@[%a  %a@] %a"
    Cil_printer.pp_typ typ (* the expression's type *)
    Cil_printer.pp_exp exp (* the expression itself *)
    pp_value value         (* the expression's value *)

(* Pretty printer that prints a list of expressions with their values. *)
let pp_exps_with_values =
  Format.pp_print_list
    ~pp_sep:Format.pp_print_newline
    pp_exp_with_value

(* Pretty printer that prints a list of sub-expressions of a given expression
   with their values. *)
let pp_all_subexps ?(include_top_exp=false) eval_exp fmt exp =
  (* Prepare a list of pairs: sub-expression and its evaluated value. *)
  let (subexps_with_values: (exp * Cvalue.V.t) list) =
    let all_subexps: Exp.Set.t = subexps_of_exp ~include_top_exp exp in
    let interesting_subexps: Exp.Set.t = filter_interesting_exps all_subexps in
    let subexps_list: exp list = Exp.Set.elements interesting_subexps in
    List.map (fun exp -> (exp, eval_exp exp)) subexps_list
  in
  (* Print the sub-expressions and their values. *)
  pp_exps_with_values fmt subexps_with_values

(* The pretty printer which is used in the built-in. *)
let abstract_print_subexps
    ?(include_top_exp=false)
    eval_exp fmt (description, exp) =
  Format.fprintf
    fmt "@\nValues of all the sub-expressions of %s:@\n%a"
    description                                    (* the description string *)
    (pp_all_subexps ~include_top_exp eval_exp) exp (* all the sub-expressions *)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
