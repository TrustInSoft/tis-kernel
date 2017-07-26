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
open Cil

(* ************************************************************************** *)
(** {2 Expressions} *)
(* ************************************************************************** *)

let is_integral_const = function
  | CInt64 _ | CEnum _ | CChr _ -> true
  | CStr _ | CWStr _ | CReal _ -> false

let rec possible_value_of_integral_const = function
  | CInt64 (i, _, _) -> Some i
  | CEnum { eival = e; _ } -> possible_value_of_integral_expr e
  | CChr c -> Some (Integer.of_int (Char.code c))
  (* This is against the ISO C norm! See Cil.charConstToInt  *)
  | _ -> None

and possible_value_of_integral_expr e =
  match (stripInfo e).enode with
  | Const c -> possible_value_of_integral_const c
  | _ -> None

let value_of_integral_const c =
  match possible_value_of_integral_const c with
  | None -> assert false
  | Some i -> i

let value_of_integral_expr e =
  match possible_value_of_integral_expr e with
  | None -> assert false
  | Some i -> i

let constant_expr ~loc i = new_exp ~loc (Const(CInt64(i,IInt,None)))

let rec is_null_expr e = match (stripInfo e).enode with
  | Const c when is_integral_const c ->
    Integer.equal (value_of_integral_const c) Integer.zero
  | CastE(_,e) -> is_null_expr e
  | _ -> false

let rec is_non_null_expr e = match (stripInfo e).enode with
  | Const c when is_integral_const c ->
    not (Integer.equal (value_of_integral_const c) Integer.zero)
  | CastE(_,e) -> is_non_null_expr e
  | _ -> false

(* ************************************************************************** *)
(** {2 Logical terms} *)
(* ************************************************************************** *)

let is_integral_logic_const = function
  | Integer _ | LEnum _ | LChr _ -> true
  | LStr _ | LWStr _ | LReal _ -> false

let possible_value_of_integral_logic_const = function
  | Integer (i, _) -> Some i
  | LEnum { eival = e; _ } -> possible_value_of_integral_expr e
  | LChr c -> Some (Integer.of_int (Char.code c))
  (* This is against the ISO C norm! See Cil.charConstToInt  *)
  | _ -> None

let value_of_integral_logic_const c =
  match possible_value_of_integral_logic_const c with
  | None -> assert false
  | Some i -> i

let possible_value_of_integral_term t =
  match t.term_node with
  | TConst c -> possible_value_of_integral_logic_const c
  | _ -> None

let term_lvals_of_term t =
  let l = ref [] in
  ignore
    (Cil.visitCilTerm
       (object
         inherit nopCilVisitor
         method! vterm_lval lv =
           l := lv :: !l;
           DoChildren
       end)
       t);
  !l

let behavior_assumes b =
  Logic_const.pands (List.map Logic_const.pred_of_id_pred b.b_assumes)

let behavior_postcondition b k =
  let assumes = Logic_const.pold (behavior_assumes b) in
  let postcondition =
    Logic_const.pands
      (Extlib.filter_map (fun (x,_) -> x = k)
         (Extlib.($) Logic_const.pred_of_id_pred snd) b.b_post_cond)
  in
  Logic_const.pimplies (assumes,postcondition)

let behavior_precondition b =
  let assumes = behavior_assumes b in
  let requires = Logic_const.pands
      (List.rev_map Logic_const.pred_of_id_pred b.b_requires)
  in
  Logic_const.pimplies (assumes,requires)

let precondition spec =
  Logic_const.pands (List.map behavior_precondition spec.spec_behavior)

(** find the behavior named [name] in the list *)
let get_named_bhv bhv_list name =
  try Some (List.find (fun b -> b.b_name = name) bhv_list)
  with Not_found -> None

let get_named_bhv_assumes spec bhv_names =
  let bhvs = match bhv_names with
    | [] -> (* no names ==> all named behaviors *)
      List.filter (fun b -> not (is_default_behavior b)) spec.spec_behavior
    | _ ->
      let rec get l = match l with [] -> []
                                 | name::tl ->
                                   match get_named_bhv spec.spec_behavior name with
                                   | None -> (* TODO: warn ? *) get tl
                                   | Some b -> b::(get tl)
      in
      get bhv_names
  in
  List.map behavior_assumes bhvs

let complete_behaviors spec bhv_names =
  let bhv_assumes = get_named_bhv_assumes spec bhv_names in
  Logic_const.pors bhv_assumes

let disjoint_behaviors spec bhv_names =
  let bhv_assumes = get_named_bhv_assumes spec bhv_names in
  let mk_disj_bhv b1 b2 = (* ~ (b1 /\ b2) *)
    let p = Logic_const.pands [b1; b2] in
    Logic_const.pnot p
  in
  let do_one_with_list prop b lb =
    let lp = List.map (mk_disj_bhv b) lb in
    Logic_const.pands (prop::lp)
  in
  let rec do_list prop l = match l with [] -> prop
                                      | b::tl ->
                                        let prop = do_one_with_list prop b tl in
                                        do_list prop tl
  in
  do_list Logic_const.ptrue bhv_assumes

let merge_assigns_internal (get:'b -> 'c assigns) (origin:'b -> string list)
    (acc:(('a*(bool * string list))*int) option) (bhvs: 'b list) =
  let cmp_assigns acc b =
    let a' = get b in
    match acc,a' with
    | _, WritesAny -> acc
    | None, Writes l ->
      (* use the number of assigned terms as measure *)
      Some ((a',(false,origin b)),List.length l)
    | (Some((a,(w,orig)),n)), Writes l ->
      let w = (* warning is needed? *)
        w || (a != a' && a <> WritesAny)
      in (* use the number of assigned terms as measure *)
      let m = List.length l in
      if n<0 || m<n then Some((a',(w,origin b)),m) else Some((a,(w,orig)),n)
  in List.fold_left (* find the smallest one *)
    cmp_assigns acc bhvs

(** Returns the assigns from complete behaviors and ungarded behaviors. *)
let merge_assigns_from_complete_bhvs ?warn ?(ungarded=true) bhvs complete_bhvs =
  let merge_assigns_from_complete_bhvs bhv_names =
    try (* For merging assigns of a "complete" set of behaviors *)
      let behaviors = match bhv_names with
        (* Extract behaviors from their names. *)
        | [] -> (* All behaviors should be taken except the default behavior *)
          List.filter (fun b -> not (Cil.is_default_behavior b)) bhvs
        | _ -> (* Finds the corresponding behaviors from the set *)
          List.map
            (fun b_name ->
               List.find (fun b -> b.b_name = b_name) bhvs) bhv_names
      in
      (* Merges the assigns of the complete behaviors.
         Once one of them as no assumes, that means the merge
         of the ungarded behavior did already the job *)
      Writes
        (List.fold_left
           (fun acc b -> match b.b_assigns with
              | Writes l when b.b_assumes <> [] -> l @ acc
              | _ -> raise Not_found) [] behaviors)
    with Not_found ->
      (* One of these behaviors is not found or has no assumes *)
      WritesAny
  in
  let acc =
    if ungarded then (* Looks first at unguarded behaviors. *)
      let unguarded_bhvs = List.filter (fun b -> b.b_assumes = []) bhvs
      in merge_assigns_internal  (* Chooses the smalest one *)
        (fun b -> b.b_assigns) (fun b -> [b.b_name])
        None unguarded_bhvs
    else None
  in
  let acc = match acc with
    | Some (((Writes _),_),_) ->
      (* Does not look further since one has been found *)
      acc
    | _ ->
      (* Look at complete behaviors *)
      merge_assigns_internal (* Chooses the smalest one *)
        merge_assigns_from_complete_bhvs
        (fun bhvnames -> bhvnames)
        acc
        complete_bhvs
  in
  match acc with
  | None -> WritesAny (* No unguarded behavior -> assigns everything *)
  | Some ((a,(w,orig)),_) -> (* The smallest one *)
    let warn = match warn with
      | None -> w
      | Some warn -> warn
    in
    if warn then begin
      let orig =
        if orig = [] then List.map (fun b -> b.b_name) bhvs else orig
      in
      Kernel.warning ~once:true ~current:true
        "keeping only assigns from behaviors: %a"
        (Pretty_utils.pp_list ~sep:",@ " Format.pp_print_string) orig
    end;
    a

(** Returns the assigns from complete behaviors and ungarded behaviors. *)
let merge_assigns_from_spec ?warn (spec :funspec) =
  merge_assigns_from_complete_bhvs
    ?warn spec.spec_behavior spec.spec_complete_behaviors

(** Returns the assigns of an unguarded behavior. *)
let merge_assigns ?warn (bhvs : funbehavior list) =
  let unguarded_bhvs = List.filter (fun b -> b.b_assumes = []) bhvs in
  let acc = merge_assigns_internal
      (fun b -> b.b_assigns) (fun b -> [b.b_name])
      None unguarded_bhvs
  in
  match acc with
  | None -> WritesAny (* No unguarded behavior -> assigns everything *)
  | Some((a,(w,orig)),_) -> (* The smallest one *)
    let warn = match warn with
      | None -> w
      | Some warn -> warn
    in
    if warn then
      Kernel.warning ~once:true ~current:true
        "keeping only assigns from behaviors: %a"
        (Pretty_utils.pp_list ~sep:",@ " Format.pp_print_string) orig;
    a

let variable_term loc v =
  {
    term_node = TLval(TVar v,TNoOffset);
    term_loc = loc;
    term_type = v.lv_type;
    term_name = [];
  }

let constant_term loc i =
  {
    term_node = TConst(Integer(i,None));
    term_loc = loc;
    term_type = Ctype intType;
    term_name = [];
  }

let rec is_null_term t = match t.term_node with
  | TConst c when is_integral_logic_const c ->
    Integer.equal (value_of_integral_logic_const c) Integer.zero
  | TCastE(_,t) -> is_null_term t
  | _ -> false

(* ************************************************************************** *)
(** {2 Statements} *)
(* ************************************************************************** *)

let is_loop_statement s = match s.skind with Loop _ -> true | _ -> false

let get_sid s = match s with
  | Kglobal -> assert false
  | Kstmt s -> s.sid

let mkassign lv e loc = Set(lv,e,loc)

let mkassign_statement lv e loc = mkStmt (Instr(mkassign lv e loc))

let is_block_local v b = List.exists (fun vv -> v.vid = vv.vid) b.blocals

(* ************************************************************************** *)
(** {2 Functions} *)
(* ************************************************************************** *)

let is_function_type vi = isFunctionType vi.vtype

module Function = struct

  let formal_args called_vinfo = match called_vinfo.vtype with
    | TFun (_,Some argl,_,_) ->
      argl
    | TFun _ ->
      []
    | _ -> assert false

  let is_formal v fundec =
    List.exists (fun vv -> v.vid = vv.vid) fundec.sformals

  let is_local v fundec = List.exists (fun vv -> v.vid = vv.vid) fundec.slocals

  let is_formal_or_local v fundec =
    (not v.vglob) && ((is_formal v fundec) || (is_local v fundec))

  let is_formal_of_prototype v vi =
    let formals = try getFormalsDecl vi with Not_found -> [] in
    List.exists (fun x -> x.vid = v.vid) formals

  let is_definition = function
    | Definition _ -> true
    | Declaration _ -> false

  let get_vi = function
    | Definition (d, _) -> d.svar
    | Declaration (_,vi,_, _) -> vi

  let get_name f = (get_vi f).vname
  let get_id f = (get_vi f).vid

end

exception FoundBlock of block

let block_of_local (fdec:fundec) vi =
  let find_in_block bl =
    if is_block_local vi bl then raise (FoundBlock bl)
  in
  let find_in_stmt stmt =
    match stmt.skind with
    | Block b
    | Switch (_, b, _, _)
    | Loop (_, b, _, _, _) -> find_in_block b
    | If (_, be, bt, _) -> find_in_block be; find_in_block bt
    | TryExcept (b1, _, b2, _)
    | TryFinally (b1, b2, _) -> find_in_block b1; find_in_block b2
    | TryCatch (b, l, _) ->
      find_in_block b;
      let aux (cb, b) =
        find_in_block b;
        match cb with
        | Catch_all -> ()
        | Catch_exn (vi', _l) ->
          (* note: vars in [_l] (related to exception subtyping)
             are generated and never pretty-printed, so need not be compared *)
          if Cil_datatype.Varinfo.equal vi vi' then raise (FoundBlock b)
      in
      List.iter aux l
    | _ -> ()
  in
  try
    find_in_block fdec.sbody;
    List.iter find_in_stmt fdec.sallstmts;
    Kernel.abort "[local_declaration_block]: block not found for %a in %a"
      Cil_datatype.Varinfo.pretty vi Cil_datatype.Varinfo.pretty fdec.svar
  with FoundBlock bl-> bl

(* ************************************************************************** *)
(** {2 Types} *)
(* ************************************************************************** *)

let array_type ?length ?(attr=[]) ty = TArray(ty,length,empty_size_cache (),attr)

let direct_array_size ty =
  match unrollType ty with
  | TArray(_ty,Some size,_,_) -> value_of_integral_expr size
  | TArray(_ty,None,_,_) -> Integer.zero
  | _ -> assert false

let rec array_size ty =
  match unrollType ty with
  | TArray(elemty,Some _,_,_) ->
    if isArrayType elemty then
      Integer.mul (direct_array_size ty) (array_size elemty)
    else direct_array_size ty
  | TArray(_,None,_,_) -> Integer.zero
  | _ -> assert false

let direct_element_type ty = match unrollType ty with
  | TArray(eltyp,_,_,_) -> eltyp
  | _ -> assert false

let element_type ty =
  let rec elem_type ty = match unrollType ty with
    | TArray(eltyp,_,_,_) -> elem_type eltyp
    | _ -> ty
  in
  match unrollType ty with
  | TArray(eltyp,_,_,_) -> elem_type eltyp
  | _ -> assert false

let direct_pointed_type ty =
  match unrollType ty with
  | TPtr(elemty,_) -> elemty
  | _ -> assert false

let pointed_type ty =
  match unrollType (direct_pointed_type ty) with
  | TArray _ as arrty -> element_type arrty
  | ty -> ty


(* Fold on all offsets of the given type (i.e. offsets of all fields and / or
   indices, descending recursively into the type's structure) in the correct
   order (i.e. how they are arranged in memory). *)
let rec fold_type_offsets (f : 'a -> offset -> 'a) (acc : 'a) (typ : typ) : 'a =

  match unrollType typ with

  (* A structure: fold on all the fields... *)
  | TComp ({ cstruct = true; _ } as compinfo, _, _) ->
    let fields : fieldinfo list = compinfo.cfields in
    List.fold_left
      (fun (acc : 'a) (field : fieldinfo) ->
         let field_typ : typ = field.ftype in
         fold_type_offsets
           (fun acc offset ->
              let offset' = Field(field, offset) in
              f acc offset')
           acc
           field_typ)
      acc
      fields

  (* An array: fold on all the indices... *)
  | TArray (cell_typ, arr_length_exp_option, _, _) ->
    let arr_length : int =
      try lenOfArray arr_length_exp_option
      with LenOfArray -> 0 (* Should only happen when the array is the
                              flexible array member of a struct *)
    in
    let rec fold_on_indices (acc : 'a) (index : int) =
      if index < arr_length
      then
        let acc =
          fold_type_offsets
            (fun acc offset ->
               (* TODO: Find something better than builtinLoc ... *)
               let offset' =
                 let index_exp =
                   kinteger64 ~loc:builtinLoc (Integer.of_int index)
                 in
                 Index(index_exp, offset)
               in
               f acc offset')
            acc
            cell_typ
        in
        fold_on_indices acc (index + 1)
      else
        acc
    in
    fold_on_indices acc 0

  (* Any other type: it has no internal structure, so building the offset
     ends here. *)
  | _ -> f acc NoOffset


(* Field's or padding's offset and size in bits. *)
type offset_and_size = int * int

(* Information about a structure's single field or padding
   with its offset and size in bits. *)
type field_or_padding =
  | SField of offset_and_size
  | SPadding of offset_and_size

(* Fold on all fields and paddings between fields (i.e. offsets and sizes of
   all the fields / paddings) of a given type in the order that they are
   stored in memory. *)
let fold_type_fields_and_paddings
    (f : 'a -> field_or_padding -> 'a) (acc : 'a) (typ : typ) : 'a =

  (* The structure field's offset-and-size-in-bits folding function.
     + The first argument, i.e. the accumulator, is a pair:
       - the top-level (field-and-padding-level) accumulator,
       - one offset after the previously treated field's last offset.
     + The second argument is the first offset and the size of the currently
       treated field.
     + The result is a pair:
       - the new field-and-padding-level accumulator,
       - the offset after the current field's last offset. *)
  let offset_and_size_f
      (acc, prev_after_last_offset        : 'a * int)
      (current_first_offset, current_size : offset_and_size)
    : 'a * int =

    (* The previous field must have ended before the current field
       begins... *)
    assert (prev_after_last_offset <= current_first_offset);

    (* Compute the first offset and the size of potential padding
       (i.e padding between the previous field and the current one). *)
    let padding_first_offset = prev_after_last_offset in
    let padding_size = current_first_offset - padding_first_offset in
    assert (padding_size >= 0);

    (* Treat padding if it exists. *)
    let acc =
      (* Is there padding between the previous field and the current one? *)
      if padding_size > 0
      then
        (* There is some padding: call the field-and-padding-level
           folding function. *)
        let padding = SPadding (padding_first_offset, padding_size) in
        f acc padding
      else
        (* There is no padding: just pass on the field-and-padding-level
           accumulator unchanged. *)
        acc
    in

    (* Treat the current field if it exists. *)
    let acc =
      (* Zero size means it's a dummy field. *)
      if current_size > 0
      then
        let field = SField (current_first_offset, current_size) in
        f acc field
      else
        acc
    in

    (* Pass a pair:
       - the field-and-padding-level accumulator,
       - and the offset after the current field's last offset
       in the offset-and-size-level accumulator. *)
    let current_after_last_offset = current_first_offset + current_size in
    acc, current_after_last_offset
  in

  (* Fold on all the structure fields' offsets-and-sizes. *)
  let (acc, after_last_offset : 'a * int) =
    try
      fold_type_offsets
        (fun (acc' : 'a * int) (offset : offset) ->
           offset_and_size_f acc' (bitsOffset typ offset))
        (acc, 0)
        typ
    with SizeOfError _ -> assert false (* Should not happen if the
                                          assignment [lval1 = lval2;]
                                          was accepted during typing phase. *)
  in

  (* Finish with an artificial offset-and-size of a dummy field positioned
     right after the structure, so that the potential padding after the
     structure's last field is taken into account. *)
  let (acc, _) : ('a * int) =
    try
      offset_and_size_f
        (acc, after_last_offset)
        (bitsSizeOf typ, 0) (* The dummy field. *)
    with SizeOfError _ -> assert false (* Should not happen if the
                                          assignment [lval1 = lval2;]
                                          was accepted during typing phase. *)
  in

  (* Folding done! *)
  acc

(* Fold on all paddings between fields (i.e. offsets and sizes of all the
   paddings) of the given type. *)
let fold_type_paddings (f : 'a -> offset_and_size -> 'a) : 'a -> typ -> 'a =
  fold_type_fields_and_paddings
    (fun acc -> function
       | SField _ -> acc
       | SPadding offset_and_size -> f acc offset_and_size)

(* A version of fold_type_fields_and_paddings that does not need
   the field_or_padding type, so can be put in the interface. *)
let fold_type_fields_and_paddings
    (f : 'a -> (bool * int * int) -> 'a) (acc : 'a) (typ : typ) : 'a =
  let f' acc = function
    | SField (offset, size) -> f acc (true, offset, size)
    | SPadding (offset, size) -> f acc (false, offset, size)
  in
  fold_type_fields_and_paddings f' acc typ


(* ************************************************************************** *)
(** {2 Predefined} *)
(* ************************************************************************** *)

let can_be_cea_function name =
  (String.length name >= 6 &&
   name.[0] = 'F' && name.[1] = 'r' && name.[2] = 'a' &&
   name.[3] = 'm' && name.[4] = 'a' && name.[5] = '_') ||
  (String.length name >= 4 &&
   name.[0] = 't' && name.[1] = 'i' && name.[2] = 's' &&
   name.[3] = '_')

let is_cea_function name =
  Extlib.string_prefix "Frama_C_show_each" name ||
  Extlib.string_prefix "tis_show_each" name

let is_cea_dump_function name = (name = "Frama_C_dump_each")

let is_cea_dump_file_function name =
  Extlib.string_prefix "Frama_C_dump_each_file" name

let is_frama_c_builtin n =
  can_be_cea_function n &&
  (is_cea_dump_function n ||
   is_cea_function n ||
   is_cea_dump_file_function n)

let () = Cil.add_special_builtin_family is_frama_c_builtin

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
