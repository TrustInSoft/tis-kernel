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

(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(* mergecil.ml *)
(* This module is responsible for merging multiple CIL source trees into a
   single, coherent CIL tree which contains the union of all the definitions in
   the source files. It effectively acts like a linker, but at the source code
   level instead of the object code level. *)

open Cil_types
open Cil
open Cil_datatype

open Logic_utils

let dkey = Kernel.register_category "mergecil"


(** ---===[ MERGING PARAMETERS ]===--- *)
module Parameter : sig
  (* Debug printing. *)
  val enable_dump_graph : bool

  (* Find-union performance. *)
  val enable_path_compression : bool

  (* Merging behavior options. *)
  val enable_merging_synonyms : bool
  val enable_aggressive_merging : bool
  val aggressive_merging_static_functions : unit -> bool
  val aggressive_merging_inline_functions : unit -> bool
end = struct

  (** Dump graph of some equivalence tables to [.dot] files. *)
  let enable_dump_graph = false

  (** Whether to use path compression (this concerns find and union). *)
  let enable_path_compression = true

  (** Try to merge structures with the same name. However, do not complain if
      they are not the same. *)
  let enable_merging_synonyms = true

  (** Aggressive merging: We may attempt to merge definitions of static and / or
      inline functions that are included multiple times in the sources. *)
  let enable_aggressive_merging = true
  (* NOTE: This can slow down the merger by an order of magnitude !!! *)

  let aggressive_merging_static_functions () =
    match Kernel.AggressiveMerging.get () with
    | "static" | "both" -> enable_aggressive_merging
    | "off" | "inline" -> false
    | _ -> assert false (* Should have been rejected before as invalid input. *)

  let aggressive_merging_inline_functions () =
    match Kernel.AggressiveMerging.get () with
    | "inline" | "both" -> enable_aggressive_merging
    | "off" | "static" -> false
    | _ -> assert false (* Should have been rejected before as invalid input. *)

end

(* ---===[ AUXILIARY ]===--- *)

module Aux = struct

  (** [is_prefix p s] returns true if [s] starts with the prefix [p]. *)
  let is_prefix p s =
    let p_len = String.length p in
    let s_len = String.length s in
    p_len <= s_len && String.sub s 0 p_len = p

  (* USED: in [have_same_enum_items], [combine_types] and
           [renameToRepresentativeVisitor]. *)
  let same_int64 exp_1 exp_2 =
    match Cil.constFoldToInt exp_1, Cil.constFoldToInt exp_2 with
    | Some i1, Some i2 -> Integer.equal i1 i2
    | _ -> false

  (* NOTE: Useless right now. Kept just because this is the correct way of
     determining if a variable is weak or not. *)
  let is_weak varinfo =
    Cil.hasAttribute "weak" varinfo.vattr

  let _d_varinfo_is_weak fmt varinfo =
    Format.pp_print_string fmt (if is_weak varinfo then "WEAK" else "STRONG")

  let _d_fundec_is_weak fmt fundec =
    Format.fprintf fmt "%a" _d_varinfo_is_weak fundec.svar

end

open Aux

module PP = struct

  let pp_cstruct fmt cstruct =
    Format.pp_print_string fmt (if cstruct then "struct" else "union")

  let pp_enumitems =
    Pretty_utils.pp_list ~pre:"{" ~suf:"}" ~sep:",@ "
      (fun fmt enumitem ->
         Format.fprintf fmt "%s=%a"
           enumitem.eiorig_name
           Printer.pp_exp enumitem.eival)

  let pp_table ~iter ~pp_name ~pp_binding fmt (table : 'table) =
    Format.fprintf fmt "/--<[ %a table ]>--@." pp_name table;
    iter
      (fun key value ->
         Format.fprintf fmt "| %a@." pp_binding (key, value))
      table;
    Format.fprintf fmt "\\------------------------------@."

  let pp_name_of_string name fmt _table =
    Format.pp_print_string fmt name

  let pp_hashtable ~name ~pp_binding fmt table =
    let pp_name = pp_name_of_string name in
    Format.fprintf fmt "%a"
      (pp_table ~iter:Hashtbl.iter ~pp_name ~pp_binding) table

  let pp_string_set_hashtable ~name fmt table =
    let pp_binding fmt (key, _) = Format.pp_print_string fmt key in
    pp_hashtable ~name ~pp_binding fmt table

  let pp_varinfo_set_table ~name fmt table =
    let pp_binding fmt (varinfo, _) =
      Format.fprintf fmt "%a" Printer.pp_varinfo varinfo
    in
    pp_table
      ~iter:Varinfo.Hashtbl.iter
      ~pp_name:(pp_name_of_string name)
      ~pp_binding
      fmt table

  let pp_list_as_args_list pp_arg fmt =
    let pp_non_empty_args_list =
      Pretty_utils.pp_list ~pre:"(" ~suf:")" ~sep:",@ " pp_arg
    in
    function
    | [] -> Format.pp_print_string fmt "()"
    | args -> pp_non_empty_args_list fmt args

end

open PP

(** We use a number of alpha conversion tables. We ought to keep one table for
    each name space.
    NOTE: Unfortunately, because of the way the C lexer works, type names must
    be different from variable names! So we have one alpha table both for
    variables and types. *)
type alpha_conversion_table =
  (string, location Alpha.alphaTableData ref) Hashtbl.t


(* ---===[ FILE AND IN-FILE INDICES ]===--- *)

module MyIndex : sig
  type t
  val zero : t
  val succ : t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val pretty : Format.formatter -> t -> unit
end = struct
  include Integer
  let pretty = pretty ~hexa:false
end

module FileIndex = MyIndex
module InFileIndex = MyIndex


(* ---===[ THE MERGED FILE ]===--- *)

(* The merged file intermediary form. *)
module MergedFile : sig

  (* Basically this is a list of indexed globals with a double index:
     the index of the file that the global comes from and the index of the
     global in that file. *)
  type t

  (* Prepare from the list of input files. *)
  val of_input_files : file list -> t

  (* Build the merged file. *)
  val to_result_file : t -> string -> file

  (* Operations on the indexed globals. *)
  val map : (FileIndex.t -> InFileIndex.t -> global -> global list) -> t -> t
  val iter : (FileIndex.t -> InFileIndex.t -> global -> unit) -> t -> unit

end = struct

  let dkey = Kernel.register_category "mergecil:MergedFile"

  type file_index = FileIndex.t
  type in_file_index = InFileIndex.t

  (* We keep the file names here. *)
  (* NOTE: Right now this is pretty useless. *)
  let file_names : (file_index, string) Hashtbl.t = Hashtbl.create 43
  let add_file_name file_index file =
    Hashtbl.add file_names file_index file.fileName
  let _file_name_of_file_index file_index =
    try Hashtbl.find file_names file_index
    with Not_found -> failwith "file_name_of_file_index"

  type indexed_global = (file_index * in_file_index) * global
  type t = indexed_global list

  let make_indexed_global file_index in_file_index global =
    ((file_index, in_file_index), global)

  (* NOTE: The key aspect of this function is to remain tail-recursive, as the
           number of globals in each file can be enormous. *)
  let of_input_files files =
    let file_f (file_index, file_f_acc) file =
      add_file_name file_index file;
      let global_f (in_file_index, global_f_acc) global =
        let indexed_global =
          make_indexed_global file_index in_file_index global
        in
        let global_f_acc' = indexed_global :: global_f_acc in
        let in_file_index' = InFileIndex.succ in_file_index in
        (in_file_index', global_f_acc')
      in
      let _in_file_index, file_f_acc' =
        let global_f_acc = (InFileIndex.zero, file_f_acc) in
        List.fold_left global_f global_f_acc file.globals
      in
      let file_index' = FileIndex.succ file_index in
      (file_index', file_f_acc')
    in
    let _file_index, indexed_globals =
      List.fold_left file_f (FileIndex.zero, []) files
    in
    List.rev indexed_globals

  let map global_f indexed_globals =
    let f acc ((file_index, in_file_index), global) =
      let indexed_globals' =
        let globals' = global_f file_index in_file_index global in
        List.rev_map (make_indexed_global file_index in_file_index) globals'
      in
      List.rev_append indexed_globals' acc
    in
    let acc' = List.fold_left f [] indexed_globals in
    List.rev acc'

  let iter global_f indexed_globals =
    let f ((file_index, in_file_index), global) =
      global_f file_index in_file_index global
    in
    List.iter f indexed_globals

  let to_result_file merged_file result_file_name =
    (* Accumulate here the globals of the merged file. *)
    let types     : global list ref = ref [] in
    let variables : global list ref = ref [] in

    (* Add a single global to the merged file. *)
    let push_indexed_global _file_index _in_file_index global =
      Kernel.debug ~dkey "Pushing a global:@.@[%a@]"
        Printer.pp_global global;
      Cil.pushGlobal ~types ~variables global
    in

    (* Add all the globals. *)
    iter push_indexed_global merged_file;

    (* Get all the globals in the rignt order. *)
    let all_globals = List.rev_append !types (List.rev !variables) in

    (* Build the file. *)
    {
      fileName = result_file_name;
      globals = all_globals;
      globinit = None;
      globinitcalled = false
    }

end


(* ---===[ NODES AND MERGING ]===--- *)

(** Possible reasons for a failure when merging:
    - two definitions of a given object,
    - or definitions of two different objects. *)
type merge_failure_reason =
  (** Two enumerations differ on number of enumeration elements. *)
  | Enum_elements_number of enuminfo * enuminfo
  (** Two enumerations differ on names of enumeration items. *)
  | Enum_item_names of enuminfo * string * string
  (** Two enumerations differ on values of enumeration items. *)
  | Enum_item_values of enuminfo * string * exp * exp
  (** Two integer types have different kinds. *)
  | Int_types_different of typ * typ
  (** Two floating point types types have different kinds. *)
  | Float_types_different of typ * typ
  (** Two array types differ on their size. *)
  | Array_sizes_different of typ * typ
  (** Two function declarations differ on variadicity. *)
  | Fun_variadic_different of typ * typ
  (** Two function declarations differ on number of arguments. *)
  | Fun_args_number_different of typ * typ
  (** Type constructors of two types differ. *)
  | Type_constr_different of typ * typ
  (** Two composites differ, because one is a struct and one is a union. *)
  | Comp_struct_and_union of compinfo * compinfo
  (** Two composites differ on number of fields. *)
  | Comp_fields_number_different of compinfo * compinfo
  (** Two composites differ because of bit-field info. *)
  | Comp_bitfield_info_different of compinfo * fieldinfo * fieldinfo
  (** Two composites differ on a certain field's attributes. *)
  | Comp_field_attrs_different of compinfo * fieldinfo * fieldinfo
  (** Two composites are not isomorphic. *)
  | Comp_not_isomorphic of compinfo * compinfo * merge_failure_reason
  (** Two types are not isomorphic. These can be either:
      - two definitions of a given type (if the [typeinfos] are the same),
      - or definitions of two given types (if the [typeinfos] are different). *)
  | Types_not_isomorphic of typeinfo * typeinfo * merge_failure_reason

exception Merge_failure of merge_failure_reason

let rec pp_merge_failure_reason fmt =
  function
  | Enum_elements_number (_old_enuminfo, _new_enuminfo) ->
    Format.fprintf fmt "different number of enumeration elements"
  | Enum_item_names (_enuminfo, _old_name, _new_name) ->
    Format.fprintf fmt "different names for enumeration items"
  | Enum_item_values (_enuminfo, _name, _old_value, _new_value) ->
    Format.fprintf fmt "different values for enumeration items"
  | Int_types_different (old_typ, new_typ) ->
    Format.fprintf fmt
      "different integer types %a and %a"
      Printer.pp_typ old_typ
      Printer.pp_typ new_typ
  | Float_types_different (old_typ, new_typ) ->
    Format.fprintf fmt
      "different floating point types %a and %a"
      Printer.pp_typ old_typ
      Printer.pp_typ new_typ
  | Array_sizes_different (_old_typ, _new_typ) ->
    Format.fprintf fmt "different array sizes"
  | Fun_variadic_different (_old_typ, _new_typ) ->
    Format.fprintf fmt "different vararg specifiers"
  | Fun_args_number_different (_old_typ, _new_typ) ->
    Format.fprintf fmt "different number of arguments"
  | Type_constr_different (old_typ, new_typ) ->
    Format.fprintf fmt
      "different type constructors: %a vs. %a"
      Printer.pp_typ old_typ
      Printer.pp_typ new_typ
  | Comp_struct_and_union (_old_compinfo, _new_compinfo) ->
    Format.fprintf fmt "different struct/union types"
  | Comp_fields_number_different (old_compinfo, new_compinfo) ->
    let old_field_count = List.length old_compinfo.cfields in
    let new_field_count = List.length new_compinfo.cfields in
    Format.fprintf fmt
      "different number of fields in %a %s and %a %s: %d != %d."
      pp_cstruct old_compinfo.cstruct old_compinfo.cname
      pp_cstruct old_compinfo.cstruct new_compinfo.cname
      old_field_count new_field_count
  | Comp_bitfield_info_different (_compinfo, _old_fieldinfo, _new_fieldinfo) ->
    Format.fprintf fmt "different bitfield info"
  | Comp_field_attrs_different (_compinfo, _old_fieldinfo, _new_fieldinfo) ->
    Format.fprintf fmt "different field attributes"
  | Comp_not_isomorphic (old_compinfo, new_compinfo, reason) ->
    let fields_old : string =
      Pretty_utils.sfprintf "%a"
        Printer.pp_global
        (GCompTag(old_compinfo, Location.unknown))
    in
    let fields_new : string =
      Pretty_utils.sfprintf "%a"
        Printer.pp_global
        (GCompTag(new_compinfo, Location.unknown))
    in
    let fullname_old = Cil.compFullName old_compinfo in
    let fullname_new = Cil.compFullName new_compinfo in
    begin
      match fullname_old = fullname_new, fields_old = fields_new
      (* Could also use a special comparison *)
      with
      | true, true ->
        Format.fprintf fmt
          "Definitions of %s are not isomorphic. \
           Reason follows:@\n@?%a"
          fullname_old
          pp_merge_failure_reason reason
      | false, true ->
        Format.fprintf fmt
          "%s and %s are not isomorphic. \
           Reason follows:@\n@?%a"
          fullname_old fullname_new
          pp_merge_failure_reason reason
      | true, false ->
        Format.fprintf fmt
          "Definitions of %s are not isomorphic. \
           Reason follows:@\n@?%a@\n@?%s@?%s"
          fullname_old
          pp_merge_failure_reason reason
          fields_old fields_new
      | false, false ->
        Format.fprintf fmt
          "%s and %s are not isomorphic. \
           Reason follows:@\n@?%a@\n@?%s@?%s"
          fullname_old fullname_new
          pp_merge_failure_reason reason
          fields_old fields_new
    end
  | Types_not_isomorphic (old_typeinfo, new_typeinfo, reason) ->
    let old_name = old_typeinfo.tname in
    let new_name = new_typeinfo.tname in
    if old_name = new_name
    then
      Format.fprintf fmt
        "Definitions of type %s are not isomorphic. \
         Reason follows:@\n@?%a"
        old_name
        pp_merge_failure_reason reason
    else
      Format.fprintf fmt
        "Types %s and %s are not isomorphic. \
         Reason follows:@\n@?%a"
        old_name new_name
        pp_merge_failure_reason reason


(** Node. *)

module Node : sig
  (** Find-union node for declarations, definitions, etc. *)

  module Loc : sig
    (** Location where node is defined and index within the file of the
        definition. May also be undefined: this means that the given node
        actually DOES NOT appear in the given file. *)
    type t
    type in_file_index = InFileIndex.t

    val defined : location -> in_file_index -> t
    val undefined : t
    val is_defined : t -> bool
    val get_in_file_index_opt : t -> in_file_index option
    val pretty : Format.formatter -> t -> unit
    val pretty' : Format.formatter -> t -> unit
  end

  type ('id, 'data) t
  type file_index = FileIndex.t

  val make : (file_index * 'id) -> 'data -> Loc.t -> ('id, 'data) t
  val make_my_node': ('id, 'data) t -> 'data -> ('id, 'data) t

  (* [get_id node]
     USED: outside of [Merging] only in [GlobalVarsEnv.add_node]. *)
  val get_id : ('id, 'data) t -> 'id
  val get_file_index : ('id, 'data) t -> file_index
  val get_data : ('id, 'data) t -> 'data

  val get_id_in_file : ('id, 'data) t -> (file_index * 'id)

  val get_loc : ('id, 'data) t -> Loc.t
  val refine_loc : ('id, 'data) t -> Loc.t -> unit
  val get_in_file_index_opt : ('id, 'data) t -> Loc.in_file_index option
  val get_location_opt : ('id, 'data) t -> location option

  (** Does this node really appear in its file? *)
  val is_defined : ('id, 'data) t -> bool

  (** Get the node one step closer to the representative. If the representative
      pointer of a node points to the node itself then this node is the
      representative. *)
  val get_rep_ptr : ('id, 'data) t -> ('id, 'data) t
  val is_representative : ('id, 'data) t -> bool

  (** Take two nodes, set the representative pointer of the one which is defined
      later to the representative of the one which is defined earlier. *)
  val set_rep_of_later_to_rep_of_earlier :
    ('id, 'data) t -> ('id, 'data) t -> unit

  (* Whether we have merged the synonyms for the node of this id. *)
  val are_syns_merged : ('id, 'data) t -> bool
  val mark_syns_merged : ('id, 'data) t -> unit


  (** [find node] returns the representative for a node and compresses the paths
      in the process. *)
  (** USED: outside this module only in [treat_global_varinfo_pass_1]. *)
  val find : ?do_path_compression:bool -> ('id, 'data) t -> ('id, 'data) t

  (** [union node_1 node_2] unions the equivalence classes of two provided nodes
      and returns a pair:
      - the new representative of the nodes' common class,
      - an undo function that can be used to reverse the union operation.
      We prefer to choose as the representative the node defined earlier. We try
      not to use as representatives nodes that are not defined in their files.
      Note: Make sure that between the union operation and calling the undo
      function you do not do any path compression. *)
  (* NOTE: The [undo] is actually used only in the [match_compinfo] function.
           Apparently it was necessary in this case in order to avoid potential
           infinite recursion, though I did not check if it is actually
           necessary. *)
  (* USED: in [match_compinfo], [match_enuminfo], [match_typeinfo],
           [treat_global_varinfo_pass_1], [treat_one_global_pass_2]. *)
  val union :
    (Format.formatter -> ('id, 'data) t -> unit) ->
    ('id, 'data) t -> ('id, 'data) t -> ('id, 'data) t * (unit -> unit)

end = struct

  module Loc : sig
    type t
    type in_file_index = InFileIndex.t
    val defined : location -> in_file_index -> t
    val undefined : t
    val is_defined : t -> bool
    val get_location_opt : t -> location option
    val get_in_file_index_opt : t -> in_file_index option
    val pretty : Format.formatter -> t -> unit
    val pretty' : Format.formatter -> t -> unit
  end = struct

    type in_file_index = InFileIndex.t

    type defined = {
      location : location;
      in_file_index : in_file_index;
    }

    type t =
      | Defined of defined
      | Undefined

    let defined location in_file_index = Defined { location; in_file_index }
    let undefined = Undefined

    let get_location_opt = function
      | Defined defined -> Some defined.location
      | Undefined -> None

    let get_in_file_index_opt = function
      | Defined defined -> Some defined.in_file_index
      | Undefined -> None

    let is_defined loc = loc <> Undefined

    let pretty fmt = function
      | Defined defined ->
        Format.fprintf fmt "%a(%a)"
          InFileIndex.pretty defined.in_file_index
          Printer.pp_location defined.location
      | Undefined -> ()

    (** Debug printing. *)
    let pretty' fmt = function
      | Defined defined ->
        Format.fprintf fmt "%a at %a"
          InFileIndex.pretty defined.in_file_index
          Printer.pp_location defined.location
      | Undefined -> Format.fprintf fmt "undefined"

  end

  type file_index = FileIndex.t

  (** Find-union node for declarations, definitions, etc. *)
  type ('id, 'data) t = {
    id : 'id; (* The actual name. *)
    file_index : file_index; (* The file index. *)
    data : 'data; (* Data associated with the node. *)
    mutable defn_loc : Loc.t;
    (* Location where node is defined and index within the file of the
       definition. If undefined then it means that this node actually DOES NOT
       appear in the given file. In rare occasions we need to talk in a given
       file about types that are not defined in that file. This happens with
       undefined structures but also due to cross-contamination of types in a
       few of the cases of combineType (see the definition of [combine_types]).
       We try never to choose as representatives nodes without a definition. We
       also choose as representative the one that appears the earliest. *)
    mutable rep_ptr : ('id, 'data) t;
    (* A pointer to another node in its class (one step closer to the
       representative). The [rep_ptr] node is always in an earlier file,
       except for the case where a name is undefined in one file and defined in
       a later file. If this pointer points to the node itself then this is the
       representative. *)
    mutable are_syns_merged : bool
    (* Whether we have merged the synonyms for the node of this name. *)
  }

  let make (file_index, id) data defn_loc =
    let rec node = {
      id; file_index; data; defn_loc;
      rep_ptr = node; (* Link back to this node. *)
      are_syns_merged = false;
    } in
    node

  (* -- id, file_index, data, id_in_file -- *)
  let get_id node = node.id
  let get_file_index node = node.file_index
  let get_data node = node.data
  let get_id_in_file node = (get_file_index node, get_id node)

  (* -- defn_loc -- *)
  let get_loc node = node.defn_loc
  let get_in_file_index_opt node = Loc.get_in_file_index_opt (get_loc node)
  let get_location_opt node = Loc.get_location_opt (get_loc node)

  let is_defined node = Loc.is_defined (get_loc node)

  let refine_loc node node_loc =
    assert (not (is_defined node));
    assert (Loc.is_defined node_loc);
    node.defn_loc <- node_loc

  (* -- rep_ptr -- *)
  let get_rep_ptr node = node.rep_ptr
  let is_representative node = get_rep_ptr node == node
  let update_rep_ptr node new_rep_ptr = node.rep_ptr <- new_rep_ptr

  let set_rep_of_later_to_rep_of_earlier node_1 node_2 =
    let file_index_1 = get_file_index node_1 in
    let file_index_2 = get_file_index node_2 in
    if file_index_1 < file_index_2
    then update_rep_ptr node_2 (get_rep_ptr node_1)
    else update_rep_ptr node_1 (get_rep_ptr node_2)

  (* -- are_syns_merged flag -- *)
  let are_syns_merged node = node.are_syns_merged
  let mark_syns_merged node = node.are_syns_merged <- true

  (* A strange low-level operation performed only in the function
     [global_annot_pass_2] in the [Dmodel_annot] case. *)
  let make_my_node' my_node data =
    let my_node' = { my_node with data } in
    update_rep_ptr my_node  my_node'; (* my_node' represents my_node *)
    update_rep_ptr my_node' my_node';
    my_node'

  (* -- find and union operations -- *)

  (* Find the representative of the equivalence class that this node belongs to,
     with or without performing path compression. *)
  let rec find ?(do_path_compression=true) node =
    (* TODO: Kernel.debug ~dkey "find (%a)" pp_node node; *)
    (* Is it the representative? *)
    if is_representative node
    then begin
      (* Yes, this is the representative. We return it directly. *)
      (* TODO: Kernel.debug ~dkey "> found = %a" pp_node node; *)
      node
    end else
      (* No, this is not the representative. We go deeper. *)
      let representative = find ~do_path_compression (get_rep_ptr node) in
      assert (is_representative representative);
      (* Maybe compress the paths. *)
      if Parameter.enable_path_compression
      && do_path_compression
      && get_rep_ptr node != representative
      then update_rep_ptr node representative;
      (* Return the found representative. *)
      representative

  (* Union two nodes and return the new representative. We prefer as the
     representative the node defined earlier. We try not to use as
     representatives nodes that are not defined in their files. We return a
     function for undoing the union. Make sure that between the union and the
     undo you do not do path compression. *)
  let union pp_node node_1 node_2 =
    Kernel.debug ~dkey "union (%a, %a)" pp_node node_1 pp_node node_2;
    (* Move to the representatives of each node. *)
    let node_1 = find node_1 in
    let node_2 = find node_2 in
    match node_1 == node_2 with
    (* The nodes are already equivalent. *)
    | true ->
      (* It can happen that we are trying to union two nodes that are already
         equivalent. This is because between the time we check that two nodes
         are not already equivalent and the time we invoke the union operation
         we check type isomorphism, and this which might change the equivalence
         classes. *)
      Kernel.debug ~dkey "union: nodes already equivalent! (%a)" pp_node node_1;
      node_1, fun x -> x
    (* The nodes are different. *)
    | false ->
      (* Choose the node which will be the representative. *)
      let earlier_node, later_node =
        match is_defined node_1, is_defined node_2 with
        (* Both nodes are undefined, it does not matter which one we choose. *)
        | false, false ->
          (* NOTE: This may happen for example with anonymous enums declared
             inside structs, i.e. [struct s { enum e; } x;] repeated in two
             files. *)
          Kernel.debug ~current:true
            "Merging two undefined elements: %a and %a"
            pp_node node_1 pp_node node_2;
          node_1, node_2
        (* One node is defined, the other is not. We choose the defined one. *)
        | true, false -> (* node_1 is defined, node_2 is not *) node_1, node_2
        | false, true -> (* node_2 is defined, node_1 is not *) node_2, node_1
        (* Both nodes are defined. We choose the earliest one. *)
        | true, true ->
          let file_index_1 = get_file_index node_1 in
          let file_index_2 = get_file_index node_2 in
          match FileIndex.compare file_index_1 file_index_2 with
          | -1 -> (* file_index_1 < file_index_2 *) node_1, node_2
          | +1 -> (* file_index_1 > file_index_2 *) node_2, node_1
          |  z -> (* file_index_1 = file_index_2 *) assert (z = 0);
            begin
              (* File indices are equal, so the nodes are in the same file.
                 We choose the one with the earliest in-file index . *)
              let in_file_index_1 = Extlib.the (get_in_file_index_opt node_1) in
              let in_file_index_2 = Extlib.the (get_in_file_index_opt node_2) in
              match InFileIndex.compare in_file_index_1 in_file_index_2 with
              | -1 -> (* in_file_index_1 < in_file_index_2 *) node_1, node_2
              | +1 -> (* in_file_index_1 > in_file_index_2 *) node_2, node_1
              |  z -> (* in_file_index_1 = in_file_index_2 *) assert (z = 0);
                (* They have the same index within the file. *)
                Kernel.warning
                  "Merging two elements (%a and %a) in the same file (%a) \
                   with the same index (%a) within the file"
                  pp_node node_1 pp_node node_2
                  FileIndex.pretty file_index_1
                  InFileIndex.pretty in_file_index_1;
                node_1, node_2
            end

      in
      Kernel.debug ~dkey "union: chose node %a" pp_node earlier_node;
      (* Prepare the [undo] function which restores the representatives. *)
      let undo =
        let old_representative = get_rep_ptr later_node in
        fun () -> update_rep_ptr later_node old_representative
      in
      (* Do the union: set the representative of the "loser" (i.e. [later_node])
         to the representative of the "winner" (i.e. [earlier_node]). *)
      update_rep_ptr later_node earlier_node;
      (* Return the "winner" and the [undo] function. *)
      earlier_node, undo

end

(** Merging. *)

module type HashedAndOrderedType = sig
  include Hashtbl.HashedType
  (** The [Hashtbl.HashedType] signature includes:
        type t
        val equal : t -> t -> bool
        val hash : t -> int
  *)
  val compare : t -> t -> int
  (* NOTE: Instead of putting the [compare] function directly here it would be
     more elegant to put [include Map.OrderedType], but we cannot, because this
     signature also contains type t. *)
end

module type MergeableTypeS = sig
  include HashedAndOrderedType

  val name : string

  (** Whether this name should be considered for merging or not.
      It is either implemented as:
      - in the most common [PlainMerging] case, it is always returning [true],
        unless the name is prefixed by "__anon", when it returns [false];
      - in all the other cases, it is just always returning [true]. *)
  val should_merge_synonym : t -> bool

  val pretty : Format.formatter -> t -> unit
end

module type MergingS = sig

  (* In the PlainMerging case the [node_id] type is string. *)
  type node_id
  type node_file_index = FileIndex.t

  type node_id_in_file = node_file_index * node_id
  val pp_id_in_file : Format.formatter -> node_id_in_file -> unit

  type 'node_data my_node = (node_id, 'node_data) Node.t
  (* Actual node types look like that:
     type varinfo_node = (string, varinfo) node
     type compinfo_node = (string, compinfo) node
     type enuminfo_node = (enuminfo, enuminfo) node
     type typeinfo_node = (string, typeinfo) node
  *)
  val pp_node : Format.formatter -> 'node_data my_node -> unit

  module Eq_table : sig
    type 'node_data t
    type key = node_id_in_file

    (* USED: in [global_annot_pass_2], [treat_one_global_pass_2], and
             [renameToRepresentativeVisitor] (which is in turn used exactly in
             the two mentioned [*_pass_2] functions). *)
    val find : 'node_data t -> key -> 'node_data my_node

    (* USED: in [global_annot_pass_2] and [merge]. *)
    (* NOTE: if it was not used in [global_annot_pass_2] the nodes would be
       put in the table only through [mk_self_node]... *)
    val add : 'node_data t -> 'node_data my_node -> unit

    (* USED: in [merge] to copy all the nodes from [aggressive_merging] eq_table
             to [vars] eq_table. *)
    val copy : 'node_data t -> 'node_data t -> unit

  end

  module Syn_table : sig
    type 'node_data t
    type 'node_data match_f =
      node_file_index -> 'node_data ->
      node_file_index -> 'node_data -> unit
  end

  type 'node_data tables = 'node_data Eq_table.t * 'node_data Syn_table.t

  (* USED: to create all the tables. *)
  val create_tables : string -> 'node_data tables

  (* USED: in [clear_environment] to clear all the tables. *)
  val clear_tables : 'node_data tables -> unit

  val pp_eq_table : Format.formatter -> 'node_data tables -> unit
  val pp_syn_table : Format.formatter -> 'node_data tables -> unit
  val dump_eq_graph : 'node_data tables -> unit

  (** [mk_self_node tables node_id_in_file node_data node_loc]
      makes a new node with a self loop (i.e. it is its own representative). *)
  (* USED: outside this module only in [treat_global_varinfo_pass_1]. *)
  val mk_self_node : 'node_data tables -> node_id_in_file:node_id_in_file ->
    node_data:'node_data -> node_loc:Node.Loc.t -> 'node_data my_node

  (** [get_node tables node_id_in_file node_data node_loc]
      IF
        A node corresponding to the given [node_id_in_file]
        (i.e. the given file index and node id)
        exists in any equivalence class?
      THEN:
      - we return the representative node of this the provided node's class.
      OTHERWISE
      - We make a node for these file index and node id
        (using the provided data and node location)
      - we put it in the tables as a new singleton equivalence class,
      - and we return it. *)
  val get_node : 'node_data tables ->
    node_id_in_file -> 'node_data -> 'node_data my_node

  (* [get_node] variant where we refine the node's location if possible. *)
  val get_node' : 'node_data tables ->
    node_id_in_file -> 'node_data -> Node.Loc.t -> 'node_data my_node

  (** USED: in [treat_one_global_pass_1], cases: [GFun], [GType], [GCompTag],
                                                 [GEnumTag], [GAnnot];
            ([GAnnot] case is in [global_annot_pass_2] function.) *)
  val create_node : 'node_data tables ->
    node_id_in_file -> 'node_data -> Node.Loc.t -> unit

  (** [find_replacement tables node_id_in_file]
      If the node corresponding to the given file index and id belongs to an
      equivalence class (i.e. is in the eq_table), but is not his
      class's representative, then it returns the data and the file index
      associated with this class's representative.
      If the node corresponding to the given file index and it either
      - does not belong to any class
      - or is the representative of its class
      then it returns [None]. *)
  (** USED: in [renameVisitor], [renameInlineOrStaticVisitor],
            [process_varinfo];
      AND through [has_no_replacement]. *)
  val find_replacement : 'node_data tables ->
    node_id_in_file -> ('node_data * node_file_index) option

  (** [has_no_replacement tables node_id_in_file] returns [true]
      if and only if the node corresponding to the given file index and id
      either:
      - does not belong to any class
      - or is the representative of its class. *)
  (* NOTE: This was made a separate function, because [find_replacement] was
     used in many cases only for this purpose which involved useless matching
     of the result afterwards. *)
  (* USED: in [global_annot_pass_2] and
           in [treat_one_global_pass_2] [GCompTag], [GEnumTag], [GType]. *)
  val has_no_replacement : 'node_data tables -> node_id_in_file -> bool

  (** [merge_synonyms tables merge_f] attempts to merge all the synonyms
      registered in the synonyms table.

      [merge_f file_index_1 data_1 file_index_2 data_2] is a function that
      tries to merge the nodes corresponding to:
      - node_1 ~ (file_index_1, data_1) and
      - node_2 ~ (file_index_2, data_2);
      If it succeeds then nothing happens,
      if it fails it raises a [Merge_failure] exception.

      The aim of the [merge_synonyms] function is basically to try to apply the
      given merging function [merge_f] to all the possible pairs of nodes that
      have been registered in the synonyms table, i.e. literally try to merge
      all potentially mergable synonyms (as defined by the synonyms table).
      However, we do not actually call the merging function on all possible
      pairs of nodes, thanks to two properties of our "merging":
      - "Merging" here means (on the node level!) "putting in the same
        equivalence class". So when two nodes [x] and [y] have been already
        merged together, then if we merge another node [z] with [x] it is also
        merged with [y]. Thus when we are going through all the nodes we can
        just try to merge each node with a single representative of each
        equivalence class and not with every other node.
      - Also every node that has been treated is permanently marked as done
        (i.e. [Node.mark_syns_merged]) and no longer needs be considered. This
        facilitates the algorithm.
  *)
  val merge_synonyms : 'node_data tables ->
    'node_data Syn_table.match_f -> unit

end

module Merging(NodeId : MergeableTypeS) :
  MergingS with type node_id = NodeId.t =
struct

  let dkey = Kernel.register_category ("mergecil:" ^ NodeId.name)

  type node_id = NodeId.t
  let pp_node_id = NodeId.pretty

  type node_file_index = FileIndex.t
  let pp_file_index = FileIndex.pretty

  type node_id_in_file = node_file_index * node_id

  module NodeIdInFile :
    HashedAndOrderedType with type t = node_id_in_file =
  struct
    type t = node_id_in_file

    let hash (file_index, id) =
      FileIndex.hash file_index + NodeId.hash id

    let equal (file_index_1, id_1) (file_index_2, id_2) =
      FileIndex.equal file_index_1 file_index_2 && NodeId.equal id_1 id_2

    let compare (file_index_1, id_1) (file_index_2, id_2) =
      match FileIndex.compare file_index_1 file_index_2 with
      | 0 -> NodeId.compare id_1 id_2
      | res -> res
  end

  type 'node_data my_node = (node_id, 'node_data) Node.t


  (* Pretty printing. *)

  let pp_id_in_file fmt (file_index, node_id) =
    Format.fprintf fmt "`%a' @@ file %a"
      pp_node_id node_id
      pp_file_index file_index

  let pp_node_description fmt ((node_file_index, node_id), node_loc) =
    let pp_node_file_index fmt =
      Format.fprintf fmt " @@ file %a" pp_file_index
    in
    let pp_node_loc_if_defined fmt node_loc =
      if Node.Loc.is_defined node_loc
      then Format.fprintf fmt "with loc=(%a)" Node.Loc.pretty' node_loc
    in
    Format.fprintf fmt "N[%a%a%a]"
      pp_node_id node_id
      pp_node_file_index node_file_index
      pp_node_loc_if_defined node_loc

  let pp_node fmt node =
    pp_node_description fmt (Node.get_id_in_file node, Node.Loc.undefined)

  let _pp_node_with_loc fmt node =
    pp_node_description fmt (Node.get_id_in_file node, Node.get_loc node)


  (** The basic merging table scaffolding for bot equivalence and synonyms
      tables. *)
  module MergingTable
      (MergingTableDescription : sig val table_kind : string end)
      (MergingTableKey : sig
         type key
         val of_node : 'node_data my_node -> key
         val pretty : Format.formatter -> key -> unit
         include HashedAndOrderedType with type t = key
       end) =
  struct

    let table_kind = MergingTableDescription.table_kind

    type key = MergingTableKey.t
    let key_of_node = MergingTableKey.of_node
    let pp_key = MergingTableKey.pretty

    (* The hashtable underneath and the merging table type. *)
    module Table = Hashtbl.Make(MergingTableKey)

    type 'node_data t = {
      name : string;
      table : ('node_data my_node) Table.t
    }

    let pp_name fmt (table: 'node_data t) =
      Format.fprintf fmt "%s %s" table.name table_kind

    (* Checking table invariants. *)
    exception Invariant_is_false

    let check_binding_invariant (key : key) node =
      MergingTableKey.equal key (key_of_node node)

    let check_table_invariant table =
      try
        Table.iter
          (fun key value ->
             if not (check_binding_invariant key value)
             then raise Invariant_is_false)
          table.table;
        true
      with Invariant_is_false -> false

    (** Some table operations. *)

    let create table_name =
      Kernel.debug ~dkey "creating %s %s table" table_name table_kind;
      { name = table_name;
        table = Table.create 137; }

    let clear table =
      assert (check_table_invariant table);
      Kernel.debug ~dkey "clearing %a table" pp_name table;
      Table.clear table.table

    let is_empty table = Table.length table.table = 0

    let add table node =
      let key = key_of_node node in
      assert (check_table_invariant table);
      Kernel.debug ~dkey "adding %a to %a table"
        pp_node node pp_name table;
      if Table.mem table.table key then
        Kernel.debug ~dkey "%a table: overloading existing binding!@,(%a -> %a)"
          pp_name table pp_key key pp_node node;
      Table.add table.table key node;
      assert (check_table_invariant table)

    let iter f table =
      assert (check_table_invariant table);
      Table.iter f table.table

    let pretty fmt =
      let pp_binding fmt (_, node) =
        match Node.is_representative node with
        | true -> Format.fprintf fmt "%a" pp_node node
        | false -> Format.fprintf fmt "%a -> %a"
                     pp_node node pp_node (Node.get_rep_ptr node)
      in
      pp_table ~pp_binding ~iter ~pp_name fmt

  end

  module Eq_table : sig
    type 'node_data t
    type key = node_id_in_file
    val create : string -> 'node_data t
    val find : 'node_data t -> key -> 'node_data my_node
    val add : 'node_data t -> 'node_data my_node -> unit
    (* val iter : (key -> 'node_data my_node -> unit) -> 'node_data t -> unit *)
    val copy : 'node_data t -> 'node_data t -> unit
    val clear : 'node_data t -> unit
    val pretty : Format.formatter -> 'node_data t -> unit
    val dump_graph : 'node_data t -> unit
  end = struct

    (** The basic merging table scaffolding. *)

    module Description = struct let table_kind = "equivalence" end

    module Key = struct
      type key = node_id_in_file
      let of_node = Node.get_id_in_file
      let pretty = pp_id_in_file
      include NodeIdInFile
    end

    include MergingTable(Description)(Key)

    (** The specific equivalence classes table stuff. *)

    let find table key =
      assert (check_table_invariant table);
      Table.find table.table key

    module Iter_sorted = FCMap.Make(Key)

    let iter_sorted f table =
      (* Add all the elements of the given hashtable one by one to an empty
         association table, then iterate f on them in order. *)
      assert (check_table_invariant table);
      let sorted =
        Table.fold Iter_sorted.add table.table Iter_sorted.empty
      in
      Iter_sorted.iter f sorted

    (* Overriding [iter] with [iter_sorted]. *)
    (* NOTE: This might be unnecessary, but I prefer to keep the semantics. *)
    let iter = iter_sorted

    let copy src_table dest_table =
      iter (fun _key node -> add dest_table node) src_table

    (* Dump a graph defined by the table into "graph_[table.name].dot" file. *)
    let dump_graph (table: 'node_data t) : unit =
      let graph_of_what = table.name in
      if is_empty table then
        Kernel.debug ~dkey "Graph for %s is empty." graph_of_what
      else
        begin
          let graph_out_filename =
            Format.asprintf "graph_%s.dot" graph_of_what
          in
          Kernel.debug ~dkey
            "Printing the %s graph to %s..."
            graph_of_what graph_out_filename;
          (* Prepare the output channel. *)
          let graph_out_channel = open_out graph_out_filename in
          let graph_fmt = Format.formatter_of_out_channel graph_out_channel in
          (* Start the graph. *)
          Format.fprintf graph_fmt "digraph \"%s\" {\n" graph_of_what;
          Format.fprintf graph_fmt "  label=\"%s\";\n" graph_of_what;
          (* Iterate on the graph nodes. *)
          iter
            (fun (node_file_index, node_id) node ->
               assert (check_binding_invariant (node_file_index, node_id) node);
               let subgraph_id : string =
                 Format.asprintf "\"cluster_%a\"" pp_file_index node_file_index
               in
               let subgraph_label : string =
                 Format.asprintf "\"File #%a\"" pp_file_index node_file_index
               in
               let node_id : string =
                 Format.asprintf "\"%a\"" pp_node node
               in
               let node_label : string =
                 let def_or_undef =
                   if not (Node.is_defined node) then " (undef)" else ""
                 in
                 Format.asprintf "\"%a%s\""
                   pp_node_id (Node.get_id node) def_or_undef
               in
               (* A graph node. *)
               Format.fprintf graph_fmt
                 "  subgraph %s { label=%s; %s [label=%s]};\n"
                 subgraph_id subgraph_label node_id node_label;
               if not (Node.is_representative node)
               then
                 (* If this is not the representative node, make an arrow to the
                    representative node. *)
                 let representative_node = Node.get_rep_ptr node in
                 let representative_node_id : string =
                   Format.asprintf "\"%a\"" pp_node representative_node
                 in
                 (* A graph arrow. *)
                 Format.fprintf graph_fmt
                   "  %s -> %s;\n"
                   node_id representative_node_id
            ) table;
          (* End the graph. *)
          Format.fprintf graph_fmt "}\n";
          close_out graph_out_channel
        end

  end


  module Syn_table : sig
    type 'node_data t
    val create : string -> 'node_data t
    val clear : 'node_data t -> unit
    val add : 'node_data t -> 'node_data my_node -> unit
    val pretty : Format.formatter -> 'node_data t -> unit
    type 'node_data match_f =
      node_file_index -> 'node_data -> node_file_index -> 'node_data -> unit
    val merge_synonyms : 'node_data t -> 'node_data match_f -> unit
  end = struct

    (** The basic merging table scaffolding. *)

    module Description = struct let table_kind = "synonyms" end

    module Key = struct
      type key = node_id
      let of_node = Node.get_id
      include NodeId
      let pretty fmt = Format.fprintf fmt "`%a'" pretty
    end

    include MergingTable(Description)(Key)

    (** The specific synonyms table stuff. *)

    let find_all table key =
      assert (check_table_invariant table);
      Table.find_all table.table key

    type 'node_data match_f =
      node_file_index -> 'node_data -> node_file_index -> 'node_data -> unit

    (* The [compare] functions passed here are all from the [match*] family
       (e.g. [match_compinfo], [match_enuminfo], [match_typeinfo]).
       They compare the two objects, but they also have side-effects. *)
    let merge_synonyms table (match_f : 'node_data match_f) =
      (* First transform the given comparison function using the [Merge_failure]
         exception to one returning a boolean. *)
      let are_nodes_in_the_same_class node_1 node_2 =
        try
          match_f
            (Node.get_file_index node_1) (Node.get_data node_1)
            (Node.get_file_index node_2) (Node.get_data node_2);
          true
        (* TODO: If this one exception handler was not here, we could move the
           [Merge_failure] code to a much better place: just before all the
           [match_*] functions. *)
        with Merge_failure _ -> false
      in
      (* Classes are a list of representatives for the node id.
         We'll select an appropriate one according to the comparison
         function. *)
      (* NOTE: This is basically comparing the given node with all the class
         representative nodes and if the node belongs to any class, then nothing
         happens; if it does not belong to any class, then it is added a the end
         of the list as a representative of a new class. *)
      let rec insert_into_classes
          node
          (classes_representatives : 'node_data my_node list)
        : 'node_data my_node list =
        match classes_representatives with
        (* No more classes left to try. *)
        | [] -> [node] (* Add this as a new class. *)
        (* This belongs to the current class: stop right here. *)
        | class_representative :: remaining_classes
          when are_nodes_in_the_same_class class_representative node ->
          class_representative :: remaining_classes
        (* Try the next class. *)
        | _class_representative :: remaining_classes ->
          insert_into_classes node remaining_classes
      in
      let merge_one
          (classes_representatives : 'node_data my_node list)
          node : 'node_data my_node list =
        (* INVARIANT: in [classes_representatives] at all times each pair of
           nodes compares to [false] with the [are_nodes_in_the_same_class]
           function. *)
        Node.mark_syns_merged node;
        (* Compare in turn with all the classes we have so far. *)
        insert_into_classes node classes_representatives
      in
      iter
        (fun node_id node ->
           assert (check_binding_invariant node_id node);
           (* For each node that has not been treated yet. *)
           if not (Node.are_syns_merged node) then
             (* Find all the nodes with the same id. *)
             let all_nodes_with_the_same_id : 'node_data my_node list =
               find_all table node_id
             in
             (* Start with an empty set of classes for this id. *)
             (* NOTE: Using this [fold_left] with [ignore] is seriously
                strange. *)
             ignore (List.fold_left merge_one [] all_nodes_with_the_same_id))
        table

  end

  (* Managing both merging tables in the same time. *)
  type 'node_data tables = 'node_data Eq_table.t * 'node_data Syn_table.t

  let create_tables name = (Eq_table.create name, Syn_table.create name)

  let clear_tables (eq_table, syn_table) =
    Eq_table.clear eq_table;
    Syn_table.clear syn_table

  let pp_eq_table fmt (eq_table, _syn_table) = Eq_table.pretty fmt eq_table
  let pp_syn_table fmt (_eq_table, syn_table) = Syn_table.pretty fmt syn_table
  let dump_eq_graph (eq_table, _syn_table) = Eq_table.dump_graph eq_table

  (* Make a node with a self loop (i.e. it is its own representative). *)
  let mk_self_node (eq_table, syn_table) ~node_id_in_file ~node_data ~node_loc =
    Kernel.debug ~dkey "mk_self_node (%a)" pp_id_in_file node_id_in_file;

    (* 1. Prepare the node. *)
    let node = Node.make node_id_in_file node_data node_loc in

    (* 2. Add the node to the eqivalence class table. *)
    Eq_table.add eq_table node;

    (* [Parameter.enable_merging_synonyms] is not active for anonymous types,
       probably because it is licit to have two distinct anonymous types in two
       different files (which should not be merged). However, for anonymous
       enums, they can, and are, in fact merged by CIL. Hence, we permit the
       merging of anonymous enums with the same base name. *)
    (* 3. Maybe also add the node to the synonyms table. *)
    let (_node_file_index, node_id) = node_id_in_file in
    if Parameter.enable_merging_synonyms
    && NodeId.should_merge_synonym node_id
    then Syn_table.add syn_table node;

    node

  (* Check and / or improve node's location. *)
  let node_check_and_improve_loc node node_loc =
    match Node.is_defined node, Node.Loc.is_defined node_loc with
    (* Maybe we have a better location now... *)
    | false, true -> Node.refine_loc node node_loc
    (* Check for duplicate definitions in the same file. *)
    | true, true when not (Extlib.opt_equal InFileIndex.equal
                             (Node.get_in_file_index_opt node)
                             (Node.Loc.get_in_file_index_opt node_loc)) ->
      (* NOTE: This should happen only for aggressive merging nodes if a given
         aggressive merging candidate function has both declaration and
         definition: thus [get_node'] called for the definition will find the
         declaration's node, but the in-file indices of the two will differ. *)
      Kernel.debug ~dkey ~current:true
        "Duplicate definition of node %a at in-file indices %a and %a"
        pp_node node
        Node.Loc.pretty (Node.get_loc node)
        Node.Loc.pretty node_loc
    (* Everything all right; nothing to improve. *)
    | _ -> ()

  (* NOTE: In around half of cases this is called with [ignore], just for the
     side-effects, which is a bit disturbing. *)
  let get_node' (eq_table, syn_table) node_id_in_file node_data node_loc =
    Kernel.debug ~dkey "get_node(%a, %a)"
      pp_id_in_file node_id_in_file Node.Loc.pretty' node_loc;
    try
      let node = Eq_table.find eq_table node_id_in_file in
      Kernel.debug ~dkey "> node found!";
      (* Sanity check: file index and id of the retrieved node. *)
      assert (NodeIdInFile.equal node_id_in_file (Node.get_id_in_file node));
      (* Check and / or improve node's location. *)
      node_check_and_improve_loc node node_loc;
      (* Get the representative (no path compression!). *)
      Node.find ~do_path_compression:false node
    with Not_found ->
      let node =
        mk_self_node (eq_table, syn_table) ~node_id_in_file ~node_data ~node_loc
      in
      Kernel.debug ~dkey "> made a new node!";
      node

  let get_node tables node_id_in_file node_data =
    get_node' tables node_id_in_file node_data Node.Loc.undefined

  let create_node tables node_id_in_file node_data node_loc =
    ignore (get_node' tables node_id_in_file node_data node_loc)

  let find_replacement (eq_table, _syn_table) node_id_in_file =
    Kernel.debug ~dkey "find_replacement (%a)" pp_id_in_file node_id_in_file;
    try
      (* First find the node using the provided file index and id. *)
      let node = Eq_table.find eq_table node_id_in_file in
      (* Sanity check: file index and id of the retrieved node. *)
      assert (NodeIdInFile.equal node_id_in_file (Node.get_id_in_file node));
      match Node.is_representative node with
      | true -> (* This node is the representative of its class. *)
        (* No replacement! *)
        Kernel.debug ~dkey "> %a is a representative!" pp_node node;
        None
      | false -> (* This node is not the representative. *)
        (* Find the representative, it will be used as replacement. *)
        let rep_node = Node.find node in
        Kernel.debug ~dkey "> representative node found = %a" pp_node rep_node;
        (* Sanity check:
           - this should be the representative,
           - and it should be different from the node found initially.*)
        assert (Node.is_representative rep_node);
        assert (rep_node != node);
        (* Return the representative's data and file index. *)
        Some (Node.get_data rep_node, Node.get_file_index rep_node)
    with Not_found -> (* This node is not in any equivalence class. *)
      (* No replacement! *)
      Kernel.debug ~dkey "> node %a not found in the table!"
        pp_id_in_file node_id_in_file;
      None

  let has_no_replacement tables node_id_in_file =
    not (Extlib.has_some (find_replacement tables node_id_in_file))

  let merge_synonyms (_eq_table, syn_table) = Syn_table.merge_synonyms syn_table

end

(* The original mergecil uses plain old Hashtbl for everything. *)
module PlainMergeableType :
  (MergeableTypeS with type t = string) =
struct
  let name = "PlainMergeable"
  type t = string
  let hash = Datatype.String.hash
  let equal = (=)
  let compare = compare
  let should_merge_synonym name = not (is_prefix "__anon" name)
  let pretty = Format.pp_print_string
end

module PlainMerging = Merging(PlainMergeableType)


(* Anonymous enums. *)

module AnonEnums : sig
  (* USED: in [EnumMergeableType.equal] and [treat_one_global_pass_1]. *)
  val newAlphaName : enuminfo -> location -> string
end = struct
  let alphaTable : alpha_conversion_table = Hashtbl.create 57

  let newAlphaName enuminfo loc =
    let new_ename, _ =
      Alpha.newAlphaName
        ?undolist:None
        ~alphaTable
        ~lookupname:enuminfo.ename
        ~data:loc
    in
    new_ename

  let _reset () = Hashtbl.clear alphaTable
end

(* USED: in [EnumMergeableType] and [match_enuminfo]. *)
(* NOTE: It may seem suprising, but this comparison function DOES NOT have any
   side effects! Miraculous indeed. *)
let match_enumitems old_enuminfo new_enuminfo : unit =
  (* Check if the number of enumitems is the same. *)
  if List.length old_enuminfo.eitems <>
     List.length new_enuminfo.eitems then
    raise (Merge_failure
             (Enum_elements_number
                (old_enuminfo, new_enuminfo)));
  (* Check if corresponding enumitems are defined in the same way. *)
  let match_enumitem old_enumitem new_enumitem =
    (* Check if enumitem names are the same. *)
    let old_name = old_enumitem.einame in
    let new_name = new_enumitem.einame in
    if old_name <> new_name then
      raise (Merge_failure
               (Enum_item_names
                  (old_enuminfo, old_name, new_name)));
    (* Check if enumitem values are the same. *)
    let old_value = old_enumitem.eival in
    let new_value = new_enumitem.eival in
    if not (same_int64 old_value new_value) then
      raise (Merge_failure
               (Enum_item_values
                  (old_enuminfo, old_name, old_value, new_value)))
  in
  List.iter2 match_enumitem old_enuminfo.eitems new_enuminfo.eitems

module EnumMergeableType :
  (MergeableTypeS with type t = enuminfo) =
struct
  let name = "EnumMergeable"

  type t = enuminfo

  let hash s = Datatype.String.hash s.ename

  let same_enum_items old_enuminfo new_enuminfo : bool =
    try match_enumitems old_enuminfo new_enuminfo; true
    with Merge_failure (Enum_elements_number _)
       | Merge_failure (Enum_item_names _)
       | Merge_failure (Enum_item_values _) -> false

  let is_anonymous_enum enuminfo : bool = is_prefix "__anonenum" enuminfo.ename

  (* NOTE: Right! So here in a function called [equal] we actually have
     side-effects which change one of the operands. *)
  let equal enuminfo_1 enuminfo_2 =
    ((is_anonymous_enum enuminfo_1 &&
      is_anonymous_enum enuminfo_2) &&
     (same_enum_items enuminfo_1 enuminfo_2 ||
      (enuminfo_1.ename = enuminfo_2.ename &&
       (* NOTE: So let me get it straight... This little hack here does this:

          If the enumerations fulfill the following 3 conditions:
          - both are anonymous,
          - they do not have the same items,
          - they have equal names,
          then we change one of their names (so that they differ now) and we
          return [false] here. This way we will also return [false] in the end,
          as we will check the "or their names are equal" condition again in
          the end and the names will effectively differ because of the change
          we have just made...

          This seems to make some sense here in fact, as we do it only when
          both enums are anonymous, they have different items, and the same
          name (the three conditions checked just before), so we give them
          different names on the fly so that they become distinguishable
          faster.

          But either way: WTF?!?!?!?!?!?!?!?!? *)
       begin
         let new_ename = AnonEnums.newAlphaName enuminfo_2 Location.unknown in
         enuminfo_2.ename <- new_ename;
         false
       end))) ||
    enuminfo_1.ename = enuminfo_2.ename

  let compare_int exp_1 exp_2 =
    match Cil.constFold true exp_1, Cil.constFold true exp_2 with
    | { enode = Const (CInt64 (i1, _, _)); _ },
      { enode = Const (CInt64 (i2, _, _)); _ } ->
      Integer.compare i1 i2
    | e1, e2 -> Exp.compare e1 e2
  (* not strictly accurate, but should do the trick anyway *)

  let compare_enum_item enum_item_1 enum_item_2 =
    match String.compare enum_item_1.einame enum_item_2.einame with
    | 0 -> compare_int enum_item_1.eival enum_item_2.eival
    | res -> res

  let compare enuminfo_1 enuminfo_2 =
    match is_anonymous_enum enuminfo_1, is_anonymous_enum enuminfo_2 with
    | true, false -> -1
    | false, true -> +1
    | false, false -> (* None is anonymous, compare their names. *)
      String.compare enuminfo_1.ename enuminfo_2.ename
    | true, true -> (* Both are anonymous, compare the enumitems. *)
      Extlib.list_compare compare_enum_item enuminfo_1.eitems enuminfo_2.eitems

  let should_merge_synonym _ = true

  let pretty fmt enuminfo =
    Printer.pp_global fmt (GEnumTag (enuminfo, Location.unknown))
end

module EnumMerging = Merging(EnumMergeableType)


module VolatileMergeableType :
  (MergeableTypeS with type t = identified_term list) =
struct
  let name = "VolatileMergeable"

  type t = identified_term list

  let hash = function
    | [] -> 0
    | h :: _ -> Logic_utils.hash_term h.it_content

  let equal = Logic_utils.is_same_list Logic_utils.is_same_identified_term

  let compare =
    Extlib.list_compare
      (fun identified_term_1 identified_term_2 ->
         Logic_utils.compare_term
           identified_term_1.it_content identified_term_2.it_content)

  let should_merge_synonym _ = true

  let pretty fmt x =
    Pretty_utils.pp_list ~sep:",@ " Printer.pp_identified_term fmt x
end

module VolatileMerging = Merging(VolatileMergeableType)


module ModelMergeableType :
  (MergeableTypeS with type t = string * typ) =
struct
  let name = "ModelMergeable"

  type t = string * typ

  let hash_type typ =
    let rec hash_type' acc depth = function
      | TVoid _ -> acc
      | TInt (ikind,_) -> 3 * acc + Hashtbl.hash ikind
      | TFloat (fkind,_) -> 5 * acc + Hashtbl.hash fkind
      | TPtr(typ',_) when depth < 5 -> hash_type' (7*acc) (depth+1) typ'
      | TPtr _ -> 7 * acc
      | TArray (typ',_,_,_) when depth < 5 -> hash_type' (9*acc) (depth+1) typ'
      | TArray _ -> 9 * acc
      | TFun (typ',_,_,_) when depth < 5 -> hash_type' (11*acc) (depth+1) typ'
      | TFun _ -> 11 * acc
      | TNamed (typ',_) -> 13 * acc + Hashtbl.hash typ'.tname
      | TComp(compinfo,_,_) ->
        let mul = if compinfo.cstruct then 17 else 19 in
        mul * acc + Hashtbl.hash compinfo.cname
      | TEnum (enuminfo,_) -> 23 * acc + Hashtbl.hash enuminfo.ename
      | TBuiltin_va_list _ -> 29 * acc
    in
    hash_type' 117 0 typ

  let hash (s, t) =
    Datatype.String.hash s + 3 * hash_type t

  let equal (s1, t1 : t) (s2, t2 : t) =
    s1 = s2 && TypByName.equal t1 t2

  let compare (s1, t1) (s2, t2) =
    match String.compare s1 s2 with
    | 0 -> TypByName.compare t1 t2
    | res -> res

  let should_merge_synonym _ = true

  let pretty fmt (s, t) =
    Format.fprintf fmt "model@ %a@ { %s }" Printer.pp_typ t s
end

module ModelMerging = Merging(ModelMergeableType)


(* Functions with different argument types are not synonyms thanks
   to overloading. *)
module LogicFunctionMergeableType :
  (MergeableTypeS with type t = logic_info) =
struct
  let name = "LogicFunctionMergeable"

  type t = logic_info

  let equal_profile profile_1 profile_2 =
    try
      List.for_all2
        (fun logic_var_1 logic_var_2 ->
           Logic_type_ByName.equal logic_var_1.lv_type logic_var_2.lv_type)
        profile_1 profile_2
    with Invalid_argument _ -> false

  let rec compare_profile profile_1 profile_2 =
    match profile_1, profile_2 with
    | [], [] -> 0
    | [], _::_ -> +1
    | _::_, [] -> -1
    | x1::r1, x2::r2 ->
      let r = Logic_type_ByName.compare x1.lv_type x2.lv_type in
      if r <> 0 then r
      else compare_profile r1 r2

  let hash logic_info = Hashtbl.hash logic_info.l_var_info.lv_name

  let equal logic_info_1 logic_info_2 =
    logic_info_1.l_var_info.lv_name = logic_info_2.l_var_info.lv_name
    && equal_profile logic_info_1.l_profile logic_info_2.l_profile

  let compare logic_info_1 logic_info_2 =
    let r =
      String.compare
        logic_info_1.l_var_info.lv_name
        logic_info_2.l_var_info.lv_name
    in
    if r <> 0 then r
    else compare_profile logic_info_1.l_profile logic_info_2.l_profile

  (* There are no anonymous logic functions, hence all synonyms must
     be considered. *)
  let should_merge_synonym _ = true

  let pretty = Logic_info.pretty
end

module LogicFunctionMerging = Merging(LogicFunctionMergeableType)


(* ---===[ EQUIVALENCE AND SYNONYMS TABLES ]===--- *)

(* TODO: Quick way to access the equivalence table. (A nicer type would be much
         better...) *)
let eq_table = fst

(* For each name space we define a set of equivalence classes. *)
(* Sometimes we want to merge synonyms. We keep some tables indexed by names.
   Each name is mapped to multiple entries. *)
(* NOTE: - Composites: structs and unions,
         - Types: type names. *)
(* [Eq_table] operations used:
   - all tables use [create] (here) and [clear] (in [clear_environment]);
   - [find] is used for:
     - [composites],
     - [logic_funs],
     - [logic_types],
     - [model];
   - [add] is used for [vars] and [model];
   - [iter] is used for [aggressive_merging]. *)
let vars = PlainMerging.create_tables "vars" (* NONE *)
let composites = PlainMerging.create_tables "composites" (* match_compinfo *)
let enums = EnumMerging.create_tables "enums" (* match_enuminfo *)
let types = PlainMerging.create_tables "types" (* match_typeinfo *)
let aggressive_merging = PlainMerging.create_tables "aggressive_merging" (* match_aggressive_merging *)

let logic_funs = LogicFunctionMerging.create_tables "logic_funs" (* match_logic_info *)
let logic_types = PlainMerging.create_tables "logic_types" (* match_logic_type_info *)
let logic_ctors = PlainMerging.create_tables "logic_ctors" (* match_logic_ctor *)
let logic_axiomatics = PlainMerging.create_tables "logic_axiomatics" (* match_logic_axiomatic *)
let logic_lemmas = PlainMerging.create_tables "logic_lemmas" (* match_logic_lemma *)
let logic_custom = PlainMerging.create_tables "logic_custom" (* NONE *)

let volatile = VolatileMerging.create_tables "volatile_merging" (* match_volatile_clause *)
let model = ModelMerging.create_tables "model_merging" (* match_model_info *)


(* ---===[ MERGING END ]===--- *)

module Pass1Env : sig

  val clear : unit -> unit

  module GlobalVarsEnv : sig
    (** A global environment for variables. Put in here only the non-static
        variables, indexed by their name. *)
    (* USED: [add] and [get] in: [treat_global_varinfo_pass_1]. *)
    val pretty : Format.formatter -> unit
    val add_node : (string, varinfo) Node.t -> unit
    val get_node : string -> (string, varinfo) Node.t option
  end

end = struct

  module GlobalVarsEnv = struct
    (* --[ global_vars_environment ]-- *)

    let table : (string, (string, varinfo) Node.t) Hashtbl.t =
      Hashtbl.create 111

    let name = "global_vars_environment"
    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let pretty fmt =
      let pp_binding fmt (_, node) = PlainMerging.pp_node fmt node in
      pp_hashtable ~name ~pp_binding fmt table

    let add_node node =
      let key = Node.get_id node in
      let value = node in
      Kernel.debug ~dkey "adding variable node %a" PlainMerging.pp_node value;
      if Hashtbl.mem table key then
        Kernel.feedback "%s: covering existing binding!" name;
      Hashtbl.add table key value

    let get_node = Extlib.find_or_none (Hashtbl.find table)

    let clear () = Hashtbl.clear table
  end

  let clear () = GlobalVarsEnv.clear ()

end

module BothPassesEnv : sig

  val clear : unit -> unit

  module VarOrTypeAlpha : sig
    (** Variables and types alpha conversion table. *)
    (* Basically: all non-static variables' names are registered during pass 1,
       non-static variables' names are not. Then, during pass 2, we can create
       new names for static variables avoiding collisions with existing static
       and non-static variables. *)
    (* NOTE: Unfortunately, because of the way the C lexer works, type names
       must be different from variable names! We have one alpha table both for
       variables and types. *)
    (* USED:
       - for variables:
         - [register_name] in [treat_nonstaticlike_varinfo_pass_1],
         - [new_name] in [process_staticlike_varinfo];
       - for types:
         - [new_name] in [treat_one_global_pass_2]: [GEnumTag] and [GType]. *)
    val register_name : ?loc:location -> string -> unit
    val new_name : ?loc:location -> string -> string
  end

  module FormalArgsNames : sig
    (** Keep track, for all global function definitions, of the names of the
        formal arguments. They might change during merging of function types if
        the prototype occurs after the function definition and uses different
        names. We'll restore the names at the end. *)
    (* USED: [add] in: [treat_one_global_pass_1];
             [get] in: [treat_one_global_pass_2] *)
    val maybe_add : (FileIndex.t * varinfo) -> unit
    val get : (FileIndex.t * string) -> string list option
    val pretty : Format.formatter -> unit
  end

  module AggressiveMerging : sig
    (** A global environment for storing data about aggressive merging
        candidates.  *)
    type candidate_data = FileIndex.t * global
    val pretty : Format.formatter -> unit
    val add_candidate_node :
      varinfo -> location -> (string, varinfo) Node.t -> unit
    val get_candidate_varinfos_set : unit -> Varinfo.Set.t
    val iter_on_candidate_node_lists :
      ((string, varinfo) Node.t list -> unit) -> unit
    val add_candidate_data : varinfo -> candidate_data -> unit
    val get_candidate_data : varinfo -> candidate_data
  end

end = struct

  (* --[ vars_and_types_alpha ]-- *)
  module VarOrTypeAlpha = struct
    let name = "vars_and_types_alpha"
    let alpha_table : alpha_conversion_table = Hashtbl.create 57
    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let clear () = Hashtbl.clear alpha_table

    let new_name ?(loc = Cil.CurrentLoc.get ()) name =
      let new_name, _ =
        Alpha.newAlphaName
          ?undolist:None
          ~alphaTable:alpha_table
          ~lookupname:name
          ~data:loc
      in
      Kernel.debug ~dkey "name was `%s' -> new name is `%s'" name new_name;
      new_name

    let register_name ?(loc = Cil.CurrentLoc.get ()) name =
      Kernel.debug ~dkey "registering name for `%s'" name;
      Alpha.registerAlphaName
        ?undolist:None
        ~alphaTable:alpha_table
        ~lookupname:name
        ~data:loc
  end

  (* --[ formal_args_names ]-- *)
  module FormalArgsNames = struct
    let table :
      (FileIndex.t * string, string list) Hashtbl.t =
      Hashtbl.create 111

    let name = "formal_args_names"
    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let pp_string_args_list = pp_list_as_args_list Format.pp_print_string

    let pretty fmt =
      let pp_binding fmt ((file_index, variable_name), formals) =
        Format.fprintf fmt "%s (file %a) -> %a"
          variable_name FileIndex.pretty file_index pp_string_args_list formals
      in
      pp_hashtable ~name ~pp_binding fmt table

    let formal_args_names_of_fundec varinfo =
      let _, formal_args, _, _ = Cil.splitFunctionTypeVI varinfo in
      let formal_args_list = Cil.argsToList formal_args in
      List.map (fun (arg_name, _, _) -> arg_name) formal_args_list

    let add (file_index, varinfo) =
      let key = (file_index, varinfo.vname) in
      let value = formal_args_names_of_fundec varinfo in
      Kernel.debug ~dkey "setting formals for %a as %a"
        PlainMerging.pp_id_in_file key
        pp_string_args_list value;
      if Hashtbl.mem table key then
        Kernel.feedback "%s: covering existing binding!" name;
      Hashtbl.add table key value

    let get = Extlib.find_or_none (Hashtbl.find table)

    let maybe_add (file_index, varinfo) =
      if get (file_index, varinfo.vname) = None
      then add (file_index, varinfo)

    let clear () = Hashtbl.clear table
  end

  module AggressiveMerging = struct
    (* --[ aggressive_merging ]-- *)

    type candidate_key = string * (string * string)

    let table : (candidate_key, (string, varinfo) Node.t list) Hashtbl.t =
      Hashtbl.create 111

    let name = "aggressive_merging"
    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let candidate_key_of_loc loc : string * string =
      let begin_position, end_position = loc in
      Lexing.(begin_position.pos_fname, end_position.pos_fname)

    let append key value =
      let values =
        match Extlib.find_or_none (Hashtbl.find table) key with
        | None -> []
        | Some nodes -> nodes
      in
      Hashtbl.replace table key (value :: values)

    let reversed = ref false

    let add_candidate_node varinfo loc node =
      assert (not !reversed);
      let key = (varinfo.vname, candidate_key_of_loc loc) in
      let value = node in
      Kernel.debug ~dkey "adding variable node %a" PlainMerging.pp_node value;
      append key value

    let reverse_all () =
      Hashtbl.iter
        (fun k v -> Hashtbl.replace table k (List.rev v))
        table;
      reversed := true

    let iter_on_candidate_node_lists f =
      if not !reversed then reverse_all ();
      Hashtbl.iter (fun _k v -> f v) table

    let get_candidate_varinfos_set () =
      let set = ref Varinfo.Set.empty in
      let add_node_to_set node =
        set := Varinfo.Set.add (Node.get_data node) !set
      in
      Hashtbl.iter (fun _key -> List.iter add_node_to_set) table;
      !set

    let pretty fmt =
      let rev_or_id = if !reversed then fun x -> x else List.rev in
      let pp_binding fmt (_, nodes) =
        let pp_nodes =
          Pretty_utils.pp_list ~pre:"{ " ~suf:" }" ~sep:", "
            PlainMerging.pp_node
        in
        pp_nodes fmt (rev_or_id nodes)
      in
      pp_hashtable ~name ~pp_binding fmt table

    type candidate_data = FileIndex.t * global

    let candidates = Varinfo.Hashtbl.create 111

    let add_candidate_data varinfo (file_index, global) =
      let old_data_option =
        Extlib.find_or_none (Varinfo.Hashtbl.find candidates) varinfo
      in
      match old_data_option, global with
      | None, _ ->
        (* No previous data, first global can be a declaration or a definition. *)
        Varinfo.Hashtbl.add candidates varinfo (file_index, global)
      | Some (_, GFunDecl _), GFun _ ->
        (* Declaration was already treated, now this can only be a definition,
           we should simply forget what was remembered for the declaration. *)
        Varinfo.Hashtbl.replace candidates varinfo (file_index, global)
      | Some (_, ((GFunDecl (_, _old_varinfo, _)) as _old_global)), GFunDecl _
      | Some (_, ((GFun ({ svar = _old_varinfo; _ }, _)) as _old_global)), _ ->
        (* These cases do not usually happen and in most situations the only
           possible options after the [Cabs2cil] preprocessing are:
           - a single declaration,
           - a single definition,
           - a single declaration, then a single definition.
           Yet sometimes in strange circumstances these might actually happen
           (for example when using [alias] attributes we can end up with
           multiple declarations of the same function present *after* [Cabs2cil]
           preprocessing is done), so we must hadle them. In such situations we
           just assume that the associated globals are the same, so there is
           nothing more to do. *)
        ()
      | _ -> assert false

    let get_candidate_data varinfo =
      assert (Varinfo.Hashtbl.mem candidates varinfo);
      Varinfo.Hashtbl.find candidates varinfo

    let clear () =
      Hashtbl.clear table;
      Varinfo.Hashtbl.clear candidates;
      reversed := false

  end

  let clear () =
    VarOrTypeAlpha.clear ();
    FormalArgsNames.clear ();
    AggressiveMerging.clear ()

end

(* Some enumerations have to be turned into an integer. We implement this by
   introducing a special enumeration type which we'll recognize later to be
   an integer. *)
(* USED: in [int_enuminfo_node] and [renameVisitorClass]. *)
let int_enuminfo : enuminfo =
  let name = "!!!intEnumInfo!!!"
  (* We pick an invalid C name. Can't clash with anything. *)
  in {
    eorig_name  = name;
    ename       = name;
    eitems      = [];
    eattr       = [];
    ereferenced = false;
    ekind       = IInt;
  }

(* USED: in [match_enuminfo]. *)
let int_enuminfo_node : (enuminfo, enuminfo) Node.t =
  let node_loc_defined_unknown =
    Node.Loc.defined Location.unknown InFileIndex.zero
  in
  (* And we add it to the equivalence graph. *)
  EnumMerging.get_node' enums
    (FileIndex.zero, int_enuminfo) int_enuminfo
    node_loc_defined_unknown


(* Combine the attributes of two types. *)
(* NOTE: Only in three cases attributes are not simply combined with this:
   - the "missingproto" attribute (treated specially here in the [TFun] case),
   - attributes of fields of composite types (in [match_compinfo]: it seems that
     if a field is redefined with different attributes then an appropriate
     [Merge_failure] exception is raised),
   - the "const" attribute (treated specially in [treat_global_varinfo_pass_1]
     function inside the [file_pass_1] function). *)
let combine_attributes old_attrs new_attrs =
  Cil.addAttributes old_attrs new_attrs

(* Combine the types. Raise an appropriate [Merge_failure] exception in case if
   they cannot be combined. *)
let rec combine_types old_file_index old_typ new_file_index new_typ =
  match old_typ, new_typ with
  (* Both are void. *)
  | TVoid old_attrs,
    TVoid new_attrs ->
    let attrs = combine_attributes old_attrs new_attrs in
    TVoid attrs

  (* Both are integers. *)
  | TInt (old_ikind, old_attrs),
    TInt (new_ikind, new_attrs) ->
    let ikind =
      if old_ikind == new_ikind
      then old_ikind
      else
      if Cil.bytesSizeOfInt old_ikind = Cil.bytesSizeOfInt new_ikind
      && Cil.isSigned       old_ikind = Cil.isSigned       new_ikind
      then
        (* The types contain the same sort of values but are not equal.
           For example on x86_16 machep unsigned short and unsigned int. *)
        if Cil.rank old_ikind < Cil.rank new_ikind
        then new_ikind
        else old_ikind
      else
        raise (Merge_failure (Int_types_different (old_typ, new_typ)))
    in
    let attrs = combine_attributes old_attrs new_attrs in
    TInt (ikind, attrs)

  (* Both are float. *)
  | TFloat (old_fkind, old_attrs),
    TFloat (new_fkind, new_attrs) ->
    let fkind =
      if old_fkind == new_fkind
      then old_fkind
      else raise (Merge_failure (Float_types_different (old_typ, new_typ)))
    in
    let attrs = combine_attributes old_attrs new_attrs in
    TFloat (fkind, attrs)

  (* Both are enumerations. *)
  | TEnum (old_enuminfo, old_attrs),
    TEnum (new_enuminfo, new_attrs) ->
    (* Matching enumerations always succeeds. But sometimes it maps both
       enumerations to integers *)
    match_enuminfo old_file_index old_enuminfo new_file_index new_enuminfo;
    let attrs = combine_attributes old_attrs new_attrs in
    TEnum (old_enuminfo, attrs)

  (* Strange one: old type is enumeration, new type is integer.
     But it seems to be handled by GCC. *)
  | TEnum (old_enuminfo, old_attrs),
    TInt (IInt, new_attrs) ->
    let attrs = combine_attributes old_attrs new_attrs in
    TEnum(old_enuminfo, attrs)

  (* Strange one: old type is integer, new type is enumeration.
     But it seems to be handled by GCC.
     Warning: here we are leaking types from new to old. *)
  | TInt (IInt, old_attrs),
    TEnum (new_enuminfo, new_attrs) ->
    let attrs = combine_attributes old_attrs new_attrs in
    TEnum(new_enuminfo, attrs)

  (* Both are composite types. *)
  | TComp (old_compinfo, _, old_attrs),
    TComp (new_compinfo, _, new_attrs) ->
    match_compinfo old_file_index old_compinfo new_file_index new_compinfo;
    (* If we get here we were successful. *)
    let attrs = combine_attributes old_attrs new_attrs in
    TComp (old_compinfo, Cil.empty_size_cache (), attrs)

  (* Both are arrays. *)
  | TArray (old_basetype, old_size_exp_opt, _, old_attrs),
    TArray (new_basetype, new_size_exp_opt, _, new_attrs) ->
    let basetype : typ =
      combine_types
        old_file_index old_basetype
        new_file_index new_basetype
    in
    let size_exp_opt : exp option =
      match old_size_exp_opt, new_size_exp_opt with
      | None,   Some _ -> new_size_exp_opt
      | Some _, None   -> old_size_exp_opt
      | None,   None   -> old_size_exp_opt
      | Some old_size_exp, Some new_size_exp ->
        if same_int64 old_size_exp new_size_exp
        then old_size_exp_opt
        else raise (Merge_failure (Array_sizes_different (old_typ, new_typ)))
    in
    let attrs = combine_attributes old_attrs new_attrs in
    TArray (basetype, size_exp_opt, Cil.empty_size_cache (), attrs)

  (* Both are pointers. *)
  | TPtr (old_basetype, old_attrs),
    TPtr (new_basetype, new_attrs) ->
    let basetype =
      combine_types
        old_file_index old_basetype
        new_file_index new_basetype
    in
    let attrs = combine_attributes old_attrs new_attrs in
    TPtr (basetype, attrs)

  (* Both are functions. *)
  | TFun (old_return_type, old_args, old_is_variadic, old_attrs),
    TFun (new_return_type, new_args, new_is_variadic, new_attrs) ->
    let return_type =
      combine_types
        old_file_index old_return_type
        new_file_index new_return_type
    in
    if old_is_variadic != new_is_variadic then
      raise (Merge_failure (Fun_variadic_different (old_typ, new_typ)));
    let old_has_attr_missingproto = Cil.hasAttribute "missingproto" old_attrs in
    let new_has_attr_missingproto = Cil.hasAttribute "missingproto" new_attrs in
    let missingproto = old_has_attr_missingproto || new_has_attr_missingproto in
    (* If one does not have arguments, believe the one with the
       arguments if and only if no one has a missing prototype
       attribute. (inserted during parsing) *)
    let args =
      if old_args = None && not missingproto then new_args else
      if new_args = None && not missingproto then old_args else
        let old_argslist = Cil.argsToList old_args in
        let new_argslist = Cil.argsToList new_args in
        if List.length old_argslist <> List.length new_argslist then
          raise (Merge_failure (Fun_args_number_different (old_typ, new_typ)))
        else
          (* Go over the arguments and update the old ones with the
             adjusted types. *)
          let combine_arg
              (old_arg_name, old_arg_type, old_arg_attrs)
              (new_arg_name, new_arg_type, new_arg_attrs) =
            let arg_name =
              if new_arg_name <> ""
              then new_arg_name
              else old_arg_name
            in
            let arg_type =
              combine_types
                old_file_index old_arg_type
                new_file_index new_arg_type
            in
            let arg_attrs = combine_attributes old_arg_attrs new_arg_attrs in
            (arg_name, arg_type, arg_attrs)
          in
          Some (List.map2 combine_arg old_argslist new_argslist)
    in
    let attrs =
      let old_attrs =
        if new_has_attr_missingproto then old_attrs
        else Cil.dropAttribute "missingproto" old_attrs
      in
      let new_attrs =
        if old_has_attr_missingproto then new_attrs
        else Cil.dropAttribute "missingproto" new_attrs
      in
      combine_attributes old_attrs new_attrs
    in
    TFun (return_type, args, old_is_variadic, attrs)

  (* Both are variadic argument list objects. *)
  | TBuiltin_va_list old_attrs,
    TBuiltin_va_list new_attrs ->
    let attrs = combine_attributes old_attrs new_attrs in
    TBuiltin_va_list (attrs)

  (* Both are defined types. *)
  | TNamed (old_typeinfo, old_attrs),
    TNamed (new_typeinfo, new_attrs) ->
    match_typeinfo
      old_file_index old_typeinfo
      new_file_index new_typeinfo;
    (* If we get here we were able to match. *)
    let attrs = combine_attributes old_attrs new_attrs in
    TNamed(old_typeinfo, attrs)

  (* The new type is a named type. *)
  | _, TNamed (new_typeinfo, new_attrs) ->
    (* Unroll the new type first. *)
    let typ =
      combine_types
        old_file_index old_typ
        new_file_index new_typeinfo.ttype
    in
    Cil.typeAddAttributes new_attrs typ

  (* The old type is a named type. *)
  | TNamed (old_typeinfo, old_attrs), _ ->
    (* Unroll the old type as well if necessary. *)
    let typ =
      combine_types
        old_file_index old_typeinfo.ttype
        new_file_index new_typ
    in
    Cil.typeAddAttributes old_attrs typ

  (* Two different type constructors. *)
  | _ ->
    raise (Merge_failure
             (Type_constr_different
                (old_typ, new_typ)))

(* Match two compinfos:
   - merge them if they are compatible,
   - throw an appropriate [Merge_failure] if they are not. *)
and match_compinfo
    old_file_index old_compinfo
    new_file_index new_compinfo =
  (* Check if both are structures or both are unions. *)
  if old_compinfo.cstruct <> new_compinfo.cstruct then
    raise (Merge_failure (Comp_struct_and_union (old_compinfo, new_compinfo)));
  (* Find representative nodes / create new nodes for these compinfos. *)
  let old_compinfo_node : (string, compinfo) Node.t =
    let node_id_in_file = (old_file_index, old_compinfo.cname) in
    let node_data = old_compinfo in
    PlainMerging.get_node composites node_id_in_file node_data
  in
  let new_compinfo_node : (string, compinfo) Node.t =
    let node_id_in_file = (new_file_index, new_compinfo.cname) in
    let node_data = new_compinfo in
    PlainMerging.get_node composites node_id_in_file node_data
  in
  if old_compinfo_node != new_compinfo_node then begin
    (* Replace with the representative data. *)
    let old_compinfo    = Node.get_data old_compinfo_node in
    let new_compinfo    = Node.get_data new_compinfo_node in
    let old_file_index  = Node.get_file_index old_compinfo_node in
    let new_file_index  = Node.get_file_index new_compinfo_node in
    let old_field_count = List.length old_compinfo.cfields in
    let new_field_count = List.length new_compinfo.cfields in
    (* It is easy to catch here the case when the new structure is undefined
       and the old one was defined. We just reuse the old. *)
    (* More complicated is the case when the old one is not defined but the
       new one is. We still reuse the old one and we'll take care of defining
       it later with the new fields.
       GN: 7/10/04, I could not find when is "later", so I added it below. *)
    if new_field_count <> 0
    && old_field_count <> 0
    && old_field_count <> new_field_count
    then begin
      let current_loc = Cil.CurrentLoc.get () in (* d_global blows this away. *)
      Cil.CurrentLoc.set current_loc;
      raise (Merge_failure
               (Comp_fields_number_different
                  (old_compinfo, new_compinfo)))
    end;
    (* We check that they are defined in the same way. While doing this there
       might be recursion and we have to watch for going into an infinite
       loop. So we add the assumption that they are equal. *)
    let representative_node, undo_union =
      Node.union PlainMerging.pp_node old_compinfo_node new_compinfo_node
    in
    (* We check the fields but watch for [Merge_failure]. We only do the check
       when the lengths (i.e. numbers of fields) are the same. Due to the code
       above this the other possibility is that one of the lengths is 0, in
       which case we reuse the old compinfo. *)
    (* But what if the old one is the empty one ? *)
    if old_field_count = new_field_count then begin
      let match_fieldinfo old_fieldinfo new_fieldinfo =
        if old_fieldinfo.fbitfield <> new_fieldinfo.fbitfield then
          raise (Merge_failure
                   (Comp_bitfield_info_different
                      (old_compinfo, old_fieldinfo, new_fieldinfo)));
        if not (Attributes.equal
                  old_fieldinfo.fattr new_fieldinfo.fattr) then
          raise (Merge_failure
                   (Comp_field_attrs_different
                      (old_compinfo, old_fieldinfo, new_fieldinfo)));
        (* Make sure the types are compatible. *)
        let ftype =
          combine_types
            old_file_index old_fieldinfo.ftype
            new_file_index new_fieldinfo.ftype
        in
        (* Change the type in the representative *)
        old_fieldinfo.ftype <- ftype
      in
      try List.iter2 match_fieldinfo old_compinfo.cfields new_compinfo.cfields
      with Merge_failure reason ->
        (* Our assumption was wrong. Forget the isomorphism. *)
        undo_union ();
        raise (Merge_failure
                 (Comp_not_isomorphic
                    (old_compinfo, new_compinfo, reason)))
    end else begin
      (* We will reuse the old one. One of them is empty. If the old one is
         empty, copy over the fields from the new one. Won't this result in
         all sorts of undefined types??? *)
      if old_field_count = 0
      then old_compinfo.cfields <- new_compinfo.cfields;
    end;
    (* We get here when we succeeded checking that they are equal, or one of
       them was empty. *)
    let attrs = combine_attributes old_compinfo.cattr new_compinfo.cattr in
    let representative_compinfo = Node.get_data representative_node in
    representative_compinfo.cattr <- attrs
  end

(* Match two enuminfos:
   - merge them if they are compatible,
   - throw an appropriate [Merge_failure] if they are not. *)
and match_enuminfo
    old_file_index old_enuminfo
    new_file_index new_enuminfo =
  (* Find representative nodes / create new nodes for these enuminfos. *)
  let old_enuminfo_node : (enuminfo, enuminfo) Node.t =
    let node_id_in_file = (old_file_index, old_enuminfo) in
    let node_data = old_enuminfo in
    EnumMerging.get_node enums node_id_in_file node_data
  in
  let new_enuminfo_node : (enuminfo, enuminfo) Node.t =
    let node_id_in_file = (new_file_index, new_enuminfo) in
    let node_data = new_enuminfo in
    EnumMerging.get_node enums node_id_in_file node_data
  in
  if old_enuminfo_node != new_enuminfo_node then begin
    (* Replace with the representative data. *)
    let old_enuminfo = Node.get_data old_enuminfo_node in
    let new_enuminfo = Node.get_data new_enuminfo_node in
    (* Try to match them. If you cannot just make them both integers. *)
    try
      match_enumitems old_enuminfo new_enuminfo;
      (* Set the representative. *)
      let representative_node, _ =
        Node.union EnumMerging.pp_node old_enuminfo_node new_enuminfo_node
      in
      (* We only get here if the enumerations match. *)
      let attrs = combine_attributes old_enuminfo.eattr new_enuminfo.eattr in
      let representative_enuminfo = Node.get_data representative_node in
      representative_enuminfo.eattr <- attrs
    with Merge_failure reason ->
      if old_enuminfo_node != int_enuminfo_node
      && new_enuminfo_node != int_enuminfo_node then
        Kernel.warning
          "@[merging definitions of enum %s using int type@ (%a);\
           @ items %a and@ %a@]"
          old_enuminfo.ename
          pp_merge_failure_reason reason
          pp_enumitems old_enuminfo.eitems
          pp_enumitems new_enuminfo.eitems;
      (* Get here if you cannot merge two enumeration nodes. *)
      if old_enuminfo_node != int_enuminfo_node then
        ignore (Node.union EnumMerging.pp_node old_enuminfo_node int_enuminfo_node);
      if new_enuminfo_node != int_enuminfo_node then
        ignore (Node.union EnumMerging.pp_node new_enuminfo_node int_enuminfo_node);
  end

(* Match two typeinfos:
   - merge them if they are compatible,
   - throw an appropriate [Merge_failure] if they are not. *)
and match_typeinfo
    old_file_index old_typeinfo
    new_file_index new_typeinfo =
  (* SANITY CHECK: None of them should be anonymous. *)
  if old_typeinfo.tname = ""
  || new_typeinfo.tname = ""
  then Kernel.fatal "match_typeinfo for anonymous type";
  (* Find representative nodes / create new nodes for these typeinfos. *)
  let old_typeinfo_node : (string, typeinfo) Node.t =
    let node_id_in_file = (old_file_index, old_typeinfo.tname) in
    let node_data = old_typeinfo in
    PlainMerging.get_node types node_id_in_file node_data
  in
  let new_typeinfo_node : (string, typeinfo) Node.t =
    let node_id_in_file = (new_file_index, new_typeinfo.tname) in
    let node_data = new_typeinfo in
    PlainMerging.get_node types node_id_in_file node_data
  in
  if old_typeinfo_node != new_typeinfo_node then begin
    (* Replace with the representative data. *)
    let old_typeinfo = Node.get_data old_typeinfo_node in
    let new_typeinfo = Node.get_data new_typeinfo_node in
    let old_file_index = Node.get_file_index old_typeinfo_node in
    let new_file_index = Node.get_file_index new_typeinfo_node in
    (* Match the types if they are isomorphic. *)
    begin
      try ignore (combine_types
                    old_file_index old_typeinfo.ttype
                    new_file_index new_typeinfo.ttype)
      with Merge_failure reason ->
        raise (Merge_failure
                 (Types_not_isomorphic
                    (old_typeinfo, new_typeinfo, reason)));
    end;
    (* Union the nodes. *)
    ignore (Node.union PlainMerging.pp_node old_typeinfo_node new_typeinfo_node)
  end

(* NOTE: Only used in [merge]. *)
let match_logic_info =

  let has_static_ref_logic_function : logic_info -> bool =
    let static_var_visitor = object
      inherit nopCilVisitor
      method! vvrbl varinfo =
        if varinfo.vstorage = Static
        then raise Exit;
        DoChildren
    end
    in
    fun logic_info ->
      try ignore (visitCilLogicInfo static_var_visitor logic_info); false
      with Exit -> true
  in

  fun
    old_file_index old_logic_info
    new_file_index new_logic_info ->
    let old_logic_info_node =
      let node_id_in_file = (old_file_index, old_logic_info) in
      let node_data = old_logic_info in
      LogicFunctionMerging.get_node logic_funs node_id_in_file node_data
    in
    let new_logic_info_node =
      let node_id_in_file = (new_file_index, new_logic_info) in
      let node_data = new_logic_info in
      LogicFunctionMerging.get_node logic_funs node_id_in_file node_data
    in
    if old_logic_info_node != new_logic_info_node then begin
      let old_logic_info = Node.get_data old_logic_info_node in
      let new_logic_info = Node.get_data new_logic_info_node in
      match Logic_utils.is_same_logic_info old_logic_info new_logic_info with
      | true ->
        if has_static_ref_logic_function old_logic_info
        then
          Kernel.abort
            "multiple inclusion of logic function %s \
             referring to a static variable"
            old_logic_info.l_var_info.lv_name
        else
          Node.set_rep_of_later_to_rep_of_earlier
            old_logic_info_node new_logic_info_node
      | false ->
        Kernel.abort
          "invalid multiple logic function declarations %s"
          new_logic_info.l_var_info.lv_name
    end

(* NOTE: Only used in [merge]. *)
let match_logic_type_info
    old_file_index old_logic_type_info
    new_file_index new_logic_type_info =
  let old_logic_type_info_node =
    let node_id_in_file = (old_file_index, old_logic_type_info.lt_name) in
    let node_data = old_logic_type_info in
    PlainMerging.get_node logic_types node_id_in_file node_data
  in
  let new_logic_type_info_node =
    let node_id_in_file = (new_file_index, new_logic_type_info.lt_name) in
    let node_data = new_logic_type_info in
    PlainMerging.get_node logic_types node_id_in_file node_data
  in
  if old_logic_type_info_node != new_logic_type_info_node then begin
    let old_logic_type_info = Node.get_data old_logic_type_info_node in
    let new_logic_type_info = Node.get_data new_logic_type_info_node in
    match
      Logic_utils.is_same_logic_type_info
        old_logic_type_info new_logic_type_info
    with
    | true ->
      Node.set_rep_of_later_to_rep_of_earlier
        old_logic_type_info_node new_logic_type_info_node
    | false ->
      Kernel.error ~current:true
        "invalid multiple logic type declarations %s"
        new_logic_type_info.lt_name
  end

(* NOTE: Only used in [merge]. *)
let match_logic_ctor
    old_file_index old_logic_ctor_info
    new_file_index new_logic_ctor_info =
  let old_logic_ctor_info_node =
    let node_id_in_file = (old_file_index, old_logic_ctor_info.ctor_name) in
    let node_data = old_logic_ctor_info in
    PlainMerging.get_node logic_ctors node_id_in_file node_data
  in
  let new_logic_ctor_info_node =
    let node_id_in_file = (new_file_index, new_logic_ctor_info.ctor_name) in
    let node_data = new_logic_ctor_info in
    PlainMerging.get_node logic_ctors node_id_in_file node_data
  in
  if old_logic_ctor_info_node != new_logic_ctor_info_node then
    Kernel.error ~current:true
      "invalid multiple logic constructors declarations %s"
      new_logic_ctor_info.ctor_name

(* NOTE: Only used in [merge]. *)
let match_logic_axiomatic
    old_file_index (old_id, _ as old_logic_axiomatic)
    new_file_index (new_id, _ as new_logic_axiomatic) =
  let old_logic_axiomatic_node =
    let node_id_in_file = (old_file_index, old_id) in
    let node_data = old_logic_axiomatic in
    PlainMerging.get_node logic_axiomatics node_id_in_file node_data
  in
  let new_logic_axiomatic_node =
    let node_id_in_file = (new_file_index, new_id) in
    let node_data = new_logic_axiomatic in
    PlainMerging.get_node logic_axiomatics node_id_in_file node_data
  in
  if old_logic_axiomatic_node != new_logic_axiomatic_node then begin
    let _old_id, old_annotations = Node.get_data old_logic_axiomatic_node in
    let _new_id, new_annotations = Node.get_data new_logic_axiomatic_node in
    match Logic_utils.is_same_axiomatic old_annotations new_annotations with
    | true ->
      Node.set_rep_of_later_to_rep_of_earlier
        old_logic_axiomatic_node new_logic_axiomatic_node
    | false ->
      Kernel.error ~current:true
        "invalid multiple axiomatic declarations %s" new_id
  end

(* NOTE: Only used in [merge]. *)
let match_logic_lemma
    old_file_index (old_id, _ as old_logic_lemma)
    new_file_index (new_id, _ as new_logic_lemma) =
  let old_logic_lemma_node =
    let node_id_in_file = (old_file_index, old_id) in
    let node_data = old_logic_lemma in
    PlainMerging.get_node logic_lemmas node_id_in_file node_data
  in
  let new_logic_lemma_node =
    let node_id_in_file = (new_file_index, new_id) in
    let node_data = new_logic_lemma in
    PlainMerging.get_node logic_lemmas node_id_in_file node_data
  in
  if old_logic_lemma_node != new_logic_lemma_node then begin
    let old_id, (old_ax, old_labels, old_types, old_st, old_loc) =
      let old_id, old_logic_lemma = Node.get_data old_logic_lemma_node in
      old_id, old_logic_lemma
    in
    let new_id, (new_ax, new_labels, new_types, new_st, new_loc) =
      let new_id, new_logic_lemma = Node.get_data new_logic_lemma_node in
      new_id, new_logic_lemma
    in
    match
      Logic_utils.is_same_global_annotation
        (Dlemma (old_id, old_ax, old_labels, old_types, old_st, old_loc))
        (Dlemma (new_id, new_ax, new_labels, new_types, new_st, new_loc))
    with
    | true ->
      Node.set_rep_of_later_to_rep_of_earlier
        old_logic_lemma_node new_logic_lemma_node
    | false ->
      Kernel.error ~current:true
        "invalid multiple lemmas or axioms  declarations for %s" new_id
  end

(* NOTE: Only used in [merge]. *)
let match_volatile_clause
    old_file_index (old_id, _ as old_volatile_clause)
    new_file_index (new_id, _ as new_volatile_clause) =
  let old_volatile_clause_node =
    let node_id_in_file = (old_file_index, old_id) in
    let node_data = old_volatile_clause in
    VolatileMerging.get_node volatile node_id_in_file node_data
  in
  let new_volatile_clause_node =
    let node_id_in_file = (new_file_index, new_id) in
    let node_data = new_volatile_clause in
    VolatileMerging.get_node volatile node_id_in_file node_data
  in
  if old_volatile_clause_node != new_volatile_clause_node then begin
    let old_id, (old_r, old_w, old_loc) =
      Node.get_data old_volatile_clause_node
    in
    let new_id, (new_r, new_w, new_loc) =
      Node.get_data new_volatile_clause_node
    in
    match
      Logic_utils.is_same_global_annotation
        (Dvolatile (old_id, old_r, old_w, old_loc))
        (Dvolatile (new_id, new_r, new_w, new_loc))
    with
    | true ->
      Node.set_rep_of_later_to_rep_of_earlier
        old_volatile_clause_node new_volatile_clause_node
    | false ->
      Kernel.error ~current:true
        "invalid multiple volatile clauses for locations %a"
        (Pretty_utils.pp_list ~sep:",@ " Printer.pp_identified_term) new_id
  end

(* NOTE: Only used in [merge]. *)
let match_model_info
    old_file_index
    ({ mi_name = old_name; mi_base_type = old_type; _ } as old_model_info)
    new_file_index
    ({ mi_name = new_name; mi_base_type = new_type; _ } as new_model_info) =
  let old_model_info_node =
    let node_id_in_file = (old_file_index, (old_name, old_type)) in
    let node_data = old_model_info in
    ModelMerging.get_node model node_id_in_file node_data
  in
  let new_model_info_node =
    let node_id_in_file = (new_file_index, (new_name, new_type)) in
    let node_data = new_model_info in
    ModelMerging.get_node model node_id_in_file node_data
  in
  if old_model_info_node != new_model_info_node then begin
    let old_model_info = Node.get_data old_model_info_node in
    let new_model_info = Node.get_data new_model_info_node in
    match
      Logic_utils.is_same_type
        old_model_info.mi_field_type new_model_info.mi_field_type
    with
    | true ->
      Node.set_rep_of_later_to_rep_of_earlier
        old_model_info_node new_model_info_node
    | false ->
      Kernel.error ~current:true
        "Model field %s of type %a is declared with different logic type: \
         %a and %a"
        new_model_info.mi_name
        Printer.pp_typ        new_model_info.mi_base_type
        Printer.pp_logic_type new_model_info.mi_field_type
        Printer.pp_logic_type old_model_info.mi_field_type
  end

(* NOTE: Only used in [merge]. *)
let match_aggressive_merging
    old_file_index old_varinfo
    new_file_index new_varinfo =
  let old_varinfo_node : (string, varinfo) Node.t =
    let node_id_in_file = (old_file_index, old_varinfo.vname) in
    let node_data = old_varinfo in
    PlainMerging.get_node aggressive_merging node_id_in_file node_data
  in
  let new_varinfo_node : (string, varinfo) Node.t =
    let node_id_in_file = (new_file_index, new_varinfo.vname) in
    let node_data = new_varinfo in
    PlainMerging.get_node aggressive_merging node_id_in_file node_data
  in
  if old_varinfo_node != new_varinfo_node then begin
    (* Replace with the representative data *)
    let old_varinfo = Node.get_data old_varinfo_node in
    let new_varinfo = Node.get_data new_varinfo_node in
    (* There is an old definition. We must combine the types. Do this first
       because it might fail. *)
    let typ =
      combine_types
        (Node.get_file_index old_varinfo_node) old_varinfo.vtype
        (Node.get_file_index new_varinfo_node) new_varinfo.vtype
    in
    Cil.update_var_type old_varinfo typ;
    (* We get here if we have success *)
    (* Combine the attributes as well *)
    let attrs =
      Cil.addAttributes old_varinfo.vattr new_varinfo.vattr
    in
    old_varinfo.vattr <- attrs
    (* Do not union them yet because we do not know that they are the same.
       We have checked only the types so far. *)
  end

(* USED: in [global_annot_pass_1] and [global_annot_pass_2]. *)
let loc_of_global_annot : global_annotation -> location = function
  | Dvolatile(_, _, _, loc)
  | Daxiomatic(_, _, loc)
  | Dfun_or_pred (_, loc)
  | Dtype_annot (_, loc)
  | Dmodel_annot (_, loc)
  | Dcustom_annot (_, _, loc)
  | Dinvariant (_, loc)
  | Dtype (_, loc)
  | Dlemma (_, _, _, _, _, loc) -> loc

(* TODO: Each call to [get_node] here is almost the same. Could refactor it. *)
(* USED: in [treat_one_global_pass_1] [GAnnot]. *)
let rec global_annot_pass_1 file_index in_file_index global_annot =
  let node_loc : Node.Loc.t =
    let loc : location = loc_of_global_annot global_annot in
    Cil.CurrentLoc.set loc;
    Node.Loc.defined loc in_file_index
  in
  match global_annot with
  | Dfun_or_pred (logic_info, _loc) ->
    let node_id_in_file = (file_index, logic_info) in
    let node_data = logic_info in
    let logic_info_node =
      LogicFunctionMerging.get_node logic_funs node_id_in_file node_data
    in
    (* NB: in case of mix decl/def it is the decl location that is taken. *)
    if not (Node.is_defined logic_info_node) then
      LogicFunctionMerging.create_node logic_funs
        node_id_in_file node_data node_loc
  | Dvolatile (identified_terms, rvi, wvi, loc) ->
    let node_id_in_file = (file_index, identified_terms) in
    let node_data = (identified_terms, (rvi, wvi, loc)) in
    VolatileMerging.create_node volatile node_id_in_file node_data node_loc
  | Daxiomatic (id, decls, _loc) ->
    let node_id_in_file = (file_index, id) in
    let node_data = (id, decls) in
    PlainMerging.create_node logic_axiomatics node_id_in_file node_data node_loc;
    List.iter (global_annot_pass_1 file_index in_file_index) decls
  | Dtype (logic_type_info, _loc) ->
    let node_id_in_file = (file_index, logic_type_info.lt_name) in
    let node_data = logic_type_info in
    PlainMerging.create_node logic_types node_id_in_file node_data node_loc
  | Dlemma (id, is_ax, labs, typs, st, loc) ->
    let node_id_in_file = (file_index, id) in
    let node_data = (id, (is_ax, labs, typs, st, loc)) in
    PlainMerging.create_node logic_lemmas node_id_in_file node_data node_loc
  | Dinvariant (logic_info, _loc)  ->
    let node_id_in_file = (file_index, logic_info) in
    let node_data = logic_info in
    LogicFunctionMerging.create_node logic_funs
      node_id_in_file node_data node_loc
  | Dtype_annot (logic_info, _loc) ->
    let node_id_in_file = (file_index, logic_info) in
    let node_data = logic_info in
    LogicFunctionMerging.create_node logic_funs
      node_id_in_file node_data node_loc
  | Dmodel_annot (mfi, _loc) ->
    let node_id_in_file = (file_index, (mfi.mi_name, mfi.mi_base_type)) in
    let node_data = mfi in
    ModelMerging.create_node model node_id_in_file node_data node_loc
  | Dcustom_annot (c, n, loc) ->
    let node_id_in_file = (file_index, n) in
    let node_data = (n, (c, loc)) in
    Format.eprintf "Mergecil : custom@.";
    PlainMerging.create_node logic_custom node_id_in_file node_data node_loc


(* Inline-related functions. *)

type inlining_mode =
  | AlwaysInline
  | NeverInline

let inlining_mode () =
  match Kernel.InliningMode.get () with
  | "always" -> AlwaysInline
  | "never" | "warn" -> NeverInline
  | _ -> assert false (* Should have been rejected before as invalid input. *)

(** Is the varinfo an "inline definition"? (It is an "inline definition" iff it
    is inline and does not have a storage specifier.) *)
let is_inline_definition varinfo =
  (* TODO: What with Register storage? *)
  varinfo.vinline && varinfo.vstorage = NoStorage

(** Does the varinfo have the "always_inline" attribute? *)
let is_always_inline =
  let always_inline_attr_string = "always_inline" in
  fun varinfo -> (* Check both varinfo and type attributes. *)
    Cil.hasAttribute always_inline_attr_string varinfo.vattr ||
    Cil.typeHasAttributeDeep always_inline_attr_string varinfo.vtype

(** Should this inline function be treated as a static function? *)
let treat_inline_as_static varinfo =
  assert varinfo.vinline;
  is_inline_definition varinfo
  && (is_always_inline varinfo || inlining_mode () = AlwaysInline)

(** Should the body of this function be discarded? *)
let should_discard_inline_body varinfo =
  is_inline_definition varinfo
  && not (is_always_inline varinfo)
  && inlining_mode () = NeverInline

(** Emit a warning about discarding the inline definition's body if
    necessary. *)
let warn_discarding_inline_body varinfo loc =
  if Kernel.InliningMode.get () = "warn" then
    Kernel.warning ~current:true
      "Discarding inline definition's body for function `%a' at %a."
      Printer.pp_varinfo varinfo
      Printer.pp_location loc

(** A function is treated like static if either:
    - the function is actually static
    - or if it is an inline definition which should be inlined. *)
let treat_like_static varinfo =
  varinfo.vstorage = Static ||
  (is_inline_definition varinfo && treat_inline_as_static varinfo)

(** Is given function suitable for aggressive merging? *)
let is_aggressive_merging_candidate varinfo =
  isFunctionType varinfo.vtype &&
  treat_like_static varinfo &&
  begin
    (* Either: *)
    (* - we are merging static functions AND this is a static function; *)
    (Parameter.aggressive_merging_static_functions () &&
     varinfo.vstorage = Static) ||
    (* - OR we are merging inline functions AND this is an inline function. *)
    (Parameter.aggressive_merging_inline_functions () &&
     varinfo.vinline)
  end

(* USED: in [treat_one_global_pass_1] and [treat_one_global_pass_2]. *)
let loc_of_global global =
  match global with
  | GVar     (_, _, loc)
  | GVarDecl (_, loc)
  | GFunDecl (_, _, loc)
  | GFun (_, loc)
  | GType (_, loc)
  | GCompTag (_, loc)
  | GEnumTag (_, loc)
  | GAnnot (_, loc)
  | GCompTagDecl (_, loc)
  | GEnumTagDecl (_, loc)
  | GPragma (_, loc)
  | GAsm (_, loc) -> Some loc
  | GText _ -> None

(* USED: in [treat_one_global_pass_1]. *)
let make_not_referenced global =
  match global with
  | GVar (   varinfo, _, _)
  | GVarDecl (   varinfo,    _)
  | GFunDecl (_, varinfo,    _) -> varinfo.vreferenced <- false
  | GFun (fundec,   _) -> fundec.svar.vreferenced <- false
  | GType (typeinfo, _) ->
    typeinfo.treferenced <- false;
    begin
      (* Go inside and clean the referenced flag for the declared tags. *)
      (* NOTE: If [tname] is not empty then we do not clean the flag! *)
      match typeinfo.tname = "", typeinfo.ttype with
      | true, TComp (compinfo, _, _) -> compinfo.creferenced <- false
      | true, TEnum (enuminfo, _) -> enuminfo.ereferenced <- false
      | _ -> () (* BUG? *)
    end
  | GCompTag (compinfo, _)
  | GCompTagDecl (compinfo, _) -> compinfo.creferenced <- false
  | GEnumTag (enuminfo, _)
  | GEnumTagDecl (enuminfo, _) -> enuminfo.ereferenced <- false
  | GAnnot _ | GText _ | GPragma _ | GAsm _ -> ()

let treat_varinfo_type_pass_1
    old_varinfo_file_index old_varinfo_location old_varinfo
    new_varinfo_file_index new_varinfo_location new_varinfo
    rep_varinfo =
  (* There is an old definition. We must combine the types. Do this first
     because it might fail. *)
  let typ =
    try
      combine_types
        old_varinfo_file_index old_varinfo.vtype
        new_varinfo_file_index new_varinfo.vtype;
    with Merge_failure reason ->
      Kernel.abort
        "@[<hov>Incompatible declaration for %a:@ %a@\n\
         First declaration was at %a@\n\
         Current declaration is at %a"
        Printer.pp_varinfo new_varinfo
        pp_merge_failure_reason reason
        Printer.pp_location old_varinfo_location
        Printer.pp_location new_varinfo_location
  in

  (* We do not want to turn non-"const" globals into "const" one. That can
     happen if one file declares the variable a non-const while others
     declare it as "const". *)
  let typ' =
    let is_varinfo_const varinfo =
      Cil.typeHasAttribute "const" varinfo.vtype
    in
    (* If their const-status is different then we remove the "const"
       attribute altogether from the representative's type. *)
    if is_varinfo_const new_varinfo = is_varinfo_const old_varinfo
    then typ
    else Cil.typeRemoveAttributes ["const"] typ
  in

  (* Set the type of the representative. *)
  Cil.update_var_type rep_varinfo typ'

let treat_varinfo_storage_pass_1
    old_varinfo old_varinfo_location new_varinfo rep_varinfo =
  assert (isFunctionType old_varinfo.vtype =
          isFunctionType new_varinfo.vtype);
  match old_varinfo.vstorage, new_varinfo.vstorage with
  (* Old and new variable storage specifiers are the same. *)
  | old_varinfo_storage, new_varinfo_storage
    when old_varinfo_storage = new_varinfo_storage ->
    rep_varinfo.vstorage <- old_varinfo_storage
  (* Static variables and functions should not have ended up here. *)
  | Static, _ | _, Static -> assert false
  (* If one of the specifiers is [Extern], we always take the other one.
     NOTE: In some cases we will need to reestablish the [Extern] specifier
     later on. *)
  | varinfo_storage, Extern | Extern, varinfo_storage ->
    rep_varinfo.vstorage <- varinfo_storage
  (* None of the above cases applies. *)
  | old_varinfo_storage, new_varinfo_storage ->
    Kernel.warning ~current:true
      "Inconsistent storage specification for %a. \
       Now is %a and previous was %a at %a"
      Printer.pp_varinfo new_varinfo
      Printer.pp_storage new_varinfo_storage
      Printer.pp_storage old_varinfo_storage
      Printer.pp_location old_varinfo_location;
    rep_varinfo.vstorage <- new_varinfo_storage

let treat_varinfo_attrs_pass_1
    old_varinfo new_varinfo rep_varinfo =
  if false then (* TODO: WTF? Why would we merge attributes?... *)
    let attrs = combine_attributes old_varinfo.vattr new_varinfo.vattr in
    rep_varinfo.vattr <- attrs

(* We scan each file and we look at all global varinfos. We see if globals
   with the same name have been encountered before and we merge their
   types. *)
(* USED: in [treat_one_global_pass_1] [GVar], [GVarDecl], [GFunDecl], [GFun]. *)
let treat_nonstaticlike_varinfo_pass_1 new_varinfo_file_index new_varinfo
    new_varinfo_location new_varinfo_in_file_index =
  (* Only called in the context where [new_varinfo] is not static. *)
  assert(not (treat_like_static new_varinfo));
  (* Register alpha name. *)
  BothPassesEnv.VarOrTypeAlpha.register_name new_varinfo.vname;
  (* Make a node for the varinfo and put it in [vars]. *)
  let new_varinfo_node : (string, varinfo) Node.t =
    let node_id_in_file = (new_varinfo_file_index, new_varinfo.vname) in
    let node_data = new_varinfo in
    let node_loc =
      Node.Loc.defined new_varinfo_location new_varinfo_in_file_index
    in
    (* NOTE: This is how we can create multiple nodes (and thus multiple
       bindings in [vars]) for a given [file_index], [name] pair. *)
    PlainMerging.mk_self_node vars ~node_id_in_file ~node_data ~node_loc
  in
  (* NOTE: This match is the only use of the [global_vars_environment]! *)
  match Pass1Env.GlobalVarsEnv.get_node new_varinfo.vname with
  | None ->
    (* Not present in the previous files. Remember it for later. *)
    Pass1Env.GlobalVarsEnv.add_node new_varinfo_node

  | Some old_varinfo_node ->
    (* NOTE: This becomes the representative of the old node! *)
    let old_varinfo_node : (string, varinfo) Node.t =
      Node.find old_varinfo_node
    in
    let old_varinfo = Node.get_data old_varinfo_node in
    let old_varinfo_file_index = Node.get_file_index old_varinfo_node in
    let old_varinfo_location =
      if not (Node.is_defined old_varinfo_node) then
        Kernel.fatal "old variable is undefined";
      Extlib.the (Node.get_location_opt old_varinfo_node)
    in

    (* Get the varinfo corresponding to the representative. *)
    let rep_varinfo =
      (* Union the old with the new and get the representative. *)
      let rep_node, _ =
        Node.union PlainMerging.pp_node old_varinfo_node new_varinfo_node
      in
      (* Extract the varinfo. *)
      Node.get_data rep_node
    in

    (* Combine the types. *)
    treat_varinfo_type_pass_1
      old_varinfo_file_index old_varinfo_location old_varinfo
      new_varinfo_file_index new_varinfo_location new_varinfo
      rep_varinfo;

    (* Clean up the storage. *)
    treat_varinfo_storage_pass_1
      old_varinfo old_varinfo_location new_varinfo rep_varinfo;

    (* Take care of the attributes. *)
    treat_varinfo_attrs_pass_1
      old_varinfo new_varinfo rep_varinfo

let treat_staticlike_varinfo_pass_1 () = ()

let treat_global_varinfo_pass_1
    new_varinfo_file_index new_varinfo
    new_varinfo_location new_varinfo_in_file_index =
  if treat_like_static new_varinfo
  then
    treat_staticlike_varinfo_pass_1 ()
  else
    treat_nonstaticlike_varinfo_pass_1
      new_varinfo_file_index new_varinfo
      new_varinfo_location new_varinfo_in_file_index

let treat_one_global_pass_1 file_index in_file_index global =
  Kernel.debug ~dkey
    "---------------------- treat_one_global_pass_1 ----------------------@.\
     ----------------[ file index = %a, in-file index = %a ]-------------@.\
     @[%a@]\
     ---------------------------------------------------------------------"
    FileIndex.pretty file_index
    InFileIndex.pretty in_file_index
    Printer.pp_global global;

  (* Clean the referenced flag. *)
  make_not_referenced global;

  (* Update the reference to the current location if appropriate. *)
  Extlib.may Cil.CurrentLoc.set (loc_of_global global);

  match global with
  (* A variable definition. *)
  | GVar (varinfo, _, loc)
  (* A variable declaration for a variable with object type. *)
  | GVarDecl (varinfo, loc) ->
    treat_global_varinfo_pass_1 file_index varinfo loc in_file_index

  (* A function declaration, i.e. a prototype. *)
  | GFunDecl (_, varinfo, loc)
  (* A function definition. *)
  | GFun ( { svar = varinfo; _ } , loc) ->
    (* Save the names of the formal arguments. *)
    BothPassesEnv.FormalArgsNames.maybe_add (file_index, varinfo);

    treat_global_varinfo_pass_1 file_index varinfo loc in_file_index;

    if is_aggressive_merging_candidate varinfo
    then begin
      (* Create the node for functions suitable for aggressive merging. *)
      let node =
        let node_id_in_file = (file_index, varinfo.vname) in
        let node_data = varinfo in
        PlainMerging.get_node' aggressive_merging
          node_id_in_file node_data
          (Node.Loc.defined loc in_file_index)
      in
      (* Store the node for this aggressive merging candidate. *)
      BothPassesEnv.AggressiveMerging.add_candidate_node varinfo loc node;
      (* Store the data related to this aggressive merging candidate. *)
      let candidate_data = (file_index, global) in
      BothPassesEnv.AggressiveMerging.add_candidate_data varinfo candidate_data
    end

  (* A typedef. *)
  | GType (typeinfo, loc) ->
    (* Make nodes for the defined type and structure and enum tags. *)
    begin
      match typeinfo.tname, typeinfo.ttype with
      (* The empty names are for introducing undefined comp tags. *)
      | "", TComp (compinfo, _, _) ->
        (* Create a node for it. *)
        let node_id_in_file = (file_index, compinfo.cname) in
        let node_data = compinfo in
        PlainMerging.create_node composites
          node_id_in_file node_data
          Node.Loc.undefined
      | "", TEnum (enuminfo, _) ->
        (* Create a node for it. *)
        let node_id_in_file = (file_index, enuminfo) in
        let node_data = enuminfo in
        EnumMerging.create_node enums
          node_id_in_file node_data
          Node.Loc.undefined
      | "", _ -> Kernel.fatal "Anonymous Gtype is not TComp nor TEnum"
      (* Not anonymous. *)
      | _ ->
        (* Create a node for it. *)
        let node_id_in_file = (file_index, typeinfo.tname) in
        let node_data = typeinfo in
        PlainMerging.create_node types
          node_id_in_file node_data
          (Node.Loc.defined loc in_file_index)
    end

  (* Defines a struct/union tag with some fields. *)
  | GCompTag (compinfo, loc) ->
    let node_id_in_file = (file_index, compinfo.cname) in
    let node_data = compinfo in
    PlainMerging.create_node composites
      node_id_in_file node_data
      (Node.Loc.defined loc in_file_index)

  (* Declares a struct/union tag. *)
  | GCompTagDecl (_compinfo, _loc) -> ()

  (* Declares an enumeration tag with some fields. *)
  | GEnumTag (enuminfo, loc) ->
    ignore (AnonEnums.newAlphaName enuminfo loc);
    let node_id_in_file = (file_index, enuminfo) in
    let node_data = enuminfo in
    EnumMerging.create_node enums
      node_id_in_file node_data
      (Node.Loc.defined loc in_file_index)

  (* Declares an enumeration tag. *)
  | GEnumTagDecl (_enuminfo, _loc) -> ()

  (* A global annotation. *)
  | GAnnot (global_annot, _loc) ->
    global_annot_pass_1 file_index in_file_index global_annot

  | GText _ | GPragma _ | GAsm _ -> ()


(************************************************************
 *
 *  PASS 2
 *
 *
 ************************************************************)

(* [make_function_printout ~with_alpha_convert global] accepts as arguments
   only GFun globals. *)
let make_function_printout :
  with_alpha_convert:bool -> global -> string option =

  (* Temporarily turn off printing of line numbers and print a global. *)
  let printout_global global =
    let open Printer_api in
    (* Save printing settings. *)
    let saved_line_directive_style = Cil_printer.state.line_directive_style in
    (* Temporarily turn off printing of line numbers. *)
    Cil_printer.state.line_directive_style <- None;
    (* Do the printout. *)
    let printout = Pretty_utils.sfprintf "%a" Printer.pp_global global in
    (* Restore printing settings. *)
    Cil_printer.state.line_directive_style <- saved_line_directive_style;
    (* Done! *)
    printout
  in

  (* Prepare a pair of functions to perform the renaming and undo it later. *)
  let make_rename_and_undo_funs () : (varinfo -> unit) * (varinfo -> unit) =
    let current_name_id = ref 0 in
    let saved_variable_names : string list ref = ref [] in
    let rename_one varinfo =
      saved_variable_names := varinfo.vname :: !saved_variable_names;
      incr current_name_id;
      varinfo.vname <- "___alpha" ^ string_of_int !current_name_id
    in
    let undo_rename_one varinfo =
      match !saved_variable_names with
      | saved_variable_name :: remaining_saved_variable_names ->
        saved_variable_names := remaining_saved_variable_names;
        varinfo.vname <- saved_variable_name
      | _ -> Kernel.fatal "undo_rename_one"
    in
    rename_one, undo_rename_one
  in

  fun ~with_alpha_convert -> function
    | GFun(_fundec, _loc) as global when not with_alpha_convert ->
      Some (printout_global global)

    | GFun(fundec, _loc) as global when with_alpha_convert ->

      (* Remeber the original function name and type. *)
      let saved_name = fundec.svar.vname in
      let saved_type = fundec.svar.vtype in

      (* Prepare the rename and undo functions. *)
      let rename_one, undo_rename_one = make_rename_and_undo_funs () in

      (* 1. If we must do the alpha conversion then temporarily set the names of
            the function, local variables and formals in a standard way. *)
      (* 1.1. Rename the function. *)
      fundec.svar.vname <- "@@alphaname@@";
      (* 1.2. Rename the formals. *)
      List.iter rename_one fundec.sformals;
      (* 1.3. Reflect formals renaming in the type. *)
      Cil.setFormals fundec fundec.sformals;
      (* 1.4. Rename the locals. *)
      List.iter rename_one fundec.slocals;

      (* 2. Now print it! *)
      let printout = printout_global global in

      (* 3. We undo the renaming. *)
      (* 3.1. Undo the function renaming. *)
      fundec.svar.vname <- saved_name;
      (* 3.2. Undo the locals renaming (in reverse order). *)
      List.iter undo_rename_one (List.rev fundec.slocals);
      (* 3.3. Undo the formals renaming (in reverse order). *)
      List.iter undo_rename_one (List.rev fundec.sformals);
      (* 3.4. Restore the type. *)
      Cil.update_var_type fundec.svar saved_type;

      (* 4. We are done... *)
      Some printout

    | GFunDecl _ -> None

    | _ -> assert false


module MergeFunspec : sig

  (** We keep only one declaration for each function. The other ones are simply
      discarded, but we need to merge their spec. This is done at the end of the
      2nd pass, to avoid going through [MergedFile.variables] too many times. *)
  (* USED: [get] in [global_merge_funspec] *)
  val pp_funspec_to_merge : Format.formatter -> unit
  val get_funspecs_to_merge : varinfo -> funspec list option

  (** Renaming to be performed in spec found in declarations when there is
      a definition for the given function. *)
  (* USED: [add] in: [treat_one_global_pass_2] [GFun];
           [get] in: [global_merge_funspec]. *)
  val pp_formals_renaming : Format.formatter -> unit
  val add_formals_renaming : varinfo -> fundec -> unit
  val get_formals_renaming : varinfo -> cilVisitor option

  (* USED: in [treat_one_global_pass_2] [GFunDecl]. *)
  val merge_funspec : varinfo -> varinfo -> funspec -> unit

end = struct

  (* --[ funspec_to_merge ]-- *)

  let funspec_to_merge : funspec list Varinfo.Hashtbl.t =
    Varinfo.Hashtbl.create 59

  let name = "funspec_to_merge"
  let table = funspec_to_merge
  let dkey = Kernel.register_category ("mergecil:" ^ name)

  let pp_funspec_to_merge fmt =
    let pp_funspecs_list fmt =
      function
      | [] -> Format.fprintf fmt "()"
      | non_empty_funspecs_list ->
        Format.fprintf fmt "%a"
          (Pretty_utils.pp_list ~pre:"(" ~suf:")" ~sep:", "
             Printer.pp_funspec) non_empty_funspecs_list
    in
    let pp_binding fmt (varinfo, funspecs) =
      Format.fprintf fmt "%a -> %a"
        Printer.pp_varinfo varinfo
        pp_funspecs_list funspecs
    in
    pp_table
      ~iter:Varinfo.Hashtbl.iter
      ~pp_name:(pp_name_of_string name)
      ~pp_binding
      fmt table

  (* USED: internally, in [merge_funspec]. *)
  let add_to_funspec_to_merge varinfo funspec =
    Kernel.debug ~dkey
      "adding new funspec for variable %a: %a!"
      Printer.pp_varinfo varinfo
      Printer.pp_funspec funspec;
    let funspecs =
      try Varinfo.Hashtbl.find table varinfo
      with Not_found -> []
    in Varinfo.Hashtbl.replace table varinfo (funspec::funspecs)

  let get_funspecs_to_merge = Extlib.find_or_none (Varinfo.Hashtbl.find table)

  (* --[ funspec_to_merge ]-- *)

  let formals_renaming :
    cilVisitor Varinfo.Hashtbl.t =
    Varinfo.Hashtbl.create 59

  let name = "formals_renaming"
  let table = formals_renaming
  let dkey = Kernel.register_category ("mergecil:" ^ name)

  let pp_formals_renaming fmt = pp_varinfo_set_table ~name fmt table

  let get_formals_renaming = Extlib.find_or_none (Varinfo.Hashtbl.find table)

  let pp_varinfos_args_list =
    pp_list_as_args_list
      (fun fmt varinfo ->
         Format.fprintf fmt "`%a' (id = %d)"
           Printer.pp_varinfo varinfo varinfo.vid)

  let add_formals_renaming rep_varinfo new_fundec =
    let new_varinfo = new_fundec.svar in
    match Extlib.find_or_none Cil.getFormalsDecl rep_varinfo with
    | None -> ()
    | Some rep_formal_args ->
      (* CHECK: Why [new_fundec.sformals] and not
                [Cil.getFormalsDecl new_varinfo]? *)
      let new_formal_args = new_fundec.sformals in
      let varinfo, replaced_args_varinfos, replacing_args_varinfos =
        Kernel.debug ~dkey "file_pass_2: %s: %d -> %d"
          rep_varinfo.vname new_varinfo.vid rep_varinfo.vid;
        rep_varinfo, rep_formal_args, new_formal_args
      in
      try
        let alpha_renaming : cilVisitor =
          Cil.create_alpha_renaming
            replaced_args_varinfos replacing_args_varinfos
        in
        Kernel.debug ~dkey
          "adding formals renaming for variable %a: %a -> %a!"
          Printer.pp_varinfo varinfo
          pp_varinfos_args_list replaced_args_varinfos
          pp_varinfos_args_list replacing_args_varinfos;
        if (Varinfo.Hashtbl.mem table varinfo) then
          Kernel.feedback "%s: covering existing binding!" name;
        Varinfo.Hashtbl.add table varinfo alpha_renaming
      with Invalid_argument _ ->
        (* Raised by [Cil.create_alpha_renaming] when [old_args_varinfos] and
           [new_args_varinfos] have different lengths.
           May occur at least when trying to merge incompatible declarations. *)
        Kernel.debug ~dkey
          "adding formals renaming for variable %a ABORTED!"
          Printer.pp_varinfo varinfo

  let dkey = Kernel.register_category ("mergecil:" ^ "merge_funspec")

  (* --[ funspec_to_merge AND formals_renaming ]-- *)

  (* USES: [get_formals_renaming] and [add_funspec_to_merge].  *)
  let merge_funspec varinfo_to_keep varinfo_to_discard funspec =
    (* No need to keep empty specs. *)
    if not (Cil.is_empty_funspec funspec) then
      let funspec =
        match
          Extlib.find_or_none Cil.getFormalsDecl varinfo_to_discard,
          Extlib.find_or_none Cil.getFormalsDecl varinfo_to_keep
        with
        | None, _ | _, None -> funspec
        | Some discarded_varinfo_args, Some kept_varinfo_args ->
          begin
            Kernel.debug ~dkey "Renaming arguments: %a -> %a"
              pp_varinfos_args_list discarded_varinfo_args
              pp_varinfos_args_list kept_varinfo_args;
            let alpha_visitor : cilVisitor =
              Cil.create_alpha_renaming discarded_varinfo_args kept_varinfo_args
            in
            try
              Kernel.debug ~dkey
                "Renaming spec of function %a@.original spec is %a"
                Varinfo.pretty varinfo_to_discard
                Printer.pp_funspec funspec;
              let funspec' = visitCilFunspec alpha_visitor funspec in
              Kernel.debug ~dkey "renamed spec is %a"
                (* BUG? Shouldn't print [funspec']? *)
                (* TODO: That's the same thing... *)
                Printer.pp_funspec funspec;
              funspec'
            with Not_found -> assert false
          end
      in
      let funspec =
        match get_formals_renaming varinfo_to_keep with
        | None -> funspec
        | Some alpha_visitor ->
          let funspec = visitCilFunspec alpha_visitor funspec in
          Kernel.debug ~dkey "renamed spec with definition's formals is %a"
            Printer.pp_funspec funspec;
          funspec
      in
      add_to_funspec_to_merge varinfo_to_keep funspec

end


let dkey_rename_to_representative =
  Kernel.register_category ("mergecil:" ^ "rename_to_representative")

(** A visitor that renames uses of variables and types inside globals to the
    representative for each name (using the global equivalence tables). *)
(* USED: in [treat_one_global_pass_2]. *)
class renameToRepresentativeClass file_index =

  let dkey = dkey_rename_to_representative in

  (** All the logic stuff is treated in a pretty similar way. *)

  let rename_associated_logic_var logic_var =
    match logic_var.lv_origin with
    | None ->
      begin
        (* Look for a constant logic function of this name. *)
        let logic_info =
          { l_var_info = logic_var;
            l_labels = [];
            l_tparams = [];
            l_type = Some logic_var.lv_type;
            l_profile = [];
            l_body = LBnone }
        in
        match
          LogicFunctionMerging.find_replacement logic_funs
            (file_index, logic_info)
        with
        | None -> DoChildren
        | Some (logic_info', _file_index') ->
          let logic_var' = logic_info'.l_var_info in
          if logic_var == logic_var'
          then DoChildren (* Replacement already done... *)
          else ChangeTo logic_var'
      end
    | Some varinfo when not varinfo.vglob -> DoChildren
    | Some varinfo ->
      begin
        match
          PlainMerging.find_replacement vars (file_index, varinfo.vname)
        with
        | None -> DoChildren
        | Some (varinfo', _) ->
          varinfo'.vreferenced <- true;
          if varinfo == varinfo'
          then DoChildren (* Replacement was already done... *)
          else begin
            match varinfo'.vlogic_var_assoc with
            | None -> varinfo'.vlogic_var_assoc <- Some logic_var; DoChildren
            | Some logic_var' -> ChangeTo logic_var'
          end
      end
  in

  let rename_logic_info =
    let pp_profiles fmt logic_info =
      Pretty_utils.pp_list ~sep:",@ "
        Printer.pp_logic_type fmt
        (List.map (fun logic_var -> logic_var.lv_type) logic_info.l_profile)
    in
    fun logic_info ->
      match
        LogicFunctionMerging.find_replacement logic_funs
          (file_index, logic_info)
      with
      | None ->
        Kernel.debug ~level:2 ~dkey "Using logic function %s(%a)(%a)"
          logic_info.l_var_info.lv_name
          pp_profiles logic_info FileIndex.pretty file_index;
        DoChildren
      | Some (logic_info', file_index') ->
        Kernel.debug ~level:2 ~dkey
          "Renaming use of logic function %s(%a)(%a) to %s(%a)(%a)"
          logic_info.l_var_info.lv_name
          pp_profiles logic_info FileIndex.pretty file_index
          logic_info'.l_var_info.lv_name
          pp_profiles logic_info' FileIndex.pretty file_index';
        ChangeTo logic_info'
  in

  let rename_logic_type_info logic_type_info =
    match
      PlainMerging.find_replacement logic_types
        (file_index, logic_type_info.lt_name)
    with
    | None ->
      Kernel.debug ~level:2 ~dkey
        "Using logic type %s(%a)"
        logic_type_info.lt_name FileIndex.pretty file_index;
      DoChildren
    | Some (logic_type_info', file_index') ->
      Kernel.debug ~level:2 ~dkey
        "Renaming use of logic type %s(%a) to %s(%a)"
        logic_type_info.lt_name FileIndex.pretty file_index
        logic_type_info'.lt_name FileIndex.pretty file_index';
      ChangeTo logic_type_info'
  in

  let rename_logic_ctor_info logic_ctor_info =
    match
      PlainMerging.find_replacement logic_ctors
        (file_index, logic_ctor_info.ctor_name)
    with
    | None ->
      Kernel.debug ~level:2 ~dkey
        "Using logic constructor %s(%a)"
        logic_ctor_info.ctor_name FileIndex.pretty file_index;
      DoChildren
    | Some (logic_ctor_info', file_index') ->
      Kernel.debug ~level:2 ~dkey
        "Renaming use of logic constructor %s(%a) to %s(%a)"
        logic_ctor_info.ctor_name FileIndex.pretty file_index
        logic_ctor_info'.ctor_name FileIndex.pretty file_index';
      ChangeTo logic_ctor_info'
  in

  (** *)
  let find_enumitem_replacement enumitem =
    match
      EnumMerging.find_replacement enums (file_index, enumitem.eihost)
    with
    | None -> None
    | Some (enuminfo, _) when enuminfo == int_enuminfo ->
      (* Two different enums have been merged into an int type.
         Switch to an integer constant. *)
      begin
        match (Cil.constFold true enumitem.eival).enode with
        | Const constant -> Some constant
        | _ -> Kernel.fatal ~current:true
                 "non constant value for an enum item"
      end
    | Some (enuminfo, _) ->
      (* Merged with an isomorphic type. Find the appropriate enumitem. *)
      let enumitem' =
        let emumitem_index =
          Extlib.find_index
            (fun enumitem' -> enumitem'.einame = enumitem.einame)
            enumitem.eihost.eitems
        in
        List.nth enuminfo.eitems emumitem_index
      in
      assert (same_int64 enumitem.eival enumitem'.eival);
      Some (CEnum enumitem')
  in

  (** [index_of_field fieldinfo] returns the index of the given field in the
      composite type it comes from (i.e. [fieldinfo.fcomp]). *)
  let index_of_field fieldinfo : int =
    let rec index_of_field' index_acc = function
      | [] -> Kernel.fatal "index_of_field: cannot find field %s in %s"
                fieldinfo.fname (Cil.compFullName fieldinfo.fcomp)
      | fieldinfo' :: _ when fieldinfo' == fieldinfo -> index_acc
      | _ :: remaining_fieldinfos ->
        index_of_field' (index_acc + 1) remaining_fieldinfos
    in
    index_of_field' 0 fieldinfo.fcomp.cfields
  in

  let find_fieldinfo_replacement fieldinfo =
    match
      PlainMerging.find_replacement composites
        (file_index, fieldinfo.fcomp.cname)
    with
    | None -> None (* We did not replace it. *)
    | Some (compinfo', _file_index') ->
      (* Find the corresponding field in the replacement composite. *)
      let fieldinfo' =
        (* Find what is the index if the field in its composite type. *)
        let fieldinfo_index = index_of_field fieldinfo in
        if List.length compinfo'.cfields <= fieldinfo_index then
          Kernel.fatal "Too few fields in replacement %s for %s"
            (Cil.compFullName compinfo')
            (Cil.compFullName fieldinfo.fcomp);
        (* Get the field which is at the same index in the replacement
           composite.*)
        List.nth compinfo'.cfields fieldinfo_index
      in
      Some fieldinfo'
  in

  object (self)
    inherit nopCilVisitor

    (* logic_var, logic_info, logic_type_info, logic_ctor_info *)
    method! vlogic_var_decl = rename_associated_logic_var
    method! vlogic_var_use = rename_associated_logic_var
    method! vlogic_info_use = rename_logic_info
    method! vlogic_info_decl = rename_logic_info
    method! vlogic_type_info_use = rename_logic_type_info
    method! vlogic_type_info_decl = rename_logic_type_info
    method! vlogic_ctor_info_use = rename_logic_ctor_info
    method! vlogic_ctor_info_decl = rename_logic_ctor_info

    (* This is either a global variable which we took care of, or a local
       variable. Must do its type and attributes. *)
    method! vvdec _varinfo = DoChildren

    (* This is a variable use. See if we must change it. *)
    method! vvrbl varinfo : varinfo Cil.visitAction =
      match varinfo.vglob, varinfo.vreferenced with
      | false, _ -> DoChildren
      | true, true -> DoChildren
      | true, false ->
        match
          PlainMerging.find_replacement vars (file_index, varinfo.vname)
        with
        | None -> DoChildren
        | Some (varinfo', file_index') ->
          Kernel.debug ~dkey
            "Renaming use of variable %a from file %a \
             to variable %a from file %a"
            Printer.pp_varinfo varinfo FileIndex.pretty file_index
            Printer.pp_varinfo varinfo' FileIndex.pretty file_index';
          varinfo'.vreferenced <- true;
          ChangeTo varinfo'

    (* The use of a type. Change only those types whose underlying info
       is not a root. *)
    method! vtype typ =
      match typ with

      | TComp (compinfo, _, attrs) when not compinfo.creferenced ->
        begin
          match
            PlainMerging.find_replacement composites
              (file_index, compinfo.cname)
          with
          | None ->
            Kernel.debug ~dkey "No renaming needed %s(%a)"
              compinfo.cname FileIndex.pretty file_index;
            DoChildren
          | Some (compinfo', file_index') ->
            Kernel.debug ~dkey "Renaming use of %s(%a) to %s(%a)"
              compinfo.cname FileIndex.pretty file_index
              compinfo'.cname FileIndex.pretty file_index';
            let attrs' = visitCilAttributes (self :> cilVisitor) attrs in
            ChangeTo (TComp (compinfo', Cil.empty_size_cache (), attrs'))
        end

      | TEnum (enuminfo, attrs) when not enuminfo.ereferenced ->
        begin
          match
            EnumMerging.find_replacement enums (file_index, enuminfo)
          with
          | None -> DoChildren
          | Some (enuminfo', _) when enuminfo' == int_enuminfo ->
            (* This is actually our friend [int_enuminfo]. *)
            let attrs' = visitCilAttributes (self :> cilVisitor) attrs in
            ChangeTo (TInt(IInt, attrs'))
          | Some (enuminfo', _) ->
            let attrs' = visitCilAttributes (self :> cilVisitor) attrs in
            ChangeTo (TEnum (enuminfo', attrs'))
        end

      | TNamed (typeinfo, attrs) when not typeinfo.treferenced ->
        begin
          match
            PlainMerging.find_replacement types (file_index, typeinfo.tname)
          with
          | None -> DoChildren
          | Some (typeinfo', _) ->
            let attrs' = visitCilAttributes (self :> cilVisitor) attrs in
            ChangeTo (TNamed (typeinfo', attrs'))
        end

      | TComp (compinfo, _,  _) ->
        Kernel.debug ~dkey "%s(%a) referenced. No change"
          compinfo.cname FileIndex.pretty file_index;
        DoChildren

      | _ -> DoChildren

    method! vexpr exp =
      match exp.enode with
      | Const (CEnum enumitem) ->
        begin
          match find_enumitem_replacement enumitem with
          | None -> DoChildren
          | Some constant -> ChangeTo { exp with enode = Const constant }
        end
      | CastE _ ->
        (* Maybe the cast is no longer necessary if an enum has been replaced
           by an integer type. *)
        let post_action exp =
          match exp.enode with
          | CastE(typ, exp) when TypByName.equal (Cil.typeOf exp) typ -> exp
          | _ -> exp
        in
        ChangeDoChildrenPost (exp, post_action)
      | _ -> DoChildren

    method! vterm term =
      match term.term_node with
      | TConst (LEnum enumitem) ->
        begin
          match find_enumitem_replacement enumitem with
          | None -> DoChildren
          | Some constant ->
            let term' =
              { term with
                term_type =
                  visitCilLogicType (self :> cilVisitor) term.term_type;
                term_node =
                  TConst (Logic_utils.constant_to_lconstant constant);
              } in
            ChangeTo term'
        end
      | _ -> DoChildren

    (* The Field offset might need to be changed to use new compinfo. *)
    method! voffs = function
      | Field (fieldinfo, _offset) when fieldinfo.fcomp.creferenced ->
        (* See if the compinfo was changed. *)
        DoChildren
      | Field (fieldinfo, offset) ->
        begin
          match find_fieldinfo_replacement fieldinfo with
          | None -> DoChildren
          | Some fieldinfo' ->
            ChangeDoChildrenPost (Field (fieldinfo', offset), fun x -> x)
        end
      | _ -> DoChildren

    method! vterm_offset = function
      | TField (fieldinfo, _offset) when fieldinfo.fcomp.creferenced ->
        (* See if the compinfo was changed. *)
        DoChildren
      | TField (fieldinfo, offset) ->
        begin
          match find_fieldinfo_replacement fieldinfo with
          | None -> DoChildren
          | Some fieldinfo' ->
            ChangeDoChildrenPost (TField (fieldinfo', offset), fun x -> x)
        end
      | TModel(model_info, term_offset) ->
        begin
          let model_node_id = model_info.mi_name, model_info.mi_base_type in
          match
            ModelMerging.find_replacement model (file_index, model_node_id)
          with
          | None ->
            (* We might have changed the field before choosing it as
               representative. Check that. *)
            let model_info' =
              let model_info_node' =
                ModelMerging.Eq_table.find (eq_table model)
                  (file_index, model_node_id)
              in
              Node.get_data model_info_node'
            in
            if model_info == model_info'
            then DoChildren (* Already the representative. *)
            else ChangeDoChildrenPost (TModel (model_info', term_offset),
                                       fun x -> x)
          | Some (model_info', _) ->
            ChangeDoChildrenPost (TModel(model_info', term_offset),
                                  fun x -> x)
        end
      | _ -> DoChildren

    method! vinitoffs offset =
      (* Treat initializer offsets same as lvalue offsets. *)
      (self#voffs offset)

  end

(* USED: in [treat_one_global_pass_2]. *)
let renameToRepresentativeVisitor = new renameToRepresentativeClass


(** A visitor that renames uses of inline or static functions that were
    discovered in pass 2 to be used before they are defined. This is like the
    [renameVisitor] except it only looks at the variables (thus it is a bit more
    efficient) and it also renames forward declarations of the inlines or
    statics to be removed. *)
(* USED: in [treat_one_global_pass_2] *)
class renameInlineOrStaticVisitorClass file_index get_original_var_name = object
  inherit nopCilVisitor

  (* This is a variable use. See if we must change it. *)
  method! vvrbl varinfo : varinfo Cil.visitAction =
    match varinfo.vglob, varinfo.vreferenced with
    | false, _ -> DoChildren (* Not global. *)
    | true, true -> DoChildren (* Already renamed. *)
    | true, false ->
      match PlainMerging.find_replacement vars (file_index, varinfo.vname) with
      | None -> DoChildren
      | Some (varinfo', file_index') ->
        Kernel.debug ~dkey
          "Renaming use of variable %a from file %a \
           to variable %a from file %a"
          Printer.pp_varinfo varinfo FileIndex.pretty file_index
          Printer.pp_varinfo varinfo' FileIndex.pretty file_index';
        varinfo'.vreferenced <- true;
        ChangeTo varinfo'

  (* And rename some declarations of inlines or statics to remove.
     We drop this declaration (see small1/combineinline6). *)
  method! vglob = function
    | GFunDecl(funspec, varinfo, loc) ->
      (* Get the original name. *)
      let original_var_name = get_original_var_name varinfo.vname in
      (* Now see if this must be replaced. *)
      begin
        match
          PlainMerging.find_replacement vars (file_index, original_var_name)
        with
        | None -> DoChildren
        | Some (varinfo', _file_index') ->
          (* TODO: visit the funspec to change references to formals. *)
          ChangeTo [GFunDecl (funspec, varinfo', loc)]
      end
    | _ -> DoChildren

end

(* USED: in [treat_one_global_pass_2]. *)
let renameInlinesAndStaticsVisitor = new renameInlineOrStaticVisitorClass

(* USED: in [treat_one_global_pass_2]. *)
(* BUG? Why sometimes we always visit global and sometimes only when
        [in_axiomatic] is false? *)
let rec global_annot_pass_2
    ?(in_axiomatic=false) file_index global global_annot =
  let renameVisitor = renameToRepresentativeVisitor file_index in
  Cil.CurrentLoc.set (loc_of_global_annot global_annot);
  match global_annot with
  | Dfun_or_pred (logic_info, _loc)
    when LogicFunctionMerging.has_no_replacement
        logic_funs (file_index, logic_info) ->
    let globals' =
      if not in_axiomatic
      then visitCilGlobal renameVisitor global
      else []
    in
    Logic_utils.add_logic_function logic_info;
    (* FIXME: should we perform same actions as in the Dlogic_reads case? *)
    globals'

  | Dvolatile (identified_terms, _, _, _loc)
    when VolatileMerging.has_no_replacement volatile
        (file_index, identified_terms) ->
    visitCilGlobal renameVisitor global

  | Daxiomatic (id, decls, _loc)
    when PlainMerging.has_no_replacement logic_axiomatics (file_index, id) ->
    assert (not in_axiomatic);
    (* TODO: Optimize this one day... *)
    (visitCilGlobal renameVisitor global) @
    (List.flatten
       (List.map
          (global_annot_pass_2 ~in_axiomatic:true file_index global)
          decls))

  | Dtype (logic_type_info, _loc)
    when PlainMerging.has_no_replacement
        logic_types (file_index, logic_type_info.lt_name) ->
    let globals' =
      if not in_axiomatic
      then visitCilGlobal renameVisitor global
      else []
    in
    let my_node =
      PlainMerging.Eq_table.find (eq_table logic_types)
        (file_index, logic_type_info.lt_name)
    in
    Logic_env.add_logic_type logic_type_info.lt_name (Node.get_data my_node);
    globals'

  | Dlemma (id, _, _, _, _, _loc)
    when PlainMerging.has_no_replacement logic_lemmas (file_index, id) ->
    if not in_axiomatic
    then visitCilGlobal renameVisitor global
    else []

  | Dinvariant (logic_info, _loc)
    when LogicFunctionMerging.has_no_replacement
        logic_funs (file_index, logic_info) ->
    assert (not in_axiomatic);
    let globals' = visitCilGlobal renameVisitor global in
    let my_node =
      LogicFunctionMerging.Eq_table.find (eq_table logic_funs)
        (file_index, logic_info)
    in
    Logic_utils.add_logic_function (Node.get_data my_node);
    globals'

  | Dtype_annot (logic_info, _loc)
    when LogicFunctionMerging.has_no_replacement
        logic_funs (file_index, logic_info) ->
    let globals' =
      let globals' = visitCilGlobal renameVisitor global in
      if not in_axiomatic then globals' else []
    in
    let my_node =
      LogicFunctionMerging.Eq_table.find (eq_table logic_funs)
        (file_index, logic_info)
    in
    Logic_utils.add_logic_function (Node.get_data my_node);
    globals'

  | Dmodel_annot (mfi, loc)
    when ModelMerging.has_no_replacement
        model (file_index, (mfi.mi_name, mfi.mi_base_type)) ->
    let mfi' = visitCilModelInfo renameVisitor mfi in
    if mfi' != mfi then begin
      let my_node =
        ModelMerging.Eq_table.find (eq_table model)
          (file_index, (mfi'.mi_name, mfi'.mi_base_type))
      in
      (* Adds a new representative. Do not replace directly [my_node], as
         there might be some pointers to it from other files. *)
      let my_node' = Node.make_my_node' my_node mfi' in
      (* my_node' is is the canonical representative. *)
      ModelMerging.Eq_table.add (eq_table model) my_node';
      (* BUG? What are we doing here exactly?... This seems to make a second
         binding in [model] for the same name and file index (we've just
         retrieved it with [find]). *)
    end;
    let globals' =
      if not in_axiomatic
      then [GAnnot (Dmodel_annot(mfi', loc), loc)]
      else []
    in
    let my_node =
      ModelMerging.Eq_table.find (eq_table model)
        (file_index, (mfi'.mi_name, mfi'.mi_base_type))
    in
    Logic_env.add_model_field (Node.get_data my_node);
    globals'

  | Dcustom_annot (_c, n, _loc)
    when PlainMerging.has_no_replacement logic_custom (file_index, n) ->
    let globals' = visitCilGlobal renameVisitor global in
    if not in_axiomatic then globals' else []

  (* All the above cases when there is a replacement. *)
  | Dfun_or_pred _ | Dtype _ | Dinvariant _ | Dtype_annot _ | Dmodel_annot _
  | Dcustom_annot _ | Dlemma _ | Dvolatile _ | Daxiomatic _ -> []


(* sm: First attempt at a semantic checksum for function bodies.
   Ideally, two function's checksums would be equal only when their bodies were
   provably equivalent; but I'm using a much simpler and less accurate heuristic
   here.  It should be good enough for the purpose I have in mind, which is
   doing duplicate removal of multiply-instantiated template functions. *)
let function_checksum fundec : int =

  (* Checksum of the structure of the statements (only). *)
  let rec stmts_sum (stmts : stmt list) : int =
    List.fold_left (fun sum stmt -> sum + (stmt_sum stmt)) 0 stmts
  and stmt_sum (stmt : stmt) : int =
    (* Strategy is to just throw a lot of prime numbers into the computation in
       hopes of avoiding accidental collision. *)
    match stmt.skind with
    | Instr _ -> 13 + 67
    | Return _  -> 17
    | Goto _ -> 19
    | Break _ -> 23
    | Continue _ -> 29
    | If (_, block_1, block_2, _) ->
      31 +
      37 * block_sum block_1 +
      41 * block_sum block_2
    | Switch(_, block, _, _) ->
      43 +
      47 * block_sum block
    | Loop (_, block, _, _, _) ->
      49 +
      53 * block_sum block
    | Block block ->
      59 +
      61 * block_sum block
    | UnspecifiedSequence seq ->
      127 +
      131 * seq_sum seq
    | Throw _ -> 137
    | TryCatch (block, catch_binders, _) ->
      139 +
      149 * block_sum block +
      151 * catch_binders_sum catch_binders
    | TryFinally (block_1, block_2, _) ->
      103 +
      113 * block_sum block_1 +
      119 * block_sum block_2
    (* don't look at stmt list b/c is not part of tree *)
    | TryExcept (block_1, _, block_2, _) ->
      67 +
      83 * block_sum block_1 +
      97 * block_sum block_2
  and block_sum block = stmts_sum block.bstmts
  and seq_sum seq =
    let stmts = List.map (fun (stmt, _, _, _, _) -> stmt) seq in
    stmts_sum stmts
  and catch_binders_sum catch_binders =
    List.fold_left
      (fun sum (_, block) -> sum + stmts_sum block.bstmts)
      0 catch_binders
  in

  (* Disabled 2nd and 3rd measure because they appear to yield different values,
     for the same code, depending on whether the code was just parsed into CIL
     or had previously been parsed into CIL, printed out, then re-parsed into
     CIL. *)
  let a, b, c, d, e =
    List.length fundec.sformals,     (* number of formals *)
    0 (*List.length dec.slocals*),   (* number of locals *)
    0 (*dec.smaxid*),                (* estimate of internal statement count *)
    List.length fundec.sbody.bstmts, (* number of statements at outer level *)
    stmts_sum fundec.sbody.bstmts    (* checksum of statement structure *)
  in
  2*a + 3*b + 5*c + 7*d + 11*e


(* BUG! Apparently this does not work correctly (at least that's what the
        comments inside say). *)
module Equal : sig
  val init_opts : init option -> init option -> bool
end = struct

  (* sm: equality for initializers, etc.; this is like '=', except when we reach
     shared pieces (like references into the type structure), we use '==', to
     prevent circularity *)
  (* update: that's no good; I'm using this to find things which are equal but
     from different CIL trees, so nothing will ever be '=='.. as a hack I'll
     just change those places to 'true', so these functions are not now checking
     proper equality.. places where equality is not complete are marked "INC" *)
  let rec equal_inits (init_1 : init) (init_2 : init) : bool =
    match init_1, init_2 with
    | SingleInit(init_exp_1),
      SingleInit(init_exp_2) -> equal_exps init_exp_1 init_exp_2

    | CompoundInit(_typ_1, inits_1),
      CompoundInit(_typ_2, inits_2) ->
      let rec equal_inits' inits_1 inits_2 : bool =
        match inits_1, inits_2 with
        | [], [] -> true
        | (offset_1, init_1) :: remaining_inits_1,
          (offset_2, init_2) :: remaining_inits_2 ->
          (equal_offsets offset_1 offset_2) &&
          (equal_inits   init_1   init_2) &&
          (equal_inits'  remaining_inits_1 remaining_inits_2)
        | _, _ -> false
      in
      (* (_typ_1 == _typ_2) && (*INC: types need to be identically equal.*) *)
      (equal_inits' inits_1 inits_2)

    | _, _ -> false

  and equal_offsets (offset_1 : offset) (offset_2 : offset) : bool =
    match offset_1, offset_2 with
    | NoOffset,
      NoOffset -> true

    | Field(fieldinfo_1, remaining_offset_1),
      Field(fieldinfo_2, remaining_offset_2) ->
      (fieldinfo_1.fname = fieldinfo_2.fname) && (*INC: same fieldinfo name.*)
      (equal_offsets remaining_offset_1 remaining_offset_2)

    | Index(index_exp_1, remaining_offset_1),
      Index(index_exp_2, remaining_offset_2) ->
      (equal_exps    index_exp_1        index_exp_2) &&
      (equal_offsets remaining_offset_1 remaining_offset_2)

    | _,_ -> false

  and equal_exps (exp_1 : exp) (exp_2 : exp) : bool =
    match exp_1.enode, exp_2.enode with
    | Const(constant_1),
      Const(constant_2) ->
      Cil.compareConstant constant_1 constant_2 ||
      begin
        (* CIL changes (unsigned)0 into 0U during printing. *)
        match constant_1, constant_2 with
        | CInt64(integer_1,_,_), CInt64(integer_2,_,_) ->
          (* ok if they're both 0 *)
          (Integer.equal integer_1 Integer.zero) &&
          (Integer.equal integer_2 Integer.zero)
        | _,_ -> false
      end

    | Lval(lval_1),
      Lval(lval_2) -> equal_lvals lval_1 lval_2

    | SizeOf(_typ_1),
      SizeOf(_typ_2) -> true (*INC: _typ_1 == _typ_2*)

    | SizeOfE(exp_1),
      SizeOfE(exp_2) -> equal_exps exp_1 exp_2

    | AlignOf(_typ_1),
      AlignOf(_typ_2) -> true (*INC: _typ_1 == _typ_2*)

    | AlignOfE(exp_1),
      AlignOfE(exp_2) -> equal_exps exp_1 exp_2

    | UnOp(unop_1, exp_1, _typ_1),
      UnOp(unop_2, exp_2, _typ_2) ->
      unop_1 = unop_2 &&
      equal_exps exp_1 exp_2 &&
      true (*INC: _typ_1 == _typ_2*)

    | BinOp(binop_1, left_exp_1, right_exp_1, _typ_1),
      BinOp(binop_2, left_exp_2, right_exp_2, _typ_2) ->
      binop_1 = binop_2 &&
      equal_exps left_exp_1  left_exp_2 &&
      equal_exps right_exp_1 right_exp_2 &&
      true (*INC: _typ_1 == _typ_2*)

    | CastE(_typ_1, exp_1), CastE(_typ_2, exp_2) ->
      (*INC: _typ_1 == _typ_2 &&*)
      equal_exps exp_1 exp_2

    | AddrOf(lval_1),
      AddrOf(lval_2) -> equal_lvals lval_1 lval_2

    | StartOf(lval_1),
      StartOf(lval_2) -> equal_lvals lval_1 lval_2

    (* initializers that go through CIL multiple times sometimes lose casts they
       had the first time; so allow a different of a cast *)
    | CastE(_typ, casted_exp), _ -> (equal_exps casted_exp exp_2)
    | _, CastE(_typ, casted_exp) -> (equal_exps exp_1 casted_exp)

    | _, _ -> false

  and equal_lvals (lval_1 : lval) (lval_2 : lval) : bool =
    match lval_1, lval_2 with
    | (Var _varinfo_1, offset_1),
      (Var _varinfo_2, offset_2) ->
      (* I tried, I really did.. the problem is I see these names before merging
         collapses them, so __T123 != __T456, so whatever *)
      (* (xv.vname = vy.vname) && (*INC: same varinfo names...*) *)
      equal_offsets offset_1 offset_2
    | (Mem(exp_1), offset_1),
      (Mem(exp_2), offset_2) ->
      (equal_exps    exp_1    exp_2) &&
      (equal_offsets offset_1 offset_2)
    | _, _ -> false

  let equal_init_opts
      (init_opt_1: init option) (init_opt_2: init option) : bool =
    match init_opt_1, init_opt_2 with
    | None, None -> true
    | Some(init_1), Some(init_2) -> equal_inits init_1 init_2
    | _,_ -> false

  let init_opts = equal_init_opts

end

(* This is the environment kept all the way through the pass 2 on all the files
   and used afterwards in the [merge] function. *)
(* USED: all in different cases of [treat_one_global_pass_2]. *)
module Pass2Env : sig

  val clear : unit -> unit

  (** Flags. *)

  module EncounteredAggressiveMergingCandidateFlag : sig
    (* If we merge aggressively some inline or static functions which are used
       before being defined, we mark this flag so that we can make another pass
       over the file. *)
    val get : unit -> bool
    val set : unit -> unit
  end

  module EncounteredTentativeDefinitionFlag : sig
    (* Set to true if we need to make an additional path for changing tentative
       definition into plain declaration because a real definition has been
       found. *)
    val get : unit -> bool
    val set : unit -> unit
  end

  module OriginalVarNames : sig
    (** A mapping from the new names to the original names. Used in PASS2 when
        we rename variables. *)
    (* USED: [remember] in [process_varinfo];
             [get] in: [treat_one_global_pass_2] [GFun]; and passed to
             to [renameInlinesAndStaticsVisitor] in [treat_one_global_pass_2]. *)
    val pretty : Format.formatter -> unit
    val remember : string -> string -> unit
    val get_if_remembered : string -> string
  end

  (** Alpha tables. *)
  (* USED: [CompAlpha.new_name] in [GCompTag];
           [EnumAlpha.new_name] in [GEnumTag]. *)
  module CompAlpha : sig
    (** Structures and unions (they have the same name space). *)
    val new_name : ?loc:location -> string -> string
  end
  module EnumAlpha : sig
    (** Enumerations. *)
    val new_name : ?loc:location -> string -> string
  end

  module CompsDeclared : sig
    (** Remember the composite types that we have already declared. *)
    (* USED: [add] in: [GCompTag] and [GCompTagDecl];
             [mem] in: [GCompTagDecl].*)
    val pretty : Format.formatter -> unit
    val add : compinfo -> unit
    val mem : compinfo -> bool
  end

  module VarsDeclared : sig
    (** Remember the declared variables also. *)
    (* USED: [add] in: [GVarDecl], [GFunDecl], [GVar];
             [mem] in: [GVarDecl], [GFunDecl]. *)
    val pretty : Format.formatter -> unit
    val add : varinfo -> unit
    val mem : varinfo -> bool
  end

  module FunDefinitions : sig
    (** Keep track of externally-visible function definitions;
        name maps to declaration, location, and semantic checksum. *)
    (* USED: all in [GFun]. *)
    type t = fundec * location * int
    val pretty : Format.formatter -> unit
    val add : fundec -> location -> int -> unit
    val get : fundec -> t option
    val mem : fundec -> bool
  end

  module VarDefinitions : sig
    (** Same for variable definitions; variable's name maps to GVar fields. *)
    (* USED: all three in [GVar], [get] also in epilogue. *)
    type t = varinfo * init option * location
    val pretty : Format.formatter -> unit
    val add : varinfo -> t -> unit
    val replace : varinfo -> t -> unit
    val get : varinfo -> t option
  end

end = struct

  (* --[ flags ]-- *)

  module EncounteredAggressiveMergingCandidateFlag = struct
    let flag = ref false
    let get () = !flag
    let set () = flag := true
  end

  module EncounteredTentativeDefinitionFlag = struct
    let flag = ref false
    let get () = !flag
    let set () = flag := true
  end

  module OriginalVarNames = struct
    (* --[ original_var_names ]-- *)
    let name = "original_var_names"
    let dkey = Kernel.register_category "original_var_names"
    let table : (string, string ) Hashtbl.t = Hashtbl.create 113

    let pretty fmt =
      let pp_binding fmt (varinfo_name, original_varinfo_name) =
        Format.fprintf fmt "%s -> %s" varinfo_name original_varinfo_name
      in
      pp_hashtable ~name ~pp_binding fmt table

    let remember new_name original_name =
      Kernel.debug ~dkey
        "remembered variable's `%s' old name `%s'!"
        new_name original_name;
      (* NOTE: Each "new" variable name should be unique and should correspond
         to exactly one original variable name. *)
      if Hashtbl.mem table new_name then
        (* TODO: Why does this happen sometimes? The new name is created by
           [Alpha.newAlphaName], so it should be unique... *)
        Kernel.debug ~dkey "%s: covering existing binding!" name;
      Hashtbl.add table new_name original_name

    let get_original_var_name =
      Extlib.find_or_none (Hashtbl.find table)

    let get_if_remembered var_name =
      Extlib.opt_conv var_name (get_original_var_name var_name)
  end

  module CompAlpha = struct
    (* --[ comps_alpha ]-- *)
    let name = "comps_alpha"
    let alpha_table : alpha_conversion_table = Hashtbl.create 57
    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let clear () = Hashtbl.clear alpha_table

    let new_name ?(loc = Cil.CurrentLoc.get ()) name =
      let new_name, _ =
        Alpha.newAlphaName
          ?undolist:None
          ~alphaTable:alpha_table
          ~lookupname:name
          ~data:loc
      in
      Kernel.debug ~dkey "name was `%s' -> new name is `%s'" name new_name;
      new_name
  end

  module EnumAlpha = struct
    (* --[ enums_alpha ]-- *)
    let name = "enums_alpha"
    let alpha_table : alpha_conversion_table = Hashtbl.create 57
    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let clear () = Hashtbl.clear alpha_table

    let new_name ?(loc = Cil.CurrentLoc.get ()) name =
      let new_name, _ =
        Alpha.newAlphaName
          ?undolist:None
          ~alphaTable:alpha_table
          ~lookupname:name
          ~data:loc
      in
      Kernel.debug ~dkey "name was `%s' -> new name is `%s'" name new_name;
      new_name
  end

  (* --[ comp_declarations ]-- *)
  module CompsDeclared = struct
    let name = "comp_declarations"
    let table : (string, unit) Hashtbl.t = Hashtbl.create 113

    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let clear () = Hashtbl.clear table

    let pretty fmt = pp_string_set_hashtable ~name fmt table

    let add compinfo =
      let key = compinfo.cname in
      Kernel.debug ~dkey
        "marking %a `%s' as declared!"
        pp_cstruct compinfo.cstruct compinfo.cname;
      Hashtbl.add table key ()

    let mem compinfo =
      let key = compinfo.cname in
      Hashtbl.mem table key
  end

  (* --[ var_declarations ]-- *)
  module VarsDeclared = struct
    let name = "var_declarations"
    let table : (string, unit) Hashtbl.t = Hashtbl.create 113

    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let clear () = Hashtbl.clear table

    let pretty fmt = pp_string_set_hashtable ~name fmt table

    let add varinfo =
      let key = varinfo.vname in
      Kernel.debug ~dkey
        "marking variable `%a' as declared!"
        Printer.pp_varinfo varinfo;
      Hashtbl.add table key ()

    let mem varinfo =
      let key = varinfo.vname in
      Hashtbl.mem table key
  end

  module VarDefinitions = struct
    (* --[ var_definitions ]-- *)
    type t = varinfo * init option * location

    let name = "var_definitions"
    let table : (string, t) Hashtbl.t = Hashtbl.create 113

    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let clear () = Hashtbl.clear table

    let pp_var_definition fmt (name, (varinfo, init_option, loc)) =
      Format.fprintf fmt "variable `%s' definition at %a: @[%a@]"
        name Printer.pp_location loc
        Printer.pp_global (GVar(varinfo, {init = init_option}, loc))

    let pp_binding = pp_var_definition

    let pretty fmt = pp_hashtable ~name ~pp_binding fmt table

    let add varinfo var_definition_details =
      let key = varinfo.vname in
      let value = var_definition_details in
      Kernel.debug ~dkey "adding %a" pp_var_definition (key, value);
      if Hashtbl.mem table key then
        Kernel.feedback "%s: covering existing binding!" name;
      Hashtbl.add table key value

    let replace varinfo var_definition_details =
      let key = varinfo.vname in
      let value = var_definition_details in
      assert (Hashtbl.mem table key);
      Kernel.debug ~dkey "replacing %a" pp_var_definition (key, value);
      Hashtbl.replace table key value

    let get varinfo =
      let key = varinfo.vname in
      Extlib.find_or_none (Hashtbl.find table) key
  end

  (* --[ fun_definitions ]-- *)
  module FunDefinitions = struct
    type t = fundec * location * int

    let name = "fun_definitions"
    let table : (string, t) Hashtbl.t = Hashtbl.create 113

    let dkey = Kernel.register_category ("mergecil:" ^ name)

    let clear () = Hashtbl.clear table

    let pp_fun_definition fmt (name, (fundec, loc, checksum)) =
      Format.fprintf fmt "function `%s' def at %a (checksum = %d):@.| @[%a@]"
        name Printer.pp_location loc checksum
        Printer.pp_global (GFun(fundec, loc))

    let pp_binding = pp_fun_definition

    let pretty fmt = pp_hashtable ~name ~pp_binding fmt table

    let add fundec loc checksum =
      let key = fundec.svar.vname in
      let value = (fundec, loc, checksum) in
      Kernel.debug ~dkey "adding %a" pp_fun_definition (key, value);
      if Hashtbl.mem table key then
        Kernel.feedback "%s: covering existing binding!" name;
      Hashtbl.add table key value

    let get fundec =
      let key = fundec.svar.vname in
      Extlib.find_or_none (Hashtbl.find table) key

    let mem fundec =
      match get fundec with
      | None -> false
      | Some _ -> true
  end

  let clear () =
    EncounteredAggressiveMergingCandidateFlag.flag := false;
    EncounteredTentativeDefinitionFlag.flag := false;
    CompAlpha.clear ();
    EnumAlpha.clear ();
    CompsDeclared.clear ();
    VarsDeclared.clear ();
    FunDefinitions.clear ();
    VarDefinitions.clear ()

end

(* dkey for debug printing in following varinfo-processing functions. *)
let dkey_process_varinfo = Kernel.register_category "mergecil:process_varinfo"

(* Process a varinfo. Reuse an old one, or rename it if necessary. *)
(* NOTE: This function was extracted from [treat_one_global_pass_2]. *)
(* [process_staticlike_varinfo] modifies varinfo's fields:
   - [vreferenced] is set to [true],
   - [vname] is set to a new unique name based on the current [vname],
   - [vid] is set to a fresh number.
   Also:
   - In the hashtable behind [Pass2Env.OriginalVarNames.remember]:
       it adds a binding from the new variable name to the original name.
   - In the [Cil.FormalsDecl] hashtable:
       if the variable corresponds to a function declaration it sets / replaces
       the formals corresponding to the new variable with the ones corresponding
       to the original one. *)
let process_staticlike_varinfo new_varinfo new_loc =
  let dkey = dkey_process_varinfo in
  assert (not new_varinfo.vreferenced);
  assert (treat_like_static new_varinfo);

  (* 1. -- Prepare the modifications. -- *)
  (* Retrieve the original varinfo's id and name *)
  let original_varinfo_id = new_varinfo.vid in
  let original_varinfo_name = new_varinfo.vname in
  (* Create a new unique name for the variable. *)
  let new_varinfo_name =
    BothPassesEnv.VarOrTypeAlpha.new_name original_varinfo_name
  in
  (* Remember the original variable's name. *)
  Pass2Env.OriginalVarNames.remember new_varinfo_name original_varinfo_name;
  (* Retrieve the formals registered for this variable (should retrieve
     something only if this variable corresponds to a function prototype). *)
  let new_formals_decl_opt =
    Extlib.find_or_none Cil.getFormalsDecl new_varinfo
  in

  (* 2. -- Modify the variable. -- *)
  (* Mark the variable as already done. *)
  new_varinfo.vreferenced <- true;
  (* Rename the variable. *)
  new_varinfo.vname <- new_varinfo_name;
  (* Set the variables id to a fresh number. *)
  Cil_const.set_vid new_varinfo;
  (* Register the formals for the new variable's id (if there were any). *)
  Extlib.may (Cil.unsafeSetFormalsDecl new_varinfo) new_formals_decl_opt;

  (* 3. -- Debug printing. -- *)
  Kernel.debug ~dkey ~current:true
    "renamed static-like variable `%s'(%d) to `%s'(%d) (at %a)"
    original_varinfo_name original_varinfo_id new_varinfo_name new_varinfo.vid
    Printer.pp_location new_loc

let process_nonstaticlike_varinfo new_file_index new_varinfo =
  let dkey = dkey_process_varinfo in
  assert (not new_varinfo.vreferenced);
  assert (not (treat_like_static new_varinfo));

  (* If this is an inline definition and the body will be discarded then we
     should not consider this function as defined until we encounter a proper
     external definition. *)
  if should_discard_inline_body new_varinfo then new_varinfo.vdefined <- false;

  (* What do we do depends on whenever the variable is in an equivalence class
     and on the representative of this class. *)
  match
    PlainMerging.find_replacement vars (new_file_index, new_varinfo.vname)
  with
  (* This is the representative. *)
  | None ->
    Kernel.debug ~dkey ~current:true "%a this is representative"
      Printer.pp_varinfo new_varinfo;
    (* We are done. *)
    None
  (* The representative is in the same file.*)
  | Some (rep_varinfo, rep_file_index) when rep_file_index = new_file_index ->
    Kernel.debug ~dkey ~current:true "%a representative in the same file"
      Printer.pp_varinfo new_varinfo;
    (* Sanity check: if the representative is in the same file, the
       representative varinfo is the same varinfo that the current one. *)
    assert (new_varinfo == rep_varinfo);
    (* Mark it as done already. *)
    rep_varinfo.vreferenced <- true;
    (* We are done. *)
    None
  | Some (rep_varinfo, rep_file_index) -> (* Reuse the representative. *)
    Kernel.debug ~dkey ~current:true "%a representative in a different file"
      Printer.pp_varinfo new_varinfo;
    (* Sanity check: if the representative is NOT in the same file, the
       representative varinfo is different than the current one. *)
    assert (rep_file_index != new_file_index);
    assert (new_varinfo != rep_varinfo);
    (* Mark it as done already. *)
    (* QUESTION: Why don't we mark the [new_varinfo] as done? *)
    rep_varinfo.vreferenced <- true;
    (* [vaddrof] is [true] if the address of the variable is taken. *)
    rep_varinfo.vaddrof <- new_varinfo.vaddrof || rep_varinfo.vaddrof;
    (* [vdefined] is [true] if the variable or function is defined in the
       file. *)
    rep_varinfo.vdefined <- new_varinfo.vdefined || rep_varinfo.vdefined;
    (* The original variable and the retrieved representative should have
       both the same ghost or non-ghost status. *)
    if Extlib.xor rep_varinfo.vghost new_varinfo.vghost then
      Kernel.abort
        "Cannot merge: Global %a has both ghost and non-ghost status"
        Printer.pp_varinfo rep_varinfo;
    (* If the varinfo has a logic binding, add one to the representative
       if needed. *)
    begin
      match rep_varinfo.vlogic_var_assoc, new_varinfo.vlogic_var_assoc with
      | None, Some _ -> ignore (Cil.cvar_to_lvar rep_varinfo)
      | _ -> ()
    end;
    (* We are done. *)
    Some rep_varinfo

(* If [Some rep_varinfo] returned then always [new_varinfo != rep_varinfo]. *)
let process_varinfo file_index new_varinfo new_loc =
  let dkey = dkey_process_varinfo in
  if new_varinfo.vreferenced
  then begin (* The variable was already processed, we are done. *)
    Kernel.debug ~dkey ~current:true "%a already referenced"
      Printer.pp_varinfo new_varinfo;
    None
  end else
    (* Rename it if it is static, find the representative if it is not. *)
    match treat_like_static new_varinfo with
    | true ->
      (* If the variable is an aggressive merging candidate: leave it be. *)
      if not (is_aggressive_merging_candidate new_varinfo)
      then begin
        (* If the variable is static-like and not an aggressive merging
           candidate: rename it. *)
        Kernel.debug ~dkey ~current:true "%a rename static-like"
          Printer.pp_varinfo new_varinfo;
        process_staticlike_varinfo new_varinfo new_loc
      end;
      None
    | false -> (* The variable is not static; find the representative. *)
      process_nonstaticlike_varinfo file_index new_varinfo


(* dkeys for debug printing in two most difficult match cases. *)
let dkey_merging_gfundecl = Kernel.register_category "mergecil:merging_gfundecl"
let dkey_merging_gfun = Kernel.register_category "mergecil:merging_gfun"

(* [set_formal_args_names fundec new_formal_args_names] sets the names of
   the formal arguments of function [fundec] to those provided in the list
   of strings [new_formal_args_names] and updates the function's type
   accordingly. If the number of function's arguments and number of
   provided strings is not equal raises [Invalid_argument]. *)
let set_formal_args_names fundec new_formal_args_names =
  (* Get the varinfos corresponding to functions's formal arguments. *)
  let formal_args_varinfos = fundec.sformals in
  (* Sanity check: number of arguments and provided names is the same. *)
  if List.length formal_args_varinfos <> List.length new_formal_args_names
  then raise (Invalid_argument "set_formal_args_names");
  (* Set the name of each formal argument. *)
  List.iter2
    (fun formal_arg_varinfo new_name ->
       if new_name <> ""
       then formal_arg_varinfo.vname <- new_name)
    formal_args_varinfos new_formal_args_names;
  (* Reflect the new arguments in the function's type. *)
  Cil.setFormals fundec fundec.sformals

(** This restores the formal arguments' names of a current function
    declaration to these of the representant. *)
let restore_formal_args_names file_index rep_varinfo new_fundec =
  (* Sanity check: the representant should be a function. *)
  assert (isFunctionType rep_varinfo.vtype);
  (* Get the names of the arguments corresponding to the representant. *)
  let rep_formal_args_names_opt : string list option =
    let original_name : string =
      Pass2Env.OriginalVarNames.get_if_remembered rep_varinfo.vname
    in
    BothPassesEnv.FormalArgsNames.get (file_index, original_name)
  in
  assert (rep_formal_args_names_opt <> None);
  (* Now restore the parameter names (and reflect them in type). *)
  try Extlib.may (set_formal_args_names new_fundec) rep_formal_args_names_opt
  with Invalid_argument msg when msg = "set_formal_args_names" ->
    Kernel.fatal ~current:true
      "After merging the function has different number of arguments"

let treat_one_global_pass_2 file_index in_file_index global =
  Kernel.debug ~dkey
    "---------------------- treat_one_global_pass_2 ----------------------@.\
     ----------------[ file index = %a, in-file index = %a ]-------------@.\
     @[%a@]\
     ---------------------------------------------------------------------"
    FileIndex.pretty file_index
    InFileIndex.pretty in_file_index
    Printer.pp_global global;

  (* Debug printing. *)
  Kernel.debug ~dkey "%t" Pass2Env.OriginalVarNames.pretty;

  (* Specialize the two important functions used quite a lot here. *)
  let process_varinfo = process_varinfo file_index in
  let rename_to_representative_inside_global =
    let renameVisitor = renameToRepresentativeVisitor file_index in
    visitCilGlobal renameVisitor
  in

  (* Update the reference to the current location if appropriate. *)
  Extlib.may Cil.CurrentLoc.set (loc_of_global global);

  match global with
  | GVarDecl (new_varinfo, new_loc) ->
    begin
      match process_varinfo new_varinfo new_loc with
      | None when not (Pass2Env.VarsDeclared.mem new_varinfo) ->
        (* Remember that it was declared. *)
        Pass2Env.VarsDeclared.add new_varinfo;
        (* Go in there and rename everything as needed. *)
        rename_to_representative_inside_global global
        (* Emit the declaration. *)
      | None -> assert (Pass2Env.VarsDeclared.mem new_varinfo); []
      | Some rep_varinfo -> assert (new_varinfo != rep_varinfo); []
    end

  | GVar (new_varinfo, new_initinfo, new_loc) ->
    begin
      let rep_varinfo =
        match process_varinfo new_varinfo new_loc with
        | Some rep_varinfo -> assert (new_varinfo != rep_varinfo); rep_varinfo
        | None -> new_varinfo
      in

      (* Remember that it was declared. *)
      Pass2Env.VarsDeclared.add new_varinfo;

      (* We must keep this definition even if we reuse this varinfo,
         because maybe the previous one was a declaration. *)
      let var_definition_details = (rep_varinfo, new_initinfo.init, new_loc) in

      let emit_var_definition () =
        let global' = GVar (rep_varinfo, new_initinfo, new_loc) in
        rename_to_representative_inside_global global'
      in

      match Pass2Env.VarDefinitions.get rep_varinfo with
      (* No previous definition. EMIT! *)
      | None ->
        Pass2Env.VarDefinitions.add rep_varinfo var_definition_details;
        emit_var_definition ()

      (* Previously defined, same initializer. DO NOT EMIT! *)
      | Some (_, prev_init_opt, _)
        when Equal.init_opts prev_init_opt new_initinfo.init
          || new_initinfo.init = None -> []

      (* The previous occurence was only a tentative definition. Now we have
         a real one. Set the correct value in the table, and remeber that we
         still need to change the previous one into a [GVarDecl]. EMIT! *)
      | Some (_, None, _) ->
        Pass2Env.VarDefinitions.replace rep_varinfo var_definition_details;
        Pass2Env.EncounteredTentativeDefinitionFlag.set ();
        emit_var_definition ()

      (* Previously defined, both have initializers. DO NOT EMIT! *)
      | Some (_, _, prev_loc) ->
        Kernel.error ~current:true
          "global var %a at %a has different initializer than %a"
          Printer.pp_varinfo rep_varinfo
          Printer.pp_location new_loc
          Printer.pp_location prev_loc;
        []
    end

  (* Special case: leave the aggressive merging candidates be. *)
  | GFunDecl (_, new_varinfo, _new_loc)
  | GFun ({ svar = new_varinfo; _ }, _new_loc)
    when is_aggressive_merging_candidate new_varinfo ->
    Pass2Env.EncounteredAggressiveMergingCandidateFlag.set ();
    rename_to_representative_inside_global global

  | GFunDecl (new_funspec, new_varinfo, new_loc) ->
    begin
      let dkey = dkey_merging_gfundecl in

      let processed_varinfo = process_varinfo new_varinfo new_loc in
      match processed_varinfo with

      | Some rep_varinfo ->
        begin
          assert (new_varinfo != rep_varinfo);

          Kernel.debug ~dkey ~current:true
            "Merging GFunDecl (%a at %a): we have a representative"
            Printer.pp_varinfo new_varinfo
            Printer.pp_location new_loc;

          (* Go in there and rename everything as needed. *)
          ignore (rename_to_representative_inside_global global);

          let was_already_declared =
            Pass2Env.VarsDeclared.mem rep_varinfo
          in
          match was_already_declared with
          (* Inconsistent case! If the current varinfo is different than the
             reference varinfo, then this must have been already declared. *)
          | false -> assert false

          (* The reference varinfo and the current one are different. *)
          | true ->
            begin
              (* Drop the declaration, keep the spec. *)
              MergeFunspec.merge_funspec rep_varinfo new_varinfo new_funspec;
              match
                Extlib.find_or_none Cil.getFormalsDecl rep_varinfo,
                Extlib.find_or_none Cil.getFormalsDecl new_varinfo
              with
              (* The reference varinfo already has formals,
                 everything is renamed accordingly. *)
              | Some _, _ -> ()
              (* Otherwise, if we have formals here, we register them with
                 the reference varinfo. *)
              | None, Some new_varinfo_formals ->
                Cil.unsafeSetFormalsDecl rep_varinfo new_varinfo_formals;
                ()
              (* Neither declaration has formals. Do nothing. *)
              | None, None -> ()
            end;
            []
        end

      | None ->
        begin

          Kernel.debug ~dkey ~current:true
            "Merging GFunDecl (%a at %a): there is no representative"
            Printer.pp_varinfo new_varinfo
            Printer.pp_location new_loc;

          (* Go in there and rename everything as needed. *)
          let globals' = rename_to_representative_inside_global global in

          let was_already_declared =
            Pass2Env.VarsDeclared.mem new_varinfo
          in
          match was_already_declared with
          (* They are the same and this was already emitted. *)
          | true ->
            Kernel.debug ~dkey ~current:true
              "Merging GFunDecl (%a at %a): was already declared!"
              Printer.pp_varinfo new_varinfo
              Printer.pp_location new_loc;

            MergeFunspec.merge_funspec new_varinfo new_varinfo new_funspec;
            []

          (* They are the same and this was NOT emitted yet. *)
          | false ->
            Kernel.debug ~dkey ~current:true
              "Merging GFunDecl (%a at %a): was not declared yet!"
              Printer.pp_varinfo new_varinfo
              Printer.pp_location new_loc;

            (* Remember that it was declared. *)
            Pass2Env.VarsDeclared.add new_varinfo;
            (* Emit the declaration. *)
            globals'
        end
    end

  | GFun (new_fundec, new_loc) ->
    begin
      let dkey = dkey_merging_gfun in

      let new_varinfo = new_fundec.svar in
      let processed_varinfo = process_varinfo new_varinfo new_loc in
      let is_static_like = treat_like_static new_fundec.svar in

      match
        processed_varinfo,
        Pass2Env.VarsDeclared.mem new_varinfo,
        Pass2Env.FunDefinitions.mem new_fundec,
        should_discard_inline_body new_varinfo
      with

      (* -------------- STATIC (OR WE ARE NOT MERGING GLOBALS) -------------- *)
      | _ when is_static_like ->

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): static"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* SANITY CHECK*)
        assert (processed_varinfo = None);
        assert (not (Pass2Env.FunDefinitions.mem new_fundec));
        assert (not (should_discard_inline_body new_varinfo));
        (* NOTE: It was proven by hand that if [treat_like_static varinfo]
                 is true then [should_discard_inline_body varinfo] must be
                 false. *)

        rename_to_representative_inside_global global

      | Some _rep_varinfo, false, _, _ -> assert false
      | Some rep_varinfo, _,  _, _
        when new_varinfo == rep_varinfo -> assert false

      (* --------------------------- ALREADY DEFINED ---------------------------
         YES there is a previous definition;
         MAYBE there is also a previous declaration, it does not matter.

         SO: The old definition is cool and dandy, drop the new one, just
             see for what exact reason. *)
      | Some rep_varinfo, true, true, true ->

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           already defined, dropping the body"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* SANITY CHECK *)
        (* Two definitions of the same function in the same file are invalid
           at the file level, so they should have been rejected before,
           already in [Cabs2cil]. *)
        assert (new_varinfo != rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem rep_varinfo);

        (* Warn about discarding the body if necessary. *)
        warn_discarding_inline_body new_varinfo new_loc;

        (* Treat exactly as if it was a declaration in a corresponding case. *)
        let new_funspec = new_fundec.sspec in
        begin
          (* Drop the declaration, keep the spec. *)
          MergeFunspec.merge_funspec rep_varinfo new_varinfo new_funspec;
          match
            Extlib.find_or_none Cil.getFormalsDecl rep_varinfo,
            Extlib.find_or_none Cil.getFormalsDecl new_varinfo
          with
          (* The reference varinfo already has formals,
             everything is renamed accordingly. *)
          | Some _, _ -> ()
          (* Otherwise, if we have formals here, we register them with
             the reference varinfo. *)
          | None, Some new_varinfo_formals ->
            Cil.unsafeSetFormalsDecl rep_varinfo new_varinfo_formals;
            ()
          (* Neither declaration has formals. Do nothing. *)
          | None, None -> ()
        end;
        []

      | Some rep_varinfo, true, true, false ->

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           already defined"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* SANITY CHECK *)
        (* Two definitions of the same function in the same file are invalid
           at the file level, so they should have been rejected before,
           already in [Cabs2cil]. *)
        assert (new_varinfo != rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem rep_varinfo);

        let new_checksum = function_checksum new_fundec in
        let (_old_fundec, old_loc, old_checksum) =
          Extlib.the (Pass2Env.FunDefinitions.get new_fundec)
        in

        begin
          match new_checksum = old_checksum with
          | true ->
            Kernel.warning ~current:true
              "dropping duplicate def'n of func %a at %a \
               in favor of that at %a"
              Printer.pp_varinfo new_fundec.svar
              Printer.pp_location new_loc
              Printer.pp_location old_loc
          | false ->
            (* the checksums differ, so print a warning but keep the
               older one to avoid a link error later.  I think this is
               a reasonable approximation of what ld does. *)
            Kernel.warning ~current:true
              "def'n of func %a at %a (sum %d) conflicts with the one \
               at %a (sum %d); keeping the one at %a."
              Printer.pp_varinfo new_fundec.svar
              Printer.pp_location new_loc
              new_checksum
              Printer.pp_location old_loc
              old_checksum Printer.pp_location old_loc
        end;
        []

      (* ------------------ NOT DEFINED YET, ALREADY DECLARED ------------------
         NO there is no previous definition;
         YES there is a previous declaration.

         SO: Both the old declaration and the new definition can stay,
             pas d'jaloux. *)
      | Some rep_varinfo, true, false, true ->

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           not defined yet, already declared (v1), dropping the body"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* SANITY CHECK *)
        assert (new_varinfo != rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem new_varinfo);

        (* Warn about discarding the body if necessary. *)
        warn_discarding_inline_body new_varinfo new_loc;

        (* Make the declaration [extern], as it is the external definition
           that should be used in this case. *)
        rep_varinfo.vstorage <- Extern;

        (* Treat exactly as if it was a declaration in a corresponding case. *)
        let new_funspec = new_fundec.sspec in
        begin
          (* Drop the declaration, keep the spec. *)
          MergeFunspec.merge_funspec rep_varinfo new_varinfo new_funspec;
          match
            Extlib.find_or_none Cil.getFormalsDecl rep_varinfo,
            Extlib.find_or_none Cil.getFormalsDecl new_varinfo
          with
          (* The reference varinfo already has formals,
             everything is renamed accordingly. *)
          | Some _, _ -> ()
          (* Otherwise, if we have formals here, we register them with
             the reference varinfo. *)
          | None, Some new_varinfo_formals ->
            Cil.unsafeSetFormalsDecl rep_varinfo new_varinfo_formals
          (* Neither declaration has formals. Do nothing. *)
          | None, None -> ()
        end;
        []

      | Some rep_varinfo, true, false, false ->

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           not defined yet, already declared (v1)"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* SANITY CHECK *)
        assert (new_varinfo != rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem new_varinfo);

        MergeFunspec.add_formals_renaming rep_varinfo new_fundec;

        if new_fundec.svar.vinline && new_fundec.svar.vstorage = Extern
        then rep_varinfo.vstorage <- Extern;

        new_fundec.svar <- rep_varinfo;

        (* Go in there and rename everything as needed. *)
        let globals' = rename_to_representative_inside_global global in

        (* Now restore the parameter names. *)
        restore_formal_args_names file_index rep_varinfo new_fundec;

        (* There was no previous definition. *)
        let current_checksum = (function_checksum new_fundec) in
        Pass2Env.FunDefinitions.add new_fundec new_loc current_checksum;
        (* TODO: Remember the declaration too? *)

        globals'


      | None, _, true, _ -> assert false

      (* ------------------ NOT DEFINED YET, ALREADY DECLARED ------------------
         NO there is no previous definition;
         YES there is a previous declaration.

         SO: Both the old declaration and the new definition can stay,
             pas d'jaloux. *)
      | None, true, false, true -> (* i.e. new_varinfo == rep_varinfo *)

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           not defined yet, already declared (v2), dropping the body"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* SANITY CHECK *)
        let rep_varinfo = new_varinfo in
        assert (Pass2Env.VarsDeclared.mem rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem new_varinfo);

        (* Warn about discarding the body if necessary. *)
        warn_discarding_inline_body new_varinfo new_loc;

        (* Make the declaration [extern], as it is the external definition
           that should be used in this case. *)
        rep_varinfo.vstorage <- Extern;

        (* Treat exactly as if it was a declaration in a corresponding case. *)
        let new_funspec = new_fundec.sspec in
        begin
          (* Drop the declaration, keep the spec. *)
          MergeFunspec.merge_funspec rep_varinfo new_varinfo new_funspec;
          match
            Extlib.find_or_none Cil.getFormalsDecl rep_varinfo,
            Extlib.find_or_none Cil.getFormalsDecl new_varinfo
          with
          (* The reference varinfo already has formals,
             everything is renamed accordingly. *)
          | Some _, _ -> ()
          (* Otherwise, if we have formals here, we register them with
             the reference varinfo. *)
          | None, Some new_varinfo_formals ->
            Cil.unsafeSetFormalsDecl rep_varinfo new_varinfo_formals
          (* Neither declaration has formals. Do nothing. *)
          | None, None -> ()
        end;
        []

      | None, true, false, false -> (* i.e. new_varinfo == rep_varinfo *)

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           not defined yet, already declared (v2)"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* SANITY CHECK *)
        let rep_varinfo = new_varinfo in
        assert (Pass2Env.VarsDeclared.mem rep_varinfo);
        assert (Pass2Env.VarsDeclared.mem new_varinfo);

        MergeFunspec.add_formals_renaming rep_varinfo new_fundec;

        if new_fundec.svar.vinline && new_fundec.svar.vstorage = Extern
        then rep_varinfo.vstorage <- Extern;

        new_fundec.svar <- rep_varinfo;

        (* Go in there and rename everything as needed. *)
        let globals' = rename_to_representative_inside_global global in

        (* Now restore the parameter names. *)
        restore_formal_args_names file_index rep_varinfo new_fundec;

        (* There was no previous definition. *)
        let current_checksum = (function_checksum new_fundec) in
        Pass2Env.FunDefinitions.add new_fundec new_loc current_checksum;
        (* TODO: Remember the declaration too? *)

        globals'


      (* ------------------ NOT DEFINED YET, NOT DECLARED YET ------------------
         NO there is no previous definition;
         NO there is no previous declaration.

         SO: There was nothing before so the early bird (i.e. definition)
             gets the worm (i.e. emitted). *)
      | None, false, false, true -> (* i.e. new_varinfo == rep_varinfo *)

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           not defined yet, not declared yet, dropping the body"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* 1. rename everything to representative inside the new function
              definition,
           2. push the new function definition,
           3. remember that this function is defined now. *)

        (* SANITY CHECK *)
        assert (not (Pass2Env.VarsDeclared.mem new_varinfo));

        Pass2Env.VarsDeclared.add new_varinfo;

        (* Go in there and rename everything as needed. *)
        ignore (rename_to_representative_inside_global global);

        (* Warn about discarding the body if necessary. *)
        warn_discarding_inline_body new_varinfo new_loc;

        let make_GFunDecl_of_GFun global =
          match global with
          | GFun (fundec, location) ->
            assert (should_discard_inline_body fundec.svar);
            (* Make the declaration [extern], as it is the external definition
               that should be used in this case. *)
            fundec.svar.vstorage <- Extern;
            let funspec = fundec.sspec in
            let varinfo = fundec.svar in
            GFunDecl (funspec, varinfo, location)
          | _ -> assert false
        in

        let global' = make_GFunDecl_of_GFun global in
        [global']

      | None, false, false, false -> (* i.e. new_varinfo == rep_varinfo *)

        Kernel.debug ~dkey ~current:true
          "Merging GFun (%a at %a): \
           not defined yet, not declared yet"
          Printer.pp_varinfo new_fundec.svar
          Printer.pp_location new_loc;

        (* 1. rename everything to representative inside the new function
              definition,
           2. push the new function definition,
           3. remember that this function is defined now. *)

        (* SANITY CHECK *)
        assert (not (Pass2Env.VarsDeclared.mem new_varinfo));

        Pass2Env.VarsDeclared.add new_varinfo;

        (* Go in there and rename everything as needed. *)
        let globals' = rename_to_representative_inside_global global in

        (* There was no previous definition. *)
        let new_checksum = function_checksum new_fundec in
        Pass2Env.FunDefinitions.add new_fundec new_loc new_checksum;

        globals'

    end


  | GCompTag (compinfo, _loc)
    when not compinfo.creferenced
      (* We are not the representative. Drop this declaration because we'll not
         be using it. *)
      && PlainMerging.has_no_replacement composites
           (file_index, compinfo.cname) ->
    (* This is a new declaration, we must rename it and keep the definition. *)
    begin
      (* Make sure this is root. *)
      try
        let compinfo_node : (string, compinfo) Node.t =
          PlainMerging.Eq_table.find
            (eq_table composites) (file_index, compinfo.cname)
        in
        if not (Node.is_representative compinfo_node) then
          Kernel.fatal
            "Setting creferenced for struct %s which is not root!"
            compinfo.cname
      with Not_found ->
        Kernel.fatal
          "Setting creferenced for struct %s which is not in the \
           composites!"
          compinfo.cname
    end;
    let new_name = Pass2Env.CompAlpha.new_name compinfo.cname in
    compinfo.cname <- new_name;
    compinfo.creferenced <- true;
    (* Now we should visit the fields as well. *)
    Pass2Env.CompsDeclared.add compinfo; (* Remember that we emitted it. *)
    rename_to_representative_inside_global global

  | GCompTag _ -> []

  | GEnumTag (enuminfo, _loc)
    when not enuminfo.ereferenced
      (* Drop this since we are reusing it from before. *)
      && EnumMerging.has_no_replacement enums (file_index, enuminfo) ->
    (* We must rename it. *)
    let new_name = Pass2Env.EnumAlpha.new_name enuminfo.ename in
    enuminfo.ename <- new_name;
    enuminfo.ereferenced <- true;
    (* And we must rename the items to using the same name space
       as the variables. *)
    List.iter
      (fun enumitem ->
         let new_name =
           let loc = enumitem.eiloc in
           BothPassesEnv.VarOrTypeAlpha.new_name enumitem.einame ~loc
         in
         enumitem.einame <- new_name)
      enuminfo.eitems;
    rename_to_representative_inside_global global

  | GEnumTag _ -> []

  | GCompTagDecl (compinfo, _loc)
    (* This is here just to introduce an undefined structure. But maybe
       the structure was defined already. *)
    when not (Pass2Env.CompsDeclared.mem compinfo) ->
    (* It was not declared yet. *)
    Pass2Env.CompsDeclared.add compinfo;
    (* Keep it as a declaration. *)
    [global]

  | GCompTagDecl (_compinfo, _loc) -> []

  | GEnumTagDecl (_enuminfo, _loc) ->
    (* Keep it as a declaration. *)
    [global]

  | GType (typeinfo, _loc)
    when not typeinfo.treferenced
      (* Drop this since we are reusing it from before. *)
      && PlainMerging.has_no_replacement types (file_index, typeinfo.tname) ->
    (* We must rename it and keep it. *)
    let new_name = BothPassesEnv.VarOrTypeAlpha.new_name typeinfo.tname in
    typeinfo.tname <- new_name;
    typeinfo.treferenced <- true;
    rename_to_representative_inside_global global

  | GType _ -> []

  | GAnnot (global_annot, _loc) ->
    global_annot_pass_2 file_index global global_annot

  | _ -> rename_to_representative_inside_global global


(* Replace tentative definition by a declaration when we found a real
   definition somewhere else. *)
let replace_tentative_def_by_decl merged_file =
  (* NOTE: This actually could be performed either way, without taking into
     account if the flag [encountered_tentative_definition] was set, as if this
     flag was not set then no tentative definitions were emitted, so nothing
     would actually happen here, as we only affect tentative definitions here.
  *)
  match Pass2Env.EncounteredTentativeDefinitionFlag.get () with
  | false -> merged_file
  | true ->
    (* For each variable definition without an initializer, if there exists
       a definition of the same variable which has an initializer, then turn
       this definition (i.e. the one without an initializer) into a declaration.
       Leave all the other definitions as they were. *)
    let replace_tentative_def_by_decl' _file_index _in_file_index global =
      match global with
      | GVar(varinfo, {init = None}, loc) ->
        (match Pass2Env.VarDefinitions.get varinfo with
         | Some (_, Some _, _) -> [GVarDecl(varinfo, loc)]
         | Some (_, None, _) -> [global]
         | None -> [global])
      | _ -> [global]
    in
    MergedFile.map replace_tentative_def_by_decl' merged_file


(* ---===[ AUXILIARY FUNCTIONS FOR MERGE ]===--- *)

(* USED in: [merge] *)
(* USES: [MergeFunspec.get_funspecs_to_merge] and
         [MergeFunspec.get_formals_renaming]. *)
let global_merge_funspec : global -> unit =

  let merge_funspecs varinfo orig_funspec funspecs_to_merge =
    let merging_verbosity = MergeVerbosely varinfo in
    (* NOTE: I guess this is to *copy* [orig_funspec] so that we can compare to
       it even though it is modified during merging. *)
    let initial_funspec =
      (* CHECK: What is the purpose of this *copy*? *)
      { orig_funspec with spec_behavior = orig_funspec.spec_behavior }
    in
    let merge_one_funspec funspec_to_merge =
      if not (Logic_utils.is_same_spec initial_funspec funspec_to_merge)
      then
        (* NOTE: warnings "found two X for Y" most probably come from here. *)
        Logic_utils.merge_funspec
          merging_verbosity orig_funspec funspec_to_merge
    in
    List.iter merge_one_funspec funspecs_to_merge
  in

  let rename_formals_in_funspec varinfo funspec =
    match MergeFunspec.get_formals_renaming varinfo with
    | Some alpha -> ignore (visitCilFunspec alpha funspec)
    | None -> ()
  in

  let decl_or_def_merge_funspec varinfo funspec loc =
    match MergeFunspec.get_funspecs_to_merge varinfo with
    | Some funspecs_to_merge ->
      Kernel.debug ~dkey "-> Merging with %a" Printer.pp_funspec funspec;
      (* CHECK: What can this comment mean? *)
      (* The registered funspecs might also need renaming up to
         definition's formals instead of declaration's ones. *)
      List.iter
        (fun funspec_to_merge ->
           Kernel.debug ~dkey
             "-> Found spec to merge %a"
             Printer.pp_funspec funspec_to_merge;
           rename_formals_in_funspec varinfo funspec_to_merge;
           Kernel.debug ~dkey
             "-> After renaming:@\n%a"
             Printer.pp_funspec funspec_to_merge)
        funspecs_to_merge;
      Kernel.debug ~dkey "-> Renaming %a" Printer.pp_funspec funspec;
      Cil.CurrentLoc.set loc;
      rename_formals_in_funspec varinfo funspec;
      Kernel.debug ~dkey "-> Renamed to %a" Printer.pp_funspec funspec;
      Cil.CurrentLoc.set loc;
      merge_funspecs varinfo funspec funspecs_to_merge;
      Kernel.debug ~dkey "-> Merged into %a" Printer.pp_funspec funspec;
    | None ->
      Kernel.debug ~dkey "-> No funspecs_to_merge";
      rename_formals_in_funspec varinfo funspec;
      Kernel.debug ~dkey "-> Renamed to %a" Printer.pp_funspec funspec;
  in

  function
  | GFun ({ sspec = funspec; svar = varinfo; _ }, loc) ->
    Kernel.debug ~dkey
      "Merging global function definition %a"
      Printer.pp_varinfo varinfo;
    decl_or_def_merge_funspec varinfo funspec loc

  | GFunDecl (funspec, varinfo, loc) ->
    Kernel.debug ~dkey
      "Merging global function declaration %a"
      Printer.pp_varinfo varinfo;
    decl_or_def_merge_funspec varinfo funspec loc

  | _ -> ()


(* USED: in [merge]. *)
(* DOES NOT USE ANY GLOBAL TABLES! *)
let move_spec : global list -> global list =

  let get_C_and_logic_decls_of_global global : Varinfo.Set.t * Logic_var.Set.t =
    let c_vars = ref Varinfo.Set.empty in
    let logic_vars = ref Logic_var.Set.empty in
    let get_vars_declarations_visitor =
      object(self)
        inherit nopCilVisitor
        method! vvdec varinfo =
          c_vars := Varinfo.Set.add varinfo !c_vars;
          DoChildren
        method! vlogic_var_decl logic_var =
          logic_vars := Logic_var.Set.add logic_var !logic_vars;
          SkipChildren
        method! vspec _ =
          SkipChildren
        method! vfunc fundec =
          ignore (self#vvdec fundec.svar);
          Extlib.may
            (fun logic_var -> ignore (self#vlogic_var_decl logic_var))
            fundec.svar.vlogic_var_assoc;
          SkipChildren
      end
    in
    ignore (visitCilGlobal get_vars_declarations_visitor global);
    !c_vars, !logic_vars
  in

  let get_used_logic_vars_of_global global : Logic_var.Set.t =
    let used_logic_vars = ref Logic_var.Set.empty in
    let local_logic_vars = ref Logic_var.Set.empty in
    let get_used_logic_vars_visitor : nopCilVisitor =
      object
        inherit nopCilVisitor
        method! vlogic_var_decl logic_var =
          (* Add to local logic vars. *)
          local_logic_vars :=
            Logic_var.Set.add logic_var !local_logic_vars;
          SkipChildren
        method! vlogic_var_use logic_var =
          if not (Logic_var.Set.mem logic_var !local_logic_vars)
          && not (Logic_env.is_builtin_logic_function logic_var.lv_name)
          && not (logic_var.lv_name = "\\exit_status")
          then
            used_logic_vars := Logic_var.Set.add logic_var !used_logic_vars;
          SkipChildren
      end
    in
    ignore (visitCilGlobal get_used_logic_vars_visitor global);
    !used_logic_vars
  in

  let pp_missing fmt to_declare =
    let pp_one_binding fmt logic_vars =
      let pp_logic_var fmt logic_var =
        Format.fprintf fmt "%a;@ " Printer.pp_logic_var logic_var
      in
      Logic_var.Set.iter (pp_logic_var fmt) logic_vars
    in
    let pp_entry fmt varinfo (_, logic_vars) =
      Format.fprintf fmt "@[%a:@[%a@]@]@\n"
        Printer.pp_varinfo varinfo
        pp_one_binding logic_vars
    in
    Varinfo.Map.iter (pp_entry fmt) to_declare
  in

  (* [update_vars_to_declare] takes as arguments:
     - a set of [known_logic_vars]
     - and the map [to_declare], which associates to each [varinfo] a pair:
       - a single [global]
       - and a set of [logic vars] which are missing.
     Now, for each [varinfo]:
     1. we remove the known logic variables from the missing logic variables
        corresponding to this [varinfo],
     2. if there are no missing variables left then we put it in the
        [can_declare] accumulator,
     3. if there are still some missing variables left then we update the
        association in the [to_declare] map in the accumulator. *)
  let update_logic_vars_to_declare
      (known_logic_vars : Logic_var.Set.t)
      (to_declare : (global * Logic_var.Set.t) Varinfo.Map.t) =
    let aux
        (* The binding = varinfo -> (global, missing_logic_vars) *)
        varinfo (* the map's key is a varinfo *)
        (global, missing_logic_vars) (* the map's value is a pair =
                                          a global and a logic vars set *)
        (can_declare_acc, to_declare_acc) (* the accumulator *) =
      (* Remove the known vars from the missing vars. *)
      let updated_missing_logic_vars =
        Logic_var.Set.diff missing_logic_vars known_logic_vars
      in
      (* Check if any missing vars remain. *)
      match Logic_var.Set.is_empty updated_missing_logic_vars with
      | true -> (* There are no more missing vars! *)
        (* This global can be now declared, add it to the [can_declare] list. *)
        let can_declare' = global :: can_declare_acc in
        can_declare', to_declare_acc
      | false -> (* There are still some missing vars! *)
        (* This global cannot be declared yet, put the updated binding
           [global -> missing_logic_vars] in the [to_declare] map. *)
        let to_declare' =
          Varinfo.Map.add
            varinfo (global, updated_missing_logic_vars) to_declare_acc
        in
        (* Effect: either the map is the same or some elements have disappeared
           from the [missing_logic_vars] set corresponding to the given
           varinfo. *)
        can_declare_acc, to_declare'
    in
    let empty_acc = ([], Varinfo.Map.empty) in
    Varinfo.Map.fold aux to_declare empty_acc

  in

  let move_spec globals =
    let aux
        (globals_acc, known_C_vars, known_logic_vars, logic_vars_to_declare)
        global =
      let known_logic_vars', known_C_vars' =
        let c_decls_of_global, logic_decls_of_global =
          get_C_and_logic_decls_of_global global
        in
        Logic_var.Set.union logic_decls_of_global known_logic_vars,
        Varinfo.Set.union c_decls_of_global known_C_vars
      in
      let can_declare, logic_vars_to_declare =
        update_logic_vars_to_declare known_logic_vars' logic_vars_to_declare
      in
      let also_can_declare, logic_vars_to_declare =
        let missing_logic_vars =
          let used_logic_vars = get_used_logic_vars_of_global global in
          Logic_var.Set.diff used_logic_vars known_logic_vars'
        in
        let nothing_missing = Logic_var.Set.is_empty missing_logic_vars in
        match nothing_missing, global with
        | false, GFunDecl (_funspec, varinfo, loc) ->
          (* There are still some missing vars! *)
          let also_can_declare =
            let global' = GFunDecl (Cil.empty_funspec (), varinfo, loc) in
            global' :: globals_acc
          in
          let logic_vars_to_declare =
            Varinfo.Map.add
              varinfo (global, missing_logic_vars)
              logic_vars_to_declare
          in
          also_can_declare, logic_vars_to_declare
        | false, GFun ({ svar = varinfo; _ } as _fundec, loc) ->
          (* There are still some missing vars! *)
          let also_can_declare =
            (* BUG? This was [known_C_vars] (i.e. the original function
               argument), not [known_C_vars']! *)
            if Varinfo.Set.mem varinfo known_C_vars'
            then
              globals_acc
            else
              let global' = GFunDecl (Cil.empty_funspec (), varinfo, loc) in
              global' :: globals_acc
          in
          let logic_vars_to_declare =
            Varinfo.Map.add
              varinfo (global, missing_logic_vars)
              logic_vars_to_declare
          in
          also_can_declare, logic_vars_to_declare
        (* Either no more vars are missing or this is not a function declaration
           nor definition. *)
        | _ -> global :: globals_acc, logic_vars_to_declare
      in
      (can_declare @ also_can_declare,
       known_C_vars', known_logic_vars',
       logic_vars_to_declare)
    in
    let (globals, _, _, to_declare) =
      List.fold_left aux
        ([], Varinfo.Set.empty, Logic_var.Set.empty, Varinfo.Map.empty)
        globals
    in
    assert
      (Kernel.verify (Varinfo.Map.is_empty to_declare)
         "Some globals contain dangling references after link:@\n%a"
         pp_missing to_declare);
    List.rev globals
  in

  move_spec


(** In the merging process, when handling function declarations and definitions,
    we sometimes register the function formal arguments into the FormalsDecl
    table (using the [Cil.unsafeSetFormalsDecl] function). Some of these
    function declarations / definitions may not actually end up in the result
    file. As this depends on declarations / definitions that appear later, we
    cannot know in advance (before everything has been merged) if a given
    function will end up in the AST or not.
    This may obviously create an inconsistency, as in the end the FormalsDecl
    table may possibly contain functions that are not in the AST. Thus at the
    end of merging we reestablish the consistency by removing all the functions
    whose declarations / definitions are not in the result file from the
    FormalsDecl table. *)
let reestablish_FormalsDecl_consistency result_file =
  (* Check if the given varinfo corresponds to any function declared / defined
     in the result file. *)
  let is_fun_varinfo_declared =
    (* For efficiency reasons first we put all the varinfos correcponding to
       declared / defined functions into a data structure in order to perform
       lookups faster. *)
    let am_I_true_to_functional_programming_and_all_that_is_holy = true in
    match am_I_true_to_functional_programming_and_all_that_is_holy with
    | true ->
      (* A solution using a beautiful functional set. *)
      let all_declared_fun_varinfos =
        List.fold_left
          (fun varinfos -> function
             | GFunDecl (_, varinfo, _)
             | GFun ({ svar = varinfo; _ }, _) -> Varinfo.Set.add varinfo varinfos
             | _ -> varinfos)
          Varinfo.Set.empty
          result_file.globals
      in
      fun varinfo -> Varinfo.Set.mem varinfo all_declared_fun_varinfos
    | false ->
      (* Another solution using a horrendous imperative hashtable. *)
      let all_declared_fun_varinfos = Varinfo.Hashtbl.create 137 in
      List.iter
        (function
          | GFunDecl (_, varinfo, _)
          | GFun ({ svar = varinfo; _ }, _) ->
            Varinfo.Hashtbl.add all_declared_fun_varinfos varinfo ()
          | _ -> ())
        result_file.globals;
      fun varinfo -> Varinfo.Hashtbl.mem all_declared_fun_varinfos varinfo
  in
  (* Each function whose formals have been registered at some point in the
     FormalsDecl table, but in the end neither its declaration nor definition
     has found its way to the result file, should be removed from the
     FormalsDecl table in order to reestablish consistency. *)
  Cil.iterFormalsDecl
    (fun varinfo _formal_arguments ->
       if not (is_fun_varinfo_declared varinfo)
       then Cil.removeFormalsDecl varinfo)

(* Clear the module's environment. *)
let clear_environment ?(all=true) () =

  (* Environment *)
  BothPassesEnv.clear ();
  Pass1Env.clear ();
  (* NOTE: Why [anon_enums_alpha] is not cleared ??? *)

  (* Eq tables *)
  if all then PlainMerging.clear_tables vars;
  PlainMerging.clear_tables composites;
  EnumMerging.clear_tables enums;
  PlainMerging.clear_tables types;
  PlainMerging.clear_tables aggressive_merging;
  LogicFunctionMerging.clear_tables logic_funs;
  PlainMerging.clear_tables logic_types;
  PlainMerging.clear_tables logic_ctors;
  PlainMerging.clear_tables logic_axiomatics;
  PlainMerging.clear_tables logic_lemmas;
  VolatileMerging.clear_tables volatile;
  ModelMerging.clear_tables model;

  (* Environment *)
  BothPassesEnv.clear ();

  if all then Logic_env.prepare_tables ()


(* dkeys to use in [merge] *)
let dkey_merge_after_pass_1 =
  Kernel.register_category "mergecil:after_pass_1"
let dkey_merge_after_pass_2 =
  Kernel.register_category "mergecil:after_pass_2"
let dkey_merge_after_aggressive_merging =
  Kernel.register_category "mergecil:after_aggressive_merging"


let dkey_aggressive_merging =
  Kernel.register_category "mergecil:dkey_aggressive_merging"

let pp_varinfo_with_vid fmt varinfo =
  Format.fprintf fmt "%a(%d)" Printer.pp_varinfo varinfo varinfo.vid

(* Instead of using just an option type, this is more readable. *)
type aggressive_mergability =
  | Mergability_determined of bool
  | Mergability_is_being_determined

let aggressive_merging_pass merged_file =

  let dkey = dkey_aggressive_merging in

  assert (Parameter.enable_aggressive_merging);

  let is_an_aggressive_merging_candidate varinfo =

    Kernel.debug ~dkey "%t" BothPassesEnv.AggressiveMerging.pretty;

    (* Get the candidate varinfos. *)
    let aggressive_merging_candidates =
      BothPassesEnv.AggressiveMerging.get_candidate_varinfos_set ()
    in

    (* ADDITIONAL DEBUG PRINTING *)
    Kernel.debug ~dkey "<- aggressive_merging_candidates ->";
    BothPassesEnv.AggressiveMerging.iter_on_candidate_node_lists
      (fun nodes ->
         Kernel.debug ~dkey "%a"
           (Pretty_utils.pp_list ~sep:" - " pp_varinfo_with_vid)
           (List.map Node.get_data nodes));

    Varinfo.Set.mem varinfo aggressive_merging_candidates
  in

  let find_aggressive_merging_candidates_used_in_global global =
    (* The order of these variables is crucial! We will want to compare each
       variable use (which is more exactly a call to another aggressive merging
       candidate) in two functions that have already been proven to have equal
       printouts.

       In fact this corresponds to a graph with labelled edges. For a pair of
       nodes we will want to follow the edges which have the same label. And the
       label on an edge is here simulated by the index of the variable in the
       produced list. *)
    let add_used_var, get_used_vars =
      let used_vars = ref [] in
      let add varinfo = used_vars := varinfo :: !used_vars in
      let get () =
        List.filter is_an_aggressive_merging_candidate (List.rev !used_vars)
      in
      add, get
    in
    let get_used_vars_visitor =
      object
        inherit nopCilVisitor
        method! vvrbl varinfo =
          add_used_var varinfo;
          DoChildren
      end
    in
    ignore (visitCilGlobal get_used_vars_visitor global);
    get_used_vars ()
  in

  let get_printout, get_candidates_called_inside =
    (* Table used to memoize printouts and used variables per function.
       We can do that safely as the variables and their names are either
       already fixed or will not change until the end of the aggressive merging
       pass. *)
    let memo_table = Hashtbl.create 111 in
    let compute varinfo =
      if not (Hashtbl.mem memo_table varinfo.vid)
      then begin
        assert (is_an_aggressive_merging_candidate varinfo);
        let file_index, global =
          BothPassesEnv.AggressiveMerging.get_candidate_data varinfo
        in
        (* Here renaming to representative inside the global is necessary in
           order to get the correct printouts (i.e. taking into account the
           non-aggressive merging which was already performed). *)
        (* NOTE that it is sure that this has not been done before, because this
           step was skipped for aggressive merging candidate functions in
           previous merging passes! *)
        (* ALSO it's exactly because of this that file index MUST be stored for
           each global, we cannot correctly call [renameToRepresentativeVisitor]
           otherwise. *)
        ignore
          (visitCilGlobal (renameToRepresentativeVisitor file_index) global);
        (* Compute the printout. *)
        let printout = make_function_printout ~with_alpha_convert:true global in
        (* Get the aggressive merging candidate functions called inside. *)
        let candidates_called =
          find_aggressive_merging_candidates_used_in_global global
        in
        (* Debug printing. *)
        Kernel.debug ~dkey ~current:true
          "@\nVARINFO = %a(%d)@\nprintout=@\n@[%a@]@\ncandidates_called=%a"
          Printer.pp_varinfo varinfo varinfo.vid
          (Pretty_utils.pp_opt Format.pp_print_string) printout
          (Pretty_utils.pp_list pp_varinfo_with_vid) candidates_called;
        (* Memoize the result. *)
        Hashtbl.add memo_table varinfo.vid (printout, candidates_called)
      end;
      Hashtbl.find memo_table varinfo.vid
    in
    (fun varinfo -> fst (compute varinfo)),
    (fun varinfo -> snd (compute varinfo))
  in

  let should_be_merged : varinfo -> varinfo -> bool =
    (* This function verifies if two varinfos (corresponding to
       aggressive merging candidate nodes) are indeed fit for merging. *)
    let mergability_results = Hashtbl.create 111 in

    let rec should_be_merged' f g =
      (* The key in the [mergability_results] hashatable is the pair of
         variables' ids. *)
      let key = (f.vid, g.vid) in
      (* If the result has already been computed: no need to do anything. *)
      if not (Hashtbl.mem mergability_results key)
      then begin
        (* First we need to temporarily mark these nodes as "work in progress".
           If we get back to them, because of a cycle in the function call
           graph, and they were not determinded as unmergable in the meantime,
           they should be temporarily considered as mergable.
           This is necessary in order to avoid infinite recursion. *)
        Hashtbl.add mergability_results key Mergability_is_being_determined;
        (* Check if:
           - the functions' addresses are not used anywhere,
           - and if their printouts match. *)
        if (f.vaddrof || g.vaddrof)
        || not (Extlib.opt_equal String.equal (get_printout f) (get_printout g))
        then begin
          (* Either:
             - the address of [f] and [g] is used somewhere,
             - or the printouts of [f] and [g] differ.
             Thus: they are not mergable. *)
          Hashtbl.replace mergability_results key (Mergability_determined false)
        end else begin
          (* Printouts are the same. *)
          (* Get all the pairs of corresponding aggressive merging candidate
             functions called inside. *)
          let f_vars = get_candidates_called_inside f in
          let g_vars = get_candidates_called_inside g in
          (* If all the pairs are mergable then this pair too. *)
          let mergability =
            try
              List.for_all2
                (fun f' g' ->
                   (* Only aggressive merging candidate functions should end up
                      here. *)
                   assert (is_an_aggressive_merging_candidate f');
                   assert (is_an_aggressive_merging_candidate g');
                   (* Follow recursively... *)
                   (match should_be_merged' f' g' with
                    | Mergability_is_being_determined -> true
                    | Mergability_determined mergability -> mergability))
                f_vars g_vars
            with Invalid_argument msg when msg = "List.for_all2" ->
              (* If variable lists for [f] and [g] have different sizes in spite
                 of matching printouts it means that there exists a call inside
                 them which seems to be the same call in both functions, but in
                 fact in one case it leads to another aggressive merging
                 candidate and in the other case to a function which is not a
                 candidate but has the same name. This should never happen! *)
              assert false
          in
          (* Store the final result. *)
          Hashtbl.replace mergability_results
            key (Mergability_determined mergability)
        end
      end;
      Hashtbl.find mergability_results key
    in

    fun f g ->
      Kernel.debug ~dkey ~current:true
        "Should be merged: %a %a ?"
        pp_varinfo_with_vid f pp_varinfo_with_vid g;
      match should_be_merged' f g with
      | Mergability_is_being_determined ->
        (* The [should_be_merged'] function always returns a determinded
           mergability, the [Mergability_is_being_determined] constructor is
           used only internally. *)
        assert false
      | Mergability_determined mergability ->
        Kernel.debug ~dkey ~current:true
          "Should be merged: %a %a = %b"
          pp_varinfo_with_vid f pp_varinfo_with_vid g mergability;
        mergability
  in

  begin
    (* This is where we treat all pairs of aggressive merging candidate nodes:
       - if a pair is indeed fit for merging, we union the two nodes,
       - otherwise nothing needs to be done.
       NOTE: Unfortunately now, without any optimization this is basically
       quadratic on the number of aggressive merging candidates. *)
    let rec treat_aggressive_merging_candidates nodes =
      match nodes with
      | [] -> ()
      | node_f :: remaining_nodes ->
        (* Get the f node representative. *)
        let node_f = Node.find node_f in
        List.iter
          (fun node_g ->
             (* Get the g node representative. *)
             let node_g = Node.find node_g in
             (* Get the associated variables. *)
             let varinfo_f = Node.get_data node_f in
             let varinfo_g = Node.get_data node_g in
             (* Do the merging if it should be done. *)
             if should_be_merged varinfo_f varinfo_g
             then ignore (Node.union PlainMerging.pp_node node_f node_g))
          remaining_nodes;
        (* Treat recursively the remaining_nodes. *)
        treat_aggressive_merging_candidates remaining_nodes
    in
    BothPassesEnv.AggressiveMerging.iter_on_candidate_node_lists
      treat_aggressive_merging_candidates
  end;

  begin
    (* Here we modify the globals corresponding to the
       aggressive merging candidate functions.

       Basically at this point:
       1. All the aggressive merging candidate functions which SHOULD be kept
          are their own representatives:
          - either because they cannot be merged with other candidates,
          - or because they have been arbitararily chosen as the one of their
            aggressive merging candidates equivalence class that should replace
            the others.
       2. All the All the aggressive merging candidate functions that SHOULD NOT
          be kept have, as a representative, an equivalent candidate function
          that will be kept; occurences of these functions elswhere (i.e. calls
          from other functions) will be replaced in the next pass to these
          representatives.

       So, for each aggressive merging candidate function:
       - if it is its owe representative, we process it as a normal static-like
         function and we keep it,
       - otherwise we remove it, as an equivalent function was / will be used in
         its place. *)
    let treat_aggressively_mergable_global file_index _in_file_index global =
      (* TODO: Somehow we end up with multiple declarations of same functions
         here. Only the ones that should actually stay in the file (so
         everything remains correct), but still... Something fishy is happenning
         here and I suspect the [PlainMerging.has_no_replacement] function or
         somehow we are comparing wrong varinfos. *)
      match global with
      | GFunDecl (_, new_varinfo, new_loc)
      | GFun ({ svar = new_varinfo; _ }, new_loc)
        when not new_varinfo.vreferenced
          && is_aggressive_merging_candidate new_varinfo ->
        (match
           PlainMerging.has_no_replacement aggressive_merging
             (file_index, new_varinfo.vname)
         with
         | true -> (* This is the representative. *)
           Kernel.debug ~dkey ~current:true
             "REPLACING %a(%d)@\n%a"
             Printer.pp_varinfo new_varinfo new_varinfo.vid
             Printer.pp_global global;
           (* Treat it like a normal static-like function. *)
           process_staticlike_varinfo new_varinfo new_loc;
           let globals' =
             visitCilGlobal (renameToRepresentativeVisitor file_index) global
           in
           Kernel.debug ~dkey ~current:true
             "REPLACED %a(%d)@\n%a"
             Printer.pp_varinfo new_varinfo new_varinfo.vid
             (Pretty_utils.pp_list Printer.pp_global) globals';
           globals'
         | false -> (* This is not the representative. *)
           (* Remove it. *)
           [])
      | _ -> [global]
    in
    MergedFile.map treat_aggressively_mergable_global merged_file
  end

let aggressive_merging_repeat_pass_2 merged_file =
  assert (Parameter.enable_aggressive_merging);
  (* We re-visit the globals because aggressive merging has been performed in
     the meantime. *)
  MergedFile.map
    (fun file_index _in_file_index global ->
       let renameInlinesAndStaticsVisitor =
         renameInlinesAndStaticsVisitor file_index
           Pass2Env.OriginalVarNames.get_if_remembered
       in
       visitCilGlobal renameInlinesAndStaticsVisitor global)
    merged_file


let merge (files : file list) (result_file_name : string) : file =

  (* === PREPARE === *)

  clear_environment ();
  Errorloc.clear_errors ();

  (* TODO: What is that exactly?*)
  List.iter
    (fun file ->
       if file.globinitcalled || file.globinit <> None then
         Kernel.warning ~current:true
           "Merging file %s has global initializer" file.fileName)
    files;

  (* Transformed the list of files to a single list of indexed globals. *)
  let merged_file = MergedFile.of_input_files files in

  (* === PASS 1 === *)

  (* Scan all files and perform three things:
     1. Clean the referenced flags.
     2. Initialize the alpha renaming tables with the names of the globals so
        that when we come in the pass 2 to generate new names, we do not run
        into conflicts.
     3. For all declarations of globals unify their types. In the process
        construct a set of equivalence classes on type names, structure and
        enumeration tags. *)
  (* NOTE: The "referenced flags" seem to serve as a temporary variable of a
           kind here in order to note what has already been treated and what has
           not been treated yet. *)
  MergedFile.iter treat_one_global_pass_1 merged_file;

  (* DEBUG PRINTING after pass 1. *)
  begin
    let dkey = dkey_merge_after_pass_1 in
    Kernel.debug ~dkey "%a" PlainMerging.pp_eq_table vars;
    Kernel.debug ~dkey "%a" PlainMerging.pp_syn_table vars;
    Kernel.debug ~dkey "%a" PlainMerging.pp_eq_table aggressive_merging;
    Kernel.debug ~dkey "%a" PlainMerging.pp_syn_table aggressive_merging;
    Kernel.debug ~dkey "%t" Pass1Env.GlobalVarsEnv.pretty;
    Kernel.debug ~dkey "%t" BothPassesEnv.FormalArgsNames.pretty;
  end;


  (* === MERGE SYNONYMS === *)

  (* Now maybe try to force synonyms to be equal. *)
  if Parameter.enable_merging_synonyms then begin
    (* Composite, enumeration, typedef. *)
    PlainMerging.merge_synonyms composites match_compinfo;
    EnumMerging.merge_synonyms enums match_enuminfo;
    PlainMerging.merge_synonyms types match_typeinfo;

    (* Logic. *)
    LogicFunctionMerging.merge_synonyms logic_funs match_logic_info;
    PlainMerging.merge_synonyms logic_types match_logic_type_info;
    PlainMerging.merge_synonyms logic_ctors match_logic_ctor;
    PlainMerging.merge_synonyms logic_axiomatics match_logic_axiomatic;
    PlainMerging.merge_synonyms logic_lemmas match_logic_lemma;

    (* Volatile, model. *)
    VolatileMerging.merge_synonyms volatile match_volatile_clause;
    ModelMerging.merge_synonyms model match_model_info;

    (* Aggressive merging. *)
    if Parameter.enable_aggressive_merging then begin
      (* Copy all the nodes from the [aggressive_merging_eq_table] to
         [vars_eq_table] as well. This is needed because [vars_eq_table] will be
         used for variable renaming. *)
      PlainMerging.Eq_table.copy (eq_table aggressive_merging) (eq_table vars);
      (* TODO: This [merge_synonyms] does not change anything... *)
      PlainMerging.merge_synonyms aggressive_merging match_aggressive_merging;

      let dkey = dkey_merge_after_aggressive_merging in
      Kernel.debug ~dkey "-- after taking aggressive merging into account --";
      Kernel.debug ~dkey "%a" PlainMerging.pp_eq_table aggressive_merging;
      Kernel.debug ~dkey "%a" PlainMerging.pp_syn_table aggressive_merging;
    end
  end;

  (* Maybe dump the graphs. *)
  if Parameter.enable_dump_graph then begin
    PlainMerging.dump_eq_graph types;
    PlainMerging.dump_eq_graph composites;
    EnumMerging.dump_eq_graph enums;
    PlainMerging.dump_eq_graph vars;
    if Parameter.enable_aggressive_merging
    then PlainMerging.dump_eq_graph aggressive_merging;
  end;


  (* === PASS 2 === *)


  (* Make the pass 2 over the files. This is when we start rewriting the file.
     We go once more through the file and we rename the globals that we keep. We
     also scan the entire body and we replace references to the representative
     types or variables. We set the referenced flags once we have replaced the
     names. *)
  Pass2Env.clear ();
  let merged_file = MergedFile.map treat_one_global_pass_2 merged_file in

  (* After PASS 2. *)
  let merged_file = replace_tentative_def_by_decl merged_file in
  (* Aggressive merging. *)
  let merged_file =
    match Pass2Env.EncounteredAggressiveMergingCandidateFlag.get () with
    | false -> merged_file
    | true ->
      let merged_file = aggressive_merging_pass merged_file in
      let merged_file = aggressive_merging_repeat_pass_2 merged_file in
      merged_file
  in

  (* DEBUG PRINTING after pass 2. *)
  begin
    let dkey = dkey_merge_after_pass_2 in
    Kernel.debug ~dkey "%a" PlainMerging.pp_eq_table vars;
    Kernel.debug ~dkey "%a" PlainMerging.pp_syn_table vars;
    Kernel.debug ~dkey "%a" PlainMerging.pp_eq_table aggressive_merging;
    Kernel.debug ~dkey "%a" PlainMerging.pp_syn_table aggressive_merging;
    Kernel.debug ~dkey "%t" BothPassesEnv.FormalArgsNames.pretty;
    Kernel.debug ~dkey "%t" MergeFunspec.pp_funspec_to_merge;
    Kernel.debug ~dkey "%t" MergeFunspec.pp_formals_renaming;
    Kernel.debug ~dkey "%t" Pass2Env.VarsDeclared.pretty;
    Kernel.debug ~dkey "%t" Pass2Env.CompsDeclared.pretty;
    Kernel.debug ~dkey "%t" Pass2Env.VarDefinitions.pretty;
    Kernel.debug ~dkey "%t" Pass2Env.FunDefinitions.pretty;
  end;


  (* === RESULT FILE === *)

  (* Now reverse the result and return the resulting file. *)
  let result_file = MergedFile.to_result_file merged_file result_file_name in


  (* === POST-TREATMENT === *)

  (* Post-treatment of specifications. *)
  begin
    List.iter global_merge_funspec result_file.globals;
    result_file.globals <- move_spec result_file.globals;
  end;

  (* Make the GC happy BUT keep some tables. *)
  (* TODO: Why exactly do we leave some tables?... *)
  (* TODO: Shouldn't we clear Pass2Env etc? *)
  clear_environment ~all:false ();

  (* We have made many renaming changes and sometimes we have just guessed a
     name wrong. Make sure now that the local names are unique. *)
  Cil.uniqueVarNames result_file;

  if Errorloc.had_errors () then
    Kernel.abort "error encountered during linking@.";

  reestablish_FormalsDecl_consistency result_file;

  result_file

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
