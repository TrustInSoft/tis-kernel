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

(* Type check and elaborate ABS to CIL *)

(* The references to ISO means ANSI/ISO 9899-1999 *)

open Cil_types

let dkey_cabs = Kernel.register_category "cabs:print"
let category_global = Kernel.register_category "cabs2cil:create_global"

let tis_kernel_keep_block = "TIS_KERNEL_KEEP_BLOCK"
let () = Cil_printer.register_shallow_attribute tis_kernel_keep_block

let tis_kernel_implicit_behavior_name = "TIS_Kernel_implicit_init"

module IgnorePureExpHook =
  Hook.Build (struct type t = string * exp end)

let register_ignore_pure_exp_hook f =
  IgnorePureExpHook.extend (fun (x, z) -> f x z)

module ImplicitPrototypeHook =
  Hook.Build (struct type t = varinfo end)

let register_implicit_prototype_hook f = ImplicitPrototypeHook.extend f

module IncompatibleDeclHook =
  Hook.Build(struct type t = varinfo * varinfo * string end)

let register_incompatible_decl_hook f =
  IncompatibleDeclHook.extend (fun (x, y, z) -> f x y z)


module DifferentDeclHook =
  Hook.Build(struct type t = varinfo * varinfo end)

let register_different_decl_hook f =
  DifferentDeclHook.extend (fun (x, y) -> f x y)

module LocalFuncHook = Hook.Build(struct type t = varinfo end)

let register_local_func_hook = LocalFuncHook.extend

module IgnoreSideEffectHook =
  Hook.Build(struct type t = Cabs.expression * exp end)

let register_ignore_side_effect_hook f =
  IgnoreSideEffectHook.extend (fun (y, z) -> f y z)

module ConditionalSideEffectHook =
  Hook.Build(struct type t = Cabs.expression * Cabs.expression end)

module ForLoopHook =
  Hook.Build(struct
    type t =
      Cabs.for_clause * Cabs.expression * Cabs.expression * Cabs.statement
  end)

let register_for_loop_all_hook f =
  ForLoopHook.extend (fun (x, y, z, t) -> f x y z t)

let register_for_loop_init_hook f =
  ForLoopHook.extend (fun (x, _, _, _) -> f x)

let register_for_loop_test_hook f =
  ForLoopHook.extend (fun (_, x, _, _) -> f x)

let register_for_loop_incr_hook f =
  ForLoopHook.extend (fun (_, _, x, _) -> f x)

let register_for_loop_body_hook f =
  ForLoopHook.extend (fun (_, _, _, x) -> f x)

let register_conditional_side_effect_hook f =
  ConditionalSideEffectHook.extend (fun (y, z) -> f y z)


module Utility = Cabs2cil_Utility
open Utility

module Initialization = Cabs2cil_Initialization

type language = C | CPlusPlus

let default_language = C
let current_language = ref default_language

(* ---------- source error message handling ------------- *)

(** Keep a list of the variable ids for the variables that were created to
    hold the result of function calls. *)
module CallTempVars : sig
  val mem : varinfo -> bool
  val add : varinfo -> unit
  val clear : unit -> unit
end = struct
  let table : unit Datatype.Int.Hashtbl.t = Datatype.Int.Hashtbl.create 13
  let mem varinfo = Datatype.Int.Hashtbl.mem table varinfo.vid
  let add varinfo = Datatype.Int.Hashtbl.add table varinfo.vid ()
  let clear () = Datatype.Int.Hashtbl.clear table
end

(* Keep a list of functions that were called without a prototype. *)
module FunsCalledWithoutPrototype = struct
  let functions_called_without_prototype : bool Datatype.Int.Hashtbl.t =
    Datatype.Int.Hashtbl.create 13

  let clear () = Datatype.Int.Hashtbl.clear functions_called_without_prototype
  let add varinfo =
    Datatype.Int.Hashtbl.add functions_called_without_prototype varinfo.vid true
end


(***** PROCESS PRAGMAS **********)

module Pragma = Cabs2cil_Pragma

(***** COMPUTED GOTO ************)

module ComputedGoto : sig
  (* The address of labels are small integers (starting from 0). A computed goto
     is replaced with a switch on the address of the label. We generate only one
     such switch and we'll jump to it from all computed gotos. To accomplish
     this we'll add a local variable to store the target of the goto. *)

  (* The local variable in which to put the detination of the goto and the
     statement where to jump. *)
  val get_target_data : unit -> (varinfo * stmt) option
  val set_target_data : (varinfo * stmt) option -> unit

  (* The "addresses" of labels. *)
  val get_target_hash : string -> int
  val iter_target_hash : (string -> int -> unit) -> unit

  val clear_env : unit -> unit
end = struct
  (* target_data *)
  let target_data = ref None
  let get_target_data () = !target_data
  let set_target_data data = target_data := data

  (* target_hash *)
  let target_hash = Hashtbl.create 13
  let target_next_addr = ref 0

  let add_target_hash = Hashtbl.add target_hash
  let iter_target_hash f = Hashtbl.iter f target_hash

  let get_target_hash label =
    match (Extlib.find_or_none (Hashtbl.find target_hash)) label with
    | Some address -> address
    | None ->
      let target_next_address = !target_next_addr in
      incr target_next_addr;
      add_target_hash label target_next_address;
      target_next_address

  let clear_env () =
    target_data := None;
    Hashtbl.clear target_hash;
    target_next_addr := 0
end

(********** TRANSPARENT UNION ******)

module TransparentUnionArgs : sig
  (* When we process an argument list, remember the argument index which has a
     transparent union type, along with the original type. We need this to
     process function definitions. *)
  val add : int * typ -> unit
  val get : int -> typ
  val clear : unit -> unit
end = struct
  let transparent_union_args : (int * typ) list ref = ref []
  let clear () = transparent_union_args := []
  let add (index, arg) =
    transparent_union_args := (index, arg) :: !transparent_union_args
  let get index = List.assq index !transparent_union_args
end

(*** EXPRESSIONS *************)

module Globals = Cabs2cil_Globals

(********* ENVIRONMENTS ***************)

(* The environment is kept in two distinct data structures. A hashtable maps
   each original variable name into a varinfo (for variables, or an enumeration
   tag, or a type). Note that the varinfo might contain an alpha-converted name
   different from that of the lookup name.
   The Ocaml hash tables can keep multiple mappings for a single key. Each time
   the last mapping is returned and upon deletion the old mapping is restored.
   To keep track of local scopes we also maintain a list of scopes (represented
   as lists). *)
type envdata =
  | EnvVar of varinfo
  (** The name refers to a variable (which could also be a function). *)
  | EnvEnum of enumitem
  (** The name refers to an [enumitem]. *)
  | EnvTyp of typ
  (** The name is of the form "struct foo", or "union foo" or "enum foo" and
      refers to a type. Note that the name of the actual type might be different
      from "foo" due to alpha conversion. *)
  | EnvLabel of string
  (* The name refers to a label. This is useful for GCC's locally declared
     labels. The lookup name for this category is "label foo". *)

(** The environment table. *)
let env : (string, envdata * location) Hashtbl.t = Hashtbl.create 307

(** The global environment table. This is always a subset of [env]. *)
let genv : (string, envdata * location) Hashtbl.t = Hashtbl.create 307

(* In the scope we keep the original names, so we can remove them from the
   hash table easily. *)
type undo_scope =
  | UndoRemoveFromEnv of string
  | UndoResetAlphaCounter of location Alpha.alphaTableData ref *
                             location Alpha.alphaTableData
  | UndoRemoveFromAlphaTable of string

let scopes : undo_scope list ref list ref = ref []

(* When you add to [env], you also add to the current scope. *)
let add_local_to_env (name : string) (envdata : envdata) =
  (* 1. Add to [env]. *)
  Hashtbl.add env name (envdata, Cil.CurrentLoc.get ());
  (* 2. Add to the current scope. *)
  match !scopes, envdata with
  | [], EnvVar _ ->
    Kernel.fatal ~current:true
      "add_local_to_env: not in a scope when adding %s!" name
  | [], _ ->
    (* We are at the top level: add to [genv]. *)
    (* We might add types. *)
    Hashtbl.add genv name (envdata, Cil.CurrentLoc.get ())
  | scope :: _, _ ->
    (* If we are in a scope, then it means we are not at top level.
       Add the name to the scope. *)
    scope := (UndoRemoveFromEnv name) :: !scope

let add_global_to_env (global_name : string) (envdata : envdata) : unit =
  (*  ignore (E.log "%a: adding global %s to env\n" d_loc !currentLoc k); *)
  Hashtbl.add env global_name (envdata, Cil.CurrentLoc.get ());
  (* Also add it to the global environment. *)
  Hashtbl.add genv global_name (envdata, Cil.CurrentLoc.get ())

(* Create a new name based on a given name.
   The new name is formed from a prefix (obtained from the given name as the
   longest prefix that ends with a non-digit), followed by a '_' and then by a
   positive integer suffix.
   The first argument is a table mapping name prefixes with the largest suffix
   used so far for that prefix. The largest suffix is one when only the
   version without suffix has been used. *)
let alphaTable : (string, location Alpha.alphaTableData ref) Hashtbl.t =
  Hashtbl.create 307
(* Variables and enum tags. For composite types we have names like "struct foo"
   or "union bar" *)

(* NOTE: This is exported to the interface. *)
let fresh_global lookupname =
  let fresh_name, _loc =
    Alpha.newAlphaName
      ?undolist:None
      ~alphaTable
      ~lookupname
      ~data:(Cil.CurrentLoc.get ())
  in
  fresh_name

(* To keep different name scopes different, we add prefixes to names specifying
   the kind of name. The kind can be one of:
   - "" for variables or enum tags,
   - "struct" for structures and unions (they share the name space),
   - "enum" for enumerations,
   - "type" for types. *)
let kind_plus_name (kind : string) (original_name : string) : string =
  (* NOTE: typedefs live in the same namespace as normal identifiers. *)
  let prefix =
    match kind with
    | "" | "type" -> ""
    | _ -> kind ^ " "
  in
  prefix ^ original_name

module NewAlphaName : sig
  val newAlphaName : bool -> string -> string -> string * location
end = struct

  let strip_kind (kind : string) (kind_plus_name : string) : string =
    match kind with
    | "type" | "" -> kind_plus_name
    | _ ->
      let l = String.length kind + 1 in
      String.sub kind_plus_name l (String.length kind_plus_name - l)

  let is_same_kind kind info =
    match kind, info with
    | "", EnvEnum _
    | "enum", EnvTyp _
    | "type", EnvTyp _
    | "struct", EnvTyp _
    | "union", EnvTyp _
    | "label", EnvLabel _
    | "", EnvVar _ -> true
    | _, _ -> false

  let find_identifier_decl name scope =
    match scope with
    | UndoRemoveFromEnv name' -> name = name'
    | _ -> false

  let newAlphaName
      (globalscope : bool) (* The name should have global scope *)
      (kind : string)
      (original_name : string) =
    let lookupname = kind_plus_name kind original_name in
    if not globalscope
    then begin
      (* If we are in a scope then it means that we are alpha-converting a local
         name. Go and add stuff to reset the state of the alpha table but only
         to the top-most scope (that of the enclosing function). *)
      let rec find_enclosing_fun scopes =
        match scopes with
        | [] -> (* At global scope *) ()
        | [top_most_scope] ->
          let scope_elem =
            let prefix = Alpha.getAlphaPrefix ~lookupname in
            try
              let count_ref = Hashtbl.find alphaTable prefix in
              UndoResetAlphaCounter (count_ref, !count_ref)
            with Not_found ->
              UndoRemoveFromAlphaTable prefix
          in
          top_most_scope := scope_elem :: !top_most_scope
        | _ :: remaining_scopes -> find_enclosing_fun remaining_scopes
      in
      find_enclosing_fun !scopes
    end;
    let new_name, old_location =
      Alpha.newAlphaName
        ?undolist:None
        ~alphaTable
        ~lookupname
        ~data:(Cil.CurrentLoc.get ())
    in
    if new_name <> lookupname then begin
      try
        let info, _loc =
          match !scopes with
          | [] -> Hashtbl.find genv lookupname
          | scope :: _
            when List.exists (find_identifier_decl lookupname) !scope ->
            Hashtbl.find env lookupname
          | _ :: _ -> raise Not_found
        in
        Kernel.error ~current:true
          "redefinition of '%s'%s in the same scope. \
           Previous declaration was at %a"
          original_name
          (if is_same_kind kind info then "" else " with different kind")
          Cil_datatype.Location.pretty old_location
      with
      | Not_found -> () (* no clash of identifiers *)
      | Failure _ ->
        Kernel.fatal
          "finding a fresh identifier in local scope with empty scopes stack"
    end;
    strip_kind kind new_name, old_location

end

let newAlphaName = NewAlphaName.newAlphaName

(* In order to process GNU_BODY expressions we must record that a given
   COMPUTATION is interesting.
   - The [statement] is the last statement of the GNU Body.
   - The [exp] and [typ] are used to store the result and its type if any.
   - The [boolean] tells if the result will be dropped. *)
let gnu_body_result : (Cabs.statement * ((exp * typ) option ref) * bool) ref =
  let last_statement = { Cabs.stmt_ghost = false;
                         stmt_node = Cabs.NOP (cabslu "_NOP") }
  in
  let result_and_its_type = ref None in
  let should_drop_result = true in
  ref (last_statement, result_and_its_type, should_drop_result)

(*** When we do statements we need to know the current return type *)
let current_return_type : typ ref = ref (TVoid [])
let current_fun_fundec : fundec ref = ref dummy_function

module AnonStructName : sig
  val reset : unit -> unit
  val get : string -> string -> string
end = struct
  let last_struct_id = ref 0
  let reset () = last_struct_id := 0
  let get (k : string) (suggested : string) =
    incr last_struct_id;
    let suggested' =
      match suggested with
      | "" -> ""
      | _ -> "_" ^ suggested
    in
    Printf.sprintf "__anon%s%s_%d" k suggested' !last_struct_id
end

let startFile () =
  Hashtbl.clear env;
  Hashtbl.clear genv;
  Hashtbl.clear alphaTable;
  AnonStructName.reset ();
;;

module ConstrExprId : sig
  val reset : unit -> unit
  val get_next : unit -> string
end = struct
  let constr_expr_id = ref 0
  let reset () = constr_expr_id := 0
  let get_next () =
    let new_var_name =  Printf.sprintf "__constr_expr_%d" !constr_expr_id in
    incr constr_expr_id;
    new_var_name
end

(** Lookup a variable name. Return also the location of the definition.
    Might raise [Not_found]. *)
let lookup_var (name : string) : varinfo * location =
  match Hashtbl.find env name with
  | EnvVar varinfo, location -> varinfo, location
  | _ -> raise Not_found

(** Lookup a global variable name. Return also the location of the definition.
    Might raise [Not_found]. *)
let lookup_global_var (name : string) : varinfo * location =
  match Hashtbl.find genv name with
  | (EnvVar varinfo), location -> varinfo, location
  | _ -> raise Not_found

let _docEnv () =
  let acc : (string * (envdata * location)) list ref = ref [] in
  let doone fmt = function
      EnvVar varinfo, loc ->
      Format.fprintf fmt "Var(%s,global=%b) (at %a)"
        varinfo.vname varinfo.vglob Printer.pp_location loc
    | EnvEnum (_enumitem), loc ->
      Format.fprintf fmt "Enum (at %a)" Printer.pp_location loc
    | EnvTyp _typ, _loc -> Format.fprintf fmt "typ"
    | EnvLabel label, _loc -> Format.fprintf fmt "label %s" label
  in
  Hashtbl.iter (fun name d -> acc := (name, d) :: !acc) env;
  Pretty_utils.pp_list ~sep:"@\n"
    (fun fmt (name, d) -> Format.fprintf fmt "  %s -> %a" name doone d)
    Format.std_formatter !acc


(* Add a new variable. Do alpha-conversion if necessary. *)
let alpha_convert_var_and_add_to_env (add_to_env : bool) varinfo : varinfo =

  (* Announce the name to the alpha conversion table. *)
  let new_name, old_location =
    newAlphaName (add_to_env && varinfo.vglob) "" varinfo.vname
  in

  (* Make a copy of the varinfo if the name has changed. Never change the name
     for global variables. *)
  let new_varinfo =
    match varinfo.vglob with
    | _ when varinfo.vname = new_name -> varinfo
    | true when Globals.StaticLocals.mem varinfo ->
      (* Perhaps this is because we have seen a static local which happened to
         get the name that we later want to use for a global. *)
      let static_local_varinfo = Globals.StaticLocals.find varinfo in
      Globals.StaticLocals.remove varinfo;
      (* Use the new name for the static local. *)
      static_local_varinfo.vname <- new_name;
      (* And continue using the last name. *)
      varinfo
    | true when Globals.Typedefs.mem varinfo ->
      (* Or perhaps we have seen a typedef which stole our name. This is
         possible because typedefs use the same name space. *)
      let typedef_typeinfo = Globals.Typedefs.find varinfo in
      Globals.Typedefs.remove varinfo;
      (* Use the new name for the typedef instead. *)
      typedef_typeinfo.tname <- new_name;
      (* And continue using the last name. *)
      varinfo
    | true ->
      Kernel.abort ~current:true
        "It seems that we would need to rename global %s (to %s) \
         because of previous occurrence at %a"
        varinfo.vname new_name Printer.pp_location old_location
    | false ->
      (* We have changed the name of a local variable. Can we try to detect if
         the other variable was also local in the same scope? Not for now. *)
      Cil.copyVarinfo varinfo new_name
  in

  (* Store all locals in the slocals (in reversed order). We'll reverse them
     and take out the formals at the end of the function *)
  if not varinfo.vglob then
    !current_fun_fundec.slocals <- new_varinfo :: !current_fun_fundec.slocals;

  begin
    if add_to_env then
      match varinfo.vglob with
      | true -> add_global_to_env varinfo.vname (EnvVar new_varinfo)
      | false -> add_local_to_env varinfo.vname (EnvVar new_varinfo)
  end;

  new_varinfo

let get_temp_name ?(prefix="") () : string =
  let undolist = ref [] in
  let name, _ =
    let data = Cil.CurrentLoc.get() in
    let lookupname =
      match prefix with
      | "" -> "tmp"
      | _ -> prefix ^ "_" ^ "tmp"
    in
    Alpha.newAlphaName ~alphaTable ~undolist ~lookupname ~data
  in
  let undolist = !undolist in
  Alpha.undoAlphaChanges ~alphaTable ~undolist;
  name

(* Create a new temporary variable *)
let make_new_tmp_var ?(prefix="") descr (descrpure : bool) typ : varinfo =
  (* Physical equality used on purpose here! *)
  if !current_fun_fundec == dummy_function then
    Kernel.fatal ~current:true "make_new_tmp_var called outside a function";
  let varinfo =
    let typ' = !Parameters.typeForInsertedVar (Cil.stripConstLocalType typ) in
    let name = get_temp_name ~prefix () in
    Cil.makeVarinfo ~temp:true false false name typ'
  in
  varinfo.vdescr <- Some descr;
  varinfo.vdescrpure <- descrpure;
  (* Rename if clash, but do not add to the environment/ *)
  let varinfo = alpha_convert_var_and_add_to_env false varinfo in
  (* The temporary is local to the function: the normalization can use it
     wherever it wants. *)
  if false then
    !current_fun_fundec.sbody.blocals <-
      varinfo :: !current_fun_fundec.sbody.blocals;
  varinfo

let lookup_type_no_error (kind : string) (name: string) : typ * location =
  let kind_plus_name = kind_plus_name kind name in
  match Hashtbl.find env kind_plus_name with
  | EnvTyp typ, location -> typ, location
  | _ -> raise Not_found

let lookup_type (kind : string) (name : string) : typ * location =
  try lookup_type_no_error kind name
  with Not_found ->
    Kernel.abort ~current:true "Cannot find declaration of %s %s" kind name

module CompInfo = Cabs2cil_CompositeTypes.CompInfo
module CompField = Cabs2cil_CompositeTypes.CompField

(** [kind] is either "struct" or "union" or "enum", [name] is a name. *)
let find_comp_type (kind : string) (name : string) (attrs : attributes) : typ =
  let make_forward_reference () =
    (* This is a forward reference, either because we have not seen this struct
       already or because we want to create a version with different
       attributes.  *)
    match kind with
    | "enum" ->
      let enuminfo, is_new = CompInfo.createEnumInfo name ~norig:name in
      if is_new
      then
        Globals.cabsPushGlobal (GEnumTagDecl (enuminfo, Cil.CurrentLoc.get ()));
      TEnum (enuminfo, attrs)
    | "struct" | "union" ->
      let compinfo, is_new =
        let is_struct = (kind = "struct") in
        CompInfo.createCompInfo is_struct name ~norig:name
      in
      if is_new
      then
        Globals.cabsPushGlobal (GCompTagDecl (compinfo, Cil.CurrentLoc.get ()));
      TComp (compinfo, Cil.empty_size_cache (), attrs)
    | _ -> assert false
  in
  try
    let old_typ, _ = lookup_type_no_error kind name in (* Already defined.  *)
    let are_attrs_equal =
      let old_attrs = Cil.typeAttrs old_typ in
      try List.for_all2 Cil_datatype.Attribute.equal old_attrs attrs
      with Invalid_argument _ -> false
    in
    match are_attrs_equal with
    | true -> old_typ
    | false -> make_forward_reference ()
  with Not_found -> make_forward_reference ()


(******** CASTS *********)

module Casts = Cabs2cil_Casts
open Casts

module Chunk = Cabs2cil_BlockChunk
let (@@) = Chunk.(@@)
let (+++) = Chunk.(+++)

(* Maps local variables that are variable sized arrays to the expressions that
   denote their length. *)
let variable_size_arrays : exp Datatype.Int.Hashtbl.t =
  Datatype.Int.Hashtbl.create 17

(**** EXP actions ***)
type exp_action =
  | ADrop
  (** Drop the result. Only the side-effect is interesting. *)
  | AType
  (** Only the type of the result is interesting. *)
  | ASet of bool * lval * lval list * typ
  (** Put the result in a given [lval], provided it matches the type.
      - The type is the type of the [lval].
      - The [flag] indicates whether this should be considered in the effects of
        current chunk.
      - The [lval list] is the list of locations that are read in order to
        evaluate the location of the lval.
      - The location of the [lval] is guaranteed not to depend on its own value,
        e.g. p[p[0]] when p[0] is initially 0, so the location won't change
        after assignment. *)
  | AExp of typ option
  (** Return the [exp] as usual.
      Optionally we can specify an expected type. This is useful for constants.
      The expected type is informational only, we do not guarantee that the
      converted expression has that type. You must use a [doCast] afterwards to
      make sure. *)
  | AExpLeaveArrayFun
  (** Do it like an expression, but do not convert arrays of functions into
      pointers. *)

(* Do types *)
module CombineTypes = Cabs2cil_CombineTypes
open CombineTypes

(* Export to API *)
let compatibleTypes = compatibleTypes
let compatibleTypesp = compatibleTypesp
let logicConditionalConversion = logicConditionalConversion

(* Create and cache varinfos for globals.
   Starts with a varinfo but if the global has been declared already it might
   come back with another varinfo.
   Returns a pair:
   - the varinfo to use (might be the old one),
   - and an indication whether the variable exists already in the environment.
*)
let make_global_varinfo (is_a_definition: bool) varinfo : varinfo * bool =
  try
    (* See if already defined, in the global environment. We could also look it
       up in the whole environment but in that case we might see a local. This
       can happen when we declare an extern variable with global scope but we
       are in a local scope. *)

    (* We look up in the environment. *)
    let old_varinfo, old_location =
      let lookup_name = varinfo.vname in
      Kernel.debug ~dkey:category_global
        "make_global_varinfo isadef=%b vi.vname=%s (lookup = %s)"
        is_a_definition varinfo.vname lookup_name;
      (* This may throw [Not_found]. *)
      lookup_global_var lookup_name
    in
    Kernel.debug ~dkey:category_global "  %s(%d) already in the env at loc %a"
      varinfo.vname old_varinfo.vid Printer.pp_location old_location;
    (* It was already defined. We must reuse the varinfo. But clean up the
       storage.  *)
    let new_storage = (** See 6.2.2 *)
      match old_varinfo.vstorage, varinfo.vstorage with
      (* The same identifier cannot have both internal and external linkage.
         Following C11:6.2.2p7 :
         "If, within a translation unit, the same identifier appears with both
         internal and external linkage, the behavior is undefined."

         The [extern] specifier preserves prior internal linkage.
         Following C11:6.2.2p4 :
         "For an identifier declared with the storage-class specifier extern in
         a scope in which a prior declaration of that identifier is visible, if
         the prior declaration specifies internal or external linkage, the
         linkage of the identifier at the later declaration is the same as the
         linkage specified at the prior declaration."

         Exactly the same thing happens if there is no specifier.
         Following C11:6.2.2p5 :
         "If the declaration of an identifier for a function has no
         storage-class specifier, its linkage is determined exactly as if it
         were declared with the storage-class specifier extern."

         The [static] specifier makes the linkage of an identifier internal.
         Following C11:6.2.2p3 :
         "If the declaration of a file scope identifier for an object or a
         function contains the storage-class specifier static, the identifier
         has internal linkage."

         However, the [static] specifier does not override nor keep the previous
         linkage for an identifier, therefore we get an undefined behavior if it
         follows a prior declaration that establishes external linkage. *)
      (* 1) Both specifiers are the same. *)
      | NoStorage, NoStorage -> NoStorage
      | Extern, Extern -> Extern
      | Static, Static -> Static
      | Register, Register -> Register
      (* 2) The [static] specifier follows non-static declaration: rejected. *)
      | (NoStorage | Extern | Register), Static ->
        Kernel.warning ~current:true
          "static declaration of `%a' follows non-static declaration. \
           Previous declaration was at %a"
          Printer.pp_varinfo old_varinfo
          Printer.pp_location old_varinfo.vdecl;
        Static
      (* 3) The [extern] specifier preserves the linkage of prior
            declaration. *)
      | Static, Extern -> Static
      (* NOTE for the following two cases:
         - For variables we must preserve the declaration with no specifier,
           because lack of specifier has a special meaning: it makes this
           declaration the variable's definition.
         - For functions we must preserve the explicitly [extern] declaration,
           as it makes a difference for inline functions.
           Following Following C11:6.7.4p7 :
           "If all of the file scope declarations for a function in a
           translation unit include the inline function specifier without
           extern, then the definition in that translation unit is an inline
           definition." *)
      | Extern, NoStorage | NoStorage, Extern
        when not (Cil.isFunctionType old_varinfo.vtype) -> NoStorage
      | Extern, NoStorage | NoStorage, Extern ->
        assert (Cil.isFunctionType old_varinfo.vtype); Extern
      | Extern, Register | Register, Extern -> Register
      (* 4) No specifier behaves as if we had an [extern] specifier. *)
      | Static, NoStorage when not (Cil.isFunctionType old_varinfo.vtype) ->
        (* SPECIAL CASE: If this is a variable declaration, then no storage
           specifier means that it is in fact a variable definition; we should
           emit an appropriate warning exactly in this particular case. *)
        Kernel.warning ~current:true
          "non-static declaration of `%a' follows static declaration. \
           Previous declaration was at %a"
          Printer.pp_varinfo old_varinfo
          Printer.pp_location old_varinfo.vdecl;
        Static
      | Static, NoStorage -> Static
      | NoStorage, Register | Register, NoStorage -> Register
      (* 5) Inconsistent storage specifiactions (only one case remaining). *)
      | Static, Register ->
        Kernel.warning ~current:true
          "inconsistent storage specification for %a. \
           Previous declaration was at %a"
          Printer.pp_varinfo varinfo Printer.pp_location old_location;
        Static
    in
    old_varinfo.vinline <- old_varinfo.vinline || varinfo.vinline;
    (* If the new declaration has a section attribute, remove any preexisting
       section attribute. This mimics behavior of gcc that is required to
       compile the Linux kernel properly. *)
    if Cil.hasAttribute "section" varinfo.vattr then
      old_varinfo.vattr <- Cil.dropAttribute "section" old_varinfo.vattr;
    (* Union the attributes. *)
    old_varinfo.vattr <- Utility.cabsAddAttributes old_varinfo.vattr varinfo.vattr;
    begin
      try
        let my_type =
          let what : combineWhat =
            match is_a_definition with
            | false -> CombineOther
            | true ->
              let is_old_style_proto =
                Cil.hasAttribute "FC_OLDSTYLEPROTO" varinfo.vattr
              in
              CombineFundef is_old_style_proto
          in
          combineTypes what old_varinfo.vtype varinfo.vtype
        in
        let my_type =
          let types_equal =
            Cil_datatype.Typ.equal old_varinfo.vtype varinfo.vtype
          in
          match types_equal with
          | false ->
            DifferentDeclHook.apply (old_varinfo,varinfo);
            (* Const-fold the type to avoid dependencies toward previously
               defined objects. This happens with code like:
               int c[];
               int b;
               int c[sizeof b];
            *)
            constFoldType my_type
          | true -> my_type
        in
        Cil.update_var_type old_varinfo my_type
      with Failure reason ->
        Kernel.debug ~dkey:category_global "old type = %a\nnew type = %a\n"
          Printer.pp_typ old_varinfo.vtype
          Printer.pp_typ varinfo.vtype ;
        IncompatibleDeclHook.apply (old_varinfo, varinfo, reason);
        Kernel.abort ~current:true
          "Declaration of %a does not match previous declaration from %a (%s)."
          Printer.pp_varinfo varinfo
          Printer.pp_location old_location reason
    end;
    (* Update the storage and vdecl if useful. Do so only after the hooks have
       been applied, as they may need to read those fields. *)
    if old_varinfo.vstorage <> new_storage then begin
      old_varinfo.vstorage <- new_storage;
      (* Also update the location; [varinfo.vdecl] is a better
         declaration / definition site for [varinfo]. *)
      old_varinfo.vdecl <- varinfo.vdecl;
    end;
    begin
      (* Let's mutate the formals varinfos' names attributes and types for
         function prototypes. Logic specifications refer to the varinfo in this
         table. *)
      match varinfo.vtype with
      | TFun (_, Some formals, _, _) ->
        begin
          try
            let old_formals_varinfos = Cil.getFormalsDecl old_varinfo in
            List.iter2
              (fun old_formal_varinfo (new_name, new_typ, new_attrs) ->
                 if new_name <> "" then begin
                   Kernel.debug ~dkey:category_global
                     "replacing formal %s with %s"
                     old_formal_varinfo.vname new_name;
                   old_formal_varinfo.vname <- new_name;
                   Cil.update_var_type old_formal_varinfo new_typ;
                   old_formal_varinfo.vattr <- new_attrs;
                   match old_formal_varinfo.vlogic_var_assoc with
                   | None -> ()
                   | Some old_logic_var -> old_logic_var.lv_name <- new_name
                 end)
              old_formals_varinfos
              formals
          with
          | Invalid_argument _ -> Kernel.abort "Inconsistent formals"
          | Not_found -> Cil.setFormalsDecl old_varinfo varinfo.vtype
        end
      | _ -> ()
    end ;
    (* If [is_a_definition] is true, [varinfo] is a definition. *)
    if is_a_definition then begin
      (* Always favor the location of the definition. *)
      old_varinfo.vdecl <- varinfo.vdecl;
      old_varinfo.vdefined <- true;
    end;
    (* Notice that [vtemp] is immutable, and cannot be updated. Hopefully,
       temporaries have sufficiently fresh names that this is not a problem. *)
    old_varinfo, true
  with Not_found -> (* A new one. *)
    Kernel.debug ~level:2 ~dkey:category_global
      "  %s not in the env already" varinfo.vname;
    (* Announce the name to the alpha conversion table. This will not actually
       change the name of the [varinfo].
       See the definition of [alpha_convert_var_and_add_to_env]. *)
    let varinfo = alpha_convert_var_and_add_to_env true varinfo in
    (* Update the field [vdefined]. *)
    if is_a_definition then varinfo.vdefined <- true;
    varinfo.vattr <- Cil.dropAttribute "FC_OLDSTYLEPROTO" varinfo.vattr;
    varinfo, false

(* Register a builtin function. *)
let setup_builtin builtin_name (result_type, arg_types, is_variadic) : varinfo =
  let varinfo =
    let args = Some (List.map (fun typ -> ("", typ, [])) arg_types) in
    let typ = TFun(result_type, args, is_variadic, []) in
    Cil.makeGlobalVar builtin_name typ
  in
  ignore (alpha_convert_var_and_add_to_env true varinfo);
  (* Add it to the file as well. *)
  Globals.cabsPushGlobal
    (GFunDecl (Cil.empty_funspec (), varinfo, Cil.builtinLoc));
  Cil.setFormalsDecl varinfo varinfo.vtype;
  varinfo

(** ALLOCA ***)
let alloca_fun () : varinfo =
  match Cil.gccMode () with
  | true ->
    (* Use [__builtin_alloca] where possible, because this can be used
       even when gcc is invoked with [-fno-builtin]. *)
    let alloca_varinfo, _location = lookup_global_var "__builtin_alloca" in
    alloca_varinfo
  | false ->
    begin
      try
        let alloca_varinfo, _location = lookup_global_var "alloca" in
        alloca_varinfo
      with Not_found ->
        let result_type = Cil.voidPtrType in
        let arg_types = [Cil.(theMachine.typeOfSizeOf)] in
        let is_variadic = false in
        setup_builtin "alloca" (result_type, arg_types, is_variadic)
    end

(* Maps [vid] to visitor used to perform renaming on function spec when there is
   a spec on a declaration and a definition for the function. This is done after
   typing.*)
let alpha_renaming : (int, Cil.cilVisitor) Hashtbl.t = Hashtbl.create 59

let rename_spec : global -> unit = function
  | GFunDecl(spec, varinfo, _) ->
    begin
      try
        let alpha_visitor = Hashtbl.find alpha_renaming varinfo.vid in
        ignore (Cil.visitCilFunspec alpha_visitor spec)
      with Not_found -> ()
    end
  | _ -> ()

(* Export to API *)
let anonCompFieldName = CompField.anonCompFieldName
let find_field_offset = CompField.find_field_offset

(**** PEEP-HOLE optimizations ***)

(* Should we collapse [tmp = f(); lv = tmp;] where the result type of [f]
   is [tf], and the [lv] has type [tlv] *)
let allow_return_collapse ~lval_typ ~f_typ =
  Cil_datatype.Typ.equal lval_typ f_typ ||
  Kernel.DoCollapseCallCast.get () &&
  begin
    match Cil.unrollType lval_typ, Cil.unrollType f_typ with
    | TPtr _, TPtr _ -> true (* Useful for [malloc] and others. Could be
                                restricted to [void* -> any] if needed. *)
    | TInt (iklv, _), TInt (ikf, _) -> Cil.intTypeIncluded ikf iklv
    | TFloat (fklv, _), TFloat (fkf, _) -> Cil.frank fklv >= Cil.frank fkf
    | _, _ -> false
  end


let after_conversion ~ghost chunk : Chunk.t =
  (* Now scan the statements and find [Instr] blocks. *)
  (* We want to collapse sequences of the form "tmp = f(); v = tmp". This will
     help significantly with the handling of calls to [malloc], where it is
     important to have the cast at the same place as the call. *)
  let type_of_call_result f_exp =
    match Cil.unrollType (Cil.typeOf f_exp) with
    | TFun (return_typ, _, _, _) -> return_typ
    | _ -> Kernel.abort ~current:true "Function call to a non-function"
  in
  let collapse_call_cast (stmt_1, stmt_2) : stmt list option =
    match stmt_1.skind, stmt_2.skind with
    (* f() *)
    | Instr
        (Call (Some (Var call_varinfo, NoOffset),
               call_exp, call_args_exps, call_location)),
      Instr
        (Set (assign_dest_lval,
              { enode =
                  CastE (assign_new_typ,
                         { enode =
                             Lval (Var assign_varinfo, NoOffset); _ }); _ },
              _location))
      when not call_varinfo.vglob
        && assign_varinfo == call_varinfo
        && String.length call_varinfo.vname >= 3
        (* Watch out for the possibility that we have an implied cast in
           the call. *)
        && CallTempVars.mem call_varinfo
        && Cil_datatype.Typ.equal
             (type_of_call_result call_exp)
             call_varinfo.vtype
        && Cil_datatype.Typ.equal
             assign_new_typ
             (Cil.typeOfLval assign_dest_lval)
        && allow_return_collapse
             ~f_typ:(type_of_call_result call_exp)
             ~lval_typ:assign_new_typ ->
      assert (stmt_2.labels = []);
      let kind =
        Instr (Call (Some assign_dest_lval,
                     call_exp, call_args_exps, call_location))
      in
      stmt_1.skind <- kind;
      Some [stmt_1]

    | Instr
        (Call (Some (Var call_varinfo, NoOffset),
               call_exp, call_args_exps, call_location)),
      Instr
        (Set (assign_dest_lval,
              { enode = Lval (Var assign_varinfo, NoOffset); _ }, _location))
      when not call_varinfo.vglob
        && assign_varinfo == call_varinfo
        && String.length call_varinfo.vname >= 3
        (* Watch out for the possibility that we have an implied cast in
           the call. *)
        && CallTempVars.mem call_varinfo
        && Cil_datatype.Typ.equal
             call_varinfo.vtype (Cil.typeOfLval assign_dest_lval)
        && allow_return_collapse
             ~f_typ:(type_of_call_result call_exp)
             ~lval_typ:call_varinfo.vtype ->
      let kind =
        Instr (Call (Some assign_dest_lval,
                     call_exp, call_args_exps, call_location))
      in
      stmt_1.skind <- kind;
      Some [stmt_1]

    | _ -> None
  in
  let block = Chunk.to_block ~ghost ~collapse_block:false chunk in
  let stmts =
    match Kernel.DoCollapseCallCast.get () with
    | true -> Cil.peepHole2 ~agressive:false collapse_call_cast block.bstmts
    | false -> block.bstmts
  in
  (* The call to [to_block] has taken care of a possible unspecified sequence.
     We do not need to keep track of effects at this level. *)
  let result_chunk =
    let stmts_with_effects = List.rev_map (fun x -> (x, Chunk.no_effects, [])) stmts in
    { chunk with Chunk.stmts_with_effects = stmts_with_effects }
  in
  (*  Format.eprintf "Before conversion@\n%a@\nAfter conversion@\n%a@\n@."
      d_chunk c d_chunk res; *)
  result_chunk

(***** Try to suggest a name for the anonymous structures *)
let suggest_anon_name (cabs_names : Cabs.name list) : string =
  match cabs_names with
  | [] -> ""
  | (name, _, _, _) :: _ -> name

(** raise [Failure] *)
let integral_cast typ term =
  raise
    (Failure
       (Pretty_utils.sfprintf "term %a has type %a, but %a is expected."
          Printer.pp_term term
          Printer.pp_logic_type Linteger
          Printer.pp_typ typ))

(* Exception raised by the instance of Logic_typing local to this module.
   See document of [error] below. *)
exception LogicTypeError of location * string

module C_logic_env = struct
  let nb_loop = ref 0
  let is_loop () = !nb_loop > 0
  let anonCompFieldName = CompField.anonCompFieldName
  let conditionalConversion = logicConditionalConversion

  let find_macro _ = raise Not_found

  let find_var name =
    match Hashtbl.find env name with
    | EnvVar varinfo, _ -> Cil.cvar_to_lvar varinfo
    | _ -> raise Not_found

  let find_enum_tag name =
    match Hashtbl.find env name with
    | EnvEnum enumitem, _ ->
      Cil.dummy_exp (Const (CEnum enumitem)), Cil.typeOf enumitem.eival
    | _ -> raise Not_found

  let find_comp_field compinfo name = CompField.findField name compinfo.cfields

  let find_type namespace name =
    match namespace with
    | Logic_typing.Typedef ->
      let (typ, _loc) = lookup_type_no_error "type" name in typ
    | Logic_typing.Union -> find_comp_type "union" name []
    | Logic_typing.Struct -> find_comp_type "struct" name []
    | Logic_typing.Enum -> find_comp_type "enum" name []

  include Chunk.Logic_labels

  include Logic_env

  let add_logic_function =
    add_logic_function_gen Logic_utils.is_same_logic_profile

  let integral_cast = integral_cast

  (* This function raises a non-recoverable when [-continue-annot-error] is not
     set, and [LogicTypeError] otherwise. This exception must *not* escape
     Cabs2cil. Hence, each call to a function of module [Ltyping] below must
     catch it. *)
  let error (source, _ as location) msg =
    if Kernel.ContinueOnAnnotError.get ()
    then Pretty_utils.ksfprintf
        (fun e -> raise (LogicTypeError (location, e))) msg
    else Kernel.abort ~source msg

end

let enter_scope () =
  scopes := (ref []) :: !scopes;
  C_logic_env.enter_scope ()

(* Exit a scope and clean the environment. We do not yet delete from
   the name table. *)
let exit_scope () =
  let current_scope =
    match !scopes with
    | [] -> Kernel.fatal ~current:true "Not in a scope"
    | current_scope :: remaining_scopes ->
      scopes := remaining_scopes;
      current_scope
  in
  let handle_single_undo undo =
    match undo with
    | UndoRemoveFromEnv remove_me -> Hashtbl.remove env remove_me
    | UndoRemoveFromAlphaTable remove_me -> Hashtbl.remove alphaTable remove_me
    | UndoResetAlphaCounter (reset_me, old_value) -> reset_me := old_value
  in
  List.iter handle_single_undo !current_scope;
  C_logic_env.exit_scope ()


(************ Labels ***********)

module Labels : sig

  module Manage : sig
    val new_label_name : string -> string
    val gen_new_local_label : string -> string
    val lookup_label : string -> string

    val gather_labels : Cabs.block -> unit
    val register_labels : Cabs.block -> unit
  end

  module Loops : sig
    val start_loop : bool -> unit
    val exit_loop : unit -> unit

    val continue_or_label_chunk : ghost:bool -> location -> Chunk.t
    val cons_label :
      ghost:bool -> string -> Chunk.t -> location -> bool -> Chunk.t
    val cons_label_continue : ghost:bool -> Chunk.t -> Chunk.t

    val continue_used : unit -> bool
  end

end = struct

  module Manage = struct

    open Cil

    (* Sometimes we need to create new label names. *)
    let new_label_name (base : string) = fst (newAlphaName false "label" base)

    (* In GCC we can have locally declared labels. *)
    let gen_new_local_label (label : string) =
      (* Call [new_label_name] to register the label name in the alpha
         conversion table. *)
      let label' = new_label_name label in
      (* Add it to the environment *)
      add_local_to_env (kind_plus_name "label" label) (EnvLabel label');
      label'

    let lookup_label (label : string) =
      try
        match Hashtbl.find env (kind_plus_name "label" label) with
        | EnvLabel label', _ -> label'
        | _ -> raise Not_found
      with Not_found -> label

    class gatherLabelsClass : Cabsvisit.cabsVisitor = object (self)
      inherit Cabsvisit.nopCabsVisitor

      (* We have to know if a label is local to know if it is an error if
         another label with the same name exists. But a local label can be
         declared multiple times at different nesting depths. Since a [Hashtbl]
         can maintain multiple mappings, we add and remove local labels as we
         visit their blocks. We map each local label to a location option
         indicating where it was defined (if it has been). This enables us to
         raise an error if a local label is defined twice, and we can issue
         warnings if local labels are declared but never defined. *)
      val localLabels : (string, location option) Hashtbl.t = Hashtbl.create 5

      method private addLocalLabels block =
        List.iter
          (fun label -> Hashtbl.add localLabels label None)
          block.Cabs.blabels

      method private removeLocalLabels block =
        List.iter
          (fun label ->
             if Hashtbl.find localLabels label = None then
               Kernel.warning ~current:true
                 "Local label %s declared but not defined" label;
             Hashtbl.remove localLabels label)
          block.Cabs.blabels

      method! vblock block =
        (* Add the local labels, process the block, then remove the local
           labels. *)
        self#addLocalLabels block;
        ChangeDoChildrenPost (block,
                              fun _ -> self#removeLocalLabels block; block)

      method! vstmt stmt =
        Cil.CurrentLoc.set (Cabshelper.get_statementloc stmt);
        begin
          match stmt.Cabs.stmt_node with
          | Cabs.LABEL (label, _, _) ->
            begin
              try
                match Hashtbl.find localLabels label with
                | Some old_location ->
                  Kernel.error ~once:true ~current:true
                    "Duplicate local label '%s' (previous definition was at %a)"
                    label Printer.pp_location old_location
                | None ->
                  (* Mark this label as defined. *)
                  let current_location = Some (Cil.CurrentLoc.get()) in
                  Hashtbl.replace localLabels label current_location
              with Not_found -> (* [label] is not a local label. *)
                let new_name, old_location = newAlphaName false "label" label in
                if new_name <> label then
                  Kernel.error ~once:true ~current:true
                    "Duplicate label '%s' (previous definition was at %a)"
                    label Printer.pp_location old_location
            end
          | _ -> ()
        end;
        DoChildren
    end

    let gather_labels body_block =
      ignore (Cabsvisit.visitCabsBlock (new gatherLabelsClass) body_block)

    (* Enter all the labels into the alpha renaming table to prevent duplicate
       labels when unfolding short-circuiting logical operators and when
       creating labels for (some) continue statements. *)
    class registerLabelsVisitor = object
      inherit Cabsvisit.nopCabsVisitor

      method! vstmt stmt =
        let current_location = Cabshelper.get_statementloc stmt in
        begin
          match stmt.Cabs.stmt_node with
          | Cabs.LABEL (label, _, _) ->
            Alpha.registerAlphaName
              ?undolist:None
              ~alphaTable
              ~lookupname:(kind_plus_name "label" label)
              ~data:current_location
          | _ -> ()
        end;
        DoChildren
    end

    let register_labels body_block =
      ignore (Cabsvisit.visitCabsBlock (new registerLabelsVisitor) body_block)

  end

  module Loops = struct

    (* Since we turn dowhile and for loops into while we need to take care in
       processing the continue statement. For each loop that we enter we place a
       marker in a list saying what kinds of loop it is. When we see a continue
       for a [Non-while] loop we must generate a label for the continue. *)

    type loopstate =
      | While of string ref
      | NotWhile of string ref

    let continues : loopstate list ref = ref []

    let continue_or_label_chunk ~ghost (loc : location) : Chunk.t =
      match !continues with
      | [] -> Kernel.abort ~current:true "continue not in a loop"
      | While label_ref :: _ when !Parameters.doTransformWhile ->
        if !label_ref = "" then label_ref := Manage.new_label_name "__Cont";
        Chunk.Make.goto_chunk ~ghost !label_ref loc
      | While _label_ref :: _ ->
        Chunk.Make.continue_chunk ~ghost loc
      | NotWhile label_ref :: _ ->
        if !label_ref = "" then label_ref := Manage.new_label_name "__Cont";
        Chunk.Make.goto_chunk ~ghost !label_ref loc

    let start_loop (is_while : bool) : unit =
      incr C_logic_env.nb_loop;
      let continue =
        match is_while with
        | true -> While (ref "")
        | false -> NotWhile (ref "")
      in
      continues := continue :: !continues;
      Chunk.enter_break_env ()

    let exit_loop () : unit =
      decr C_logic_env.nb_loop;
      Chunk.exit_break_env ();
      match !continues with
      | [] -> Kernel.error ~once:true ~current:true "exit Loop not in a loop"
      | _ :: remaining_continues -> continues := remaining_continues

    let cons_label ~ghost (label : string) chunk (loc : location)
        (in_original_program_text : bool) : Chunk.t =
      (* Get the first statement and add the label to it. *)
      let label_stmt, stmts_with_effects =
        Chunk.get_first_stmt_in_chunk ~ghost ~loc chunk
      in
      (* Add the label. *)
      Chunk.add_label label label_stmt;
      let new_label = Label (label, loc, in_original_program_text) in
      label_stmt.labels <- new_label :: label_stmt.labels;
      if chunk.Chunk.stmts_with_effects == stmts_with_effects
      then chunk
      else { chunk with Chunk.stmts_with_effects = stmts_with_effects }

    let cons_label_continue ~ghost chunk : Chunk.t =
      begin
        match !continues with
        | While _ :: _ -> assert !Parameters.doTransformWhile
        | _ -> ()
      end;
      match !continues with
      | [] -> Kernel.fatal ~current:true "labContinue not in a loop"
      | (While label_ref | NotWhile label_ref) :: _ when !label_ref = "" ->
        chunk
      | (While label_ref | NotWhile label_ref) :: _ ->
        cons_label ~ghost !label_ref chunk (Cil.CurrentLoc.get ()) false

    (* Was a continue instruction used inside the current loop. *)
    let continue_used () : bool =
      match !continues with
      | [] -> Kernel.fatal ~current:true "not in a loop"
      | (While label_ref | NotWhile label_ref) :: _ -> !label_ref <> ""

  end

end

module Ltyping = Logic_typing.Make(C_logic_env)

(****** TYPE SPECIFIERS *******)

(* JS: return [Some attribute_string] if the attribute string is the attribute
   annotation [attribute_string] and [None] if it is not an annotation. *)
let attribute_annotation attribute_string : string option =
  try
    let regexp = Str.regexp "/\\*@ \\(.+\\) \\*/" in
    match Str.string_match regexp attribute_string 0 with
    | true -> Some (Str.matched_group 1 attribute_string)
    | false -> None
  with Not_found -> assert false


(** Local information needed to typecheck expressions and statements. *)
type local_env =
  { authorized_reads : Cil_datatype.Lval.Set.t;
    known_behaviors : string list;
    is_ghost : bool }

(** An empty local environment. *)
let empty_local_env =
  { authorized_reads = Cil_datatype.Lval.Set.empty;
    known_behaviors = [];
    is_ghost = false }

(** Same as [empty_local_env], but sets the ghost status to the value of its
    argument. *)
let ghost_local_env ghost = { empty_local_env with is_ghost = ghost }

module FallsThroughCanBreak = Cabs2cil_FallsThroughCanBreak

let append_chunk_to_annot ~ghost annot_chunk current_chunk =
  match current_chunk.Chunk.stmts_with_effects with
  | [] -> annot_chunk @@ (current_chunk, ghost)
  (* Don't forget locals of [current_chunk]. *)

  (* If we have a single statement, we can avoid enclosing it into a block. *)
  | [ (_stmt, _effects, _calls) ] -> annot_chunk @@ (current_chunk, ghost)

  (* Make a block, and put labels of the first statement on the block itself,
     so as to respect scoping rules for \at in further annotations. *)
  | _ ->
    let block = Chunk.to_block ~ghost current_chunk in
    (* The statement may contain some local variable declarations coming from
       userland. We have to shift them from the inner block, otherwise they will
       not be accessible in the next statements. *)
    let locals_varinfos = block.blocals in
    block.blocals <- [];
    let attrs_to_add = [Attr (tis_kernel_keep_block, [])] in
    block.battrs <- Cil.addAttributes attrs_to_add block.battrs;
    let block_stmt = Cil.mkStmt ~ghost (Block block) in
    let chunk = Chunk.of_stmt block_stmt in
    let chunk = { chunk with Chunk.cases = current_chunk.Chunk.cases } in
    let chunk =
      List.fold_left Chunk.local_var_chunk chunk (List.rev locals_varinfos)
    in
    annot_chunk @@ (chunk, ghost)

(*** Result of compiling conditional expressions. *)
type cond_exp_result =
  | CEExp of Chunk.t * exp (* Do a chunk and then an expression. *)
  | CEAnd of cond_exp_result * cond_exp_result
  | CEOr of cond_exp_result * cond_exp_result
  | CENot of cond_exp_result

let rec evaluate_cond_exp = function
  | CEExp (_chunk, exp) ->
    begin
      match Cil.constFoldToInt exp with
      | None -> `CUnknown
      | Some integer when Integer.is_zero integer -> `CFalse
      | Some _ -> `CTrue
    end
  | CEAnd (cond_exp_result_1, cond_exp_result_2) ->
    begin
      match evaluate_cond_exp cond_exp_result_1 with
      | `CTrue -> evaluate_cond_exp cond_exp_result_2
      | result -> result
    end
  | CEOr (cond_exp_result_1, cond_exp_result_2) ->
    begin
      match evaluate_cond_exp cond_exp_result_1 with
      | `CFalse -> evaluate_cond_exp cond_exp_result_2
      | result -> result
    end
  | CENot cont_exp_result ->
    match evaluate_cond_exp cont_exp_result with
    | `CTrue -> `CFalse
    | `CFalse -> `CTrue
    | `CUnknown -> `CUnknown

(* How should an expression be treated in the [do_expression] and [do_cond_exp]
   functions:
   - [ExpectConst]:
       The expression is supposed to be a constant expression, so if it happens
       not to be one, an appropriate warning should be emitted.
   - [DontExpectConst]:
       The expression may be a constant expression or not.
   - [DontExpectConstButSimplifyIfPossible]:
       The expression may be a constant expression or not, but if it happens to
       be a constant expression, it should be simplified. *)
type as_const =
  | ExpectConst
  | DontExpectConst
  | DontExpectConstButSimplifyIfPossible

let as_const_of_is_const is_const =
  if is_const
  then ExpectConst
  else DontExpectConst

let asconst_of_isconst_simplify is_const =
  if is_const
  then ExpectConst
  else DontExpectConstButSimplifyIfPossible

let rec assign_init ~ghost
    lval_to_assign
    ?(has_implicit_init=false)
    ?(explicit_init=(fun _ _ -> ()))
    ?(add_implicit_ensures=(fun _ -> ()))
    init init_typ
    (chunk_acc : Chunk.t) : Chunk.t =
  let open Initialization.AssignInit in

  match init with
  | SingleInit single_init_exp ->
    let _, single_init_exp' =
      Casts.castTo init_typ (Cil.typeOfLval lval_to_assign) single_init_exp
    in
    explicit_init lval_to_assign single_init_exp';
    let add_to_chunk =
      let stmt =
        let instr = Set (lval_to_assign,
                         single_init_exp',
                         Cil.CurrentLoc.get ())
        in
        Cil.mkStmtOneInstr ~ghost instr
      in
      let effects =
        let modified = [] in
        let writes = [lval_to_assign] in
        let reads = [] in
        Chunk.make_effects ~modified ~writes ~reads
      in
      stmt, effects
    in
    chunk_acc +++ add_to_chunk

  | CompoundInit (compound_init_typ, inits) -> begin
      match compound_init_typ with
      | TArray(_, None, _, _) -> chunk_acc
      | TArray (array_base_type, array_len_opt, _, _) ->
        let array_len = integerArrayLength array_len_opt in
        let number_of_inits = List.length inits in
        if number_of_inits < array_len then begin
          (* For big arrays in local variables the implicit initialization to 0
             is not done completely. We'll do that ourselves either with:
             - a bzero to 0,
             - or a contract for plugins that do not want to rely on bzero.
             All that is done at the toplevel occurence of implicit
             initialization. *)
          let host_varinfo, host_offset =
            let host, offset = lval_to_assign in
            match host with
            | Var varinfo -> varinfo, offset
            | _ -> Kernel.fatal "Trying to initialize an anonymous block"
          in
          let ensures = ref [] in
          let known_idx = ref Datatype.Integer.Set.empty in
          let explicit_init (_, offset as lval) value_exp =
            if not has_implicit_init then begin
              (* Just add ensures at the toplevel init. *)
              let post_cond =
                let pred = ensures_init host_varinfo offset value_exp in
                (Normal, Logic_const.new_predicate pred)
              in
              ensures := post_cond :: !ensures
            end;
            (* Find which index is initialized.
               This is not necessarily the last one in case of array of
               complex structures. *)
            let rec aux offset =
              let my_offset, last_offset = Cil.removeOffset offset in
              if Cil_datatype.Offset.equal host_offset my_offset then begin
                match last_offset with
                | Index(index_exp, _) ->
                  (match Cil.constFoldToInt index_exp with
                   | Some index ->
                     known_idx := Datatype.Integer.Set.add index !known_idx
                   | _ ->
                     Kernel.abort ~current:true
                       "Non-constant index in designator for array \
                        initialization: %a"
                       Printer.pp_exp index_exp)
                | NoOffset | Field _ ->
                  assert false (* We are supposed to have an array here. *)
              end else
                match last_offset with
                | NoOffset -> ()
                | _ -> aux my_offset
            in
            aux offset;
            (* NOTE: This is the [explicit_init] function from the [assign_init]
                     function arguments, not the one that we are defining! *)
            explicit_init lval value_exp
          in
          let add_implicit_ensures =
            if has_implicit_init
            then add_implicit_ensures
            else fun predicate ->
              let new_ensure = (Normal, Logic_const.new_predicate predicate) in
              ensures := new_ensure :: !ensures
          in
          (* Do the initialization of the array only. *)
          let my_init =
            let doinit off init init_typ chunk_acc =
              let lval_to_assign' = Cil.addOffsetLval off lval_to_assign in
              assign_init ~ghost lval_to_assign'
                ~has_implicit_init:true
                ~explicit_init
                ~add_implicit_ensures
                init init_typ chunk_acc
            in
            Cil.foldLeftCompound
              ~implicit:false
              ~doinit
              ~ct:compound_init_typ
              ~initl:inits
              ~acc:Chunk.empty
          in
          let base_init_block : Chunk.t =
            if has_implicit_init then
              Chunk.empty
              (* This location has already been zero-initialized by toplevel
                 implicit init. *)
            else if Kernel.InitializedPaddingLocals.get () then begin
              let stmt =
                set_to_zero ~ghost host_varinfo host_offset compound_init_typ
              in
              Chunk.of_stmt stmt
              (* Use bzero to clear whole region. *)
            end else
              zero_init ~ghost
                host_varinfo host_offset array_len array_base_type
                (* Zero-init each field, so as to leave padding bits
                   uninitialized. *)
          in
          let init_block : Chunk.t = base_init_block @@ (my_init, ghost) in
          (* Lift at toplevel contract implicit zero-initialization. *)
          let my_ensures =
            make_implicit_ensures
              host_varinfo host_offset array_base_type array_len !known_idx
          in
          add_implicit_ensures my_ensures;
          let annot_chunk =
            if has_implicit_init
            then Chunk.empty
            else begin
              let term_lval =
                Logic_utils.lval_to_term_lval ~cast:false lval_to_assign
              in
              let loc = host_varinfo.vdecl in
              let rec all_zone term_lval =
                let term_lval_typ =
                  Logic_utils.unroll_type (Cil.typeOfTermLval term_lval)
                in
                match term_lval_typ with
                | Ctype (TArray (_, len, _, _))
                | Ltype
                    ({ lt_name = "set"; _ },
                     [ Ctype (TArray (_, len, _, _)) ]) ->
                  let tlen =
                    Extlib.opt_map
                      (Logic_utils.expr_to_term ~cast:false) len
                  in
                  let upper =
                    Extlib.opt_map
                      (fun tlen ->
                         Logic_const.term ~loc
                           (TBinOp (MinusA, tlen, Logic_const.tinteger ~loc 1))
                           Linteger)
                      tlen
                  in
                  let all_range =
                    Logic_const.trange ~loc
                      (Some (Logic_const.tinteger ~loc 0), upper)
                  in
                  all_zone (Logic_const.addTermOffsetLval
                              (TIndex (all_range, TNoOffset)) term_lval)
                | logic_type ->
                  Logic_const.term ~loc (TLval term_lval) logic_type
              in
              let tlocs = all_zone term_lval in
              let code_annotation =
                let contract =
                  let spec_behavior =
                    let name = tis_kernel_implicit_behavior_name in
                    let assigns =
                      Writes [Logic_const.new_identified_term tlocs, FromAny]
                    in
                    let post_cond = List.rev !ensures in
                    [Cil.mk_behavior ~name ~assigns ~post_cond ()]
                  in
                  { spec_behavior;
                    spec_variant = None;
                    spec_terminates = None;
                    spec_complete_behaviors = [];
                    spec_disjoint_behaviors = []; }
                in
                Logic_const.new_code_annotation (AStmtSpec ([], contract))
              in
              Chunk.of_stmt
                (Cil.mkStmt ~ghost
                   (Instr
                      (Code_annot (code_annotation, Errorloc.currentLoc ()))))
            end
          in
          let init_chunk =
            append_chunk_to_annot ~ghost annot_chunk init_block
          in
          chunk_acc @@ (init_chunk, ghost)
        end else begin
          Cil.foldLeftCompound
            ~implicit:false
            ~doinit:
              (fun off i it acc ->
                 assign_init ~ghost (Cil.addOffsetLval off lval_to_assign)
                   ~has_implicit_init
                   ~explicit_init
                   ~add_implicit_ensures
                   i it acc)
            ~ct:compound_init_typ
            ~initl:inits
            ~acc:chunk_acc
        end
      | _ ->
        Cil.foldLeftCompound
          ~implicit:false
          ~doinit:
            (fun off i it acc ->
               assign_init ~ghost (Cil.addOffsetLval off lval_to_assign)
                 ~has_implicit_init
                 ~explicit_init
                 ~add_implicit_ensures
                 i it acc)
          ~ct:compound_init_typ
          ~initl:inits
          ~acc:chunk_acc
    end

(** Returns a block of statements equivalent to the initialization [init]
    applied to lvalue [lval] of type [typ]. *)
and _blockInit ~ghost lval init typ : block =
  Chunk.to_block ~ghost (assign_init ~ghost lval init typ Chunk.empty)

(** Optional constant folding of binary operations. *)
let optional_constFoldBinOp loc machdep binop exp_1 exp_2 typ =
  match Cil.(theMachine.lowerConstants) with
  | true -> Cil.constFoldBinOp ~loc machdep binop exp_1 exp_2 typ
  | false -> Cil.new_exp ~loc (BinOp (binop, exp_1, exp_2, typ))

(* Binop is always the arithmetic version. Change it to the appropriate pointer
   version if necessary. *)
let do_binop loc binop exp_1 typ_1 exp_2 typ_2 : typ * exp =
  let do_arithmetic () =
    let result_typ = Cil.arithmeticConversion typ_1 typ_2 in
    (* Keep the operator since it is arithmetic. *)
    result_typ,
    optional_constFoldBinOp loc false binop
      (Casts.makeCastT ~e:exp_1 ~oldt:typ_1 ~newt:result_typ)
      (Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:result_typ)
      result_typ
  in
  let do_arithmetic_comp () =
    let result_typ = Cil.arithmeticConversion typ_1 typ_2 in
    (* Keep the operator since it is arithemtic. *)
    Cil.intType,
    optional_constFoldBinOp loc false binop
      (Casts.makeCastT ~e:exp_1 ~oldt:typ_1 ~newt:result_typ)
      (Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:result_typ)
      Cil.intType
  in
  let do_integral_arithmetic () =
    let result_typ = Cil.unrollType (Cil.arithmeticConversion typ_1 typ_2) in
    match result_typ with
    | TInt _ ->
      result_typ,
      optional_constFoldBinOp loc false binop
        (Casts.makeCastT ~e:exp_1 ~oldt:typ_1 ~newt:result_typ)
        (Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:result_typ)
        result_typ
    | _ ->
      Kernel.fatal ~current:true "%a operator on a non-integer type"
        Printer.pp_binop binop
  in
  let are_ptrs_to_compatible_types typ_1 typ_2 =
    (* This is only called if typ_1 and typ_2 are pointer types. *)
    assert (Cil.isPointerType typ_1 && Cil.isPointerType typ_2);
    let get_pointed_type typ = Cil.(unrollType (typeOf_pointed typ)) in
    let typ_1_pointed = get_pointed_type typ_1 in
    let typ_2_pointed = get_pointed_type typ_2 in
    compatibleTypesp typ_1_pointed typ_2_pointed
  in
  (* Invariant: typ_1 and typ_2 are pointers types. *)
  let pointer_comparison exp_1 typ_1 exp_2 typ_2 =
    (* We are more lenient than the norm here (C99 6.5.8, 6.5.9), and cast
       arguments with incompatible types to a common type. *)
    let exp_1', exp_2' =
      if not (are_ptrs_to_compatible_types typ_1 typ_2)
      then
        Casts.makeCastT ~e:exp_1 ~oldt:typ_1 ~newt:Cil.voidPtrType,
        Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:Cil.voidPtrType
      else exp_1, exp_2
    in
    Cil.intType,
    optional_constFoldBinOp loc false binop exp_1' exp_2' Cil.intType
  in
  let do_shift exp_1 typ_1 exp_2 typ_2 =
    match exp_1.enode with
    | StartOf lval ->
      let enode = AddrOf (Cil.addOffsetLval (Index (exp_2, NoOffset)) lval) in
      { exp_1 with enode }
    | _ ->
      let promoted_typ_2 = Cil.integralPromotion typ_2 in
      optional_constFoldBinOp loc false PlusPI exp_1
        (Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:promoted_typ_2) typ_1
  in
  match binop with
  | Mult | Div -> do_arithmetic ()

  | Mod | BAnd | BOr | BXor -> do_integral_arithmetic ()

  | Shiftlt | Shiftrt when Cil.msvcMode () ->
    (** ISO 6.5.7. Only integral promotions. The result has the same type as the
        left hand side. *)
    (* MSVC has a bug. We duplicate it here. *)
    do_integral_arithmetic ()

  | Shiftlt | Shiftrt ->
    assert (not (Cil.msvcMode ()));
    let typ_1' = Cil.integralPromotion typ_1 in
    let typ_2' = Cil.integralPromotion typ_2 in
    typ_1',
    optional_constFoldBinOp loc false binop
      (Casts.makeCastT ~e:exp_1 ~oldt:typ_1 ~newt:typ_1')
      (Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:typ_2')
      typ_1'

  | PlusA | MinusA
    when Cil.isArithmeticType typ_1 && Cil.isArithmeticType typ_2 ->
    do_arithmetic ()

  | Eq | Ne | Lt | Le | Ge | Gt
    when Cil.isArithmeticType typ_1 && Cil.isArithmeticType typ_2 ->
    do_arithmetic_comp ()

  | PlusA when Cil.isPointerType typ_1 && Cil.isIntegralType typ_2 ->
    typ_1,
    do_shift exp_1 typ_1 exp_2 typ_2

  | PlusA when Cil.isIntegralType typ_1 && Cil.isPointerType typ_2 ->
    typ_2,
    do_shift exp_2 typ_2 exp_1 typ_1

  | MinusA when Cil.isPointerType typ_1 && Cil.isIntegralType typ_2 ->
    typ_1,
    optional_constFoldBinOp loc false MinusPI exp_1
      (Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:(Cil.integralPromotion typ_2))
      typ_1

  | MinusA when Cil.isPointerType typ_1 && Cil.isPointerType typ_2 ->
    (* Following C11:6.5.6p3 :
       "For subtraction, one of the following shall hold: (...)
        - both operands are pointers to qualified or unqualified versions of
          compatible complete object types; (...)" *)
    if not (are_ptrs_to_compatible_types typ_1 typ_2) then
      Kernel.abort ~current:true
        "Subtraction of pointers to incompatible types: `%a' and `%a'"
        Printer.pp_typ typ_1 Printer.pp_typ typ_2;
    (* Following C11:footnote 106 :
       "Another way to approach pointer arithmetic is first to convert the
       pointer(s) to character pointer(s): In this scheme the integer expression
       added to or subtracted from the converted pointer is first multiplied by
       the size of the object originally pointed to, and the resulting pointer
       is converted back to the original type. For pointer subtraction, the
       result of the difference between the character pointers is similarly
       divided by the size of the object originally pointed to."
       NOTE: If the size of the "object originally pointed to" is zero then in
       order to move on with the subtraction we would have to perform a division
       by zero. Thus the behavior is undefined. In the standard there is in fact
       no way to have something of size zero (but the size of the void type is
       unknown, so definitely a problem arises if we subtract void pointers),
       however out of the standard we allow objects of size zero, namely
       zero-length arrays and empty structs, which are both GNU extensions. *)
    if isPointedTypeSizeZero typ_1 then
      Kernel.abort ~current:true
        "Subtraction of pointers to an object of size zero (type of the object \
         originally pointed to is `%a')" Printer.pp_typ typ_1;
    let common_typ = typ_1 in
    (* Following C11:6.5.6p9 :
       "When two pointers are subtracted (...) the size of the result is
       implementation-defined, and its type (a signed integer type) is ptrdiff_t
       defined in the <stddef.h> header." *)
    let result_typ = Cil.theMachine.Cil.ptrdiffType in
    result_typ,
    optional_constFoldBinOp loc false MinusPP
      (Casts.makeCastT ~e:exp_1 ~oldt:typ_1 ~newt:common_typ)
      (Casts.makeCastT ~e:exp_2 ~oldt:typ_2 ~newt:common_typ)
      result_typ

  (* Two special cases for comparisons with the NULL pointer. We are a bit
     more permissive. *)
  | Le | Lt | Ge | Gt | Eq | Ne
    when Cil.isPointerType typ_1 && Cil.isZero exp_2 ->
    pointer_comparison exp_1 typ_1 (makeCast ~e:exp_2 ~newt:typ_1) typ_1

  | Le | Lt | Ge | Gt | Eq | Ne
    when Cil.isPointerType typ_2 && Cil.isZero exp_1 ->
    pointer_comparison (makeCast ~e:exp_1 ~newt:typ_2) typ_2 exp_2 typ_2

  | Le | Lt | Ge | Gt | Eq | Ne
    when Cil.isPointerType typ_1 && Cil.isPointerType typ_2 ->
    pointer_comparison exp_1 typ_1 exp_2 typ_2

  | Eq | Ne | Le | Lt | Ge | Gt
    when Cil.isPointerType typ_1 && Cil.isArithmeticType typ_2
      || Cil.isArithmeticType typ_1 && Cil.isPointerType typ_2 ->
    Kernel.abort ~current:true
      "comparison between pointer and non-pointer (`%a' and `%a')"
      Printer.pp_typ typ_1 Printer.pp_typ typ_2

  | _ ->
    Kernel.abort ~current:true
      "invalid operands to binary expression (`%a' and `%a')"
      Printer.pp_typ typ_1 Printer.pp_typ typ_2

let check_arg_parmN function_varinfo parmN =
  (* Check that [parmN] indeed corresponds to the last variable in the
     prototype, check if it respects all the necessary conditions, and then
     drop it. *)

  (* Get the last formal: from the type and as a varinfo. *)
  let getLastFormalArg () =
    match !current_fun_fundec.svar.vtype with
    | TFun(_, Some args, _, _) -> begin
        match List.rev args with
        | last_formal :: _ -> Some last_formal
        | _ -> None
      end
    | _ -> None
  in
  let getLastFormalVarinfo () : varinfo option =
    let sformals = !current_fun_fundec.sformals in
    match List.rev sformals with
    | last_formal :: _ -> Some last_formal
    | _ -> None
  in
  let last_formal_arg = getLastFormalArg () in
  let last_formal_varinfo : varinfo option = getLastFormalVarinfo () in
  begin
    match last_formal_arg, last_formal_varinfo with
    | None, None ->
      Kernel.warning ~current:true
        "A variadic function should have at least \
         one non-variadic formal argument"
    | Some (last_formal_name, last_formal_typ, _last_formal_attrs),
      Some (last_formal_varinfo) ->
      let is_the_last_variable_name, has_register_storage_class =
        match (dropCasts parmN).enode with
        | Lval (Var parmN_varinfo, NoOffset) ->
          (* We have three sources of information:
             - [last_formal_arg] comes from the current function's type
               information (i.e. names and types of arguments),
             - [last_formal_varinfo] comes from the current function's formal
               arguments,
             - [parmN_varinfo] comes from the [va_start] macro's second
               argument. *)

          (* Debug printing... *)
          Kernel.debug
            "==== cabs2cil ====================== %s, \
             last_formal_name = %s, \
             last_formal_varinfo.(vname = %s, storage = [%a], typ = %a), \
             parmN_varinfo.(vname = %s, storage = [%a], typ = %a)"
            function_varinfo.vname
            last_formal_name
            last_formal_varinfo.vname
            Printer.pp_storage last_formal_varinfo.vstorage
            Printer.pp_typ last_formal_varinfo.vtype
            parmN_varinfo.vname
            Printer.pp_storage parmN_varinfo.vstorage
            Printer.pp_typ parmN_varinfo.vtype;

          parmN_varinfo.vname = last_formal_name,
          parmN_varinfo.vstorage = Register
        | _ -> false, false
      in
      (* Check if the parmN argument is the last formal
         argument of the variadic function. *)
      begin
        if not is_the_last_variable_name then
          Kernel.warning ~current:true
            "The second argument in call to %a should be \
             the last formal argument"
            Printer.pp_varinfo function_varinfo
      end;
      (* Check if the parmN argument does not have the 'register'
         storage class. *)
      begin
        (* This cannot actually happen! But the standard
           mentions this case explicitly, so we check it. *)
        if has_register_storage_class then
          Kernel.warning ~current:true
            "The second argument in call to %a should not be \
             declared with the 'register' storage class"
            Printer.pp_varinfo function_varinfo
      end;
      (* Check if the ^ argument is not of
         a function pointer type or an array type.*)
      begin
        match Cil.unrollType last_formal_typ with
        | TFun _ ->
          Kernel.warning ~current:true
            "The second argument in call to %a should not be of a function type"
            Printer.pp_varinfo function_varinfo
        | TArray _ ->
          (* This cannot actually happen! But the standard mentions this case
             explicitly, so we check it. *)
          Kernel.warning ~current:true
            "The second argument in call to %a should not be of an array type"
            Printer.pp_varinfo function_varinfo
        | _ -> ()
      end;
      (* Check if the [parmN] argument type is compatible with the type that
         results after application of default argument promotions. *)
      begin
        let typ = last_formal_typ in
        if Cil.isIntegralType typ then
          let promoted_typ = Cil.integralPromotion typ in
          let are_compatible = compatibleTypesp typ promoted_typ in
          if not are_compatible then
            Kernel.warning ~current:true
              "The type of the second argument in call to %a \
               (which is %a) should be compatible with the \
               type that results after application of the \
               default argument promotions (which is %a)"
              Printer.pp_varinfo function_varinfo
              Printer.pp_typ typ
              Printer.pp_typ promoted_typ
      end
    | _, _ ->
      (* The two sources of information about the function's arguments (i.e.
         the function's type obtained from [!current_fun_fundec.svar.vtype] and
         the actual formal arguments of the function obtained from
         [!current_fun_fundec.sformals]) should be coherent. *)
      assert false
  end

let rec compile_cond_exp ~ghost
    cond_exp_result then_chunk else_chunk : Chunk.t =
  (* ATTENTION: This function has some important OCaml-level side-effects!
     It modifies certain global variables. Never call it if you don't actually
     want to use its result, e.g. in the context of [what = AType] (i.e. when
     you only want to get the type of the expression). *)
  match cond_exp_result with
  | CEAnd (cond_exp_result_1, cond_exp_result_2) ->
    let location = Cil.CurrentLoc.get () in
    let (duplicable, else_chunk_1, else_chunk_2) =
      (* If [else_chunk] is small then will copy it. *)
      try (true, else_chunk, Chunk.duplicate else_chunk)
      with Failure _ ->
        let label = Labels.Manage.new_label_name "_LAND" in
        (false,
         Chunk.Make.goto_chunk ~ghost label location,
         Labels.Loops.cons_label ~ghost label else_chunk location false)
    in
    let then_chunk' =
      compile_cond_exp ~ghost cond_exp_result_2 then_chunk else_chunk_1
    in
    if not duplicable && !Parameters.doAlternateConditional then
      let then_chunk_falls_through =
        FallsThroughCanBreak.chunk_falls_through then_chunk'
      in
      (* if st does not fall through, we do not need to add a goto
         after the else part. This prevents spurious falls-through warning
         afterwards. *)
      let else_chunk' = Chunk.duplicate else_chunk_1 in
      let label = Labels.Manage.new_label_name "_LAND" in
      let goto_stmt_chunk =
        if then_chunk_falls_through
        then Chunk.Make.goto_chunk ~ghost label location
        else Chunk.Make.skip_chunk
      in
      let label_stmt_chunk =
        if then_chunk_falls_through
        then Labels.Loops.cons_label ~ghost label Chunk.empty location false
        else Chunk.Make.skip_chunk
      in
      let (@@) s1 s2 = s1 @@ (s2, ghost) in
      (compile_cond_exp ~ghost cond_exp_result_1 then_chunk' else_chunk')
      @@ goto_stmt_chunk @@ else_chunk_2 @@ label_stmt_chunk
    else
      let else_chunk' = else_chunk_2 in
      compile_cond_exp ~ghost cond_exp_result_1 then_chunk' else_chunk'

  | CEOr (cond_exp_result_1, cond_exp_result_2) ->
    let loc = Cil.CurrentLoc.get () in
    let (duplicable, then_chunk_1, then_chunk_2) =
      (* If st is small then will copy it. *)
      try (true, then_chunk, Chunk.duplicate then_chunk)
      with Failure _ ->
        let label = Labels.Manage.new_label_name "_LOR" in
        (false,
         Chunk.Make.goto_chunk ~ghost label loc,
         Labels.Loops.cons_label ~ghost label then_chunk loc false)
    in
    if not duplicable && !Parameters.doAlternateConditional then
      let then_chunk' = Chunk.duplicate then_chunk_1 in
      let else_chunk' =
        compile_cond_exp ~ghost cond_exp_result_2 then_chunk_1 else_chunk
      in
      let else_chunk_falls_through =
        FallsThroughCanBreak.chunk_falls_through else_chunk'
      in
      let label = Labels.Manage.new_label_name "_LOR" in
      let goto_stmt_chunk =
        if else_chunk_falls_through
        then Chunk.Make.goto_chunk ~ghost label loc
        else Chunk.Make.skip_chunk
      in
      let label_stmt_chunk =
        if else_chunk_falls_through
        then Labels.Loops.cons_label ~ghost
            label Chunk.empty (Cil.CurrentLoc.get ()) false
        else Chunk.Make.skip_chunk
      in
      let (@@) s1 s2 = s1 @@ (s2, ghost) in
      (compile_cond_exp ~ghost cond_exp_result_1 then_chunk' else_chunk')
      @@ goto_stmt_chunk @@ then_chunk_2 @@ label_stmt_chunk
    else
      let then_chunk' = then_chunk_1 in
      let false_chunk' =
        compile_cond_exp ~ghost cond_exp_result_2 then_chunk_2 else_chunk
      in
      compile_cond_exp ~ghost cond_exp_result_1 then_chunk' false_chunk'

  | CENot cond_exp_result ->
    compile_cond_exp ~ghost cond_exp_result else_chunk then_chunk

  | CEExp (chunk, exp) -> begin
      match exp.enode with
      | Const( CInt64(integer, _, _) )
        when (not (Integer.equal integer Integer.zero))
          && Chunk.can_drop else_chunk ->
        chunk @@ (then_chunk, ghost)
      | Const( CInt64(integer, _, _) )
        when (Integer.equal integer Integer.zero)
          && Chunk.can_drop then_chunk ->
        chunk @@ (else_chunk, ghost)
      | _ ->
        (Chunk.empty @@ (chunk, ghost)) @@
        (Chunk.Make.if_chunk ~ghost exp exp.eloc then_chunk else_chunk, ghost)
    end

(* Given some cv (i.e. const/volatile) attributes, convert them into named
   attributes for uniform processing. *)
let convert_cv_spec_to_attr (cv_specs : Cabs.cvspec list)
  : Cabs.attribute list =
  let attribute_of_cvspec cv_spec =
    let attribute_name =
      match cv_spec with
      | Cabs.CV_CONST -> "const"
      | Cabs.CV_VOLATILE -> "volatile"
      | Cabs.CV_RESTRICT -> "restrict"
      | Cabs.CV_ATOMIC -> "_Atomic"
      | Cabs.CV_ATTRIBUTE_ANNOT attr_annot -> Cil.mkAttrAnnot attr_annot
    in
    let attribute_args = [] in
    (attribute_name, attribute_args)
  in
  List.map attribute_of_cvspec cv_specs

let storage_cabs_to_cil cabs_storage =
  match cabs_storage with
  | Cabs.NO_STORAGE -> NoStorage
  | Cabs.AUTO -> NoStorage
  | Cabs.REGISTER -> Register
  | Cabs.STATIC -> Static
  | Cabs.EXTERN -> Extern

(********** THE MAIN MUTUALLY RECURSIVE FUNCTIONS GROUP **********)

(** Returns the base type, the storage, whether it is inline and the
    (unprocessed) attributes. *)
let rec do_spec_list ghost
    (suggested_anon_name : string) (* This string will be part of the names for
                                      anonymous structures and enums. *)
    (spec_elems : Cabs.spec_elem list)
  : typ * storage * bool * Cabs.attribute list =
  (* Do one element and collect the type specifiers. *)

  (* Collect the attributes.
     NOTE: Unfortunately, we cannot treat GCC __attributes__ and ANSI C
     const/volatile the same way, since they associate with structures
     differently. Specifically, ANSI qualifiers never apply to structures
     (ISO 6.7.3), whereas GCC attributes always do (GCC manual 4.30). Therefore,
     they are collected and processed separately. *)
  let attrs : Cabs.attribute list ref = ref [] in (* __attribute__, etc. *)
  let cv_attrs : Cabs.cvspec list ref = ref [] in (* const/volatile *)

  let type_specifiers, storage, is_inline =

    (* Flag set true if [inline] specifier appears. *)
    let is_inline = ref false in
    (* The storage is placed here. *)
    let storage : storage ref = ref NoStorage in

    (* Now scan the list and collect the type specifiers. Preserve the order. *)
    let type_specifiers =
      let do_spec_elem
          (spec_elem : Cabs.spec_elem)
          (type_specifiers : Cabs.typeSpecifier list)
        : Cabs.typeSpecifier list =
        match spec_elem with
        | Cabs.SpecTypedef -> type_specifiers
        | Cabs.SpecInline -> is_inline := true; type_specifiers
        | Cabs.SpecStorage storage_spec ->
          if !storage <> NoStorage then
            Kernel.error ~once:true ~current:true "Multiple storage specifiers";
          storage := storage_cabs_to_cil storage_spec;
          type_specifiers
        | Cabs.SpecCV cv_spec ->
          cv_attrs := cv_spec :: !cv_attrs; type_specifiers
        | Cabs.SpecAttr attribute ->
          attrs := attribute :: !attrs; type_specifiers
        | Cabs.SpecType type_specifier ->
          type_specifier :: type_specifiers
        | Cabs.SpecPattern _ ->
          Kernel.abort ~current:true "SpecPattern in cabs2cil input"
      in
      List.fold_right do_spec_elem spec_elems []
    in

    (* GCC allows a named type that appears first to be followed by things
       like "short", "signed", "unsigned", or "long". *)
    let type_specifiers =
      let is_short_or_long : Cabs.typeSpecifier -> bool = function
        | Cabs.Tshort | Cabs.Tlong -> true
        | _ -> false
      in
      match type_specifiers with
      | Cabs.Tnamed _ :: (_ :: _ as following_type_specifiers)
        when Cil.gccMode ()
          && List.exists is_short_or_long following_type_specifiers ->
        (* If rest contains "short" or "long" then drop the [Tnamed]. *)
        following_type_specifiers
      | _ -> type_specifiers
    in

    let type_specifiers =
      match spec_elems, List.rev type_specifiers with
      | Cabs.SpecTypedef :: _, Cabs.Tnamed _ :: [] -> type_specifiers
      | Cabs.SpecTypedef :: _, Cabs.Tnamed _ :: rest -> List.rev rest
      | _ -> type_specifiers
    in

    (* Sort the type specifiers. *)
    let sorted_type_specifiers =
      let type_specifier_order = function (* Don't change this! *)
        | Cabs.Tvoid -> 0
        | Cabs.Tsigned -> 1
        | Cabs.Tunsigned -> 2
        | Cabs.Tchar -> 3
        | Cabs.Tshort -> 4
        | Cabs.Tlong -> 5
        | Cabs.Tint -> 6
        | Cabs.Tint64 -> 7
        | Cabs.Tint128 -> 8
        | Cabs.Tfloat -> 9
        | Cabs.Tdouble -> 10
        | _ -> 11 (* There should be at most one of the others. *)
      in
      let compare_type_specifiers type_specifier_1 type_specifier_2 =
        Datatype.Int.compare
          (type_specifier_order type_specifier_1)
          (type_specifier_order type_specifier_2)
      in
      List.stable_sort compare_type_specifiers type_specifiers
    in

    sorted_type_specifiers, !storage, !is_inline
  in

  let get_type_attrs () : Cabs.attribute list =
    (* Partitions the attributes in [!attrs].
       - Type attributes are removed from [attrs] and returned, so that they can
         go into the type definition.
       - Name attributes are left in [attrs], so they will be returned by
         [doSpecAttr] and used in the variable declaration.
       Test case: [small1/attr9.c] *)
    let name_attributes, function_attributes, type_attributes =
      cabs_partition_attributes ghost ~default_attr_class:Cil.AttrType !attrs
    in
    attrs := name_attributes; (* Save the name attributes for later. *)
    assert (function_attributes = []);
    if function_attributes <> [] then
      Kernel.error ~once:true ~current:true
        "Invalid position for function type attributes.";
    type_attributes
  in

  (* And now try to make sense of it. See ISO 6.7.2 *)
  let base_type =
    match type_specifiers with
    | [Cabs.Tvoid] -> TVoid []
    | [Cabs.Tchar] -> TInt(IChar, [])
    | [Cabs.Tbool] -> TInt(IBool, [])
    | [Cabs.Tsigned; Cabs.Tchar] -> TInt(ISChar, [])
    | [Cabs.Tunsigned; Cabs.Tchar] -> TInt(IUChar, [])

    | [Cabs.Tshort] -> TInt(IShort, [])
    | [Cabs.Tsigned; Cabs.Tshort] -> TInt(IShort, [])
    | [Cabs.Tshort; Cabs.Tint] -> TInt(IShort, [])
    | [Cabs.Tsigned; Cabs.Tshort; Cabs.Tint] -> TInt(IShort, [])

    | [Cabs.Tunsigned; Cabs.Tshort] -> TInt(IUShort, [])
    | [Cabs.Tunsigned; Cabs.Tshort; Cabs.Tint] -> TInt(IUShort, [])

    | [] -> TInt(IInt, [])
    | [Cabs.Tint] -> TInt(IInt, [])
    | [Cabs.Tsigned] -> TInt(IInt, [])
    | [Cabs.Tsigned; Cabs.Tint] -> TInt(IInt, [])

    | [Cabs.Tunsigned] -> TInt(IUInt, [])
    | [Cabs.Tunsigned; Cabs.Tint] -> TInt(IUInt, [])

    | [Cabs.Tlong] -> TInt(ILong, [])
    | [Cabs.Tsigned; Cabs.Tlong] -> TInt(ILong, [])
    | [Cabs.Tlong; Cabs.Tint] -> TInt(ILong, [])
    | [Cabs.Tsigned; Cabs.Tlong; Cabs.Tint] -> TInt(ILong, [])

    | [Cabs.Tunsigned; Cabs.Tlong] -> TInt(IULong, [])
    | [Cabs.Tunsigned; Cabs.Tlong; Cabs.Tint] -> TInt(IULong, [])

    | [Cabs.Tlong; Cabs.Tlong] -> TInt(ILongLong, [])
    | [Cabs.Tsigned; Cabs.Tlong; Cabs.Tlong] -> TInt(ILongLong, [])
    | [Cabs.Tlong; Cabs.Tlong; Cabs.Tint] -> TInt(ILongLong, [])
    | [Cabs.Tsigned; Cabs.Tlong; Cabs.Tlong; Cabs.Tint] -> TInt(ILongLong, [])

    | [Cabs.Tunsigned; Cabs.Tlong; Cabs.Tlong] -> TInt(IULongLong, [])
    | [Cabs.Tunsigned; Cabs.Tlong; Cabs.Tlong; Cabs.Tint] ->
      TInt(IULongLong, [])

    (* [int64] is to support MSVC. *)
    | [Cabs.Tint64] -> TInt(ILongLong, [])
    | [Cabs.Tsigned; Cabs.Tint64] -> TInt(ILongLong, [])

    | [Cabs.Tunsigned; Cabs.Tint64] -> TInt(IULongLong, [])

    (* [int128] is to support GCC TImode. *)
    | [Cabs.Tint128]
      when Cil.(theMachine.theMachine.has_int128) -> TInt(IInt128, [])
    | [Cabs.Tsigned; Cabs.Tint128]
      when Cil.(theMachine.theMachine.has_int128) -> TInt(IInt128, [])
    | [Cabs.Tunsigned; Cabs.Tint128]
      when Cil.(theMachine.theMachine.has_int128) -> TInt(IUInt128, [])

    | [Cabs.Tfloat] -> TFloat(FFloat, [])
    | [Cabs.Tdouble] -> TFloat(FDouble, [])

    | [Cabs.Tlong; Cabs.Tdouble] -> TFloat(FLongDouble, [])

    (* Now the other type specifiers. *)
    | [Cabs.Tnamed "__builtin_va_list"]
      when Cil.(theMachine.theMachine.has__builtin_va_list) ->
      TBuiltin_va_list []
    | [Cabs.Tnamed "__fc_builtin_size_t"] -> Cil.(theMachine.typeOfSizeOf)
    | [Cabs.Tnamed type_name] ->
      begin
        match lookup_type "type" type_name with
        | (TNamed _) as named_typ, _ -> named_typ
        | _ -> Kernel.fatal ~current:true
                 "Named type %s is not mapped correctly" type_name
      end

    | [Cabs.Tstruct (struct_name, None, _)] ->
      (* A reference to a struct *)
      if struct_name = "" then
        Kernel.error ~once:true ~current:true
          "Missing struct tag on incomplete struct";
      find_comp_type "struct" struct_name []

    | [Cabs.Tstruct (struct_name, Some nglist, extra_attrs)] ->
      (* A definition of a struct *)
      let struct_name' =
        match struct_name with
        | "" -> AnonStructName.get "struct" suggested_anon_name
        | _ -> struct_name
      in
      (* Use the (non-cv, non-name) attributes in [!attrs] now. *)
      let attributes =
        let attributes = extra_attrs @ (get_type_attrs ()) in
        do_attributes ghost attributes
      in
      make_comp_type ghost true
        struct_name' ~norig:struct_name nglist attributes

    | [Cabs.Tunion (union_name, None, _)] ->
      (* A reference to a union. *)
      if union_name = "" then
        Kernel.error ~once:true ~current:true
          "Missing union tag on incomplete union";
      find_comp_type "union" union_name []

    | [Cabs.Tunion (union_name, Some nglist, extra_attrs)] ->
      (* A definition of a union. *)
      let union_name' =
        match union_name with
        | "" -> AnonStructName.get "union" suggested_anon_name
        | _ -> union_name
      in
      (* Use the attributes now *)
      let attributes' =
        let attributes = extra_attrs @ (get_type_attrs ()) in
        do_attributes ghost attributes
      in
      make_comp_type ghost false
        union_name' ~norig:union_name nglist attributes'

    | [Cabs.Tenum (enum_name, None, _)] ->
      (* A reference to an enum. *)
      if enum_name = "" then
        Kernel.error ~once:true ~current:true
          "Missing enum tag on incomplete enum";
      find_comp_type "enum" enum_name []

    | [Cabs.Tenum (name, Some enum_items, extraAttrs)] ->
      (* A definition of an enum *)
      let name' =
        match name with
        | "" -> AnonStructName.get "enum" suggested_anon_name
        | _ -> name
      in
      (* Make a new name for this enumeration. *)
      let name'', _  = newAlphaName true "enum" name' in

      (* Create the enuminfo, or use one that was created already for a forward
         reference *)
      let enuminfo, _ = CompInfo.createEnumInfo name'' ~norig:name in
      let attributes = extraAttrs @ (get_type_attrs ()) in
      enuminfo.eattr <- do_attributes ghost attributes;
      let result_typ = TEnum (enuminfo, []) in
      let smallest = ref Integer.zero in
      let largest = ref Integer.zero in
      (* Life is fun here.
         ANSI says: enum constants are ints, and there is an
         implementation-dependent underlying integer type for the enum, which
         must be capable of holding all the enum's values.
         For MSVC, we follow these rules and assume the enum's underlying type
         is int.
         GCC allows enum constants that do not fit in int: the enum constant's
         type is the smallest type (but at least int) that will hold the value,
         with a preference for unsigned types.
         The underlying type EI of the enum is picked as follows:
         - let T be the smallest integer type that holds all the enum's
           values; T is signed if any enum value is negative, unsigned
           otherwise,
         - if the enum is packed or sizeof(T) >= sizeof(int), then EI = T;
         - otherwise EI = int if T is signed, and unsigned int otherwise.
         Note that these rules make the enum unsigned if possible. *)
      let update_enum i : ikind =
        if Integer.lt i !smallest then smallest := i;
        if Integer.gt i !largest then largest := i;
        match Cil.msvcMode () with
        | true -> IInt
        | false ->
          begin
            match Kernel.Enums.get () with
            (* [gcc-short-enum] will try to pack the enum _type_, not the enum
               constant... *)
            | "" | "help" | "gcc-enums" | "gcc-short-enums" ->
              if Cil.fitsInInt IInt i then IInt
              else if Cil.fitsInInt IUInt i then IUInt
              else if Cil.fitsInInt ILongLong i then ILongLong
              else IULongLong
            | "int" -> IInt
            | s -> Kernel.fatal "Unknown enums representations '%s'" s
          end
      in
      (* As each name,value pair is determined, this is called. *)
      let rec process_name enumitem_name (enumitem_value : exp) loc fields =
        (* Add the name to the environment, but with a faked 'typ' field;
           we don't know the full type yet (since that includes all of the
           tag values), but we won't need them in here. *)

        (* Add this tag to the list so that it ends up in the real
           environment when we're finished. *)
        let new_name, _  = newAlphaName true "" enumitem_name in
        let enumitem = { eiorig_name = enumitem_name;
                         einame = new_name;
                         eival = enumitem_value;
                         eiloc = loc;
                         eihost = enuminfo }
        in
        add_local_to_env enumitem_name (EnvEnum enumitem);
        let field = (enumitem_name, enumitem) in
        let incremented_enumitem_value = Cil.increm enumitem_value 1 in
        field :: loop incremented_enumitem_value fields

      and loop enumitem_value fields =
        match fields with
        | [] -> []
        | (enumitem_name, { Cabs.expr_node = Cabs.NOTHING; _ }, cloc)
          :: fields' ->
          (* Use the passed-in 'i' as the value, since none specified. *)
          process_name enumitem_name enumitem_value cloc fields'

        | (kname, enumitem_value_exp, cloc) :: fields' ->
          (* constant-eval 'e' to determine tag value *)
          let enumitem_value = get_int_const_exp ghost enumitem_value_exp in
          let enumitem_value =
            match Cil.constFoldToInt enumitem_value with
            | None ->
              Kernel.abort ~current:true
                "expression '%a' is not an integer constant"
                Printer.pp_exp enumitem_value
            | Some i ->
              let ikind = update_enum i in
              if Cil.(theMachine.lowerConstants) then
                Cil.kinteger64
                  ~loc:enumitem_value_exp.Cabs.expr_loc
                  ~kind:ikind
                  i
              else enumitem_value
          in
          process_name kname enumitem_value cloc fields'
      in

      (* TODO: find a better loc. *)
      let fields =
        let zero = Cil.zero ~loc:(Cil.CurrentLoc.get()) in
        (* TODO: This nightmarish pair of recursive funcitons should be written
           better... *)
        loop zero enum_items
      in

      (* Now set the right set of items. *)
      enuminfo.eitems <- List.map (fun (_, enumitem) -> enumitem) fields;
      (* Pick the enum's kind - see discussion above. *)
      begin
        let unsigned = Integer.ge !smallest Integer.zero in
        let real_ikind =
          let small_ikind = Cil.intKindForValue !smallest unsigned in
          let large_ikind = Cil.intKindForValue !largest unsigned in
          if (Cil.bytesSizeOfInt small_ikind) > (Cil.bytesSizeOfInt large_ikind)
          then small_ikind
          else large_ikind
        in
        let ikind =
          match Kernel.Enums.get () with
          | "" | "help" | "gcc-enums" ->
            if Cil.hasAttribute "packed" enuminfo.eattr
            || Cil.bytesSizeOfInt real_ikind >= Cil.bytesSizeOfInt IInt
            then real_ikind
            else if unsigned then IUInt else IInt
          | "int" ->
            if Cil.bytesSizeOfInt real_ikind > Cil.bytesSizeOfInt IInt
            then
              Kernel.abort ~current:true
                "enum %s must fit in \"int\" representation."
                enuminfo.ename
            else if unsigned then IUInt else IInt
          | "gcc-short-enums" -> real_ikind
          | s -> Kernel.fatal "Unknown enum representation '%s'" s
        in
        enuminfo.ekind <- ikind;
      end;
      (* Record the enum name in the environment *)
      add_local_to_env (kind_plus_name "enum" name') (EnvTyp result_typ);
      (* And define the tag *)
      Globals.cabsPushGlobal (GEnumTag (enuminfo, Cil.CurrentLoc.get ()));
      result_typ

    | [Cabs.TtypeofE exp] ->
      let (_, _, exp', typ) =
        do_expression (ghost_local_env ghost)
          DontExpectConst exp AExpLeaveArrayFun
      in
      let typ' =
        match exp'.enode with
        (* If this is a string literal, then we treat it as in [sizeof]. *)
        | Const (CStr string_const_val) -> begin
            match Cil.typeOf exp' with
            | TPtr(base_type, _) -> (* This is the type of array elements. *)
              TArray(base_type,
                     Some (Cil.new_exp ~loc:exp'.eloc
                             (SizeOfStr string_const_val)),
                     Cil.empty_size_cache (),
                     [])
            | _ ->
              Kernel.abort ~current:true
                "The typeOf a string is not a pointer type"
          end
        | _ -> typ
      in
      typ'

    | [Cabs.TtypeofT (specs, dt)] ->
      do_only_type ghost specs dt

    | [Cabs.Tatomic (specs, dt)] ->
      Cil.typeAddAttributes
        [ Attr ("_Atomic", []) ]
        (do_only_type ghost specs dt)

    | type_specifiers ->
      Kernel.abort ~current:true
        "invalid combination of type specifiers:@ %a"
        (Pretty_utils.pp_list ~sep:"@ " Cprint.print_type_spec) type_specifiers;
  in
  let attributes = List.rev (!attrs @ (convert_cv_spec_to_attr !cv_attrs)) in
  (base_type, storage, is_inline, attributes)


and make_varinfo_cabs
    ~(ghost : bool)
    ~(is_formal : bool)
    ~(is_global : bool)
    ?(is_generated=false)
    (decl_location : location)
    (base_type, storage, is_inline, attributes)
    (name, decl_type, attributes')
  : varinfo =

  let typ, attributes =
    do_type ghost is_formal (Cil.AttrName false)
      ~allowVarSizeArrays:is_formal  (* For locals we handle variable-size
                                        arrays before [make_varinfo_cabs];
                                        for formals we do it afterwards. *)
      base_type (Cabs.PARENTYPE (attributes, decl_type, attributes'))
  in

  if is_inline && not (Cil.isFunctionType typ) then
    Kernel.error ~once:true ~current:true "inline for a non-function: %s" name;

  let typ =
    (* Sometimes we call this on the formal argument of a function with no
       arguments. Do not call [stripConstLocalType] in that case. *)
    match is_global, is_formal with
    | false, false -> Cil.stripConstLocalType typ
    | _ -> typ
  in

  (* Prepare the varinfo. *)
  let varinfo =
    Cil.makeVarinfo ~temp:is_generated is_global is_formal name typ
  in
  varinfo.vstorage <- storage;
  varinfo.vattr <- attributes;
  varinfo.vdecl <- decl_location;
  varinfo.vghost <- ghost;
  varinfo.vinline <- is_inline;

  varinfo

(* Process a local variable declaration and allow variable-sized arrays. *)
and make_variable_size_varinfo ghost
    (decl_location : location) spec_results (name, decl_type, attributes)
  : varinfo * Chunk.t * exp * bool =
  match Cil.msvcMode () with
  | false ->
    begin
      match is_variable_sized_array ghost decl_type with
      | None ->
        let chunk = Chunk.empty in
        let length_exp = Cil.zero ~loc:decl_location in
        make_varinfo_cabs ~ghost ~is_formal:false ~is_global:false
          decl_location spec_results (name, decl_type, attributes),
        chunk, length_exp, false
      | Some (decl_type', chunk, length_exp) ->
        make_varinfo_cabs ~ghost ~is_formal:false ~is_global:false
          decl_location spec_results (name, decl_type', attributes),
        chunk, length_exp, true
    end
  | true ->
    let chunk = Chunk.empty in
    let length_exp = Cil.zero ~loc:decl_location in
    make_varinfo_cabs ~ghost ~is_formal:false ~is_global:false
      decl_location spec_results (name, decl_type, attributes),
    chunk, length_exp, false

and do_attr (ghost : bool) (cabs_attribute : Cabs.attribute) : attribute list =
  (* Strip optional leading and trailing underscore pairs. *)
  (* Quoting GCC documentation, 'Declaring Attributes of Functions':
     "You may also specify attributes with '__' preceding and following each
      keyword.  This allows you to use them in header files without being
      concerned about a possible macro of the same name." *)
  let strip_underscore (attr_string : string) : string =
    let length = String.length attr_string in
    let first =
      if length > 1
      && String.sub attr_string 0 2 = "__"
      then 2
      else 0
    in
    let last =
      if length > first + 1
      && String.sub attr_string (length - 2) 2 = "__"
      then length - 2
      else length
    in
    String.sub attr_string first (last - first)
  in
  match cabs_attribute with
  | ("__attribute__", []) -> [] (* An empty list of gcc attributes. *)
  | (attr_string, []) ->
    let attr_string = strip_underscore attr_string in
    let attribute =
      match attribute_annotation attr_string with
      | None -> Attr(attr_string, [])
      | Some s -> AttrAnnot s
    in
    [ attribute ]
  | (attr_string, cabs_exps) ->

    let rec attrparam_of_cabs_exp ?(foldenum=true) ?(strip=false)
        (cabs_exp : Cabs.expression) : attrparam =
      let prepare_variable_name variable_name =
        if strip
        then strip_underscore variable_name
        else variable_name
      in
      let loc = cabs_exp.Cabs.expr_loc in
      match cabs_exp.Cabs.expr_node with
      | Cabs.VARIABLE variable_name ->
        begin
          let variable_name = prepare_variable_name variable_name in
          (* See if this is an enumeration. *)
          try
            if not foldenum then raise Not_found;
            match Hashtbl.find env variable_name with
            | EnvEnum enumitem, _ ->
              begin
                match Cil.constFoldToInt enumitem.eival with
                | Some integer
                  when Cil.(theMachine.lowerConstants) -> AInt integer
                |  _ -> raise Not_found
              end
            | _ -> raise Not_found
          with Not_found -> ACons (variable_name, [])
        end
      | Cabs.CONSTANT (Cabs.CONST_STRING string_constant) ->
        AStr string_constant
      | Cabs.CONSTANT (Cabs.CONST_INT int_constant) ->
        begin
          match (Cil.parseIntExp ~loc int_constant).enode with
          | Const (CInt64 (v64, _, _)) -> AInt v64
          | _ ->
            Kernel.fatal ~current:true
              "Invalid attribute constant: %s" int_constant
        end
      | Cabs.CALL
          (Cabs.{ expr_node = Cabs.VARIABLE variable_name; _ }, args_exps) ->
        let variable_name = prepare_variable_name variable_name in
        let attrparams =
          List.map (fun cabs_exp -> attrparam_of_cabs_exp cabs_exp) args_exps
        in
        ACons(variable_name, attrparams)
      | Cabs.EXPR_SIZEOF cabs_exp ->
        ASizeOfE (attrparam_of_cabs_exp cabs_exp)
      | Cabs.TYPE_SIZEOF (specifier, decl_type) ->
        ASizeOf (do_only_type ghost specifier decl_type)
      | Cabs.EXPR_ALIGNOF cabs_exp ->
        AAlignOfE (attrparam_of_cabs_exp cabs_exp)
      | Cabs.TYPE_ALIGNOF (specifier, decl_type) ->
        AAlignOf (do_only_type ghost specifier decl_type)
      | Cabs.BINARY(Cabs.AND, cabs_exp_1, cabs_exp_2) ->
        ABinOp (LAnd,
                attrparam_of_cabs_exp cabs_exp_1,
                attrparam_of_cabs_exp cabs_exp_2)
      | Cabs.BINARY(Cabs.OR, cabs_exp_1, cabs_exp_2) ->
        ABinOp (LOr,
                attrparam_of_cabs_exp cabs_exp_1,
                attrparam_of_cabs_exp cabs_exp_2)
      | Cabs.BINARY (cabs_binop, cabs_exp_1, cabs_exp_2) ->
        ABinOp (Utility.convBinOp cabs_binop,
                attrparam_of_cabs_exp cabs_exp_1,
                attrparam_of_cabs_exp cabs_exp_2)
      | Cabs.UNARY(Cabs.PLUS, cabs_exp) ->
        attrparam_of_cabs_exp cabs_exp
      | Cabs.UNARY(Cabs.MINUS, cabs_exp) ->
        AUnOp (Neg, attrparam_of_cabs_exp cabs_exp)
      | Cabs.UNARY(Cabs.BNOT, cabs_exp) ->
        AUnOp (BNot, attrparam_of_cabs_exp cabs_exp)
      | Cabs.UNARY(Cabs.NOT, cabs_exp) ->
        AUnOp (LNot, attrparam_of_cabs_exp cabs_exp)
      | Cabs.MEMBEROF (cabs_exp, s) ->
        ADot (attrparam_of_cabs_exp cabs_exp, s)
      | Cabs.PAREN cabs_exp ->
        attrparam_of_cabs_exp ~strip ~foldenum cabs_exp
      | Cabs.UNARY(Cabs.MEMOF, cabs_exp) ->
        AStar (attrparam_of_cabs_exp cabs_exp)
      | Cabs.UNARY(Cabs.ADDROF, cabs_exp) ->
        AAddrOf (attrparam_of_cabs_exp cabs_exp)
      | Cabs.MEMBEROFPTR (cabs_exp, s) ->
        ADot (AStar (attrparam_of_cabs_exp cabs_exp), s)
      | Cabs.INDEX (cabs_exp_1, cabs_exp_2) ->
        AIndex (attrparam_of_cabs_exp cabs_exp_1,
                attrparam_of_cabs_exp cabs_exp_2)
      | Cabs.QUESTION(cabs_exp_1, cabs_exp_2, cabs_exp_3) ->
        AQuestion (attrparam_of_cabs_exp cabs_exp_1,
                   attrparam_of_cabs_exp cabs_exp_2,
                   attrparam_of_cabs_exp cabs_exp_3)
      | _ ->
        Kernel.fatal ~current:true
          "cabs2cil: invalid expression in attribute: %a"
          Cprint.print_expression cabs_exp
    in

    (* Sometimes we need to convert attrarg into attr. *)
    let attribute_of_attrparam attrparam : attribute =
      let encode_variable varinfo =
        (* Encode a variable v into the attribute 'sizeof(void [v])'.
           This hack needs to be reversed by the pretty-printer and by every
           plugin needing to access the variable.
           TODO: extend the attribute AST to allow C expressions directly. *)
        ASizeOf (TArray
                   (TVoid [],
                    Some (Cil.evar varinfo),
                    Cil.empty_size_cache (),
                    []))
      in
      match attrparam with
      | ACons ("cleanup" as cleanup_string, attrparams) when Cil.gccMode () ->
        begin
          match attrparams with
          | [ACons (identifier, extended_cleanup_attrparams)] ->
            (* GCC cleanup attributes have extended_cleanup == []. *)
            begin
              match Hashtbl.find env identifier with
              | EnvVar varinfo, _ when Cil.isFunctionType varinfo.vtype ->
                begin
                  match extended_cleanup_attrparams with
                  | [] -> Attr (cleanup_string, [encode_variable varinfo])
                  | attrparam :: _ ->
                    Kernel.abort ~current:true
                      "expected no argument for %s attribute function \
                       but got \'%a\'"
                      cleanup_string Printer.pp_attrparam attrparam
                end
              | EnvVar varinfo, _ ->
                assert (not (Cil.isFunctionType varinfo.vtype));
                Kernel.abort ~current:true
                  "expecting a valid function for %s attribute but got \'%s\'"
                  cleanup_string identifier
              | _ ->
                Kernel.abort ~current:true
                  "expecting a function name for %s attribute but got \'%s\'"
                  cleanup_string identifier
              | exception Not_found ->
                Kernel.abort ~current:true
                  "expecting a function name for %s attribute but got \
                   unbound identifier \'%s\'"
                  cleanup_string identifier
            end;
          | _ ->
            Kernel.abort ~current:true
              "expecting exactly one argument for %s attribute but got %d"
              cleanup_string (List.length attrparams)
        end

      | ACons ("tis_cleanup" as tis_cleanup_string, attrparams) ->
        begin
          match attrparams with
          | [ACons (identifier, extended_cleanup_attrparams)] ->
            begin
              match Hashtbl.find env identifier with
              | EnvVar varinfo, _ when Cil.isFunctionType varinfo.vtype ->
                begin
                  match extended_cleanup_attrparams with
                  | [] -> Attr (tis_cleanup_string, [encode_variable varinfo])
                  | (AInt _ as len_attrparam)::_ ->
                    Attr (tis_cleanup_string,
                          [encode_variable varinfo; len_attrparam])
                  | attrparam :: _ ->
                    Kernel.abort ~current:true
                      "expecting one integer or zero argument for %s \
                       attribute function but got \'%a\'"
                      tis_cleanup_string Printer.pp_attrparam attrparam
                end
              | EnvVar varinfo, _ ->
                assert (not (Cil.isFunctionType varinfo.vtype));
                Kernel.abort ~current:true
                  "expecting a valid function for %s \
                   attribute but got \'%s\'"
                  tis_cleanup_string identifier
              | _ ->
                Kernel.abort ~current:true
                  "expecting a function name for %s attribute but got \'%s\'"
                  tis_cleanup_string identifier
              | exception Not_found ->
                Kernel.abort ~current:true
                  "expecting a function name for %s attribute but got \
                   unbound identifier \'%s\'"
                  tis_cleanup_string identifier
            end;
          | _ ->
            Kernel.abort ~current:true
              "expecting exactly one argument for %s attribute but got %d"
              tis_cleanup_string (List.length attrparams)
        end

      | ACons (s, args) ->  Attr (s, args)

      | _ ->
        Kernel.abort ~current:true
          "invalid form of attribute: '%a'"
          Printer.pp_attrparam attrparam;
    in

    let foldenum = false in
    match attr_string with
    | "__attribute__" (* Just a wrapper for many attributes. *)
    | "__blockattribute__" -> (* Another wrapper. *)
      let strip = true in
      List.map
        (fun cabs_exp ->
           let attrparam = attrparam_of_cabs_exp ~strip ~foldenum cabs_exp in
           attribute_of_attrparam attrparam)
        cabs_exps
    | "__cleanup__" ->
      (* Need to apply the cleanup attribute transformation correctly... *)
      let strip = true in
      List.map
        (fun cabs_exp ->
           let attrparam = attrparam_of_cabs_exp ~strip ~foldenum cabs_exp in
           attribute_of_attrparam (ACons ("cleanup", [attrparam])))
        cabs_exps
    | "__tis_cleanup__" ->
      (* Need to apply the cleanup attribute transformation correctly... *)
      let strip = true in
      List.map
        (fun cabs_exp ->
           let attrparam = attrparam_of_cabs_exp ~strip ~foldenum cabs_exp in
           attribute_of_attrparam (ACons("tis_cleanup", [attrparam])))
        cabs_exps
    | "__declspec" ->
      let strip = false in
      List.map
        (fun cabs_exp ->
           let attrparam = attrparam_of_cabs_exp ~strip ~foldenum cabs_exp in
           attribute_of_attrparam attrparam)
        cabs_exps
    | _ ->
      let strip = false in
      [Attr(strip_underscore attr_string,
            List.map (attrparam_of_cabs_exp ~strip ~foldenum) cabs_exps)]

and do_attributes (ghost : bool) (attributes : Cabs.attribute list)
  : attribute list =
  let attribute_f attributes_acc attribute =
    Utility.cabsAddAttributes (do_attr ghost attribute) attributes_acc
  in
  List.fold_left attribute_f [] attributes

(* A version of [Cil.partitionAttributes] that works on CABS attributes. It
   would be better to use [Cil.partitionAttributes] instead to avoid the extra
   [do_attr] conversions here, but that's hard to do in [do_spec_list].*)
and cabs_partition_attributes ghost
    ~(default_attr_class : Cil.attributeClass)
    (attributes : Cabs.attribute list)
  : Cabs.attribute list * Cabs.attribute list * Cabs.attribute list =
  let attribute_f
      (name_attributes_acc, function_attributes_acc, type_attributes_acc)
      attribute =
    let attribute_class =
      match do_attr ghost attribute with
      | [] -> default_attr_class
      | (Attr(attribute_name, _) | AttrAnnot attribute_name) :: _ ->
        try Cil.attributeClass attribute_name
        with Not_found -> default_attr_class
    in
    match attribute_class with
    | Cil.AttrName _ -> (attribute :: name_attributes_acc,
                         function_attributes_acc,
                         type_attributes_acc)
    | Cil.AttrType -> (name_attributes_acc,
                       function_attributes_acc,
                       attribute :: type_attributes_acc)
  in
  List.fold_left attribute_f ([], [], []) attributes

and do_type (ghost : bool) (is_fun_arg : bool)
    (name_or_typedef : Cil.attributeClass)
    (** This is:
        - [AttrName] if we are doing the type for a name,
        - or [AttrType] if we are doing this type in a [typedef]. *)
    ?(allowZeroSizeArrays=false) ?(allowVarSizeArrays=false)
    (base_type : typ) decl_type
  (* Returns the new type and the accumulated name (or type attribute
     if [name_or_typedef] = [AttrType]) attributes. *)
  : typ * attribute list =

  (* Now do the declarator type. But remember that the structure of the
     declarator type is as printed, meaning that it is the reverse of the
     right one. *)
  let rec do_decl_type (base_type : typ) (attribute_acc : attribute list)
    : Cabs.decl_type -> typ * attributes =
    function
    | Cabs.JUSTBASE -> base_type, attribute_acc

    | Cabs.PARENTYPE (cabs_attributes_1, decl_type, cabs_attributes_2) ->
      let name_attributes_1, type_attributes_1 =
        let attributes_1 = do_attributes ghost cabs_attributes_1 in
        Cil.partitionAttributes ~default:Cil.AttrType attributes_1
      in
      let name_attributes_2, type_attributes_2 =
        let attributes_2 = do_attributes ghost cabs_attributes_2 in
        Cil.partitionAttributes ~default:name_or_typedef attributes_2
      in
      let base_type =
        Utility.cabsTypeAddAttributes type_attributes_1 base_type
      in
      (* Now recurse. *)
      let result_typ, result_name_attributes =
        do_decl_type base_type attribute_acc decl_type
      in
      (* Add some more type attributes. *)
      let result_typ = cabsTypeAddAttributes type_attributes_2 result_typ in
      let result_name_attributes =
        Utility.cabsAddAttributes name_attributes_1
          (Utility.cabsAddAttributes name_attributes_2 result_name_attributes)
      in
      (* Now add the name attributes and return. *)
      result_typ, result_name_attributes

    | Cabs.PTR (cabs_attributes, decl_type) ->
      let attributes = do_attributes ghost cabs_attributes in
      let name_attributes, type_attributes =
        Cil.partitionAttributes ~default:Cil.AttrType attributes
      in
      (* Now recurse. *)
      let result_typ, result_name_attributes =
        do_decl_type (TPtr (base_type, type_attributes)) attribute_acc decl_type
      in
      let result_name_attributes =
        Utility.cabsAddAttributes name_attributes result_name_attributes
      in
      (* Now add the name attributes and return. *)
      result_typ, result_name_attributes

    | Cabs.ARRAY (decl_type, cabs_attributes, length_cabs_exp) ->
      if Cil.isFunctionType base_type then
        (* TODO: Shouldn't it be an abort? *)
        Kernel.error ~once:true ~current:true
          "declaration of array of function type `%a'"
          Printer.pp_typ base_type
      else
      if not (Cil.isCompleteType ~allowZeroSizeArrays:true base_type) then
        if is_fun_arg then begin
          if !current_language <> CPlusPlus then
            (* C99:6.7.5.3p4, C11:6.7.6.3p4:
               "The parameters in a parameter type list in a function
                declarator (...) shall not have incomplete type." *)
            Kernel.warning ~once:true ~current:true
              "function parameter should not have incomplete type `%a'"
              Printer.pp_typ base_type
        end else
          Kernel.error ~once:true ~current:true
            "declaration of array of incomplete type `%a'"
            Printer.pp_typ base_type
      else if Cil.isNestedStructWithFlexibleArrayMemberType base_type then
        (* Following C11:6.7.2.1p3 :
           "(...) the last member of a structure (...) may have incomplete array
           type; such a structure (and any union containing, possibly
           recursively, a member that is such a structure) shall not be (...) an
           element of an array." *)
        Kernel.error ~once:true ~current:true
          "declaration of array of structure type with \
           flexible array member '%a`"
          Printer.pp_typ base_type
      else
      if not allowZeroSizeArrays
      && not (Cil.isCompleteType ~allowZeroSizeArrays:false base_type)
      then
        (* Because we tested previously for incomplete types and now tested
           again forbidding zero-length arrays, bt is necessarily a zero-length
           array. *)
        if Cil.gccMode () || Cil.msvcMode ()
        then
          Kernel.warning ~once:true ~current:true
            "declaration of array of 'zero-length arrays' (`%a');@ \
             zero-length arrays are a compiler extension"
            Printer.pp_typ base_type
        else
          Kernel.error ~once:true ~current:true
            "declaration of array of 'zero-length arrays' (`%a');@ \
             zero-length arrays are not allowed in C99"
            Printer.pp_typ base_type;
      let length_exp_option =
        match length_cabs_exp.Cabs.expr_node with
        | Cabs.NOTHING -> None
        | _ ->
          (* Check that [length_cabs_exp] is a constant expression.
             We used to also cast the length to int here, but that's
             theoretically too restrictive on 64-bit machines. *)
          let length_exp =
            do_pure_exp (ghost_local_env ghost) length_cabs_exp
          in
          if not (Cil.isIntegralType (Cil.typeOf length_exp)) then
            Kernel.abort ~current:true
              "size of array `%a' has non-integer type `%a'"
              Printer.pp_exp length_exp
              Printer.pp_typ (Cil.typeOf length_exp);
          if not allowVarSizeArrays then begin
            (* Assert that [length_exp] is a constant. *)
            let constant_length_exp = Cil.constFold true length_exp in
            match constant_length_exp.enode with
            | Const (CInt64 (i, _, _)) when Integer.lt i Integer.zero ->
              Kernel.error ~once:true ~current:true
                "Length of array is negative"
            | Const (CInt64 _) -> ()
            | _ when Cil.isConstant constant_length_exp ->
              (* e.g., there may be a float constant involved.
                 We'll leave it to the user to ensure the length is
                 non-negative, etc.*)
              Kernel.warning ~once:true ~current:true
                "Unable to do constant-folding on array length %a. \
                 Some CIL operations on this array may fail."
                Printer.pp_exp constant_length_exp
            | _ ->
              Kernel.error ~once:true ~current:true
                "Length of array is not a constant: %a"
                Printer.pp_exp constant_length_exp
          end;
          Some length_exp
      in
      let attributes = do_attributes ghost cabs_attributes in
      if not is_fun_arg
      && Cil.hasAttribute "static" attributes
      then
        Kernel.error ~once:true ~current:true
          "static specifier inside array argument is allowed only in \
           function argument";
      let base_type =
        TArray (base_type,
                length_exp_option,
                Cil.empty_size_cache (),
                attributes)
      in
      do_decl_type base_type attribute_acc decl_type

    | Cabs.PROTO (decl_type, cabs_args_names, is_variadic) ->
      (* Start a scope for the parameter names. *)
      enter_scope ();
      (* Intercept the old-style use of [varargs.h]:
         - On GCC this means that we have ellipsis and a last argument
           "builtin_va_alist: builtin_va_alist_t";
         - on MSVC we do not have the ellipsis and we have a last argument
           "va_alist: va_list". *)
      let cabs_args_names, is_variadic =
        if cabs_args_names != []
        && Cil.msvcMode () = not is_variadic
        then
          match List.rev cabs_args_names with
          | ([Cabs.SpecType (Cabs.Tnamed arg_type_name)],
             (arg_name, Cabs.JUSTBASE, [], _))
            :: reversed_remaining_cabs_args_names
            when isOldStyleVarArgTypeName arg_type_name
              && isOldStyleVarArgName arg_name ->
            List.rev reversed_remaining_cabs_args_names, true
          | _ -> cabs_args_names, is_variadic
        else cabs_args_names, is_variadic
      in
      (* Make the argument as for a formal *)
      let do_one_arg (cabs_specifier, (name, decl_type, attributes, cloc))
        : varinfo =
        let specifier = do_spec_list ghost name cabs_specifier in
        let varinfo =
          make_varinfo_cabs ~ghost ~is_formal:true ~is_global:false
            cloc specifier (name, decl_type, attributes)
        in
        (* Add the formal to the environment, so it can be referenced by other
           formals  (e.g. in an array type, although that will be changed to a
           pointer later, or though [typeof]).  *)
        add_local_to_env varinfo.vname (EnvVar varinfo);
        varinfo
      in
      let arguments_varinfos_option : varinfo list option =
        match List.map do_one_arg cabs_args_names with
        | [] -> None (* No argument list. *)
        | [varinfo] when Cil.isVoidType varinfo.vtype -> Some []
        | varinfos -> Some varinfos
      in
      exit_scope ();
      (* Turn [] types into pointers in the arguments and the result type.
         Turn function types into pointers to respective. This simplifies
         our life a lot, and is what the standard requires. *)
      let turn_array_into_pointer (base_type : typ)
          (array_length_option : exp option) (attributes : attributes) : typ =
        let _real_attributes = Cil.dropAttribute "static" attributes in
        let attributes : attributes =
          match array_length_option with
          | None -> []
          | Some attributes_exps ->
            let static_attribute : attribute list =
              if Cil.hasAttribute "static" attributes
              then [Attr ("static", [])]
              else []
            in
            (* Transform the length into an attribute expression. *)
            try
              let attrparam = Cil.expToAttrParam attributes_exps in
              Attr ("arraylen", [attrparam]) :: static_attribute
            with Cil.NotAnAttrParam _ ->
              Kernel.warning ~once:true ~current:true
                "Cannot represent the length '%a'of array as an attribute"
                Printer.pp_exp attributes_exps;
              static_attribute (* Leave unchanged. *)
        in
        TPtr(base_type, attributes)
      in
      let rec fixup_argument_types
          (argument_index : int) (argument_varinfos : varinfo list) : unit =
        match argument_varinfos with
        | [] -> ()
        | argument_varinfo :: remaining_argument_varinfos ->
          begin
            match Cil.unrollType argument_varinfo.vtype with
            | TArray(base_type, array_length_option, _, attributes) ->
              (* Note that for multi-dimensional arrays we strip off only
                 the first [TArray] and leave bt alone. *)
              let real_type =
                turn_array_into_pointer base_type array_length_option attributes
              in
              Cil.update_var_type argument_varinfo real_type
            | TFun _ ->
              let real_type = TPtr (argument_varinfo.vtype, []) in
              Cil.update_var_type argument_varinfo real_type
            | TComp (_, _, _) ->
              begin
                match Utility.isTransparentUnion argument_varinfo.vtype with
                | None ->  ()
                | Some fieldinfo ->
                  let new_transparent_union_arg =
                    (argument_index, argument_varinfo.vtype)
                  in
                  TransparentUnionArgs.add new_transparent_union_arg;
                  Cil.update_var_type argument_varinfo fieldinfo.ftype;
              end
            | _ -> ()
          end;
          fixup_argument_types (argument_index + 1) remaining_argument_varinfos
      in
      let args =
        match arguments_varinfos_option with
        | None -> None
        | Some arguments_varinfos ->
          fixup_argument_types 0 arguments_varinfos;
          let argument_varinfo_f varinfo =
            (varinfo.vname, varinfo.vtype, varinfo.vattr)
          in
          Some (List.map argument_varinfo_f arguments_varinfos)
      in
      let result_typ =
        let return_type =
          match Cil.unrollType base_type with
          | TArray(base_type, array_length_option, _, attributes) ->
            turn_array_into_pointer base_type array_length_option attributes
          | _ -> base_type
        in
        TFun (return_type, args, is_variadic, [])
      in
      do_decl_type result_typ attribute_acc decl_type

  in
  do_decl_type base_type [] decl_type

(* If this is a declarator for a variable size array then turn it into a
   pointer type and a length. *)
and is_variable_sized_array ghost (decl_type : Cabs.decl_type)
  : (Cabs.decl_type * Chunk.t * exp) option =
  let result = ref None in
  let decl_type' =
    let rec find_array = function
      | Cabs.JUSTBASE -> Cabs.JUSTBASE
      | Cabs.PARENTYPE (pre_attributes, decl_type', post_attributes) ->
        Cabs.PARENTYPE (pre_attributes, find_array decl_type', post_attributes)
      | Cabs.ARRAY (Cabs.JUSTBASE, attributes, length_option)
        when length_option.Cabs.expr_node != Cabs.NOTHING ->
        (* Try to compile the expression to a constant. *)
        let (_, chunk, exp, _) =
          (* NOTE: Actually we do not try to compile the expression to a
                   constant, but if it happens to be a constant we want to
                   simplify it. *)
          let as_const = DontExpectConstButSimplifyIfPossible in
          let action = AExp (Some Cil.intType) in
          do_expression (ghost_local_env ghost) as_const length_option action
        in
        if Chunk.is_not_empty chunk || not (Cil.isConstant exp) then begin
          result := Some (chunk, exp);
          Cabs.PTR (attributes, Cabs.JUSTBASE)
        end else
          Cabs.ARRAY (Cabs.JUSTBASE, attributes, length_option)
      | Cabs.ARRAY (decl_type', attributes, length_option) ->
        Cabs.ARRAY (find_array decl_type', attributes, length_option)
      | Cabs.PTR (attributes, decl_type') ->
        Cabs.PTR (attributes, find_array decl_type')
      | Cabs.PROTO (decl_type', f, a) ->
        Cabs.PROTO (find_array decl_type', f, a)
    in
    find_array decl_type
  in
  match !result with
  | None -> None
  | Some (chunk, exp) -> Some (decl_type', chunk, exp)

and do_only_type ghost
    (spec_elems : Cabs.spec_elem list) (decl_type : Cabs.decl_type) : typ =
  let base_type, storage, is_inline, attributes =
    let suggested_anon_name = "" in
    do_spec_list ghost suggested_anon_name spec_elems
  in
  if storage <> NoStorage || is_inline then
    Kernel.error ~once:true ~current:true
      "Storage or inline specifier in type only";
  let result_type, name_attributes =
    let decl_type' = Cabs.PARENTYPE(attributes, decl_type, []) in
    do_type ghost false Cil.AttrType base_type decl_type'
  in
  if name_attributes <> [] then
    Kernel.error ~once:true ~current:true "Name attributes in only_type: %a"
      Printer.pp_attributes name_attributes;
  result_type


and make_comp_type ghost (is_struct: bool)
    (name : string) ~(norig : string) (field_groups : Cabs.field_group list)
    (a : attribute list) =
  (* Make a new name for the structure *)
  let kind = if is_struct then "struct" else "union" in
  let name', _  = newAlphaName true kind name in
  (* The number of named fields of this composite type (useful later). *)
  let named_members_count = namedMembersCount field_groups in
  (* Create the self cell for use in fields and forward references. Or maybe
   * one exists already from a forward reference  *)
  let compinfo, _ = CompInfo.createCompInfo is_struct name' ~norig in
  let do_field_group is_the_last_field_group
      ((spec_elems : Cabs.spec_elem list),
       (nl : (Cabs.name * Cabs.expression option) list)) =
    (* Do the specifiers exactly once. *)
    let base_type, storage, is_inline, attributes =
      let suggested_anon_name =
        match nl with
        | [] -> ""
        | ((name, _, _, _), _) :: _ -> name
      in
      do_spec_list ghost suggested_anon_name spec_elems
    in
    (* Do the fields. *)
    let make_fieldinfo is_the_last_field_in_group
        (((n, ndt, a, cloc) : Cabs.name),
         (width_option : Cabs.expression option))
      : fieldinfo =
      (* Is it the last field of this composite type? *)
      let is_the_last_field =
        is_the_last_field_group && is_the_last_field_in_group
      in
      if storage <> NoStorage || is_inline then
        Kernel.error ~once:true ~current:true
          "Storage or inline not allowed for fields";
      (* Following C11:6.7.2.1p3 :
         "A structure or union shall not contain a member with incomplete or
         function type (hence, a structure shall not contain an instance of
         itself, but may contain a pointer to an instance of itself), except
         that the last member of a structure with more than one named member may
         have incomplete array type; such a structure (and any union containing,
         possibly recursively, a member that is such a structure) shall not be a
         member of a structure or an element of an array." *)
      let allowZeroSizeArrays = true in
      let ftype, nattr =
        do_type
          ~allowZeroSizeArrays ghost false (Cil.AttrName false) base_type
          (Cabs.PARENTYPE (attributes, ndt, a))
      in
      (* Is the type circular (i.e. an object of this type would contain an
         instance of itself)? *)
      if comp_used_in_typ compinfo ftype then
        Kernel.abort ~current:true
          "type %s %s is circular"
          (if compinfo.cstruct then "struct" else "union")
          compinfo.cname;
      (* Is the field type complete? (This also warns again about circularity.)

         Exception, following C11:6.7.2.1p18 :
         "As a special case, the last element of a structure with more than one
         named member may have an incomplete array type; this is called a
         flexible array member." *)
      begin
        let typ = Cil.unrollType ftype in
        let more_than_one_named_member = named_members_count > 1 in
        match typ, is_struct, is_the_last_field, more_than_one_named_member with
        (* OK! Correct flexible array member. *)
        | TArray(_, None, _, _), true, true, true -> ()
        (* ERROR! Flexible array member in a structure with not enough named
           members. *)
        | TArray(_, None, _, _), true, true, false ->
          Kernel.error ~current:true
            "field %s is declared with incomplete array type %a (flexible \
             array member is not allowed in otherwise empty structure)"
            n Printer.pp_typ ftype
        (* ERROR! Flexible array member in the middle of a structure. *)
        | TArray(_, None, _, _), true, false, _ ->
          Kernel.error ~current:true
            "field %s is declared with incomplete array type %a (flexible \
             array member is only allowed at the end of the structure)"
            n Printer.pp_typ ftype
        (* ERROR! Flexible array member in a union. *)
        | TArray(_, None, _, _), false, _, _ ->
          Kernel.error ~current:true
            "field %s is declared with incomplete array type %a (flexible \
             array member is not allowed in a union)"
            n Printer.pp_typ ftype
        (* OK! Field has complete type. *)
        | _ when Cil.isCompleteType ~allowZeroSizeArrays ftype -> ()
        (* ERROR! Field has incomplete type and it is not a flexible array
           member. *)
        | _ -> Kernel.error ~current:true
                 "field %s is declared with incomplete type %a"
                 n Printer.pp_typ ftype
      end; (* Now we know that either the field's type is complete or it is a
              correct flexible array member. *)
      (* Is the field type a function type? *)
      if Cil.isFunctionType ftype then
        Kernel.error ~current:true
          "field %s is declared with function type %a"
          n Printer.pp_typ ftype;
      (* Is the structure's field type a structure type with a flexible array
         member? *)
      if is_struct && Cil.isNestedStructWithFlexibleArrayMemberType ftype then
        Kernel.error ~current:true
          "field %s is declared with structure type with a flexible array \
           member (or a union type containing, possibly recursively, such a \
           stucture type) %a"
          n Printer.pp_typ ftype;
      (* Bit-field *)
      let width, ftype =
        match width_option with
        | None -> None, ftype
        | Some width_exp ->
          begin
            begin
              match Cil.unrollType ftype with
              | TInt (_, _) -> ()
              | TEnum _ -> ()
              | _ ->
                Kernel.error ~once:true ~current:true
                  "Base type for bit-field is not an integer type"
            end;
            (* Following C11:6.7.2.1p5 :
               "The expression that specifies the width of a bit-field shall
               be an integer constant expression with a nonnegative value that
               does not exceed the width of an object of the type that would
               be specified were the colon and expression omitted. If the
               value is zero, the declaration shall have no declarator." *)
            begin
              (* Is the width a constant expression? *)
              match is_int_constant ghost width_exp with
              | None ->
                Kernel.abort ~current:true
                  "bit-field width is not an integer constant"
              | Some bitfield_width ->
                (* Is the width nonnegative? *)
                if Integer.lt bitfield_width Integer.zero then
                  Kernel.error ~current:true
                    "bit-field width (%a) is negative"
                    (Integer.pretty ~hexa:false) bitfield_width;
                (* Isn't the bit-field too wide? *)
                let bitfield_width =
                  let ftype_width = Cil.bitsSizeOf ftype in
                  if Integer.gt bitfield_width (Integer.of_int ftype_width) then
                  begin
                    if !current_language <> CPlusPlus then
                      Kernel.error ~current:true
                      "bit-field width (%a) exceeds the width of an object \
                       of the type %a (%d)"
                      (Integer.pretty ~hexa:false) bitfield_width
                      Printer.pp_typ ftype ftype_width
                    else
                      (* Following N3376 [class.bit]p1:
                         "The value of the integral constant expression may be
                         larger than the number of bits in the object
                         representation of the bit-field's type; in such cases
                         the extra bits are used as padding bits and do not
                         participate in the value representation of the
                         bit-field."
                         Note that we currently ignore the extra bits rather
                         than keep them as padding. *)
                      Kernel.warning ~current:true
                      "bit-field width (%a) exceeds the width of an object \
                       of the type %a (%d), truncating it"
                      (Integer.pretty ~hexa:false) bitfield_width
                      Printer.pp_typ ftype ftype_width;
                    Integer.of_int ftype_width
                  end else
                    bitfield_width
                in
                (* Zero-width case: if the width is zero, the declaration should
                   have no declarator. In practice it boils down to checking if
                   the declaration has an identifier. The only possibility of a
                   valid declarator that does not contain an identifier are
                   'const' or 'volatile' type qualifiers, and both are accepted
                   by GCC and Clang in this case. *)
                if (Integer.is_zero bitfield_width)
                && n <> Cil.missingFieldName then
                  Kernel.error ~current:true
                    "named bit-field width is equal to zero";
                (* All checked! Add the bit-field attribute with an appropriate
                   width to the field's type. *)
                let ftype' =
                  Cil.typeAddAttributes
                    [Attr (Cil.bitfield_attribute_name,
                           [AInt bitfield_width])]
                    ftype
                in
                Some (Integer.to_int bitfield_width), ftype'
            end
          end
      in
      (* If the field is unnamed and its type is a structure of union type then
         give it a distinguished name. *)
      let name' =
        if n = Cil.missingFieldName then
          match Cil.unrollType ftype with
          | TComp _ -> CompField.nextAnonCompFieldNameWithId ()
          | _ -> n
        else n
      in
      { fcomp = compinfo;
        forig_name = n;
        fname = name';
        ftype = ftype;
        fbitfield = width;
        fattr = nattr;
        floc = cloc;
        faddrof = false;
        fsize_in_bits = None;
        foffset_in_bits = None;
        fpadding_in_bits = None;
      }
    in
    Extlib.map_flag_last make_fieldinfo nl
  in

  (* Do regular fields first. *)
  let fieldinfos =
    let fields =
      Extlib.filter_map''
        (function
          | Cabs.FIELD (specifier, fields) -> Some (specifier, fields)
          | Cabs.TYPE_ANNOT _ -> None)
        field_groups
    in
    List.concat (Extlib.map_flag_last do_field_group fields)
  in

  if compinfo.cfields <> [] then begin
    (* This appears to be a multiply defined structure. This can happen from
       a construct like "typedef struct foo { ... } A, B;". This is dangerous
       because at the time B is processed some forward references in { ... }
       appear as backward references, which could lead to circularity in
       the type structure. We do a tourough check and then we reuse the type
       for A *)
    if List.length compinfo.cfields <> List.length fieldinfos
    || List.exists2 (fun fieldinfo_1 fieldinfo_2 ->
           not (Cil_datatype.Typ.equal fieldinfo_1.ftype fieldinfo_2.ftype))
         compinfo.cfields fieldinfos
    then
      Kernel.error ~once:true ~current:true
        "%s seems to be multiply defined" (Cil.compFullName compinfo)
  end else
    compinfo.cfields <- fieldinfos;

  (*  ignore (E.log "makeComp: %s: %a\n" comp.cname d_attrlist a); *)
  compinfo.cattr <- Pragma.add_packing_attributes compinfo a;
  let result_type = TComp (compinfo, Cil.empty_size_cache (), []) in

  (* Now check that each field is uniquely named. *)
  let fields_table = Hashtbl.create 7 in
  List.iter
    (fun field ->
       if field.fname <> "" then
         if Hashtbl.mem fields_table field.fname then
           Kernel.error
             ~source:(fst field.floc) "duplicate member %a in %a"
             Printer.pp_varname field.fname
             Printer.pp_typ (TComp (compinfo, Cil.empty_size_cache (), []))
         else
           Hashtbl.add fields_table field.fname field)
    fieldinfos;

  (* This compinfo is defined, even if there are no fields *)
  compinfo.cdefined <- true;
  (* Create a typedef for this one *)
  Globals.cabsPushGlobal (GCompTag (compinfo, Cil.CurrentLoc.get ()));

  (* There must be a self cell created for this already *)
  add_local_to_env (kind_plus_name kind name) (EnvTyp result_type);
  (* Now create a typedef with just this type *)
  result_type

and preprocess_cast ghost
    (specifier : Cabs.specifier)
    (decl_type : Cabs.decl_type)
    (init_expression : Cabs.init_expression)
  : Cabs.specifier * Cabs.decl_type * Cabs.init_expression =
  let typ = do_only_type ghost specifier decl_type in
  (* If we are casting to a union type then we have to treat this as a
     constructor expression. This is to handle the gcc extension that allows
     cast from a type of a field to the type of the union. *)
  (* However, it may just be casting of a whole union to its own type. We will
     resolve this later, when we'll convert casts to unions. *)
  let init_expression' =
    match Cil.unrollType typ, init_expression with
    | TComp (c, _, _), Cabs.SINGLE_INIT _ when not c.cstruct ->
      Cabs.COMPOUND_INIT
        [(Cabs.INFIELD_INIT ("___matching_field", Cabs.NEXT_INIT),
          init_expression)]
    | _ -> init_expression
  in
  (* If the specifier contains a definition of a composite type, we want to
     avoid treating it a second time when processing the specifier again.
     Both in case of an unnamed composite and named composite definitions, we
     remove the fields definition part of each concerned specifier element and
     we leave just the composite's name (in case of an unnamed composite it is
     the, just attributed, "__anon*" name).
     E.g.:
     + named:   [struct s {int x;}] will be replaced by [struct s],
     + unnamed: [struct {int x;}] will be replaced by [struct __anonstruct_1].
  *)
  let specifier' =
    match typ with
    | TComp (compinfo, _, _)
    | TPtr (TComp (compinfo, _, _), _) ->
      let spec_elem_f = function
        (* Unnamed composite definition. *)
        | Cabs.SpecType (Cabs.Tstruct ("", _, [])) ->
          Cabs.SpecType (Cabs.Tstruct (compinfo.cname, None, []))
        | Cabs.SpecType (Cabs.Tunion ("", _, [])) ->
          Cabs.SpecType (Cabs.Tunion (compinfo.cname, None, []))
        (* Named composite definition. *)
        | Cabs.SpecType (Cabs.Tstruct (name, _, []))
          when name = compinfo.cname ->
          Cabs.SpecType (Cabs.Tstruct (compinfo.cname, None, []))
        | Cabs.SpecType (Cabs.Tunion (name, _, []))
          when name = compinfo.cname ->
          Cabs.SpecType (Cabs.Tunion (compinfo.cname, None, []))
        (* Other specifier elements. *)
        | spec_elem -> spec_elem
      in
      List.map spec_elem_f specifier
    | _ -> specifier
  in
  specifier', decl_type, init_expression'

and get_int_const_exp ghost (expression : Cabs.expression) : exp =
  let loc = expression.Cabs.expr_loc in
  let (_, chunk, exp, _) =
    let action = AExp None in
    do_expression (ghost_local_env ghost) ExpectConst expression action
  in
  if not (Chunk.is_empty chunk) then
    Kernel.error ~once:true ~current:true "Constant expression %a has effects"
      Printer.pp_exp exp;
  match exp.enode with
  (* First, filter those [Const] expressions that are integers. *)
  | Const (CInt64 _ ) | Const (CEnum _) -> exp
  | Const (CChr char_const) ->
    let int_constant = Cil.charConstToIntConstant char_const in
    Cil.new_exp ~loc (Const int_constant)
  (* Other [Const] expressions are not ok. *)
  | Const _ ->
    Kernel.fatal ~current:true "Expected integer constant and got %a"
      Printer.pp_exp exp
  (* Now, anything else that [do_expression true] returned is ok (provided that
     it did not yield side effects); this includes, in particular, the various
     [sizeof] and [alignof] expression kinds. *)
  | _ -> exp

and is_int_constant ghost (expression : Cabs.expression) : Integer.t option =
  let action = AExp None in
  match do_expression (ghost_local_env ghost) ExpectConst expression action with
  | (_, chunk, exp, _) when Chunk.is_empty chunk -> Cil.constFoldToInt exp
  | _ -> None

(* Process an expression and in the process do some type checking, extract the
   side effects as separate statements.
   [do_expression] returns the following 4-uple:
   - a list of read accesses performed for the evaluation of the expression,
   - a chunk representing side-effects occuring during evaluation,
   - the CIL expression,
   - its type. *)
and do_expression local_env
    (as_const : as_const) (* This expression is used as a constant *)
    (expression : Cabs.expression)
    (action : exp_action)
  : lval list * Chunk.t * exp * typ =
  let ghost = local_env.is_ghost in
  let loc = expression.Cabs.expr_loc in
  (* Will be reset at the end of the compilation of current expression. *)
  let old_loc = Cil.CurrentLoc.get () in
  Cil.CurrentLoc.set loc;
  (* A subexpression of array type is automatically turned into [StartOf(e)].
     Similarly an expression of function type is turned into [AddrOf].
     So [do_expression] should never return things of type [TFun] or [TArray]. *)
  let process_array_fun exp typ =
    let loc = exp.eloc in
    match exp.enode, Cil.unrollType typ with
    | (Lval lval | CastE (_, { enode = Lval lval; _ })),
      TArray (base_type, _, _, attributes) ->
      mkStartOfAndMark loc lval, TPtr(base_type, attributes)
    | (Lval lval | CastE (_, { enode = Lval lval; _ })), TFun _  ->
      mkAddrOfAndMark loc lval, TPtr(typ, [])
    | _, (TArray _ | TFun _) ->
      Kernel.fatal ~current:true
        "Array or function expression is not lval: %a@\n"
        Printer.pp_exp exp
    | _ -> exp, typ
  in
  (* Before we return we call finish_exp *)
  let finish_exp ?(newWhat=action) reads chunk exp typ =
    match newWhat with
    | ADrop | AType ->
      let (exp', typ') = process_array_fun exp typ in
      (reads, chunk, exp', typ')
    | AExpLeaveArrayFun ->
      (* It is important that we do not do call [process_array_fun] in this
         case. We exploit this when we process the [typeOf] construct. *)
      (reads, chunk, exp, typ)
    | AExp _ ->
      let (exp', typ') = process_array_fun exp typ in
      checkVoidLval exp' typ';
      (reads, chunk, exp', typ')
    | ASet (is_real_write, lval, lval_reads, lval_typ) -> begin
        (* See if the set was done already. *)
        match exp.enode with
        | Lval lval' when lval == lval' ->
          (* If this is the case, the side effects have also been taken into
             account in the chunk. *)
          (reads, chunk, exp, typ)
        | _ ->
          let (typ', exp') =
            let (exp', typ') = process_array_fun exp typ in
            Casts.castTo typ' lval_typ exp'
          in
          checkVoidLval exp' typ';
          let writes' =
            match is_real_write with
            | true -> [lval]
            | false -> []
          in
          let reads' = [] in (* the reads are incorporated in the chunk. *)
          let effects' =
            Chunk.make_effects
              ~modified:writes'
              ~writes:writes'
              ~reads:(List.filter
                        (fun lval' ->
                           not (Cil_datatype.LvalStructEq.equal lval' lval))
                        lval_reads @ reads)
          in
          let chunk' =
            ((Chunk.unspecified_chunk Chunk.empty) @@
             (Chunk.remove_reads lval chunk, ghost))
            +++
            (Cil.mkStmtOneInstr ~ghost (Set(lval, exp', Cil.CurrentLoc.get ())),
             effects')
          in
          (reads', chunk', exp', typ')
      end
  in
  let result =
    match expression.Cabs.expr_node with
    | Cabs.PAREN _ -> Kernel.fatal ~current:true "stripParen"
    | Cabs.NOTHING when action = ADrop ->
      finish_exp [] (Chunk.unspecified_chunk Chunk.empty)
        (Cil.integer ~loc 0) Cil.intType
    | Cabs.NOTHING ->
      let result_exp = Cil.new_exp ~loc (Const (CStr "exp_nothing")) in
      let result_typ = Cil.typeOf result_exp in
      finish_exp [] (Chunk.unspecified_chunk Chunk.empty) result_exp result_typ
    (* Do the potential lvalues first. *)
    | Cabs.VARIABLE variable_name -> begin
        (* Look up in the environment *)
        try
          let envdata = Hashtbl.find env variable_name in
          match envdata with
          | EnvVar varinfo, _ ->
            let lval = Cil.var varinfo in
            let reads =
              (* Always allow to read the address of an array, as it will never
                 be written to: no read/write interference is possible. *)
              if Cil.isArrayType varinfo.vtype
              || Cil_datatype.Lval.Set.mem lval local_env.authorized_reads
              then []
              else [lval]
            in
            let chunk = (Chunk.unspecified_chunk Chunk.empty) in
            let exp = Cil.new_exp ~loc (Lval lval) in
            let typ = dropQualifiers varinfo.vtype in
            finish_exp reads chunk exp typ
          | EnvEnum enumitem, _ ->
            let typ = Cil.typeOf enumitem.eival in
            let reads = [] in
            let chunk = Chunk.unspecified_chunk Chunk.empty in
            let exp =
              if Cil.theMachine.Cil.lowerConstants
              then enumitem.eival
              else Cil.new_exp ~loc (Const (CEnum enumitem))
            in
            finish_exp reads chunk exp typ
          | _ -> raise Not_found
        with Not_found ->
        match isOldStyleVarArgName variable_name with
        | true ->
          Kernel.abort ~current:true
            "Cannot resolve identifier `%s'. This could be a CIL bug due to \
             the handling of old-style variable argument functions"
            variable_name
        | false ->
          Kernel.abort ~current:true
            "Cannot resolve identifier `%s'" variable_name
      end
    | Cabs.INDEX (exp_1, exp_2) -> begin
        (* Recall that [do_expression] turns arrays into [StartOf] pointers. *)
        let action = AExp None in
        let (reads_1, chunk_1, exp_1, typ_1) =
          do_expression local_env DontExpectConst exp_1 action
        in
        let (reads_2, chunk_2, exp_2, typ_2) =
          do_expression local_env DontExpectConst exp_2 action
        in
        let chunk = chunk_1 @@ (chunk_2, ghost) in
        let (exp_1, typ_1, exp_2, result_typ) =
          (* Either [exp_1] or [exp_2] can be the pointer. *)
          match Cil.unrollType typ_1, Cil.unrollType typ_2 with
          | TPtr (pointed_typ_1, _), (TInt _ | TEnum _) ->
            exp_1, typ_1, exp_2, pointed_typ_1
          | (TInt _| TEnum _), TPtr(pointed_typ_2, _) ->
            exp_2, typ_2, exp_1, pointed_typ_2
          | _ ->
            Kernel.abort ~current:true
              "Expecting exactly one pointer type in array access %a[%a] (%a \
               and %a)"
              Printer.pp_exp exp_1 Printer.pp_exp exp_2
              Printer.pp_typ typ_1 Printer.pp_typ typ_2
        in
        (* We have to distinguish the construction based on
           the type of [exp_1]. *)
        let result_lval =
          match exp_1.enode with
          | StartOf array_lval -> (* A real array indexing operation. *)
            Cil.addOffsetLval (Index (exp_2, NoOffset)) array_lval
          | _ -> (* Turn into *(e1 + e2) *)
            let addr_exp =
              let exp_node = BinOp (IndexPI, exp_1, exp_2, typ_1) in
              Cil.new_exp ~loc:exp_1.eloc exp_node
            in
            Cil.mkMem ~addr:addr_exp ~off:NoOffset
        in
        (* Do some optimization of [StartOf]. *)
        let reads =
          (* Read on array addresses are allowed; since array addresses cannot
             be written there are no risks of read/write interferences. *)
          let reads = reads_1 @ reads_2 in
          if Cil_datatype.Lval.Set.mem result_lval local_env.authorized_reads
          || Cil.isArrayType result_typ
          then reads
          else result_lval :: reads
        in
        let result_exp = Cil.new_exp ~loc (Lval result_lval) in
        let result_typ = dropQualifiers result_typ in
        finish_exp reads chunk result_exp result_typ
      end
    | Cabs.UNARY (Cabs.MEMOF, exp) ->
      if as_const = ExpectConst then
        Kernel.warning ~current:true "MEMOF in a constant";
      let (reads, chunk, exp, typ) =
        let action = AExp None in
        do_expression local_env DontExpectConst exp action
      in
      let result_typ =
        match Cil.unrollType typ with
        | TPtr(pointed_typ, _) -> pointed_typ
        | _ ->
          Kernel.abort ~current:true
            "Expecting a pointer type in * operator's operand. Got `%a'."
            Printer.pp_typ typ
      in
      let result_lval = Cil.mkMem ~addr:exp ~off:NoOffset in
      let reads =
        if Cil_datatype.Lval.Set.mem result_lval local_env.authorized_reads
        then reads
        else result_lval :: reads
      in
      let result_exp = Cil.new_exp ~loc (Lval result_lval) in
      let result_typ = dropQualifiers result_typ in
      finish_exp reads chunk result_exp result_typ

    (* [exp.field_name] = [(& exp + offset(field_name))]
       If [exp = (be + beoff)]
       then [exp.field_name] = [(be + beoff + off(field_name))] *)
    | Cabs.MEMBEROF (exp, field_name) ->
      (* [MEMBEROF] is actually allowed if we only take the address. *)
      let (reads, chunk, exp, typ) =
        do_expression local_env DontExpectConst exp (AExp None)
      in
      let lval =
        match exp.enode with
        | Lval lval -> lval
        | CastE (_, { enode = Lval lval; _ }) -> lval
        | _ ->
          Kernel.fatal ~current:true
            "Expected an lval in MEMBEROF (field %s)" field_name
      in
      (* We are not reading the whole [lval], just a chunk of it. *)
      let reads =
        List.filter
          (fun lval' -> not (Cil_datatype.Lval.equal lval' lval)) reads
      in
      let lval =
        let field_offset =
          match Cil.unrollType typ with
          | TComp (compinfo, _, _) ->
            CompField.findField field_name compinfo.cfields
          | _ -> Kernel.abort ~current:true
                   "expecting a struct with field %s" field_name
        in
        Cil.addOffsetLval field_offset lval
      in
      let field_type = Cil.typeOfLval lval in
      (* Read on array addresses are allowed; since array addresses cannot be
         written there are no risks of read/write interferences. *)
      let reads =
        if Cil_datatype.Lval.Set.mem lval local_env.authorized_reads
        || Cil.isArrayType field_type
        then reads
        else lval:: reads
      in
      let result_exp = Cil.new_exp ~loc (Lval lval) in
      let result_typ = dropQualifiers field_type in
      finish_exp reads chunk result_exp result_typ

    (* [exp->field_name] = [* (exp + off(field_name))] *)
    | Cabs.MEMBEROFPTR (exp, field_name) ->
      if as_const = ExpectConst then
        Kernel.warning ~current:true "using '->%s' in constant" field_name;
      let (reads, chunk, exp, typ) =
        do_expression local_env DontExpectConst exp (AExp None)
      in
      let pointed_typ =
        match Cil.unrollType typ with
        | TPtr(pointed_typ, _) -> pointed_typ
        | TArray(base_type, _, _, _) -> base_type
        | _ -> Kernel.abort ~current:true
                 "invalid type for left argument of '->%s' (have '%a')"
                 field_name !Cil.pp_typ_ref typ
      in
      let field_offset =
        match Cil.unrollType pointed_typ with
        | TComp (compinfo, _, _) ->
          CompField.findField field_name compinfo.cfields
        | _ -> Kernel.abort ~current:true
                 "request for member '%s' in type '%a' which is neither a \
                  structure nor a union"
                 field_name Printer.pp_typ pointed_typ
      in
      let lval = Cil.mkMem ~addr:exp ~off:field_offset in
      let field_type = Cil.typeOfLval lval in
      (* Read on array addresses are allowed; since array addresses cannot be
       * written there are no risks of read/write interferences. *)
      let reads =
        if Cil_datatype.Lval.Set.mem lval local_env.authorized_reads
        || Cil.isArrayType field_type
        then reads
        else lval :: reads
      in
      let result_exp = Cil.new_exp ~loc (Lval lval) in
      let result_typ = dropQualifiers field_type in
      finish_exp reads chunk result_exp result_typ

    | Cabs.CONSTANT constant ->
      begin
        let has_suffix string_1 =
          let length_1 = String.length string_1 in
          fun string_2 ->
            let length_2 = String.length string_2 in
            length_1 >= length_2 &&
            string_2 = String.uppercase_ascii
              (String.sub string_1 (length_1 - length_2) length_2)
        in
        let chunk = Chunk.unspecified_chunk Chunk.empty in
        match constant with
        | Cabs.CONST_INT int_constant ->
          let result_exp = Cil.parseIntExp ~loc int_constant in
          let result_typ = Cil.typeOf result_exp in
          finish_exp [] chunk result_exp result_typ

        | Cabs.CONST_WSTRING (wide_string_constant : int64 list) ->
          let result_exp =
            Cil.new_exp ~loc (Const(CWStr wide_string_constant))
          in
          let result_typ = Cil.typeOf result_exp in
          finish_exp [] chunk result_exp result_typ

        | Cabs.CONST_STRING string_constant ->
          (* Maybe we burried __FUNCTION__ in there. *)
          let string_constant =
            try
              let start = String.index string_constant (Char.chr 0) in
              let length = String.length string_constant in
              let to_find = (String.make 1 (Char.chr 0)) ^ "__FUNCTION__" in
              let past = start + String.length to_find in
              if past <= length
              && (String.sub string_constant
                    start (String.length to_find) = to_find)
              then
                let pre =
                  if start > 0
                  then String.sub string_constant 0 start
                  else ""
                in
                let current_fun_name = !current_fun_fundec.svar.vname in
                let post =
                  if past < length
                  then String.sub string_constant past (length - past)
                  else ""
                in
                pre ^ current_fun_name ^ post
              else
                string_constant
            with Not_found -> string_constant
          in
          let result_exp = Cil.new_exp ~loc (Const (CStr string_constant)) in
          let result_typ = Cil.typeOf result_exp in
          finish_exp [] chunk result_exp result_typ

        | Cabs.CONST_CHAR (character_constant : int64 list) ->
          let constant, typ =
            Cil.interpret_character_constant character_constant
          in
          finish_exp [] chunk (Cil.new_exp ~loc (Const constant)) typ

        | Cabs.CONST_WCHAR (wide_char_constant : int64 list) ->
          (* matth: I can't see a reason for a list of more than one [char]
             here, since the [kinteger64] below will take only the lower 16 bits
             of value. (['abc'] makes sense, because [CHAR] constants have type
             [int], and so more than one [char] may be needed to represent the
             value. But [L'abc'] has type [wchar], and so is equivalent to
             [L'c']). But gcc allows [L'abc'], so I will leave this here in case
             I am missing some architecture dependent behavior. *)
          let result_exp =
            let int64_value =
              Cil.reduce_multichar Cil.(theMachine.wcharType) wide_char_constant
            in
            let int_value = Integer.of_int64 int64_value in
            Cil.kinteger64 ~loc ~kind:Cil.(theMachine.wcharKind) int_value
          in
          let result_typ = Cil.typeOf result_exp in
          finish_exp [] chunk result_exp result_typ

        | Cabs.CONST_FLOAT float_constant ->
          begin
            let base_float_string, float_kind =
              (* Maybe it ends in 'F' or 'L'. Strip those. *)
              let length = String.length float_constant in
              let has_suffix = has_suffix float_constant in
              if has_suffix "L" then
                String.sub float_constant 0 (length - 1), FLongDouble
              else if has_suffix "F" then
                String.sub float_constant 0 (length - 1), FFloat
              else if has_suffix "D" then
                String.sub float_constant 0 (length - 1), FDouble
              else
                float_constant, FDouble
            in
            try
              Floating_point.set_round_nearest_even ();
              let open Floating_point in
              let base_float = parse_kind float_kind base_float_string in
              begin
                if base_float.f_lower <> base_float.f_upper
                && Kernel.WarnDecimalFloat.get () <> "none"
                then
                  let msg =
                    if Kernel.WarnDecimalFloat.get () = "once"
                    then begin
                      Kernel.WarnDecimalFloat.set "none";
                      ". See documentation for option " ^
                      Kernel.WarnDecimalFloat.name
                    end
                    else (* all *) ""
                  in
                  Kernel.warning ~current:true
                    "Floating-point constant %s is not represented exactly. \
                     Will use %a%s"
                    float_constant
                    (Floating_point.pretty_normal ~use_hex:true)
                    base_float.f_nearest
                    msg
              end;
              let result_exp =
                let exp_node =
                  Const (CReal (base_float.f_nearest,
                                float_kind,
                                Some float_constant))
                in
                Cil.new_exp ~loc exp_node
              in
              let result_typ = TFloat(float_kind, []) in
              finish_exp [] chunk result_exp result_typ
            with Failure msg ->
              Kernel.error ~once:true ~current:true
                "float_of_string %s (%s)\n" float_constant msg;
              let result_exp =
                Cil.new_exp ~loc (Const(CStr "booo CONS_FLOAT"))
              in
              let result_typ = Cil.typeOf result_exp in
              finish_exp [] chunk result_exp result_typ
          end
      end

    | Cabs.TYPE_SIZEOF (specifier, decl_type) ->
      let typ = do_only_type local_env.is_ghost specifier decl_type in
      (* Following C11:6.5.3.4p1 :
         "The sizeof operator shall not be applied to an expression that has
         function type or an incomplete type, to the parenthesized name of such
         a type, or to an expression that designates a bit-field member." *)
      let typ' = Cil.unrollType typ in
      (* Is the operand a function type? *)
      if Cil.isFunctionType typ' then
        Kernel.error ~current:true
          "invalid sizeof operand: type %a is a function type"
          Printer.pp_typ typ;
      (* Is the operand an incomplete type? *)
      if not (Cil.isCompleteType ~allowZeroSizeArrays:true typ') then
        Kernel.error ~current: true
          "invalid sizeof operand: type %a is incomplete"
          Printer.pp_typ typ;
      let chunk = Chunk.unspecified_chunk Chunk.empty in
      let result_exp = Cil.new_exp ~loc (SizeOf(typ)) in
      let result_typ = Cil.(theMachine.typeOfSizeOf) in
      finish_exp [] chunk result_exp result_typ

    (* Intercept the [sizeof("string")]. *)
    | Cabs.EXPR_SIZEOF
        (Cabs.{ expr_node = Cabs.CONSTANT (Cabs.CONST_STRING _); _ } as exp) ->
      begin
        (* Process the string first. *)
        match do_expression local_env as_const exp (AExp None) with
        | _, _, { enode = Const (CStr string_constant); _ }, _ ->
          let chunk = Chunk.unspecified_chunk Chunk.empty in
          let result_exp = Cil.new_exp ~loc (SizeOfStr string_constant) in
          let result_typ = Cil.(theMachine.typeOfSizeOf) in
          finish_exp [] chunk result_exp result_typ
        | _ -> Kernel.abort ~current:true "cabs2cil: sizeOfStr"
      end

    | Cabs.EXPR_SIZEOF exp ->
      (* Allow non-constants in sizeof *)
      (* Do not convert arrays and functions into pointers. *)
      let (_, chunk, exp', typ) =
        do_expression local_env DontExpectConst exp AExpLeaveArrayFun
      in
      (* Following C11:6.5.3.4p1 :
         "The sizeof operator shall not be applied to an expression that has
         function type or an incomplete type, to the parenthesized name of such
         a type, or to an expression that designates a bit-field member." *)
      let typ' = Cil.unrollType typ in
      (* Is the operand of a function type? *)
      if Cil.isFunctionType typ' then
        Kernel.error ~current:true
          "invalid sizeof operand: sizeof applied to an expression that has \
           function type";
      (* Is the operand of an incomplete type? *)
      if not (Cil.isCompleteType ~allowZeroSizeArrays:true typ') then
        Kernel.error ~current: true
          "invalid sizeof operand: sizeof applied to an expression that has \
           incomplete type";
      (* Does the operand designate a bit-field? *)
      if Cil.hasAttribute Cil.bitfield_attribute_name (Cil.typeAttr typ) then
        Kernel.error ~current: true
          "invalid sizeof operand: sizeof applied to an expression that \
           designates a bit-field";
      (* !!!! The book says that the expression is not evaluated, so we
         drop the potential side-effects. *)
      let scope_chunk =
        match Chunk.is_not_empty chunk with
        | true ->
          Kernel.feedback ~once:true ~current:true
            "Dropping side-effect in sizeof. \
             Nothing to worry, this is by the book.";
          Chunk.remove_labels chunk;
          IgnoreSideEffectHook.apply (exp, exp');
          let varinfos =
            List.filter
              (fun varinfo -> Cil.appears_in_expr varinfo exp')
              chunk.Chunk.locals
          in
          List.fold_left Chunk.local_var_chunk Chunk.empty varinfos
        | false -> Chunk.empty
      in
      let size_exp =
        match exp'.enode with
        (* Maybe we are taking the sizeof a variable-sized array. *)
        | Lval (Var varinfo, NoOffset) -> begin
            try Datatype.Int.Hashtbl.find variable_size_arrays varinfo.vid
            with Not_found -> Cil.new_exp ~loc (SizeOfE exp')
          end
        | _ -> Cil.new_exp ~loc (SizeOfE exp')
      in
      let result_typ = Cil.(theMachine.typeOfSizeOf) in
      finish_exp [] scope_chunk size_exp result_typ

    | Cabs.TYPE_ALIGNOF (specifier, decl_type) ->
      let chunk = Chunk.unspecified_chunk Chunk.empty in
      let result_exp =
        let typ = do_only_type local_env.is_ghost specifier decl_type in
        Cil.new_exp ~loc (AlignOf typ)
      in
      let result_typ = Cil.(theMachine.typeOfSizeOf) in
      finish_exp [] chunk result_exp result_typ

    | Cabs.EXPR_ALIGNOF exp ->
      let (_, chunk, exp', _) =
        do_expression local_env DontExpectConst exp AExpLeaveArrayFun in
      (* !!!! The book says that the expression is not evaluated, so we drop
         the potential side-effects. *)
      if Chunk.is_not_empty chunk then begin
        Kernel.warning ~current:true "Warning: Dropping side-effect in sizeof";
        IgnoreSideEffectHook.apply (exp, exp')
      end;
      let exp' =
        (* If we are taking the [alignof] an array we must drop
           the [StartOf]. *)
        match exp'.enode with
        | StartOf lval -> Cil.new_exp ~loc:exp'.eloc (Lval lval)
        | _ -> exp'
      in
      let chunk = Chunk.unspecified_chunk Chunk.empty in
      let result_exp = Cil.new_exp ~loc (AlignOfE exp') in
      let result_typ = Cil.(theMachine.typeOfSizeOf) in
      finish_exp [] chunk result_exp result_typ

    | Cabs.CAST ((specifier, decl_type), init_expression) ->
      let specifier, decl_type, init_expression =
        preprocess_cast local_env.is_ghost specifier decl_type init_expression
      in
      (* We know now that we can do [specifier] and [decl_type] many times. *)
      let typ = do_only_type local_env.is_ghost specifier decl_type in
      let action' =
        match action with
        | AExp (Some _) -> AExp (Some typ)
        | AExp None -> action
        | ADrop | AType | AExpLeaveArrayFun -> action
        | ASet (_, _, _, lval_typ) ->
          (* If the cast from [typ] to [lval_typ] would be dropped, then we
             continue with a [Set]. *)
          if false && Cil_datatype.Typ.equal typ lval_typ
          then action
          else AExp None (* We'll create a temporary. *)
      in
      (* Remember here if we have done the Set *)
      let (reads, chunk, exp, typ'), (need_cast : bool) =
        match init_expression with
        | Cabs.NO_INIT ->
          Kernel.fatal ~current:true "missing expression in cast"
        | Cabs.SINGLE_INIT exp ->
          do_expression local_env as_const exp action', true
        | Cabs.COMPOUND_INIT _ ->
          begin
            (* Pretend that we are declaring and initializing a brand new
               variable. *)
            let new_var_name = ConstrExprId.get_next () in
            let spec_result = do_spec_list local_env.is_ghost "" specifier in
            let chunk_1 =
              match !scopes with
              | [] ->
                (* This is a global. Mark the new vars as static. *)
                let spec_result =
                  let typ, _storage, is_inline, attributes = spec_result in
                  typ, Static, is_inline, attributes
                in
                let init_name =
                  (new_var_name, decl_type, [], loc), init_expression
                in
                ignore
                  (create_global local_env.is_ghost None spec_result init_name);
                Chunk.unspecified_chunk Chunk.empty
              | _ ->
                let init_name =
                  (new_var_name, decl_type, [], loc), init_expression
                in
                createLocal local_env.is_ghost spec_result init_name
            in
            (* Now pretend that this is just a reference to the newly created
               variable. *)
            let variable_exp =
              { Cabs.expr_node = Cabs.VARIABLE new_var_name;
                expr_loc = loc }
            in
            let reads, chunk, exp, typ' =
              do_expression local_env as_const variable_exp action'
            in
            let chunk_1 =
              match Cil.unrollType typ', exp.enode with
              | TComp (compinfo, _, _), Lval lval ->
                (* When creating a struct with a compound literal we remove
                   manually the writes in the fields of the struct to avoid
                   false positives with unspecified-access. See TRUS-521. *)
                let allowed_writes : lval list =
                  List.map
                    (fun fieldinfo ->
                       Cil.addOffsetLval (Field (fieldinfo, NoOffset)) lval)
                    compinfo.cfields
                in
                let not_in_allowed_writes lval : bool =
                  List.for_all
                    (fun lval' ->
                       Cil_datatype.LvalStructEq.compare lval lval' != 0)
                    allowed_writes
                in
                let remove_writes
                    (stmt, ({ Chunk.writes = writes; _ } as effects), calls) =
                  let writes' = List.filter not_in_allowed_writes writes in
                  let effects' = { effects with Chunk.writes = writes' } in
                  (stmt, effects', calls)
                in
                let stmts_with_effects =
                  let efficient_map f l = List.rev (List.rev_map f l) in
                  efficient_map remove_writes chunk_1.Chunk.stmts_with_effects
                in
                { chunk_1 with Chunk.stmts_with_effects = stmts_with_effects }
              | _ -> chunk_1
            in
            (* If [typ] is an array then the [do_expression] above has already added a
               [StartOf]. We must undo that now so that it is done once by the
               [finish_exp] at the end of this case. *)
            let exp_2, typ_2 =
              match Cil.unrollType typ, exp.enode with
              | TArray _, StartOf lvalue -> Cil.new_exp ~loc (Lval lvalue), typ
              | _, _ -> exp, typ'
            in
            (* If we are here, then the type [typ_2] is guaranteed to match the
               type of the expression [exp_2], so we do not need a cast. We have
               to worry about this because otherwise, we might need to cast
               between arrays or structures. *)
            let chunk = chunk_1 @@ (chunk, ghost) in
            (reads, chunk, exp_2, typ_2), false
          end
      in
      let (result_typ, result_exp) =
        (* Do this to check the cast, unless we are sure that we do not
           the check. *)
        match need_cast with
        | true -> Casts.castTo ~fromsource:true typ' typ exp
        | false -> typ', exp
      in
      finish_exp reads chunk result_exp result_typ

    | Cabs.UNARY (Cabs.MINUS, exp) ->
      let (reads, chunk, exp, typ) =
        do_expression local_env as_const exp (AExp None)
      in
      if Cil.isIntegralType typ then
        let result_typ = Cil.integralPromotion typ in
        let result_exp =
          let unop_exp = Casts.makeCastT ~e:exp ~oldt:typ ~newt:result_typ in
          Cil.new_exp ~loc (UnOp (Neg, unop_exp, result_typ))
        in
        finish_exp reads chunk result_exp result_typ
      else if Cil.isArithmeticType typ then
        let result_exp = Cil.new_exp ~loc:exp.eloc (UnOp(Neg, exp, typ)) in
        finish_exp reads chunk result_exp typ
      else
        Kernel.abort ~current:true
          "invalid argument type `%a' to unary minus" Printer.pp_typ typ

    | Cabs.UNARY (Cabs.BNOT, exp) ->
      let (reads, chunk, exp, typ) =
        do_expression local_env as_const exp (AExp None)
      in
      if Cil.isIntegralType typ then
        let result_typ = Cil.integralPromotion typ in
        let result_exp =
          let unop_exp = Casts.makeCastT ~e:exp ~oldt:typ ~newt:result_typ in
          Cil.new_exp ~loc (UnOp(BNot, unop_exp, result_typ))
        in
        finish_exp reads chunk result_exp result_typ
      else
        Kernel.abort ~current:true
          "invalid argument type `%a' to bit-complement" Printer.pp_typ typ

    | Cabs.UNARY (Cabs.PLUS, exp) ->
      let (reads, chunk, exp, typ) =
        do_expression local_env as_const exp (AExp None)
      in
      (* Following C11:6.5.3.3p2 :
         "The result of the unary + operator is the value of its (promoted)
         operand. The integer promotions are performed on the operand, and the
         result has the promoted type." *)
      if Cil.isIntegralType typ then
        let result_typ = Cil.integralPromotion typ in
        let result_exp = Casts.makeCastT ~e:exp ~oldt:typ ~newt:result_typ in
        finish_exp reads chunk result_exp result_typ
      else
        (* Following C11:6.5.3.3p1 :
           "The operand of the unary + or - operator shall have arithmetic type".
           NOTE: Integral type is a subset of arithmetic type. *)
      if Cil.isArithmeticType typ
      then
        finish_exp reads chunk exp typ
      else
        Kernel.abort ~current:true
          "invalid argument type `%a' to unary plus" Printer.pp_typ typ

    | Cabs.UNARY (Cabs.ADDROF, exp) -> begin
        match exp.Cabs.expr_node with
        | Cabs.COMMA exps -> (* GCC extension. *)
          let expr_node =
            Cabs.COMMA (replaceLastInList exps
                          (fun exp ->
                             Cabs.{ exp with expr_node =
                                               Cabs.UNARY(Cabs.ADDROF, exp) }))
          in
          let exp = Cabs.{ exp with expr_node } in
          do_expression local_env DontExpectConst exp action

        | Cabs.QUESTION (exp_1, exp_2, exp_3) -> (* GCC extension *)
          let expr_node =
            Cabs.QUESTION (exp_1,
                           Cabs.{ exp_2 with expr_node =
                                               Cabs.UNARY(Cabs.ADDROF, exp_2)},
                           Cabs.{ exp_3 with expr_node =
                                               Cabs.UNARY(Cabs.ADDROF, exp_3)})
          in
          let exp = Cabs.{ exp with expr_node } in
          do_expression local_env DontExpectConst exp action

        | Cabs.BINARY
            ((Cabs.(ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN |
                    DIV_ASSIGN | MOD_ASSIGN | BAND_ASSIGN | BOR_ASSIGN |
                    XOR_ASSIGN | SHL_ASSIGN | SHR_ASSIGN | ASSIGN) as op),
             left_exp, right_exp) ->
          (*  Allowed in C++ only. *)
          (* [&(l op r )] --> [T* tmp=&l; *tmp = r; tmp] *)
          let addr_of_left_exp =
            { Cabs.expr_node = Cabs.UNARY(Cabs.ADDROF, left_exp);
              expr_loc = left_exp.Cabs.expr_loc; }
          in
          let _, _, for_printing_exp, left_typ =
            do_expression local_env DontExpectConst addr_of_left_exp AType in
          let tmp_varinfo =
            let descr =
              Pretty_utils.sfprintf "%a"
                Cil_descriptive_printer.pp_exp for_printing_exp
            in
            make_new_tmp_var descr true left_typ
          in
          let tmp_var_lval = Cil.var tmp_varinfo in
          add_local_to_env tmp_varinfo.vname (EnvVar tmp_varinfo);
          let (left_reads, left_chunk, _left_exp, _left_typ) =
            (* [T* tmp = &l;] *)
            let left_action = ASet (true, tmp_var_lval, [], left_typ) in
            do_expression local_env DontExpectConst addr_of_left_exp left_action
          in
          let tmp_var_exp = Cil.evar ~loc:right_exp.Cabs.expr_loc tmp_varinfo in
          let intermediate_stmt = (* [*p op r] *)
            let star_exp =
              { Cabs.expr_loc = exp.Cabs.expr_loc;
                expr_node =
                  Cabs.UNARY (Cabs.MEMOF,
                              Cabs.{ expr_loc = exp.expr_loc;
                                     expr_node = VARIABLE tmp_varinfo.vname })}
            in
            let computation_exp =
              { Cabs.expr_loc = exp.Cabs.expr_loc;
                expr_node = Cabs.BINARY (op, star_exp, right_exp) }
            in
            { Cabs.stmt_ghost = ghost;
              stmt_node =
                Cabs.COMPUTATION (computation_exp, exp.Cabs.expr_loc) }
          in
          let stmt_chuck = do_statement local_env intermediate_stmt in
          (* Add the instruction to the chunk. *)
          (* Change the expression to be the temporary. *)
          let chunk =
            (Chunk.local_var_chunk left_chunk tmp_varinfo) @@
            (stmt_chuck, ghost)
          in
          (left_reads, chunk, tmp_var_exp, left_typ)

        | Cabs.UNARY ((Cabs.PREINCR|Cabs.PREDECR as op), exp) ->
          let addr_of_exp_plus_minus_one =
            let constant_one =
              cabs_exp loc (Cabs.CONSTANT (Cabs.CONST_INT "1"))
            in
            let exp_plus_minus_one =
              let plus_minus_op : Cabs.binary_operator =
                match op with
                | Cabs.PREINCR -> Cabs.ADD_ASSIGN
                | Cabs.PREDECR -> Cabs.SUB_ASSIGN
                | _ -> assert false
              in
              cabs_exp loc (Cabs.BINARY(plus_minus_op, exp, constant_one))
            in
            cabs_exp loc (Cabs.UNARY (Cabs.ADDROF, exp_plus_minus_one))
          in
          do_expression local_env DontExpectConst
            addr_of_exp_plus_minus_one action

        | Cabs.PAREN exp ->
          let exp =
            Cabs.{ exp with expr_node = Cabs.UNARY (Cabs.ADDROF, exp) }
          in
          do_expression local_env DontExpectConst exp action

        | Cabs.VARIABLE variable_name
          when isOldStyleVarArgName variable_name
            && (match !current_fun_fundec.svar.vtype with
                | TFun(_, _, true, _) -> true
                | _ -> false) ->
          (* We are in an old-style variable argument function and we are taking
             the address of the argument that was removed while processing the
             function type. We compute the address based on the address of the
             last real argument. *)
          if Cil.msvcMode () then begin
            let last_formal_arg =
              let rec get_last_formal_arg = function
                | [] -> Kernel.fatal ~current:true
                          "old-style variable argument function without real \
                           arguments"
                | [ last_formal_arg ] -> last_formal_arg
                | _ :: remaining_formal_args ->
                  get_last_formal_arg remaining_formal_args
              in
              get_last_formal_arg !current_fun_fundec.sformals
            in
            let result_exp =
              mkAddrOfAndMark exp.Cabs.expr_loc (Cil.var last_formal_arg)
            in
            let result_typ = Cil.typeOf result_exp in
            let result_typ, result_exp =
              Casts.castTo result_typ (TInt(IULong, [])) result_exp
            in
            let result_exp'' =
              (* Now we must add to this address to point to the next argument.
                 Round up to a multiple of 4. *)
              let size_of_last_formal_arg =
                (((Cil.bitsSizeOf last_formal_arg.vtype) + 31) / 32) * 4
              in
              let exp_node =
                let size_of_last_formal_arg_exp =
                  Cil.kinteger ~loc IULong size_of_last_formal_arg
                in
                BinOp (PlusA,
                       result_exp,
                       size_of_last_formal_arg_exp,
                       result_typ)
              in
              Cil.new_exp ~loc exp_node
            in
            let reads =
              let lval = Cil.var last_formal_arg in
              if Cil_datatype.Lval.Set.mem lval local_env.authorized_reads
              then []
              else [ lval ]
            in
            let chunk = Chunk.unspecified_chunk Chunk.empty in
            finish_exp reads chunk result_exp'' result_typ
          end else
            (* On GCC the only reliable way to do this is to call
               [builtin_next_arg]. If we take the address of a local we are
               going to get the address of a copy of the local! *)
            let exp =
              cabs_exp loc
                (Cabs.CALL (cabs_exp loc (Cabs.VARIABLE "__builtin_next_arg"),
                            [cabs_exp loc (Cabs.CONSTANT (Cabs.CONST_INT "0"))]))
            in
            do_expression local_env as_const exp action

        | Cabs.UNARY (Cabs.MEMOF, exp) ->
          (* Following C11:6.5.3.2p3 :
             "If the operand is the result of a unary * operator, neither that
             operator nor the & operator is evaluated and the result is as if
             both were omitted, except that the constraints on the operators
             still apply and the result is not an lvalue." *)
          let (reads, chunk, exp, typ) =
            do_expression local_env as_const exp action
          in
          (* Following C11:6.5.3.2p2 :
             "The operand of the unary * operator shall have pointer type." *)
          if not (Cil.isPointerType typ) then
            Kernel.abort ~current:true
              "Expecting a pointer type in * operator's operand. Got `%a'."
              Printer.pp_typ typ;
          finish_exp reads chunk exp typ

        (* Regular lvalues. *)
        | Cabs.VARIABLE _
        | Cabs.CONSTANT (Cabs.CONST_STRING _)
        | Cabs.CONSTANT (Cabs.CONST_WSTRING _)
        | Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _
        | Cabs.CAST (_, Cabs.COMPOUND_INIT _) ->
          begin
            let (reads, chunk, exp, typ) =
              do_expression local_env DontExpectConst exp (AExp None)
            in
            (* Does the operand designate a bit-field lvalue? *)
            if Cil.hasAttribute Cil.bitfield_attribute_name (Cil.typeAttr typ)
            then Kernel.error ~current: true "address of bit-field requested";
            match exp.enode with
            | Lval lval
            | CastE (_, { enode = Lval lval; _ }) ->
              let reads =
                match lval with
                | Mem _, _ ->
                  (* We are not really reading the pointed value, just
                     calculating an offset. *)
                  reads
                | Var _, _ ->
                  if Cil_datatype.Lval.Set.mem lval local_env.authorized_reads
                  then reads
                  else lval :: reads
              in
              let result_typ =
                (* Recover type qualifiers that were dropped by [dropQualifiers]
                   when the lvalue was created. *)
                let result_pointed_typ =
                  match exp.enode with
                  | Lval lval -> Cil.typeOfLval lval
                  | _ -> typ
                in
                TPtr (result_pointed_typ, [])
              in
              let result_exp = mkAddrOfAndMark loc lval in
              finish_exp reads chunk result_exp result_typ

            | StartOf lval ->
              let result_typ =
                (* Pointer to array. *)
                TPtr (Cil.typeOfLval lval, [])
              in
              let reads =
                match lval with
                | Mem _, _ -> reads (* See above. *)
                | Var _, _ ->
                  if Cil_datatype.Lval.Set.mem lval local_env.authorized_reads
                  then reads
                  else lval :: reads
              in
              finish_exp reads chunk (mkAddrOfAndMark loc lval) result_typ

            | Const (CStr _ | CWStr _) ->
              (* String to array. *)
              let result_typ = TPtr(typ, []) in
              finish_exp reads chunk exp result_typ

            (* Function names are converted into pointers to the function.
               Taking the address-of again does not change things. *)
            | AddrOf (Var v, NoOffset) when Cil.isFunctionType v.vtype ->
              finish_exp reads chunk exp typ

            | _ ->
              Kernel.fatal ~current:true "Expected lval for ADDROF. Got %a"
                Printer.pp_exp exp
          end

        | Cabs.NOTHING | Cabs.LABELADDR _ | Cabs.EXPR_SIZEOF _
        | Cabs.TYPE_SIZEOF _ | Cabs.EXPR_ALIGNOF  _| Cabs.TYPE_ALIGNOF _
        | Cabs.EXPR_PATTERN _
        | Cabs.CALL _
        | Cabs.BINARY (Cabs.(ADD|SUB|MUL|DIV|MOD|AND|OR|BAND|BOR|XOR
                            |SHL|SHR|EQ|NE|LT|GT|LE|GE), _, _)
        | Cabs.UNARY (Cabs.(MINUS|PLUS|NOT|BNOT|ADDROF|POSINCR|POSDECR), _)
        | Cabs.GNU_BODY _
        | Cabs.CONSTANT
          Cabs.(CONST_INT _ | CONST_FLOAT _ | CONST_CHAR _ | CONST_WCHAR _)
        | Cabs.CAST (_, Cabs.(NO_INIT | SINGLE_INIT _)) ->
          Kernel.abort ~current:true "Unexpected operand for addrof"
      end

    | Cabs.UNARY((Cabs.PREINCR|Cabs.PREDECR) as unary_op, exp) -> begin
        match exp.Cabs.expr_node with
        | Cabs.COMMA exps -> (* GCC extension. *)
          let exp =
            cabs_exp loc
              (Cabs.COMMA
                 (replaceLastInList exps
                    (fun exp -> cabs_exp exp.Cabs.expr_loc
                        (Cabs.UNARY (unary_op, exp)))))
          in
          do_expression local_env as_const exp action
        | Cabs.QUESTION (exp_1, exp_2, exp_3) -> (* GCC extension. *)
          let exp =
            cabs_exp loc
              (Cabs.QUESTION
                 (exp_1,
                  cabs_exp exp_2.Cabs.expr_loc (Cabs.UNARY (unary_op, exp_2)),
                  cabs_exp exp_3.Cabs.expr_loc (Cabs.UNARY (unary_op, exp_3))))
          in
          do_expression local_env as_const exp action
        | Cabs.PAREN exp ->
          let exp = cabs_exp loc (Cabs.UNARY (unary_op, exp)) in
          do_expression local_env as_const exp action
        | Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) (* Regular lvalues. *)
        | Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _
        | Cabs.CAST _ (* A GCC extension *) ->
          if as_const = ExpectConst then
            Kernel.warning ~current:true "PREINCR or PREDECR in constant";
          let binop =
            match unary_op with
            | Cabs.PREINCR -> PlusA
            | Cabs.PREDECR -> MinusA
            | _ -> assert false
          in
          let (reads, chunk, exp, typ) =
            do_expression local_env DontExpectConst exp (AExp None)
          in
          let lval =
            match exp.enode with
            | Lval lval -> lval
            | CastE (_, { enode = Lval lval; _ }) -> lval
            (* A GCC extension. The operation is done at the cast type.
               The result is also of the cast type. *)
            | _ -> Kernel.fatal ~current:true "Expected lval for ++ or --"
          in
          let chunk = Chunk.remove_reads lval chunk in
          let reads =
            List.filter
              (fun lval' -> not (Cil_datatype.Lval.equal lval' lval))
              reads
          in
          let op_result_typ, op_result_exp =
            let one_exp = Cil.one ~loc:exp.eloc in
            do_binop loc binop exp typ one_exp Cil.intType
          in
          let chunk =
            let stmt =
              let instr =
                Set (lval,
                     Casts.makeCastT ~e:op_result_exp ~oldt:op_result_typ ~newt:typ,
                     Cil.CurrentLoc.get ())
              in
              Cil.mkStmtOneInstr ~ghost:local_env.is_ghost instr
            in
            let effects =
              Chunk.make_effects ~modified:[] ~writes:[lval] ~reads:reads
            in
            chunk +++ (stmt, effects)
          in
          finish_exp [] chunk exp typ

        | _ ->
          Kernel.abort ~current:true
            "Unexpected operand for pre-increment or pre-decrement operator"
      end

    | Cabs.UNARY((Cabs.POSINCR|Cabs.POSDECR) as unary_op, exp) -> begin
        match exp.Cabs.expr_node with
        | Cabs.COMMA exps -> (* GCC extension. *)
          let exp =
            cabs_exp loc
              (Cabs.COMMA
                 (replaceLastInList exps
                    (fun exp -> cabs_exp exp.Cabs.expr_loc
                        (Cabs.UNARY (unary_op, exp)))))
          in
          do_expression local_env as_const exp action
        | Cabs.QUESTION (exp_1, exp_2, exp_3) -> (* GCC extension. *)
          let exp =
            cabs_exp loc
              (Cabs.QUESTION
                 (exp_1,
                  cabs_exp exp_2.Cabs.expr_loc (Cabs.UNARY (unary_op, exp_2)),
                  cabs_exp exp_3.Cabs.expr_loc (Cabs.UNARY (unary_op, exp_3))))
          in
          do_expression local_env as_const exp action
        | Cabs.PAREN exp ->
          let exp = cabs_exp exp.Cabs.expr_loc (Cabs.UNARY (unary_op, exp)) in
          do_expression local_env as_const exp action
        | Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) (* Regular lvalues. *)
        | Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _
        | Cabs.CAST _ (* A GCC extension. *) ->
          if as_const = ExpectConst then
            Kernel.warning ~current:true "POSTINCR or POSTDECR in constant";
          (* If we do not drop the result then we must save the value. *)
          let binop =
            match unary_op with
            | Cabs.POSINCR -> PlusA
            | Cabs.POSDECR -> MinusA
            | _ -> assert false
          in
          let (reads, chunk', exp, typ) =
            do_expression local_env DontExpectConst exp (AExp None)
          in
          let lval =
            match exp.enode with
            | Lval lval -> lval
            | CastE (_, { enode = Lval lval; _ }) -> lval
            (* GCC extension. The addition must be be done at the cast type.
               The result is also of the cast type. *)
            | _ -> Kernel.fatal ~current:true "Expected lval for ++ or --"
          in
          let chunk = Chunk.remove_reads lval chunk' in
          let reads' =
            List.filter
              (fun lval' -> not (Cil_datatype.Lval.equal lval' lval))
              reads
          in
          let op_result_typ, op_result_exp =
            let one_exp = Cil.one ~loc:exp.eloc in
            do_binop loc binop exp typ one_exp Cil.intType
          in
          let reads, chunk, result_exp =
            match action with
            | ADrop | AType -> [], chunk', exp
            | _ ->
              let descr =
                Pretty_utils.sfprintf "%a%s"
                  Cil_descriptive_printer.pp_exp  exp
                  (if unary_op = Cabs.POSINCR then "++" else "--")
              in
              let tmp_varinfo = make_new_tmp_var descr true typ in
              let tmp_var_lval = Cil.var tmp_varinfo in
              (* The tmp variable should not be investigated for unspecified
                 writes: it occurs at the right place in the sequence. *)
              let reads = [tmp_var_lval] in
              let chunk =
                let stmt =
                  let instr =
                    Set (tmp_var_lval, exp, Cil.CurrentLoc.get ())
                  in
                  Cil.mkStmtOneInstr ~ghost:local_env.is_ghost instr
                in
                Chunk.local_var_chunk chunk tmp_varinfo
                +++
                (stmt, Chunk.no_effects)
              in
              let exp = Cil.new_exp ~loc (Lval(Cil.var tmp_varinfo)) in
              reads, chunk, exp
          in
          let chunk =
            let stmt =
              let instr =
                Set (lval,
                     Casts.makeCastT
                       ~e:op_result_exp
                       ~oldt:op_result_typ
                       ~newt:(Cil.typeOfLval lval),
                     Cil.CurrentLoc.get ())
              in
              Cil.mkStmtOneInstr ~ghost:local_env.is_ghost instr
            in
            let effects =
              Chunk.make_effects ~modified:[] ~writes:[lval] ~reads:reads'
            in
            chunk +++ (stmt, effects)
          in
          finish_exp reads chunk result_exp typ
        | _ ->
          Kernel.abort ~current:true
            "Unexpected operand for post-increment or post-decrement operator"
      end

    | Cabs.BINARY(Cabs.ASSIGN, exp_1, exp_2) ->
      begin
        match exp_1.Cabs.expr_node with
        | Cabs.COMMA exps -> (* GCC extension. *)
          do_expression local_env as_const
            (cabs_exp loc
               (Cabs.COMMA
                  (replaceLastInList exps
                     (fun exp' -> cabs_exp exp'.Cabs.expr_loc
                         (Cabs.BINARY(Cabs.ASSIGN, exp', exp_2))))))
            action

        | Cabs.QUESTION (q_exp_1, q_exp_2, q_exp_3) -> (* GCC extension. *)
          (* TODO: prevent duplication of [exp_2]: this is incorrect if it
                   contains labels. *)
          (* let r2,se2,e2,t2 = do_expression authorized_reads ghost asconst [exp_2] in *)
          do_expression local_env as_const
            (cabs_exp loc
               (Cabs.QUESTION
                  (q_exp_1,
                   cabs_exp q_exp_2.Cabs.expr_loc
                     (Cabs.BINARY (Cabs.ASSIGN, q_exp_2, exp_2)),
                   cabs_exp q_exp_3.Cabs.expr_loc
                     (Cabs.BINARY (Cabs.ASSIGN, q_exp_3, exp_2)))))
            action

        | Cabs.CAST (t, Cabs.SINGLE_INIT exp') -> (* GCC extension. *)
          do_expression local_env as_const
            (cabs_exp loc
               (Cabs.CAST
                  (t,
                   Cabs.SINGLE_INIT
                     (cabs_exp exp'.Cabs.expr_loc
                        (Cabs.BINARY
                           (Cabs.ASSIGN, exp',
                            (cabs_exp exp_2.Cabs.expr_loc
                               (Cabs.CAST (t, Cabs.SINGLE_INIT exp_2)))))))))
            action

        | Cabs.PAREN exp' ->
          do_expression local_env as_const
            (cabs_exp loc (Cabs.BINARY (Cabs.ASSIGN, exp', exp_2))) action

        | Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) (* Regular lvalues. *)
        | Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _ ->
          begin
            if as_const = ExpectConst then
              Kernel.warning ~current:true "ASSIGN in constant";
            let chunk_0 = Chunk.unspecified_chunk Chunk.empty in
            let (reads, chunk_1, exp_1, typ_1) =
              do_expression local_env DontExpectConst exp_1 (AExp None)
            in
            let lval =
              match exp_1.enode with
              | Lval lval -> lval
              | _ -> Kernel.abort ~current:true
                       "lvalue required as left operand of assignment"
            in
            let chunk_1 = Chunk.remove_reads lval chunk_1 in
            let reads =
              List.filter
                (fun lval' -> not (Cil_datatype.Lval.equal lval' lval))
                reads
            in
            let local_env =
              let authorized_reads =
                Cil_datatype.Lval.Set.add lval local_env.authorized_reads
              in
              { local_env with authorized_reads }
            in
            (* [BM]: is this useful?
               let (_, _, _) =
                 do_expression ghost DontExpectConst e2 (ASet(lv, lvt)) in *)
            (* Catch the case of an [lval] that might depend on itself,
               e.g. "p[p[0]]" when p[0] == 0. We need to use a temporary here if
               the result of the expression will be used:
               "tmp := e2; lv := tmp;" use tmp as the result
               Test: small1/assign.c *)
            let needs_temporary =
              not (Cil.isBitfield lval) && (* PC: BTS 933, 968 *)
              match action, lval with
              | (ADrop | AType), _ -> false
              | _, (Mem exp, off) ->
                not (Cil.isConstant exp) ||
                not (Cil.isConstantOffset off)
              | _, (Var _, offset) -> not (Cil.isConstantOffset offset)
            in
            let reads, lval, chunk_3 =
              match needs_temporary with
              | true ->
                let descr =
                  Pretty_utils.sfprintf "%a"
                    Cil_descriptive_printer.pp_lval lval
                in
                let tmp_varinfo = make_new_tmp_var descr true typ_1 in
                let tmp_lval = Cil.var tmp_varinfo in
                let tmp_chunk =
                  let tmp_exp = Cil.new_exp ~loc:exp_1.eloc (Lval tmp_lval) in
                  let tmp_stmt =
                    let tmp_instr = Set (lval, tmp_exp, loc) in
                    Cil.mkStmtOneInstr ~ghost:local_env.is_ghost tmp_instr
                  in
                  Chunk.of_stmt_and_effects
                    (tmp_stmt,
                     Chunk.make_effects ~modified:[lval] ~writes:[lval] ~reads)
                in
                let tmp_reads = [] in
                let tmp_chunk = Chunk.local_var_chunk tmp_chunk tmp_varinfo in
                (tmp_reads, tmp_lval, tmp_chunk)
              | false -> reads, lval, Chunk.empty
            in
            let (reads, chunk_2, _, _) =
              do_expression local_env DontExpectConst
                exp_2 (ASet(not needs_temporary,lval, reads, typ_1))
            in
            (* r1 is read in the assignment part itself *)
            let result_chunk =
              let (@@) chunk_a chunk_b = chunk_a @@ (chunk_b, ghost) in
              (Chunk.empty @@ ((chunk_0 @@ chunk_1) @@ chunk_2)) @@ chunk_3
            in
            let result_exp = (Cil.new_exp ~loc (Lval lval)) in
            finish_exp reads result_chunk result_exp typ_1
          end

        | _ -> Kernel.abort ~current:true "expression is not assignable"
      end

    | Cabs.BINARY
        ((Cabs.ADD | Cabs.SUB | Cabs.MUL | Cabs.DIV | Cabs.MOD |
          Cabs.BAND | Cabs.BOR | Cabs.XOR | Cabs.SHL | Cabs.SHR |
          Cabs.EQ | Cabs.NE | Cabs.LT | Cabs.GT | Cabs.GE | Cabs.LE)
         as cabs_binop, exp_1, exp_2) ->
      let chunk_0 = Chunk.unspecified_chunk Chunk.empty in
      let binop = convBinOp cabs_binop in
      let (reads_1, chunk_1, exp_1, typ_1) =
        do_expression local_env as_const exp_1 (AExp None)
      in
      let (reads_2, chunk_2, exp_2, typ_2) =
        do_expression local_env as_const exp_2 (AExp None)
      in
      let result_typ, result_exp = do_binop loc binop exp_1 typ_1 exp_2 typ_2 in
      let result_chunk =
        let (@@) chunk_a chunk_b = chunk_a @@ (chunk_b, ghost) in
        (chunk_0 @@ chunk_1) @@ chunk_2
      in
      let result_reads = reads_1 @ reads_2 in
      finish_exp result_reads result_chunk result_exp result_typ

    (* Assignment operators. *)
    | Cabs.BINARY
        ((Cabs.ADD_ASSIGN | Cabs.SUB_ASSIGN | Cabs.MUL_ASSIGN |
          Cabs.DIV_ASSIGN | Cabs.MOD_ASSIGN | Cabs.BAND_ASSIGN |
          Cabs.BOR_ASSIGN | Cabs.SHL_ASSIGN | Cabs.SHR_ASSIGN |
          Cabs.XOR_ASSIGN) as cabs_binop, exp_1, exp_2) ->
      begin
        let chunk_0 = Chunk.unspecified_chunk Chunk.empty in
        match exp_1.Cabs.expr_node with
        | Cabs.COMMA comma_exps -> (* GCC extension. *)
          do_expression local_env as_const
            (cabs_exp loc
               (Cabs.COMMA
                  (replaceLastInList comma_exps
                     (fun exp' ->
                        cabs_exp exp'.Cabs.expr_loc
                          (Cabs.BINARY(cabs_binop, exp', exp_2))))))
            action
        | Cabs.QUESTION (q_exp_1, q_exp_2, q_exp_3) -> (* GCC extension. *)
          do_expression local_env as_const
            (cabs_exp loc
               (Cabs.QUESTION
                  (q_exp_1,
                   cabs_exp q_exp_2.Cabs.expr_loc
                     (Cabs.BINARY(cabs_binop, q_exp_2, exp_2)),
                   cabs_exp q_exp_3.Cabs.expr_loc
                     (Cabs.BINARY(cabs_binop, q_exp_3, exp_2)))))
            action
        | Cabs.PAREN exp' ->
          do_expression local_env as_const
            (cabs_exp loc (Cabs.BINARY (cabs_binop, exp', exp_2))) action
        | Cabs.VARIABLE _ | Cabs.UNARY (Cabs.MEMOF, _) (* Regular lvalues. *)
        | Cabs.INDEX _ | Cabs.MEMBEROF _ | Cabs.MEMBEROFPTR _
        | Cabs.CAST _ (* GCC extension *) ->
          begin
            if as_const = ExpectConst then
              Kernel.warning ~current:true "op_ASSIGN in constant";
            let binop =
              match cabs_binop with
              | Cabs.ADD_ASSIGN -> PlusA
              | Cabs.SUB_ASSIGN -> MinusA
              | Cabs.MUL_ASSIGN -> Mult
              | Cabs.DIV_ASSIGN -> Div
              | Cabs.MOD_ASSIGN -> Mod
              | Cabs.BAND_ASSIGN -> BAnd
              | Cabs.BOR_ASSIGN -> BOr
              | Cabs.XOR_ASSIGN -> BXor
              | Cabs.SHL_ASSIGN -> Shiftlt
              | Cabs.SHR_ASSIGN -> Shiftrt
              | _ -> Kernel.fatal ~current:true "binary +="
            in
            let (reads_1, chunk_1, exp_1, typ_1) =
              do_expression local_env DontExpectConst exp_1 (AExp None)
            in
            let lval_1 =
              match exp_1.enode with
              | Lval lval -> lval
              | CastE (_, { enode = Lval lval; _ }) -> lval
              (* GCC extension.
                 The operation and the result are at the cast type. *)
              | _ -> Kernel.fatal ~current:true
                       "Expected lval for assignment with arith"
            in
            let chunk_1 = Chunk.remove_reads lval_1 chunk_1 in
            let reads_1 =
              List.filter
                (fun lval' -> not (Cil_datatype.Lval.equal lval' lval_1))
                reads_1
            in
            let local_env =
              let authorized_reads =
                Cil_datatype.Lval.Set.add lval_1 local_env.authorized_reads
              in
              { local_env with authorized_reads }
            in
            let (reads_2, chunk_2, exp_2, typ_2) =
              do_expression local_env DontExpectConst exp_2 (AExp None)
            in
            let chunk_2 = Chunk.remove_reads lval_1 chunk_2 in
            let result_typ, result_exp =
              do_binop loc binop exp_1 typ_1 exp_2 typ_2
            in
            (* We must cast the result to the type of the [lval_1], which may be
               different than [typ_1] if [lval_1] was a [Cast]. *)
            let _, result_exp =
              Casts.castTo result_typ (Cil.typeOfLval lval_1) result_exp
            in
            (* The type of the result is the type of the left-hand side  *)
            let result_chunk =
              let (@@) s1 s2 = s1 @@ (s2, ghost) in
              let stmt =
                let instr = Set (lval_1, result_exp, loc) in
                Cil.mkStmtOneInstr ~ghost:local_env.is_ghost instr,
                Chunk.make_effects
                  ~modified:[lval_1]
                  ~writes:[lval_1]
                  ~reads:(reads_1 @ reads_2)
              in
              chunk_0 @@ (Chunk.empty @@ (chunk_1 @@ chunk_2) +++ stmt)
            in
            finish_exp [] result_chunk exp_1 typ_1
          end
        | _ ->
          Kernel.fatal ~current:true
            "Unexpected left operand for assignment with arith"
      end

    | Cabs.BINARY((Cabs.AND | Cabs.OR), _, _)
    | Cabs.UNARY(Cabs.NOT, _) ->
      begin
        let cond_exp_result = do_cond_exp local_env as_const expression in
        (* We must normalize the result to 0 or 1 *)
        match cond_exp_result with
        | CEExp (se, ({ enode = Const c; eloc = loc; _ })) ->
          finish_exp [] se
            (match isConstTrueFalse c with
             | `CTrue -> Cil.one ~loc
             | `CFalse -> Cil.zero ~loc)
            Cil.intType
        | CEExp (se, ({ enode = UnOp (LNot, _, _); _ } as e)) ->
          (* already normalized to 0 or 1 *)
          finish_exp [] se e Cil.intType
        | CEExp (se, e) ->
          let e' =
            let te = Cil.typeOf e in
            let _, zte = Casts.castTo Cil.intType te (Cil.zero ~loc:e.eloc) in
            Cil.new_exp ~loc (BinOp(Ne, e, zte, Cil.intType))
          in
          finish_exp [] se e' Cil.intType
        | _ ->
          let tmp =
            make_new_tmp_var "<boolean expression>" true Cil.intType
          in
          let cond_chunk : Chunk.t =
            (* If we only care about the type of the result, there is no need to
               compile the conditional expression, as either way we would
               discard the side effects. *)
            match action with
            | AType -> Chunk.empty
            | _ ->
              compile_cond_exp ~ghost cond_exp_result
                (Chunk.empty +++
                 (Cil.mkStmtOneInstr ~ghost
                    (Set(Cil.var tmp, Cil.integer ~loc 1,loc)),
                  Chunk.no_effects))
                (Chunk.empty +++
                 (Cil.mkStmtOneInstr ~ghost
                    (Set(Cil.var tmp, Cil.integer ~loc 0,loc)),
                  Chunk.no_effects))
          in
          finish_exp []
            (Chunk.local_var_chunk cond_chunk tmp)
            (Cil.new_exp ~loc (Lval (Cil.var tmp)))
            Cil.intType
      end

    | Cabs.CALL(f, args) ->
      let (rf,sf, f', ft') =
        match f.Cabs.expr_node with
        (* Treat the variable case separate because we might be calling a
           function that does not have a prototype. In that case assume it takes
           ints as arguments. *)
        | Cabs.VARIABLE n -> begin
            try
              (* First look for polymorphic builtins. The typing rule is
                 luckily always the same one. *)
              let n = match n with
                | "__sync_add_and_fetch" | "__sync_sub_and_fetch"
                | "__sync_or_and_fetch" | "__sync_and_and_fetch"
                | "__sync_xor_and_fetch" | "__sync_nand_and_fetch"
                | "__sync_fetch_and_add" | "__sync_fetch_and_sub"
                | "__sync_fetch_and_or" | "__sync_fetch_and_and"
                | "__sync_fetch_and_xor" | "__sync_fetch_and_nand"
                | "__sync_bool_compare_and_swap"
                | "__sync_val_compare_and_swap"
                | "__sync_lock_release" | "__sync_lock_test_and_set"
                | "__atomic_load_n" | "__atomic_load"
                | "__atomic_store_n" | "__atomic_store"
                | "__atomic_exchange_n" | "__atomic_exchange"
                | "__atomic_compare_exchange_n" | "__atomic_compare_exchange"
                | "__atomic_add_fetch" | "__atomic_sub_fetch"
                | "__atomic_and_fetch" | "__atomic_xor_fetch"
                | "__atomic_or_fetch" | "__atomic_nand_fetch"
                | "__atomic_fetch_add" | "__atomic_fetch_sub"
                | "__atomic_fetch_and" | "__atomic_fetch_xor"
                | "__atomic_fetch_or" | "__atomic_fetch_nand"
                | "__c11_atomic_init" | "__c11_atomic_load"
                | "__c11_atomic_store" | "__c11_atomic_exchange"
                | "__c11_atomic_compare_exchange_strong"
                | "__c11_atomic_compare_exchange_weak"
                | "__c11_atomic_fetch_add" | "__c11_atomic_fetch_sub"
                | "__c11_atomic_fetch_and" | "__c11_atomic_fetch_xor"
                | "__c11_atomic_fetch_or"
                  ->
                  begin
                    match args with
                    | a1::_ ->
                      (* The available prototypes are
                         typ' f(typ* a1,typ a2,typ a3,...);
                         typ' f(typ* a1,typ a2,...);
                         typ' f(typ* a1,...);
                         Hence we just infer the right type
                         looking at the first argument. *)
                      let _,_,_,t =
                        do_expression local_env DontExpectConst a1 AType
                      in
                      let t = Cil.typeOf_pointed t in
                      Format.sprintf "%s_%sint%d_t"
                        n
                        (if Cil.isSignedInteger t then "" else "u")
                        (Cil.bitsSizeOf t)
                    | [] ->
                      Kernel.error ~once:true ~current:true
                        "Too few arguments for builtin %s" n;
                      n
                  end
                | _ -> n
              in
              let vi, _ = lookup_var n in
              let reads =
                if Cil_datatype.Lval.Set.mem
                    (Cil.var vi) local_env.authorized_reads
                || (vi.vglob && Cil.isFunctionType vi.vtype)
                then []
                else [ Cil.var vi ]
              in
              (reads, Chunk.unspecified_chunk Chunk.empty,
               Cil.new_exp ~loc:f.Cabs.expr_loc (Lval(Cil.var vi)), vi.vtype)
            (* Found. Do not use finish_exp. Simulate what = AExp None  *)
            with Not_found -> begin
                Kernel.debug ~level:3
                  "Calling function %s without prototype." n ;
                let ftype = TFun(Cil.intType, None, false,
                                 [Attr("missingproto",[])]) in
                (* Add a prototype to the environment *)
                let proto, _ =
                  make_global_varinfo false
                    (Cil.makeGlobalVar ~temp:false n ftype) in
                (* Make it EXTERN *)
                proto.vstorage <- Extern;
                FunsCalledWithoutPrototype.add proto;
                proto.vdecl <- f.Cabs.expr_loc;
                ImplicitPrototypeHook.apply proto;
                (* Add it to the file as well *)
                Globals.cabsPushGlobal
                  (GFunDecl (Cil.empty_funspec (),proto, f.Cabs.expr_loc));
                ([Cil.var proto],Chunk.unspecified_chunk Chunk.empty,
                 Cil.new_exp ~loc:f.Cabs.expr_loc (Lval(Cil.var proto)), ftype)
              end
          end
        | _ -> do_expression local_env DontExpectConst f (AExp None)
      in
      (* Get the result type and the argument types *)
      let (resType, argTypes, isvar, f'',attrs) =
        match Cil.unrollType ft' with
        | TFun(rt,at,isvar,attrs) -> (rt,at,isvar,f',attrs)
        | TPtr (t, _) -> begin
            match Cil.unrollType t with
            | TFun(rt,at,isvar,_) -> (* Make the function pointer
                                      * explicit  *)
              let f'' =
                match f'.enode with
                | AddrOf lv -> Cil.new_exp ~loc:f'.eloc (Lval(lv))
                | _ ->
                  Cil.new_exp ~loc:f'.eloc
                    (Lval (Cil.mkMem ~addr:f' ~off:NoOffset))
              in
              (rt, at, isvar, f'', [])
            | x ->
              Kernel.fatal ~current:true
                "Unexpected type of the called function %a: %a"
                Printer.pp_exp f' Printer.pp_typ x
          end
        | x ->
          Kernel.fatal ~current:true
            "Unexpected type of the called function %a: %a"
            Printer.pp_exp f' Printer.pp_typ x
      in
      let argTypesList = Cil.argsToList argTypes in
      (* Drop certain qualifiers from the result type *)
      let resType' = Cil.typeRemoveAttributes ["warn_unused_result"] resType in
      (* Before we do the arguments we try to intercept a few builtins. For
         these we have defined then with a different type, so we do not want to
         give warnings. We'll just leave the arguments of these functions alone.
      *)
      let isSpecialBuiltin =
        match f''.enode with
        | Lval (Var fv, NoOffset) -> Cil.is_special_builtin fv.vname
        | _ -> false
      in

      let force_rlarg_eval = Kernel.ForceRLArgEval.get () in
      (** If [force_rlarg_eval], make sure we evaluate args right-to-left. *)
      let force_right_to_left_evaluation (r,c, e, t) =
        (* If chunk is empty then it is not already evaluated. *)
        (* Constants don't need to be pulled out. *)
        if force_rlarg_eval && (not (Cil.isConstant e)) && not isSpecialBuiltin
        then
          (* Create a temporary. *)
          let tmp =
            make_new_tmp_var
              (Pretty_utils.sfprintf "%a" Cil_descriptive_printer.pp_exp e)
              true
              t
          in
          let c = Chunk.local_var_chunk c tmp in
          (* Create an instruction to give the e to the temporary. *)
          let i = Cil.mkStmtOneInstr ~ghost:local_env.is_ghost
              (Set(Cil.var tmp, e, loc))
          in
          (* Add the instruction to the chunk. *)
          (* Change the expression to be the temporary. *)
          (c +++ (i,Chunk.no_effects), Cil.new_exp ~loc (Lval(Cil.var tmp)), t)
        else
          (Chunk.add_reads loc r c, e, t)
      in
      let init_chunk =
        if force_rlarg_eval
        then Chunk.empty
        else Chunk.unspecified_chunk Chunk.empty
      in
      (* Do the arguments. In REVERSE order !!! Both GCC and MSVC do this. *)
      let rec loopArgs = function
        | ([], []) -> (init_chunk, [])

        | _, [] ->
          if not isSpecialBuiltin then
            Kernel.error ~once:true ~current:true
              "Too few arguments in call to %a." Printer.pp_exp f' ;
          (init_chunk, [])

        | ((_, at, _) :: atypes, a :: args) ->
          let (ss, args') = loopArgs (atypes, args) in
          (* Do not cast as part of translating the argument. We let the
             [castTo] do this work. This was necessary for test/small1/union5,
             in which a transparent union is passed as an argument. *)
          let (sa, a', att) = force_right_to_left_evaluation
              (do_expression local_env DontExpectConst a (AExp None)) in
          let (_, a'') = Casts.castTo att at a' in
          (ss @@ (sa, ghost), a'' :: args')

        | ([], args) -> (* No more types. *)
          if not isvar && argTypes != None && not isSpecialBuiltin then
            (* Do not give a warning for functions without a prototype. *)
            Kernel.error ~once:true ~current:true
              "Too many arguments in call to %a" Printer.pp_exp f';
          let rec loop = function
              [] -> (init_chunk, [])
            | a :: args ->
              let (ss, args') = loop args in
              let (sa, a', _) =
                force_right_to_left_evaluation
                  (do_expression local_env DontExpectConst a (AExp None))
              in
              (ss @@ (sa, ghost), a' :: args')
          in
          let (chunk,args as res) = loop args in
          (match argTypes, f''.enode with
           | Some _,_ ->
             if isvar then begin
               (* Use default argument promotion to infer the type of the
                  variadic actuals, see C11:6.5.2.2:7. *)
               promote_variadic_arguments res
             end else
               res
           | None, Lval (Var f, NoOffset)
             when not isSpecialBuiltin ->
             begin
               (* Use default argument promotion to infer the type of the
                  function, see 6.5.2.2.6. *)
               assert (not isvar);
               (* No nullary variadics see C11:6.7.6. *)
               let (prm_types,args) =
                 List.split
                   (Extlib.mapi default_argument_promotion args)
               in
               let typ = TFun (resType, Some prm_types, false,attrs) in
               Cil.update_var_type f typ;
               Cil.setFormalsDecl f typ;
               (chunk,args)
             end
           | None, _ -> res
           (* TODO: Treat function pointers.
              The issue is that their origin is more difficult to trace than
              plain variables (e.g. we'd have to take into account possible
              assignments, or update accordingly the signature of current
              function in case of a formal. *)
          )
      in
      let (sargs, args') = loopArgs (argTypesList, args) in
      (* Setup some pointer to the elements of the call. We may change these
         below. *)
      let s0 = Chunk.unspecified_chunk Chunk.empty in
      (* There is a sequence point between evaluations of args and the call
         itself, but we have to check that args wo side-effects (thus not
         appearing anywhere in sargs) are not modified by others... The call
         must thus be in the unspecified chunk. *)
      let sargs = if Chunk.is_empty sargs then Chunk.empty else sargs in
      let prechunk = ref ((s0 @@ (sf, ghost)) @@ (sargs, ghost)) in
      (* Do we actually have a call, or an expression? *)
      let piscall: bool ref = ref true in

      let pf: exp ref = ref f'' in (* Function to call. *)
      let pargs: exp list ref = ref args' in (* Arguments. *)
      let pis__builtin_va_arg: bool ref = ref false in
      let pwhat: exp_action ref = ref action in (* What to do with result. *)
      let locals = ref [] in

      (* If we do not have a call, this is the result. *)
      let pres: exp ref = ref (Cil.zero ~loc:expression.Cabs.expr_loc) in

      let prestype: typ ref = ref Cil.intType in

      (* Get the name of the last formal. *)
      let getNameLastFormal () : string =
        let typ = Cil.unrollType !current_fun_fundec.svar.vtype in
        match typ with
        | TFun(_, Some args, true, _) -> begin
            match List.rev args with
            | (last_par_name, _, _) :: _ -> last_par_name
            | _ -> ""
          end
        | _ -> ""
      in
      (* Check if the current function is variadic. *)
      let isCurrentFunctionVariadic () : bool =
        let typ = Cil.unrollType !current_fun_fundec.svar.vtype in
        match typ with
        | TFun (_result_typ, _formal_args, is_variadic, _attributes) -> is_variadic
        | _ -> assert false (* The type of a function must be a function type. *)
      in
      (* Try to intercept some builtins. *)
      (match (!pf).enode with
       | Lval(Var fv, NoOffset) -> begin
           (* Emit a warning if the given built-in argument is not a [va_list].
           *)
           let checkArgVaList arg_nr arg =
             let isArgVaList = Cil.isVariadicListType (Cil.typeOf arg) in
             if not isArgVaList then
               let arg_nr_string =
                 match arg_nr with
                 | None        -> ""
                 | Some arg_nr -> Printf.sprintf " %d" arg_nr
               in
               Kernel.warning ~current:true
                 "Invalid call to %a: the argument%s is not a va_list\n"
                 Printer.pp_varinfo fv arg_nr_string
           in
           match fv.vname with
           | "__builtin_stdarg_start" | "__builtin_va_start" ->
             begin
               match !pargs with
               | [va_list; parmN] -> begin
                   (* Check if the first argument is of type [va_list]. *)
                   checkArgVaList (Some 1) va_list;
                   (* Check if the calling function is variadic. *)
                   begin
                     if not (isCurrentFunctionVariadic ()) then
                       Kernel.warning ~current:true
                         "Invalid call to %a: the calling function %a is \
                          not variadic"
                         Printer.pp_varinfo fv
                         Printer.pp_varinfo !current_fun_fundec.svar
                   end;
                   (* Check if the last non-variadic argument is correct. *)
                   check_arg_parmN fv parmN;
                   (* Drop the [parmN] argument. *)
                   pargs := [ va_list ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %a\n"
                   Printer.pp_varinfo fv;
             end
           | "__builtin_va_arg" ->
             begin
               match !pargs with
               | [va_list; ({ enode = SizeOf resTyp; _ } as size)] ->
                 begin
                   (* Check if the first argument is of type [va_list]. *)
                   (* TODO: Cast check error shows up first... *)
                   checkArgVaList (Some 1) va_list;
                   (* Make a variable of the desired type. *)
                   let is_real, destlv, r, destlvtyp =
                     match !pwhat with
                     | ASet (is_real,lv, r, lvt) -> is_real, lv, r, lvt
                     | _ ->
                       let v = make_new_tmp_var "vararg" true resTyp in
                       locals := v::!locals;
                       false, Cil.var v, [], resTyp
                   in
                   pwhat := (ASet (is_real, destlv, r, destlvtyp));
                   pargs := [va_list; size;
                             Cil.new_exp ~loc
                               (CastE(Cil.voidPtrType,
                                      Cil.new_exp ~loc (AddrOf destlv)))];
                   pis__builtin_va_arg := true;
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %a"
                   Printer.pp_varinfo fv;
             end
           | "__builtin_va_copy" ->
             begin
               match !pargs with
               | [va_list_dest; va_list_src] ->
                 begin
                   (* Check if the arguments are of type [va_list]. *)
                   checkArgVaList (Some 1) va_list_dest;
                   checkArgVaList (Some 2) va_list_src;
                   pargs := [ va_list_dest; va_list_src ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %a"
                   Printer.pp_varinfo fv;
             end
           | "__builtin_va_end" ->
             begin
               match !pargs with
               | [va_list] ->
                 begin
                   (* Check if the argument is of type [va_list]. *)
                   checkArgVaList None va_list;
                   pargs := [ va_list ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %a"
                   Printer.pp_varinfo fv;
             end
           | "__builtin_varargs_start" ->
             (* We have to turn uses of __builtin_varargs_start into uses of
                [__builtin_stdarg_start] (because we have dropped the
                [__builtin_va_alist] argument from this function). *)
             begin
               (* Lookup the prototype for the replacement. *)
               let v, _  =
                 try lookup_global_var "__builtin_stdarg_start"
                 with Not_found ->
                   Kernel.abort ~current:true
                     "Cannot find __builtin_stdarg_start to replace %a"
                     Printer.pp_varinfo fv
               in
               pf := Cil.new_exp ~loc (Lval (Cil.var v))
             end
           |  "__builtin_next_arg" ->
             begin
               match !pargs with
               | last :: [] -> begin
                   let isOk =
                     match (dropCasts last).enode with
                     | Lval (Var lastv, NoOffset) ->
                       lastv.vname = getNameLastFormal ()
                     | _ -> false
                   in
                   if not isOk then
                     Kernel.warning ~current:true
                       "The argument in call to %a should be \
                        the last formal argument"
                       Printer.pp_varinfo fv;

                   pargs := [ ]
                 end
               | _ ->
                 Kernel.warning ~current:true "Invalid call to %a"
                   Printer.pp_varinfo fv;
             end
           | "__builtin_va_arg_pack" ->
             begin
               (match !pargs with
                | [ ] -> begin
                    piscall := false;
                    pres := Cil.new_exp ~loc:expression.Cabs.expr_loc (SizeOfE !pf);
                    prestype := Cil.(theMachine.typeOfSizeOf)
                  end
                | _ ->
                  Kernel.warning ~current:true
                    "Invalid call to builtin_va_arg_pack");
             end
           | "__builtin_constant_p" ->
             begin
               (* Drop the side-effects *)
               Chunk.remove_labels !prechunk;
               List.iter2
                 (fun cabs exp -> IgnoreSideEffectHook.apply(cabs, exp))
                 args
                 !pargs;
               prechunk := Chunk.empty;

               (* Constant-fold the argument and see if it is a constant. *)
               (match !pargs with
                | [ arg ] -> begin
                    match (Cil.constFold true arg).enode with
                    | Const _ -> piscall := false;
                      pres := Cil.integer ~loc:expression.Cabs.expr_loc 1 ;
                      prestype := Cil.intType

                    | _ -> piscall := false;
                      Kernel.warning ~current:true ~once:true
                        "cannot decide if __builtin_constant_p has a \
                         constant argument. Assuming it is not a constant.";
                      pres := Cil.integer ~loc:expression.Cabs.expr_loc 0;
                      prestype := Cil.intType
                  end
                | _ ->
                  Kernel.warning ~current:true
                    "Invalid call to builtin_constant_p")
             end
           | "__builtin_types_compatible_p" ->
             (* Constant-fold the argument and see if it is a constant. *)
             begin match !pargs with
               | [ { enode = SizeOf t1; _ }; { enode = SizeOf t2; _ }] ->
                 begin
                   (* Drop the side-effects. *)
                   prechunk := Chunk.empty;
                   piscall := false;
                   let compatible =
                     try ignore(combineTypes CombineOther t1 t2); true
                     with Failure _ -> false
                   in if compatible then
                     pres := Cil.integer ~loc 1
                   else
                     pres := Cil.integer ~loc 0;
                   prestype := Cil.intType
                 end
               | _ ->
                 Kernel.warning
                   ~once:true
                   ~current:true
                   "Invalid call to builtin_types_compatible_p"
             end
           | "__builtin_expect" ->
             begin
               match !pargs with
               | [ arg;_ ] ->
                 (* Keep all side-effects, including those steming from the
                    second argument. This is quite strange but compliant with
                    GCC's behavior. *)
                 piscall := false;
                 pres := arg
               | _ ->
                 Kernel.warning ~once:true ~current:true
                   "Invalid call to builtin_expect"
             end

           (* TODO: Only keep the side effects of the 1st or 2nd argument
              | "__builtin_choose_expr" ->
              begin match !pargs with
              | [ arg; e1; e2 ] ->
                begin
                  let constfolded = constFold true arg in
                  match constfolded.enode with
                  | Const _ ->
                    piscall := false;
                    if isZero constfolded then begin
                    (* Keep only 3rd arg side effects *)
                      (*TODO: prechunk := sf @@ (List.nth sargsl 2);*)
                      pres := e2;
                      prestype := typeOf e2
                    end else begin
                    (* Keep only 2nd arg side effects *)
                      (*TODO prechunk := sf @@ (List.nth sargsl 1);*)
                      pres := e1;
                      prestype := typeOf e1
                    end
                  | _ -> Kernel.warning ~once:true ~current:true
                    "builtin_choose_expr expects a constant first argument"
                end
              | _ ->
              Kernel.warning ~once:true ~current:true
                "Invalid call to builtin_choose_expr: 3 arguments are \
                 expected but %d are provided."
                (List.length !pargs)
              end *)
           | _ ->
             if as_const = ExpectConst then
               (* The last special case: we cannot allow a function call at this
                  point.*)
               begin
                 piscall := false;
                 Kernel.abort ~current:true
                   "Call to %a in constant." Printer.pp_varinfo fv;
               end
         end
       | _ -> ());

      (* Now we must finish the call. *)
      if !piscall then begin
        let addCall ?(is_real_var=true) calldest res t =
          let my_write =
            match calldest with
            | None -> []
            | Some c when is_real_var -> [c]
            | Some _ -> []
          in
          prechunk :=
            (Chunk.empty @@ (!prechunk, ghost)) +++
            (Cil.mkStmtOneInstr ~ghost:local_env.is_ghost
               (Call(calldest,!pf,!pargs,loc)),
             Chunk.make_effects ~modified:[] ~writes:my_write ~reads:rf);
          pres := res;
          prestype := t
        in
        match !pwhat with
        | ADrop -> addCall None (Cil.zero ~loc:expression.Cabs.expr_loc) resType'
        | AType -> prestype := resType'
        | ASet(is_real_var, lv, _, vtype) when !pis__builtin_va_arg ->
          (* Make an exception here for [__builtin_va_arg]. *)
          addCall
            ~is_real_var
            None
            (Cil.new_exp ~loc:expression.Cabs.expr_loc (Lval(lv)))
            vtype

        | ASet(is_real_var, lv, _, vtype)
          when (allow_return_collapse ~f_typ:resType' ~lval_typ:vtype)
          ->
          (* We can assign the result directly to [lv]. *)
          addCall
            ~is_real_var
            (Some lv)
            (Cil.new_exp ~loc:expression.Cabs.expr_loc (Lval(lv)))
            vtype

        | _ -> begin
            let restype'' = match !pwhat with
              | AExp (Some t)
                when allow_return_collapse ~f_typ:resType' ~lval_typ:t -> t
              | _ -> resType'
            in
            let descr =
              Pretty_utils.sfprintf "%a(%a)"
                Cil_descriptive_printer.pp_exp !pf
                (Pretty_utils.pp_list ~sep:", "
                   Cil_descriptive_printer.pp_exp)
                !pargs
            in
            let tmp = make_new_tmp_var descr false restype'' in
            tmp.vdecl <- loc;
            locals:=tmp::!locals;
            (* Remember that this variable has been created for this specific
               call. We will use this in [collapseCallCast]. *)
            CallTempVars.add tmp;
            addCall
              ~is_real_var:false
              (Some (Cil.var tmp))
              (Cil.new_exp ~loc:expression.Cabs.expr_loc (Lval(Cil.var tmp)))
              restype'';
          end
      end;
      List.iter
        (fun v -> prechunk:= Chunk.local_var_chunk !prechunk v) !locals;
      finish_exp [] !prechunk !pres !prestype

    | Cabs.COMMA el ->
      if as_const = ExpectConst then
        Kernel.warning ~current:true "COMMA in constant";
      (* We must ignore [AExpLeaveArrayFun] (a.k.a. 'do not decay pointers')
         if the expression at hand is a sequence with strictly more than one
         expression, because the exception for sizeof and typeof only apply when
         the expression is directly the argument of the operators.
         See C99 and C11 6.3.2.1p3.) *)
      let what =
        if action <> AExpLeaveArrayFun || List.length el = 1
        then action
        else (AExp None)
      in
      let rec loop sofar = function
        | [e] ->
          let (r, se, e', t') =
            do_expression local_env DontExpectConst e what
          in
          (* Pass on the action. *)
          (r, sofar @@ (se, ghost), e', t')
        | e :: rest ->
          let (_, se, _, _) = do_expression local_env DontExpectConst e ADrop in
          loop (sofar @@ (se, ghost)) rest
        | [] -> Kernel.fatal ~current:true "empty COMMA expression"
      in
      loop Chunk.empty el

    | Cabs.QUESTION (e1, e2, e3) -> begin
        (* Compile the conditional expression. *)
        let ghost = local_env.is_ghost in
        let ce1 = do_cond_exp local_env as_const e1 in
        let what' = match action with
          | ADrop -> ADrop
          | _ -> AExp None
        in
        (* If we are evaluating a constant expression, [e1] is supposed to
           evaluate to either true or false statically, and we can type-check
           only the appropriate branch. In fact, 6.6p3 seems to indicate that
           the dead branch can contain sub-expressions that are normally
           forbidden in a constant expression context, such as function calls.
        *)
        let is_true_cond = evaluate_cond_exp ce1 in
        if as_const = ExpectConst && is_true_cond = `CTrue then begin
          match e2.Cabs.expr_node with
          | Cabs.NOTHING ->
            (match ce1 with
             | CEExp (_,e) -> finish_exp [] Chunk.empty e (Cil.typeOf e)
             | _ ->
               finish_exp
                 [] Chunk.empty (Cil.one ~loc:e2.Cabs.expr_loc) Cil.intType
                 (* [e1] is the result of logic operations that by definition of
                    this branch evaluate to one. *))
          | _ ->
            let _,_,e2,t2 = do_expression local_env as_const e2 what' in
            finish_exp [] Chunk.empty e2 t2
        end else if as_const = ExpectConst && is_true_cond = `CFalse then begin
          let _,_,e3,t3 = do_expression local_env as_const e3 what' in
          finish_exp [] Chunk.empty e3 t3
        end else begin
          (* Now we must find the type of both branches, in order to compute
             the type of the result. *)
          let r2, se2, e2'o (* Is an option. None means use [e1]. *), t2 =
            match e2.Cabs.expr_node with
            | Cabs.NOTHING -> begin (* The same as the type of [e1]. *)
                match ce1 with
                | CEExp (_, e1') ->
                  [], Chunk.unspecified_chunk Chunk.empty, None, Cil.typeOf e1'
                (* Do not promote to bool *)
                | _ ->
                  [], Chunk.unspecified_chunk Chunk.empty, None, Cil.intType
              end
            | _ ->
              let r2, se2, e2', t2 =
                do_expression local_env as_const e2 what'
              in
              r2, se2, Some e2', t2
          in
          (* Do e3 for real *)
          let r3, se3, e3', t3 = do_expression local_env as_const e3 what' in
          if not (Chunk.is_empty se2) then
            ConditionalSideEffectHook.apply (expression,e2);
          if not (Chunk.is_empty se3) then
            ConditionalSideEffectHook.apply (expression,e3);
          let tresult = lazy (conditionalConversion t2 t3) in
          match ce1 with
          | CEExp (se1, e1')
            when isExpTrueFalse e1' = `CFalse && Chunk.can_drop se2 ->
            let tresult = Lazy.force tresult in
            finish_exp r3 ((Chunk.empty @@ (se1, ghost)) @@ (se3, ghost))
              (snd (Casts.castTo t3 tresult e3')) tresult
          | CEExp (se1, e1')
            when isExpTrueFalse e1' = `CTrue && Chunk.can_drop se3 ->
            let tresult = Lazy.force tresult in
            begin
              match e2'o with
              | None -> (* use e1' *)
                finish_exp r2
                  ((Chunk.empty @@ (se1, ghost)) @@ (se2, ghost))
                  (snd (Casts.castTo t2 tresult e1')) tresult
              | Some e2' ->
                finish_exp r2
                  ((Chunk.empty @@ (se1, ghost)) @@ (se2, ghost))
                  (snd (Casts.castTo t2 tresult e2')) tresult
            end
          | _ when what' = ADrop ->
            (* We are not interested by the result, but might want to evaluate
               [e2] and [e3] if they are dangerous expressions. *)
            (* Dummy result, that will be ultimately be dropped but the
               type must be the correct one and not [int]. *)
            let res = Cil.zero ~loc in
            (* GCC extension: conditional conversion is not enforced if the
               result is to be dropped whereas ISO 6.5.15-3 forces it. *)
            if not (Cil.gccMode ()) then
              ignore (Lazy.force tresult);
            begin match e2'o with
              | None when Is_dangerous.exp e3' || not (Chunk.is_empty se3) ->
                let descr =
                  Pretty_utils.sfprintf "%a" Cprint.print_expression e1
                in
                let tmp = make_new_tmp_var descr true t3 in
                let tmp_var = Cil.var tmp in
                let tmp_lval =
                  Cil.new_exp ~loc:expression.Cabs.expr_loc (Lval (tmp_var))
                in
                let (r1, se1, _, _) =
                  do_expression
                    local_env as_const e1
                    (ASet(false, tmp_var, [], t3))
                in
                let se1 = Chunk.local_var_chunk se1 tmp in
                let dangerous =
                  if Is_dangerous.exp e3' then
                    Chunk.keep_pure_exp
                      ~make_new_tmp_var:(make_new_tmp_var ?prefix:None)
                      ~ghost e3' loc
                  else Chunk.Make.skip_chunk
                in
                finish_exp (r1@r3)
                  ((Chunk.empty @@ (se1, ghost)) @@
                   (Chunk.Make.if_chunk ~ghost tmp_lval loc
                      Chunk.Make.skip_chunk
                      (se3 @@ (dangerous, ghost)), ghost))
                  res
                  t3
              | None ->
                (* We can drop [e3], just keep e1 in case it is dangerous. *)
                let (r1, se1, e1, t1) =
                  do_expression local_env as_const e1 ADrop
                in
                let dangerous =
                  if Is_dangerous.exp e1 then
                    Chunk.keep_pure_exp
                      ~make_new_tmp_var:(make_new_tmp_var ?prefix:None)
                      ~ghost e1 loc
                  else Chunk.Make.skip_chunk
                in
                finish_exp (r1@r3) (se1 @@ (dangerous, ghost)) res t1
              | Some e2'
                when Is_dangerous.exp e2' || Is_dangerous.exp e3'
                     || not (Chunk.is_empty se2) || not (Chunk.is_empty se3) ->
                (* We have to keep [e1] in order to know which dangerous
                   expression is to be evaluated. *)
                let se2 =
                  if Is_dangerous.exp e2' then
                    se2 @@
                    (Chunk.keep_pure_exp
                       ~make_new_tmp_var:(make_new_tmp_var ?prefix:None)
                       ~ghost e2' loc, ghost)
                  else se2
                in
                let se3 =
                  if Is_dangerous.exp e3' then
                    se3 @@ (Chunk.keep_pure_exp
                              ~make_new_tmp_var:(make_new_tmp_var ?prefix:None)
                              ~ghost e3' loc, ghost)
                  else se3
                in
                let cond : Chunk.t =
                  (* If we only care about the type of the result, there is no
                     need to compile the conditional expression, as either way
                     we would discard the side effects. *)
                  match action with
                  | AType -> Chunk.empty
                  | _ -> compile_cond_exp ~ghost ce1 se2 se3
                in
                finish_exp (r2@r3) cond res t2
              | Some _ ->
                (* We just keep e1 in case it is dangerous. Everything else can
                   be dropped. *)
                let (r1, se1, e1, t1) =
                  do_expression local_env as_const e1 ADrop
                in
                let dangerous =
                  if Is_dangerous.exp e1 then
                    Chunk.keep_pure_exp
                      ~make_new_tmp_var:(make_new_tmp_var ?prefix:None)
                      ~ghost e1 loc
                  else Chunk.Make.skip_chunk
                in
                finish_exp (r1@r2@r3) (se1 @@ (dangerous, ghost)) res t1
            end
          | _ -> (* Use a conditional. *)
            let tresult = Lazy.force tresult in
            begin match e2'o with
              | None -> (* Has form "e1 ? : e3". *)
                let descr =
                  Pretty_utils.sfprintf "%a" Cprint.print_expression e1
                in
                let tmp = make_new_tmp_var descr true tresult in
                let tmp_var = Cil.var tmp in
                let tmp_lval = Cil.new_exp ~loc:expression.Cabs.expr_loc (Lval (tmp_var)) in
                let (r1,se1, _, _) =
                  do_expression
                    local_env as_const e1 (ASet(false, tmp_var, [], tresult))
                in
                let se1 = Chunk.local_var_chunk se1 tmp in
                let newWhat = ASet(false,tmp_var, [], tresult) in
                let r3,se3,_,_ = finish_exp ~newWhat r3 se3 e3' t3 in
                finish_exp
                  (r1@r3)
                  ((Chunk.empty @@ (se1, ghost)) @@
                   (Chunk.Make.if_chunk ~ghost tmp_lval loc Chunk.Make.skip_chunk se3, ghost))
                  tmp_lval
                  tresult
              | Some e2' ->
                let is_real, lv, r, lvt, scope_chunk =
                  match action with
                  | ASet (is_real, lv, r, lvt) ->
                    is_real, lv, r, lvt, Chunk.empty
                  | _ ->
                    let descr =
                      Pretty_utils.sfprintf "%a?%a:%a"
                        Cprint.print_expression e1
                        Cil_descriptive_printer.pp_exp e2'
                        Cil_descriptive_printer.pp_exp e3'
                    in
                    let tmp = make_new_tmp_var descr true tresult in
                    false, Cil.var tmp, [], tresult,
                    Chunk.local_var_chunk Chunk.empty tmp
                in
                (* Now do [e2] and [e3] for real. *)
                let (r2,se2, _, _) =
                  finish_exp ~newWhat:(ASet(is_real,lv,r,lvt))
                    r2 se2 e2' t2
                in
                let (r3, se3, _, _) =
                  finish_exp ~newWhat:(ASet(is_real,lv, r, lvt))
                    r3 se3 e3' t3
                in
                let cond : Chunk.t =
                  (* If we only care about the type of the result, there is no
                     need to compile the conditional expression, as either way
                     we would discard the side effects. *)
                  match action with
                  | AType -> Chunk.empty
                  | _ -> compile_cond_exp ~ghost ce1 se2 se3
                in
                finish_exp
                  (r2@r3)
                  (scope_chunk @@ (cond, ghost))
                  (Cil.new_exp ~loc (Lval lv)) tresult
            end
        end
      end
    | Cabs.GNU_BODY b -> begin
        (* Find the last [Cabs.COMPUTATION] and remember it. This one is invoked
           on the reversed list of statements. *)
        let findLastComputation = function
            s :: _  ->
            let rec findLast st = match st.Cabs.stmt_node with
              | Cabs.SEQUENCE (_, s, _) -> findLast s
              | Cabs.CASE (_, s, _) -> findLast s
              | Cabs.CASERANGE (_, _, s, _) -> findLast s
              | Cabs.LABEL (_, s, _) -> findLast s
              | Cabs.COMPUTATION _ ->
                begin
                  match local_env.is_ghost,st.Cabs.stmt_ghost with
                  | true,true | false, false -> st
                  | true, false -> assert false
                  | false, true -> raise Not_found
                end
              | _ -> raise Not_found
            in
            findLast s
          | [] -> raise Not_found
        in
        (* Save the previous data. *)
        let old_gnu = ! gnu_body_result in
        let drop_block_result =
          match action with
          | ADrop -> true
          | _ -> false
        in
        let lastComp =
          try
            findLastComputation (List.rev b.Cabs.bstmts)
          with Not_found ->
          match action with
          | ADrop -> (* We are dropping the result and have no computation. *)
            Cabs.{stmt_ghost = local_env.is_ghost; stmt_node = Cabs.NOP loc}
          | _ ->
            Kernel.abort ~current:true
              "missing last expression in GNU statement-expression"
        in
        let loc = Cabshelper.get_statementloc lastComp in
        (* Prepare some data to be filled by [do_expression] ghost. *)
        let data : (exp * typ) option ref = ref None in
        gnu_body_result := (lastComp, data, drop_block_result);

        let se = doBody local_env b in

        gnu_body_result := old_gnu;
        match !data with
        | None when drop_block_result ->
          finish_exp [] se (Cil.zero ~loc:expression.Cabs.expr_loc) Cil.voidType
        | None ->
          Kernel.abort ~current:true
            "missing result in GNU statement-expression"
        | Some (e, t) ->
          let is_void = Cil.isVoidType t in
          let se, e =
            match se.Chunk.stmts_with_effects with
            | [ { skind = Block b; _ }, _effects, _calls ]
              when not is_void && not drop_block_result ->
              let vi = make_new_tmp_var "GNU.body" true t in
              b.bstmts <-
                b.bstmts @
                [Cil.mkStmtOneInstr ~ghost:local_env.is_ghost
                   (Set (Cil.var vi, e,loc))];
              (Chunk.local_var_chunk se vi,Cil.new_exp ~loc (Lval (Cil.var vi)))
            | _ -> se, e
          in
          finish_exp [] se e t
      end

    | Cabs.LABELADDR l -> (* GCC's taking the address of a label. *)
      let label = (* To support locally declared labels. *)
        Labels.Manage.lookup_label l
      in
      let addrval = ComputedGoto.get_target_hash label
      (* match ComputedGoto.get_target_hash label with
         | Some address -> address
         | None ->
           let target_next_address = ComputedGoto.get_target_next_address ()
           in
           ComputedGoto.add_target_hash label target_next_address;
           target_next_address *)
      in
      finish_exp []
        (Chunk.unspecified_chunk Chunk.empty)
        (makeCast ~e:(Cil.integer ~loc addrval) ~newt:Cil.voidPtrType)
        Cil.voidPtrType

    | Cabs.EXPR_PATTERN _ ->
      Kernel.abort ~current:true "EXPR_PATTERN in cabs2cil input"

  in
  (* let (_a,b,_c,_d) = result in
     Format.eprintf "do_expression ~const:%b ~e:" asconst ;
     Cprint.print_expression e;
     Format.eprintf "@.";
     Format.eprintf "Got: chunk:'%a'@." d_chunk b; *)
  Cil.CurrentLoc.set old_loc;
  result

(* Constant fold a conditional. This is because we want to avoid having
   conditionals in the initializers. So, we try very hard to avoid creating
   new statements. *)
and do_cond_exp local_env as_const
    (* Try to evaluate the conditional expression
       to TRUE or FALSE, because it occurs in a constant. *)
    ?ctxt (* [ctxt] is used internally to determine if we should apply the
             conditional side effects hook (see above) and should not appear
             (i.e. be None) in toplevel calls. *)
    (exp : Cabs.expression) : cond_exp_result =
  let ghost = local_env.is_ghost in
  let rec add_chunk_before_const_exp chunk_0 cond_exp_res =
    let chunk_0 = Chunk.remove_effects chunk_0 in
    match cond_exp_res with
    | CEExp (chunk, exp) ->
      CEExp ((Chunk.empty @@ (chunk_0, ghost)) @@ (chunk, ghost), exp)
    | CEAnd (cond_exp_result_1, cond_exp_result_2) ->
      CEAnd (add_chunk_before_const_exp chunk_0 cond_exp_result_1,
             cond_exp_result_2)
    | CEOr (cond_exp_result_1, cond_exp_result_2) ->
      CEOr (add_chunk_before_const_exp chunk_0 cond_exp_result_1,
            cond_exp_result_2)
    | CENot cond_exp_result ->
      CENot (add_chunk_before_const_exp chunk_0 cond_exp_result)
  in
  let rec can_drop_const_exp : cond_exp_result -> bool =
    function
    | CEExp (chunk, _exp) -> Chunk.can_drop chunk
    | CEAnd (cond_exp_result_1, cond_exp_result_2)
    | CEOr (cond_exp_result_1, cond_exp_result_2) ->
      can_drop_const_exp cond_exp_result_1 &&
      can_drop_const_exp cond_exp_result_2
    | CENot cond_exp_result_1 ->
      can_drop_const_exp cond_exp_result_1
  in
  let rec remove_effects_const_exp : cond_exp_result -> cond_exp_result =
    function
    | CEExp (chunk, exp) -> CEExp (Chunk.remove_effects chunk, exp)
    | CEAnd (cond_exp_result_1, cond_exp_result_2) ->
      CEAnd (remove_effects_const_exp cond_exp_result_1,
             remove_effects_const_exp cond_exp_result_2)
    | CEOr (cond_exp_result_1, cond_exp_result_2) ->
      CEOr (remove_effects_const_exp cond_exp_result_1,
            remove_effects_const_exp cond_exp_result_2)
    | CENot cond_exp_result ->
      CENot (remove_effects_const_exp cond_exp_result)
  in
  let loc = exp.Cabs.expr_loc in
  let cond_exp_result =
    match exp.Cabs.expr_node with
    | Cabs.BINARY (Cabs.AND, and_exp_1, and_exp_2) ->
      begin
        let cond_exp_result_1 =
          do_cond_exp local_env as_const ?ctxt and_exp_1
        in
        let cond_exp_result_2 =
          do_cond_exp local_env as_const ~ctxt:exp and_exp_2
        in
        let cond_exp_result_1 =
          remove_effects_const_exp cond_exp_result_1
        in
        match cond_exp_result_1, cond_exp_result_2 with
        | CEExp (chunk_1, ({ enode = Const constant_1; _ })), _ ->
          begin
            match isConstTrueFalse constant_1 with
            | `CTrue -> add_chunk_before_const_exp chunk_1 cond_exp_result_2
            | `CFalse when can_drop_const_exp cond_exp_result_2 ->
              cond_exp_result_1
            | `CFalse ->
              (* [chunk_2] might contain labels so we cannot always drop it. *)
              assert (not (can_drop_const_exp cond_exp_result_2));
              CEAnd (cond_exp_result_1, cond_exp_result_2)
          end
        | CEExp(chunk_1, exp_1), CEExp (chunk_2, exp_2)
          when Cil.(theMachine.useLogicalOperators)
            && Chunk.is_empty chunk_1
            && Chunk.is_empty chunk_2 ->
          CEExp (Chunk.empty,
                 Cil.new_exp ~loc
                   (BinOp
                      (LAnd,
                       makeCast ~e:exp_1 ~newt:Cil.intType,
                       makeCast ~e:exp_2 ~newt:Cil.intType,
                       Cil.intType)))
        | _ -> CEAnd (cond_exp_result_1, cond_exp_result_2)
      end

    | Cabs.BINARY (Cabs.OR, exp_1, exp_2) ->
      begin
        let cond_exp_result_1 = do_cond_exp local_env as_const ?ctxt exp_1 in
        let cond_exp_result_2 =
          do_cond_exp local_env as_const ~ctxt:exp exp_2
        in
        let cond_exp_result_1 = remove_effects_const_exp cond_exp_result_1 in
        match cond_exp_result_1, cond_exp_result_2 with
        | CEExp (chunk_1, ({ enode = Const constant_1; _ })), _ ->
          begin
            match isConstTrueFalse constant_1 with
            | `CFalse -> add_chunk_before_const_exp chunk_1 cond_exp_result_2
            | `CTrue when can_drop_const_exp cond_exp_result_2 ->
              cond_exp_result_1
            | `CTrue ->
              (* [chunk_2] might contain labels so we cannot drop it. *)
              CEOr (cond_exp_result_1, cond_exp_result_2)
          end
        | CEExp (chunk_1, exp_1), CEExp (chunk_2, exp_2)
          when Cil.(theMachine.useLogicalOperators)
            && Chunk.is_empty chunk_1
            && Chunk.is_empty chunk_2 ->
          CEExp (Chunk.empty,
                 Cil.new_exp ~loc
                   (BinOp
                      (LOr,
                       makeCast ~e:exp_1 ~newt:Cil.intType,
                       makeCast ~e:exp_2 ~newt:Cil.intType,
                       Cil.intType)))
        | _ -> CEOr (cond_exp_result_1, cond_exp_result_2)
      end

    | Cabs.UNARY (Cabs.NOT, exp_1) ->
      begin
        match do_cond_exp local_env as_const ?ctxt exp_1 with
        | CEExp (chunk_1, exp_1) when Chunk.is_empty chunk_1 ->
          let typ_1 = Cil.typeOf exp_1 in
          if not ((Cil.isPointerType typ_1) || (Cil.isArithmeticType typ_1))
          then Kernel.error ~once:true ~current:true "Bad operand to !";
          CEExp (Chunk.empty, Cil.new_exp ~loc (UnOp (LNot, exp_1, Cil.intType)))
        | cond_exp_result_1 -> CENot cond_exp_result_1
      end

    | _ ->
      let (reads, chunk, exp', typ) =
        do_expression local_env as_const exp (AExp None)
      in
      (* No need to add reads here: we'll always have a sequence point, either
         because the expression is complete, or because of a logic operator. *)
      begin
        match ctxt with
        | None -> ()
        | Some _ when Chunk.is_empty chunk -> ()
        | Some orig_exp -> ConditionalSideEffectHook.apply (orig_exp, exp)
      end;
      ignore (checkBool typ exp');
      let should_const_fold =
        as_const = ExpectConst ||
        as_const = DontExpectConstButSimplifyIfPossible
      in
      let result_chunk = Chunk.add_reads exp.Cabs.expr_loc reads chunk in
      let result_exp =
        if should_const_fold || Cil.(theMachine.lowerConstants)
        then Cil.constFold should_const_fold exp'
        else exp'
      in
      CEExp (result_chunk, result_exp)
  in
  cond_exp_result

(* A special case for conditionals. *)
and do_condition local_env (is_const: bool)
    (* If we are in constants, we do our best to eliminate the conditional. *)
    (cond_exp : Cabs.expression)
    (chunk_if_true : Chunk.t)
    (chunk_if_false : Chunk.t) : Chunk.t =
  if Chunk.is_empty chunk_if_true && Chunk.is_empty chunk_if_false
  (* TODO: Ignore the attribute [TIS_KERNEL_KEEP_BLOCK]. *)
  then
    begin
      let (_reads, cond_exp_chunk, exp, _typ) =
        do_expression local_env DontExpectConst cond_exp ADrop
      in
      match Is_dangerous.exp exp with
      | true ->
        let ghost = local_env.is_ghost in
        let tmp_varinfo = make_new_tmp_var ?prefix:None in
        cond_exp_chunk @@
        (Chunk.keep_pure_exp ~make_new_tmp_var:tmp_varinfo ~ghost exp exp.eloc,
         ghost)
      | false when Chunk.is_empty cond_exp_chunk ->
        let name = !current_fun_fundec.svar.vorig_name in
        IgnorePureExpHook.apply (name, exp);
        cond_exp_chunk
      | false ->
        assert (not (Chunk.is_empty cond_exp_chunk));
        cond_exp_chunk
    end
  else
    begin
      let const_exp_result =
        let as_const = as_const_of_is_const is_const in
        do_cond_exp local_env as_const cond_exp
      in
      let chunk =
        compile_cond_exp ~ghost:local_env.is_ghost
          const_exp_result chunk_if_true chunk_if_false
      in
      chunk
    end

and do_pure_exp local_env (exp : Cabs.expression) : exp =
  let (_, chunk, exp', _) =
    do_expression local_env ExpectConst exp (AExp None)
  in
  if Chunk.is_not_empty chunk then
    Kernel.error ~once:true ~current:true
      "%a has side-effects" Cprint.print_expression exp;
  exp'

and do_full_exp local_env as_const (exp : Cabs.expression) action =
  let (reads, chunk, exp, typ) = do_expression local_env as_const exp action in
  let chunk = Chunk.add_reads exp.eloc reads chunk in
  (* There is a sequence point after a full expression. *)
  let chunk = Chunk.empty @@ (chunk, local_env.is_ghost) in
  chunk, exp, typ

(** Return the accumulated chunk, the initializer and the new type (might be
    different if the initialized object is an array of unspecified size). *)
and do_initializer local_env (host_varinfo : varinfo)
    (init_exp : Cabs.init_expression) : Chunk.t * init * typ =
  let open Initialization.Initializer in

  let dkey = category_initializer in

  (* 1. Check if the host's type is valid for initialization. *)
  (* Following C11:6.7.9p3 :
     "The type of the entity to be initialized shall be an array of unknown size
     or a complete object type that is not a variable length array type." *)
  let has_complete_type =
    let allowZeroSizeArrays = true in
    match Cil.unrollType host_varinfo.vtype with
    | TArray(array_base_type, None, _, _) ->
      Cil.isCompleteType ~allowZeroSizeArrays array_base_type
    | initialized_obj_type ->
      Cil.isCompleteType ~allowZeroSizeArrays initialized_obj_type
  in
  if not has_complete_type then
    Kernel.abort ~current:true
      "variable `%a' has initializer but incomplete type %a"
      Printer.pp_varinfo host_varinfo
      Printer.pp_typ host_varinfo.vtype;

  let is_variable_sized_array =
    Datatype.Int.Hashtbl.mem variable_size_arrays host_varinfo.vid
  in
  if is_variable_sized_array then
    Kernel.abort ~current:true
      "variable `%a' has initializer but is a variable length array type"
      Printer.pp_varinfo host_varinfo;

  (* 2. Prepare the preinitializer. *)
  Kernel.debug ~dkey
    "@\n-- Preparing a preinitializer for %s : %a@\n"
    host_varinfo.vname Printer.pp_typ host_varinfo.vtype;
  let preinit, remaining_initializers =
    let so = Subobj.of_varinfo host_varinfo in
    do_init local_env host_varinfo.vglob Extlib.nop Preinit.none so
      [ (Cabs.NEXT_INIT, init_exp) ]
  in
  if remaining_initializers <> [] then
    (* TODO: fatal or warning? can this actually happen? *)
    Kernel.fatal ~current:true
      "Some excess initializers left that were not treated by do_init.";

  (* 3. Translate the preinitializer to a form of a CIL initializer
     and a chunk of side-effects. *)
  Kernel.debug ~dkey
    "-- Translating the preinitializer to initializer for %s@\n"
    host_varinfo.vname;
  let init, typ', side_effects_chunk =
    (* Following C11:6.7.9p23 :
       "The evaluations of the initialization list expressions are
       indeterminately sequenced with respect to one another and thus the order
       in which any side effects occur is unspecified. 152)"
       And C11:footnote 152 :
       "In particular, the evaluation order need not be the same as the order of
       subobject initialization." *)
    let accumulate_f side_effects_acc (reads, chunk, exp, typ) =
      Kernel.debug "accumulate_f: reads = %a, exp = %a, side effects %s = %a"
        (Pretty_utils.pp_list Printer.pp_lval) reads
        Printer.pp_exp exp
        (if Chunk.is_empty chunk then "(EMPTY)" else "")
        Chunk.d_chunk chunk;
      (* First always add the information about reads that came from the
         [do_expression] function call on the Cabs expression. *)
      let chunk' = Chunk.add_reads exp.eloc reads chunk in
      (* If there may be any evaluation order issues then we:
         - add a temporary variable for each expression's value,
         - encapsulate the whole chunk corresponding to each expression in a
           separate block. *)
      (* a) If a global variable is initialized then there should be no side
            effects. *)
      let initialized_var_is_global = host_varinfo.vglob in
      (* b) If the initialization expression has no side effects, then we do not
            need to care when it is evaluated. *)
      let no_side_effects = Chunk.is_empty chunk in
      (* c) If this is a single initializer, then the order of evaluation is not
            an issue. *)
      let init_expr_is_single = Preinit.is_single preinit in
      let chunk', exp' =
        if initialized_var_is_global || no_side_effects || init_expr_is_single
        then
          (* It is not neccessary to do anything, so we keep the existing
             expression and side effects chunk. *)
          chunk', exp
        else
          (* The evaluation order may cause problems, we have to take
             precautions: *)
          (* 1. We introduce a temporary variable.
             This way we can store the initialization expression's value at this
             point in case if it changes later because of other side effects. *)
          let chunk', exp' =
            Kernel.debug "accumulate_f: introducing a tmp var!";
            (* Prepare the temporary variable for the assignment.  *)
            let init_tmp_var : varinfo =
              make_new_tmp_var ~prefix:"init" "init_tmp_var" false typ
            in
            let init_tmp_var_lval : lval = Cil.var init_tmp_var in
            let chunk' =
              let set_init_tmp_var_stmt =
                let instr =
                  Set (init_tmp_var_lval, exp, Cil.CurrentLoc.get ())
                in
                Cil.mkStmtOneInstr ~ghost:local_env.is_ghost instr
              in
              let effects =
                let modified = [init_tmp_var_lval] in
                let writes = [init_tmp_var_lval] in
                let reads = reads in
                Chunk.make_effects ~modified ~writes ~reads
              in
              (* TODO: Shouldn't we put something more (i.e. from [exp]) in the
                 [reads]? I cannot understand exactly the code in [finish_exp]
                 that is basically doing the same thing and adds some more
                 stuff here... Either way, this seems to work rather fine. *)
              chunk' +++ (set_init_tmp_var_stmt, effects)
            in
            (* Add the temporary variable to the chunk's local variables. *)
            let chunk' = Chunk.local_var_chunk chunk' init_tmp_var in
            (* The value to assign to the initialized lval will be the value of
               this temporary variable. *)
            let exp' = Cil.new_exp ~loc:exp.eloc (Lval (init_tmp_var_lval)) in
            chunk', exp'
          in
          (* 2. Wrap all that in a block.
             This is necessary, because side effects for a single initialization
             expression should be evaluated in determined order (thus they are
             encapsulated in a determined-order-evaluation block), while
             evaluation of different simple initialization expressions inside a
             complex initialization expression may happen in any order (thus
             globally it should be a undertermined-order-evaluation block). *)
          (* TODO: Is this actually necessary? Basically when always wrapping it
             all in a block everything works all right for sure, so better safe
             than sorry. But one day it should be investigated, as it may
             sometimes automatically add a spurious temporary variable. *)
          let chunk' =
            Chunk.of_chunk_encapsulate_in_block ~ghost:local_env.is_ghost chunk'
          in
          (* 3. Modified side effects chunk and the initialization expression
                are ready. *)
          chunk', exp'
      in
      let side_effects_acc' =
        side_effects_acc @@ (chunk', local_env.is_ghost)
      in
      (side_effects_acc', exp')
    in
    Preinit.to_init ~accumulate_f preinit host_varinfo.vtype
  in

  (* 4. Finish up. *)
  Kernel.debug ~dkey
    "-- Finished the initializer for %s@\n  init=%a@\n  typ=%a@\n  acc=%a@\n"
    host_varinfo.vname
    Printer.pp_init init
    Printer.pp_typ host_varinfo.vtype
    Chunk.d_chunk side_effects_chunk;

  let side_effects_chunk' =
    Chunk.empty @@ (side_effects_chunk, local_env.is_ghost)
  in
  side_effects_chunk', init, typ'

(* Consume some initializers. This is used by both global and local variables
   initialization. Arguments:
   - [local_env] is the current environment.
   - [is_const] is used to indicate that expressions must be compile-time
     constant (i.e. if we are in a global initializer).
   - [add_implicit_ensures] is a callback to add an ensures clause to contracts
     above current initialized part when it is partially initialized.
     Does nothing initially. Useful only for initialization of locals.
   - [preinit] corresponds to the initializers seen previously (for globals).
   - [so] contains the information about the subobject currently being
     initialized.
   - [initializers] is the list of initializers that still have to be processed.
   [do_init] returns a tuple:
   - a preinitializer corresponding to the complete initialization,
   - the list of unused initializers if any (should be empty). *)
and do_init
    local_env is_const add_implicit_ensures
    preinit subobj initializers =
  let open Initialization.Initializer in

  (* Prepare debug printing. *)
  let dkey = category_initializer in

  let all_initializers =
    (* TODO: What are we doing here with casts?
       Well, I'm not exactly sure, but it seems to work and it does not seem
       really important to understand why... Maybe some tests with casts in
       initializers should be added to check that. *)
    let initializers =
      match initializers with
      | (Cabs.NEXT_INIT,
         Cabs.SINGLE_INIT
           (Cabs.{ expr_node = Cabs.CAST ((specifier, decl_type), init_exp); _ }
            as single_init_exp))
        :: remaining_initializers ->
        let specifier', decl_type', init_exp' =
          preprocess_cast local_env.is_ghost specifier decl_type init_exp
        in
        (Cabs.NEXT_INIT, Cabs.SINGLE_INIT
           Cabs.({ expr_node = Cabs.CAST ((specifier', decl_type'), init_exp');
                   expr_loc = single_init_exp.expr_loc }))
        :: remaining_initializers
      | _ -> initializers
    in
    (* Sometimes we have a cast in front of a compound (in GCC). This appears as
       a single initializer. Ignore the cast. *)
    let initializers =
      match initializers with
      | (designator,
         Cabs.SINGLE_INIT
           Cabs.({expr_node = Cabs.CAST ((specifier, decl_type),
                                         Cabs.COMPOUND_INIT compound_init); _ }))
        :: remaining_initializers ->
        let specifier', decl_type', _init_exp' =
          preprocess_cast
            local_env.is_ghost specifier decl_type
            (Cabs.COMPOUND_INIT compound_init)
        in
        let typ = do_only_type local_env.is_ghost specifier' decl_type' in
        if Cil_datatype.Typ.equal
            (Cil.typeDeepDropAllAttributes typ)
            (Cil.typeDeepDropAllAttributes (Subobj.get_typ subobj))
        then
          (* Drop the cast *)
          (designator, Cabs.COMPOUND_INIT compound_init)
          :: remaining_initializers
        else
          (* Keep the cast. A new variable will be created to hold the
             intermediate value. *)
          initializers
      | _ -> initializers
    in
    initializers
  in

  (* Debug printing for [do_init]. *)
  Kernel.debug ~dkey
    "++++ do_init for %a %s (current %a). Looking at: %t"
    Subobj.pp_as_lval subobj
    (if Subobj.was_end_reached subobj then "(eof)" else "")
    Subobj.pp_current_obj_as_lval subobj
    (fun fmt ->
       match all_initializers with
       | [] -> Format.fprintf fmt "[]@."
       | (designator, init_exp) :: _ ->
         Cprint.print_init_expression fmt
           (Cabs.COMPOUND_INIT [(designator, init_exp)]));

  (* Here the real do_init fun begins! *)
  match Cil.unrollType (Subobj.get_typ subobj), all_initializers with

  (* No more initializers: return. *)
  | _, [] -> preinit, []

  (* No more subobjects to initialize: return. *)
  | _, (Cabs.NEXT_INIT, _) :: _ when Subobj.was_end_reached subobj ->
    preinit, all_initializers

  (* If we are at an array of characters and the initializer is a string literal
     (optionally enclosed in braces) then explode the string into characters. *)
  | TArray(array_base_type, array_len_opt, _, _ ),
    (Cabs.NEXT_INIT,
     (Cabs.SINGLE_INIT
        (Cabs.{ expr_node = Cabs.CONSTANT (Cabs.CONST_STRING string_literal); _ }
         as string_literal_exp) |
      Cabs.COMPOUND_INIT
        [ (Cabs.NEXT_INIT,
           Cabs.SINGLE_INIT
             (Cabs.{ expr_node = Cabs.CONSTANT (Cabs.CONST_STRING string_literal); _ }
              as string_literal_exp)) ]))
    :: remaining_initializers
    when (match Cil.unrollType array_base_type with
        | TInt((IChar|IUChar|ISChar), _) -> true
        | TInt _ ->
          (* Error: array's base type is a scalar other than char. *)
          Kernel.abort ~current:true
            "Initializing an array with base type `%a' with string literal"
            Printer.pp_typ array_base_type
        | _ -> false (* OK, this is probably an array of strings.
                        Handle it with the other arrays below.*)
      ) ->
    Kernel.debug ~dkey "do_init: the string literal case...";
    (* Following C11:6.7.9p14 :
       "An array of character type may be initialized by a character string
       literal or UTF-8 string literal, optionally enclosed in braces.
       Successive bytes of the string literal (including the terminating null
       character if there is room or if the array is of unknown size) initialize
       the elements of the array." *)
    let char_inits : (Cabs.initwhat * Cabs.init_expression) list =
      (* [make_simple_char_init c] takes a char value [c] and synthesizes a
         single initializer (an element of initializer list that will be used to
         initialize a character array). This single initializer does not have a
         designator (i.e. the next subobject is going to be initialized, which
         is the next char array cell in this case) and the initialization
         expression is the char value (in form of a constant expression). *)
      let make_single_char_init (char_code_int64 : int64)
        : (Cabs.initwhat * Cabs.init_expression) =
        let designator = Cabs.NEXT_INIT in
        let init_exp =
          let exp = Cabs.CONSTANT (Cabs.CONST_CHAR [char_code_int64]) in
          Cabs.SINGLE_INIT Cabs.{ expr_node = exp;
                                  expr_loc  = string_literal_exp.expr_loc }
        in
        designator, init_exp
      in
      let final_NULL =
        (* Following C11:6.7.9p14 :
           "(...) including the terminating null character if there is room or
           if the array is of unknown size (...)"
           NOTE: We cannot rely on zero-initialization of globals, since this
           array might be a local variable. *)
        let array_size_is_specified = Extlib.has_some array_len_opt in
        let there_is_room_for_final_NULL =
          let string_len = String.length string_literal in
          let array_len = integerArrayLength array_len_opt in
          string_len < array_len
        in
        if not array_size_is_specified
        || there_is_room_for_final_NULL
        then [make_single_char_init Int64.zero]
        else []
      in
      let char_inits = ref final_NULL in
      (* TODO: Maybe replace this with a List.map or a more specialized
         high-level function that still benefits from the constant time s.[i]
         access but is more elegant than a for loop? *)
      for pos = String.length string_literal - 1 downto 0 do
        (* [char_init] is the initializer (more exactly an element of a compound
           initializer) that initializes the next element of the array to the
           value of the character at position [pos] in the string [s]. *)
        let char_init : (Cabs.initwhat * Cabs.init_expression) =
          let char_at_pos : char = string_literal.[pos] in
          let char_code_int64 : int64 = Int64.of_int (Char.code char_at_pos) in
          make_single_char_init char_code_int64
        in
        (* As the for loop goes from the last character to the first, the list
           [collector] is built from the end. Thus it is already in the right
           order: it does not need to be reversed. *)
        char_inits := char_init :: !char_inits
      done;
      !char_inits
    in

    (* Create a separate object for the array and go inside the array. *)
    let subobj_preinit, remaining_char_inits =
      let sub_so = Subobj.start_with_new_current_object subobj in
      let sub_so = Subobj.advance_into_array sub_so in
      do_init
        local_env is_const add_implicit_ensures
        Preinit.none sub_so char_inits
    in

    (* Add the preinitializer for the array, overriding any existing
       preinitializer there. *)
    let preinit' =
      Preinit.set_preinit_for_subobject preinit subobj subobj_preinit
    in

    if remaining_char_inits <> [] then
      Kernel.warning ~current:true
        "Initializer string for character array `%a' is too long"
        Subobj.pp_as_lval subobj;

    (* Advance past the array and continue. *)
    let subobj = Subobj.advance_to_next subobj in
    do_init
      local_env is_const add_implicit_ensures
      preinit' subobj remaining_initializers

  (** If we are at an array of WIDE characters and the initializer is a WIDE
      string literal (optionally enclosed in braces) then explode the WIDE
      string into WIDE characters. *)
  | TArray(array_base_type, array_len_opt, _, _),
    (Cabs.NEXT_INIT,
     (Cabs.SINGLE_INIT
        (Cabs.{ expr_node = Cabs.CONSTANT (Cabs.CONST_WSTRING wide_string_literal); _ }
         as wide_string_literal_exp) |
      Cabs.COMPOUND_INIT
        [ (Cabs.NEXT_INIT,
           Cabs.SINGLE_INIT
             (Cabs.{ expr_node =
                       Cabs.CONSTANT (Cabs.CONST_WSTRING wide_string_literal);
                     _ } as wide_string_literal_exp)) ]))
    :: remaining_initializers
    when
      begin
        match Cil.unrollType array_base_type with
        (* Compare array's base type to wchar_t, ignoring
           signed vs. unsigned. *)
        | TInt _ as typ
          when (Cil.bitsSizeOf typ) =
               (Cil.bitsSizeOf Cil.(theMachine.wcharType))
          -> true
        | TInt _ ->
          (* Error: array's base type is a scalar other than wchar_t. *)
          Kernel.abort ~current:true
            "Initializing an array with base type `%a' with wide string literal"
            Printer.pp_typ array_base_type
        | _ -> false (* OK, this is probably an array of wide strings.
                        Handle it with the other arrays below.*)
      end ->
    (* Following C11:6.7.9p15 :
       "An array with element type compatible with a qualified or unqualified
       version of wchar_t, char16_t, or char32_t may be initialized by a wide
       string literal with the corresponding encoding prefix (L, u, or U,
       respectively), optionally enclosed in braces. Successive wide characters
       of the wide string literal (including the terminating null wide character
       if there is room or if the array is of unknown size) initialize the
       elements of the array." *)
    let max_wchar = (*  (2**(bitsSizeOf wcharType)) - 1  *)
      let max_wchar_plus_one =
        Int64.shift_left Int64.one (Cil.bitsSizeOf Cil.(theMachine.wcharType))
      in
      Int64.sub max_wchar_plus_one Int64.one
    in
    let wchar_inits : (Cabs.initwhat * Cabs.init_expression) list =
      let make_single_wchar_init (c : int64)
        : (Cabs.initwhat * Cabs.init_expression) =
        if Int64.compare c max_wchar > 0 then (* if c > max_wchar *)
          Kernel.error ~once:true ~current:true
            "cab2cil:do_init:character 0x%Lx too big." c;
        let designator = Cabs.NEXT_INIT in
        let init_exp =
          let exp = Cabs.CONSTANT (Cabs.CONST_INT (Int64.to_string c)) in
          Cabs.SINGLE_INIT Cabs.{ expr_node = exp;
                                  expr_loc = wide_string_literal_exp.expr_loc }
        in
        designator, init_exp
      in
      let final_NULL =
        (* Following C11:6.7.9p15 :
           "(...) including the terminating null wide character if there is room
           or if the array is of unknown size (...)"
           NOTE: We cannot rely on zero-initialization of globals, since this
           array might be a local variable. *)
        let array_size_is_specified = Extlib.has_some array_len_opt in
        let there_is_room_for_final_NULL =
          let wide_string_len = List.length wide_string_literal in
          let array_len = integerArrayLength array_len_opt in
          wide_string_len < array_len
        in
        if not array_size_is_specified
        || there_is_room_for_final_NULL
        then [make_single_wchar_init Int64.zero]
        else []
      in
      (List.map make_single_wchar_init wide_string_literal) @ final_NULL
    in

    (* Create a separate object for the array and go inside the array. *)
    let subobj_preinit, remaining_wchar_inits =
      let sub_subobj = Subobj.start_with_new_current_object subobj in
      let sub_subobj = Subobj.advance_into_array sub_subobj in
      do_init
        local_env is_const add_implicit_ensures
        Preinit.none sub_subobj wchar_inits
    in

    (* Add the preinitializer for the array, overriding any existing
       preinitializer there. *)
    let preinit' =
      Preinit.set_preinit_for_subobject preinit subobj subobj_preinit
    in

    if remaining_wchar_inits <> [] then
      Kernel.warning ~current:true
        "Initializer string for wide character array `%a' is too long"
        Subobj.pp_as_lval subobj;

    (* Advance past the array and continue. *)
    let subobj = Subobj.advance_to_next subobj in
    do_init
      local_env is_const add_implicit_ensures
      preinit' subobj remaining_initializers

  (* If we are at an array and we see a single initializer then it must
     be one for the first element. *)
  | TArray (_, _, _, _), (Cabs.NEXT_INIT, Cabs.SINGLE_INIT _) :: _  ->
    Kernel.debug ~dkey "do_init: the array with a single initializer case...";
    let subobj = Subobj.advance_into_array subobj in
    (* Start over with the cells. *)
    do_init
      local_env is_const add_implicit_ensures
      preinit subobj all_initializers

  (* An incomplete structure with any initializer is an error. *)
  | TComp (compinfo, _, _), _ :: _ when not compinfo.cdefined ->
    Kernel.abort ~current:true
      "structure `%a' has initializer but incomplete type"
      Subobj.pp_as_lval subobj

  (* If we are at a composite and we see a single initializer of the same
     type as the composite then we grab it all. If the type is not the same
     then we must go inside the composite and try to initialize the fields. *)
  | TComp (compinfo, _, _), (Cabs.NEXT_INIT, Cabs.SINGLE_INIT single_init_exp)
                            :: remaining_initializers ->
    let reads, chunl, single_init_exp', typ' =
      let as_const = as_const_of_is_const is_const in
      do_expression local_env as_const single_init_exp (AExp None)
    in
    begin
      match Cil.unrollType typ' with

      (* Type matches. Initialize the whole composite. *)
      | TComp (compinfo', _, _) when compinfo'.ckey = compinfo.ckey ->
        let preinit =
          let single_init_exp' =
            Preinit.make_init_exp_with_details
              single_init_exp reads chunl single_init_exp' typ'
          in
          Preinit.set_init_exp_for_subobject preinit subobj single_init_exp'
        in
        (* Advance to the next subobject and continue. *)
        let subobj = Subobj.advance_to_next subobj in
        do_init
          local_env is_const add_implicit_ensures
          preinit subobj remaining_initializers

      (* Type does not match. Initialize fields one by one. *)
      | _ ->
        let subobj = Subobj.advance_into_comp subobj in
        do_init
          local_env is_const add_implicit_ensures
          preinit subobj all_initializers

    end

  (* A scalar with a single initializer. *)
  | _typ, (Cabs.NEXT_INIT, Cabs.SINGLE_INIT single_init_exp)
          :: remaining_initializers ->
    Kernel.debug ~dkey "do_init: the scalar with a single initializer case...";
    let reads, chunk, single_init_exp', typ' =
      let as_const = asconst_of_isconst_simplify is_const in
      do_expression local_env as_const
        single_init_exp (AExp (Some (Subobj.get_typ subobj)))
    in
    let preinit' =
      (* TODO: Why handling casts is different that in the "scalar with an
         initializer surrounded by a number of braces" case? *)
      (* TODO: We don't recognize some problems here, like incompatible pointer
         to integer conversion. *)
      let init_expr =
        if Cil.(theMachine.insertImplicitCasts)
        then snd (Casts.castTo typ' (Subobj.get_typ subobj) single_init_exp')
        else single_init_exp'
      in
      let init_expr' =
        Preinit.make_init_exp_with_details
          single_init_exp reads chunk init_expr typ'
      in
      Preinit.set_init_exp_for_subobject preinit subobj init_expr'
    in

    (* Advance to the next subobject and continue. *)
    let subobj = Subobj.advance_to_next subobj in
    do_init
      local_env is_const add_implicit_ensures
      preinit' subobj remaining_initializers

  (* An array with a compound initializer. The initializer is for the array
     cells. *)
  | TArray (_, _, _, _), (Cabs.NEXT_INIT, Cabs.COMPOUND_INIT compound_init)
                         :: remaining_initializers ->
    Kernel.debug ~dkey "do_init: the array with a compound initializer case...";

    (* Create a separate object for the array. Then go inside the array if the
       initializer is not empty. *)
    let preinit', compound_init' =
      match compound_init with
      | [] ->
        (* The initializer is empty: this is a GNU extension to indicate
           0-initialization. We must actually indicate that there is an
           initializer here, albeit empty. This is in particular important
           if the parent subobject is an array of indeterminate size, as the
           number of initializers of its children matters. *)
        (* NOTE: The implicit zero initialization will be performed later, when
           converting initializers to assignments. *)
        Preinit.make_compound_empty_preinit (), []
      | _ ->
        (* The initializer is not empty. Go inside the array. *)
        let sub_subobj = Subobj.start_with_new_current_object subobj in
        let sub_subobj = Subobj.advance_into_array sub_subobj in
        do_init
          local_env is_const add_implicit_ensures
          Preinit.none sub_subobj compound_init
    in

    (* Add the preinitializer for the array, overriding any existing
       preinitializer there. *)
    let preinit' =
      Preinit.set_preinit_for_subobject preinit subobj preinit'
    in

    if compound_init' <> [] then
      Kernel.warning ~current:true
        "Excess elements in array `%a' initializer"
        Subobj.pp_as_lval subobj;

    (* Advance past the array and continue. *)
    let subobj = Subobj.advance_to_next subobj in
    do_init
      local_env is_const add_implicit_ensures
      preinit' subobj remaining_initializers

  (* We have a designator that tells us to select the matching union field.
     This is to support a GCC extension. *)
  (* TODO: Check how it is supposed to work and add tests for it. *)
  | TComp(compinfo, _, _) as comp_typ,
    [(Cabs.NEXT_INIT,
      Cabs.COMPOUND_INIT
        [(Cabs.INFIELD_INIT ("___matching_field", Cabs.NEXT_INIT),
          Cabs.SINGLE_INIT single_init_exp)])]
    when not compinfo.cstruct ->
    (* Do the expression to find its type *)
    let _, _, _, typ' =
      do_expression local_env (as_const_of_is_const is_const)
        single_init_exp (AExp None)
    in
    let typ'_no_attr = Cil.typeDeepDropAllAttributes typ' in
    let rec find_field = function
      | [] -> Kernel.fatal ~current:true
                "Cannot find matching union field in cast"
      | fieldinfo :: _
        when
          Cil_datatype.Typ.equal
            (Cil.typeDeepDropAllAttributes fieldinfo.ftype)
            typ'_no_attr ->
        fieldinfo
      | _ :: remaining_fieldinfos -> find_field remaining_fieldinfos
    in
    (* If this is a cast from union X to union X. *)
    if Cil_datatype.Typ.equal typ'_no_attr
        (Cil.typeDeepDropAllAttributes comp_typ) then
      let initializers' =
        [(Cabs.NEXT_INIT, Cabs.SINGLE_INIT single_init_exp)]
      in
      do_init
        local_env is_const add_implicit_ensures
        preinit subobj initializers'
    else
      (* If this is a GNU extension with field-to-union cast find the field. *)
      let fieldinfo = find_field compinfo.cfields in
      (* Change the designator and redo. *)
      let initializers' =
        [Cabs.INFIELD_INIT (fieldinfo.fname, Cabs.NEXT_INIT),
         Cabs.SINGLE_INIT single_init_exp]
      in
      do_init
        local_env is_const add_implicit_ensures
        preinit subobj initializers'


  (* A structure with a composite initializer. We initialize the fields. *)
  | TComp (_, _, _), (Cabs.NEXT_INIT, Cabs.COMPOUND_INIT compound_init)
                     :: remaining_initializers ->

    let preinit', initializers' =
      (* Create a separate subobject iterator for the composite. Then go inside
         the composite if the initializer is not empty. *)
      match compound_init with
      | [] ->
        (* The initializer is empty: this is a GNU extension to indicate
           0-initialization. We must actually indicate that there is an
           initializer here, albeit empty. This is in particular important
           if the parent subobject is an array of indeterminate size, as the
           number of initializers of its children matters. *)
        (* NOTE: The implicit zero initialization will be performed later, when
           converting initializers to assignments. *)
        Preinit.make_compound_empty_preinit (), []
      | _ ->
        let sub_subobj = Subobj.start_with_new_current_object subobj in
        let sub_subobj = Subobj.advance_into_comp sub_subobj in
        do_init
          local_env is_const add_implicit_ensures
          Preinit.none sub_subobj compound_init
    in

    (* Add the preinitializer for the composite, overriding any existing
       preinitializer there. *)
    let preinit' =
      Preinit.set_preinit_for_subobject preinit subobj preinit'
    in

    if initializers' <> [] then
      Kernel.warning ~current:true
        "Excess elements in structure `%a' initializer"
        Subobj.pp_as_lval subobj;

    (* Advance past the structure and continue. *)
    let subobj = Subobj.advance_to_next subobj in
    do_init
      local_env is_const add_implicit_ensures
      preinit' subobj remaining_initializers

  (* A scalar with an initializer surrounded by a number of braces. *)
  | typ, (Cabs.NEXT_INIT, init_exp) :: remaining_initializers ->
    Kernel.debug ~dkey
      "do_init: the scalar with an initializer surrounded by braces case...";

    let single_init_exp =
      let rec find_single_init_exp init_exp =
        match init_exp with
        (* Two correct cases. *)
        | Cabs.SINGLE_INIT single_init_exp -> single_init_exp
        | Cabs.COMPOUND_INIT [Cabs.NEXT_INIT, init_exp'] ->
          find_single_init_exp init_exp'
        (* Incorrect case (with just a warning). *)
        | Cabs.COMPOUND_INIT ((Cabs.NEXT_INIT, init_exp') :: _) ->
          Kernel.warning ~current:true
            "excess elements in scalar initializer (of type %a)"
            Printer.pp_typ typ;
          find_single_init_exp init_exp'
        (* Incorrect cases (abort). *)
        | Cabs.COMPOUND_INIT [] ->
          Kernel.abort ~current:true "empty scalar initializer"
        | Cabs.COMPOUND_INIT _ ->
          Kernel.abort ~current:true
            "designator in initializer for scalar type `%a'"
            Printer.pp_typ typ
        (* Internal error case (fatal). *)
        | Cabs.NO_INIT ->
          Kernel.fatal ~current:true
            "malformed initializer: encountered a NO_INIT which is not at the \
             initializer top-level"
      in
      find_single_init_exp init_exp
    in

    let reads, chunk, single_init_exp', typ' =
      let as_const = as_const_of_is_const is_const in
      do_expression local_env as_const single_init_exp
        (AExp(Some (Subobj.get_typ subobj)))
    in
    let preinit' =
      (* TODO: Why handling casts is different that in the "scalar with a single
         initializer" case? *)
      (* TODO: We don't recognize some problems here, like incompatible pointer
         to integer conversion. *)
      let init_expr =
        let subobj_typ = Subobj.get_typ subobj in
        Casts.makeCastT ~e:single_init_exp' ~oldt:typ' ~newt:subobj_typ
      in
      let init_expr' =
        Preinit.make_init_exp_with_details
          single_init_exp reads chunk init_expr typ'
      in
      Preinit.set_init_exp_for_subobject preinit subobj init_expr'
    in

    (* Advance to the next subobject and continue. *)
    let subobj = Subobj.advance_to_next subobj in
    do_init
      local_env is_const add_implicit_ensures
      preinit' subobj remaining_initializers


  (* We have a designator. *)
  | _, (designator, init_exp) :: remaining_initializers
    when designator != Cabs.NEXT_INIT ->
    (* Process a designator and position to the designated subobject. *)
    let addressSubobj
        (so : Subobj.t)
        (designator : Cabs.initwhat) : Subobj.t =

      (* Prepare the subobject: start from the current object and follow
         the designator. *)
      let subobj = Subobj.reset so in

      let rec address
          (subobj : Subobj.t) (designator: Cabs.initwhat) : Subobj.t =
        match designator with

        | Cabs.NEXT_INIT -> subobj

        | Cabs.INFIELD_INIT (field_name, remaining_designator) -> begin
            match Cil.unrollType (Subobj.get_typ subobj) with
            | TComp _ ->
              let subobj = Subobj.advance_into_comp ~field_name subobj in
              address subobj remaining_designator
            | _ as subobj_typ ->
              Kernel.fatal ~current:true
                "Field designator `.%s' in a non-composite type (%a)"
                field_name Printer.pp_typ subobj_typ
          end

        | Cabs.ATINDEX_INIT (index_exp, remaining_designator) -> begin
            match Cil.unrollType (Subobj.get_typ subobj) with
            | TArray (_, array_len_opt, _, _) ->
              let array_len = integerArrayLength array_len_opt in
              let index, index_chunk =
                let (reads, index_chunk, index_exp, _) =
                  do_expression local_env ExpectConst
                    index_exp (AExp (Some Cil.intType))
                in
                let index_chunk =
                  Chunk.add_reads index_exp.eloc reads index_chunk
                in
                match Cil.constFoldToInt index_exp,
                      Chunk.is_not_empty index_chunk with
                | Some x, false -> Integer.to_int x, index_chunk
                | _ ->
                  Kernel.abort ~current:true
                    "Array index designator expression `%a' is not a constant"
                    Printer.pp_exp index_exp
              in
              if index < 0 then
                Kernel.abort ~current:true
                  "Array index designator `[%d]' is negative" index;
              if index >= array_len then
                Kernel.abort ~current:true
                  "Array index designator `[%d]' is outside array's bounds \
                   (%d ''>= %d)"
                  index index array_len;
              let subobj = Subobj.advance_into_array ~index:index subobj in
              assert (Chunk.is_empty index_chunk);
              address subobj remaining_designator

            | _ as subobj_typ ->
              Kernel.abort ~current:true
                "Array index designator for a non-array type (%a)"
                Printer.pp_typ subobj_typ
          end

        | Cabs.ATINDEXRANGE_INIT _ ->
          Kernel.abort ~current:true "addressSubobj: INDEXRANGE"
      in
      address subobj designator
    in
    (* First expand the INDEXRANGE by making copies *)
    let rec expand_range (top : Cabs.initwhat -> Cabs.initwhat) = function
      | Cabs.INFIELD_INIT (field_name, designator) ->
        expand_range
          (fun designator -> top (Cabs.INFIELD_INIT(field_name, designator)))
          designator
      | Cabs.ATINDEX_INIT (index_exp, designator) ->
        expand_range
          (fun designator -> top (Cabs.ATINDEX_INIT(index_exp, designator)))
          designator

      | Cabs.ATINDEXRANGE_INIT (idxs, idxe) ->
        let (rs, doidxs, idxs', _) =
          do_expression local_env ExpectConst idxs (AExp(Some Cil.intType))
        in
        let doidxs = Chunk.add_reads idxs'.eloc rs doidxs in
        let (re, doidxe, idxe', _) =
          do_expression local_env ExpectConst idxe (AExp(Some Cil.intType))
        in
        let doidxe = Chunk.add_reads idxe'.eloc re doidxe in
        if Chunk.is_not_empty doidxs || Chunk.is_not_empty doidxe then
          Kernel.fatal ~current:true "Range designators are not constants";
        let first, last =
          match Cil.constFoldToInt idxs', Cil.constFoldToInt idxe' with
          | Some s, Some e -> Integer.to_int s, Integer.to_int e
          | _ ->
            Kernel.fatal ~current:true
              "INDEX_RANGE initialization designator is not a constant"
        in
        if first < 0 || first > last then
          Kernel.error ~once:true ~current:true
            "start index larger than end index in range initializer";
        let rec loop (i : int) =
          if i > last
          then remaining_initializers
          else
            (top (Cabs.ATINDEX_INIT(
                 Cabs.{ expr_node = Cabs.CONSTANT(Cabs.CONST_INT(string_of_int i));
                        expr_loc = fst idxs.expr_loc, snd idxe.expr_loc},
                 Cabs.NEXT_INIT)), init_exp)
            :: loop (i + 1)
        in
        do_init
          local_env is_const add_implicit_ensures
          preinit subobj (loop first)
      | Cabs.NEXT_INIT -> (* We have not found any RANGE *)
        let so = addressSubobj subobj designator in
        do_init
          local_env is_const add_implicit_ensures preinit so
          ((Cabs.NEXT_INIT, init_exp) :: remaining_initializers)
    in
    expand_range (fun initwhat -> initwhat) designator

  | typ, (_designator, _init_exp) :: _ ->
    Kernel.abort ~current:true
      "do_init: cases for t=%a" Printer.pp_typ typ


(* Create and add to the file (if not already added) a global. Return the
   varinfo. *)
and create_global ghost logic_spec specs
    (((init_name_name, init_name_decl_type,
       init_name_attributes, init_name_loc), init_exp) : Cabs.init_name)
  : varinfo =
  Kernel.debug ~dkey:category_global ~level:2
    "create_global: %s" init_name_name;
  (* If the global is a Frama-C builtin, set the generated flag. *)
  let is_frama_c_builtin { Cabs.expr_node = enode; _ } =
    match enode with
    | Cabs.VARIABLE "FC_BUILTIN" -> true
    | _ -> false
  in
  let is_generated =
    List.exists
      (fun (_, expressions) -> List.exists is_frama_c_builtin expressions)
      init_name_attributes
  in
  (* Make a first version of the varinfo. *)
  let varinfo =
    make_varinfo_cabs ~ghost ~is_formal:false ~is_global:true ~is_generated
      init_name_loc specs
      (init_name_name, init_name_decl_type, init_name_attributes)
  in
  (* Add the variable to the environment before doing the initializer
     because it might refer to the variable itself. *)
  begin
    match Cil.isFunctionType varinfo.vtype with
    | true when init_exp != Cabs.NO_INIT ->
      Kernel.error ~once:true ~current:true
        "Function declaration with initializer (%a)"
        Printer.pp_varinfo varinfo
    | true -> ()
    | false when Extlib.has_some logic_spec ->
      let warning_or_error, msg_to_append =
        if Kernel.ContinueOnAnnotError.get ()
        then Kernel.warning, " (ignoring)"
        else Kernel.error, ""
      in
      warning_or_error ~current:true ~once:true
        "Global variable %a is not a function. It cannot have a contract%s."
        Printer.pp_varinfo varinfo msg_to_append
    | false -> ()
  end;
  let varinfo, already_in_env =
    make_global_varinfo (init_exp != Cabs.NO_INIT) varinfo
  in
  (* Do the initializer and complete the array type if necessary. *)
  let init_option : init option =
    match init_exp with
    | Cabs.NO_INIT -> None
    | _ ->
      let chunk, init, typ =
        do_initializer (ghost_local_env ghost) varinfo init_exp
      in
      (* Maybe we now have a better type? Use the type of the initializer only
         if it really differs from the type of the variable. *)
      if Cil.unrollType varinfo.vtype != Cil.unrollType typ
      then Cil.update_var_type varinfo typ;
      if Chunk.is_not_empty chunk then
        Kernel.error ~once:true ~current:true
          "invalid global initializer @[%a@]" Chunk.d_chunk chunk;
      Some init
  in

  try
    let old_loc = Globals.AlreadyDefined.find varinfo in
    if init_option != None then
      (* Function redefinition is taken care of elsewhere. *)
      Kernel.error ~once:true ~current:true
        "Global %a was already defined at %a"
        Printer.pp_varinfo varinfo Printer.pp_location old_loc;
    Kernel.debug ~dkey:category_global ~level:2
      " global %s was already defined" varinfo.vname;
    (* Do not declare it again, but update the spec if any. *)
    if Cil.isFunctionType varinfo.vtype then
      begin
        match logic_spec with
        | None -> ()
        | Some (spec, _) ->
          let formals_from_varinfo = Globals.get_formals varinfo in
          let formals_from_FormalsDecl = Cil.getFormalsDecl varinfo in
          List.iter2
            (fun formal_from_varinfo formal_from_FormalsDecl ->
               if formal_from_varinfo != formal_from_FormalsDecl then
                 Kernel.fatal
                   "Function %a: formals are not shared between AST and \
                    FormalDecls table"
                   Printer.pp_varinfo varinfo)
            formals_from_varinfo formals_from_FormalsDecl;
          try
            let known_behaviors : string list =
              Globals.find_existing_behaviors varinfo
            in
            let funspec =
              Ltyping.funspec
                known_behaviors varinfo (Some (Globals.get_formals varinfo))
                varinfo.vtype spec
            in
            Globals.update_funspec_in_theFile varinfo funspec
          with LogicTypeError ((source, _), msg) ->
            Kernel.warning ~source
              "%s. Ignoring specification of function %a" msg
              Printer.pp_varinfo varinfo
      end;
    varinfo
  with Not_found ->
    begin
      (* Not already defined. *)
      Kernel.debug ~dkey:category_global ~level:2
        " first definition for %s(%d)\n" varinfo.vname varinfo.vid;
      if init_option != None then begin
        (* weimer: Sat Dec  8 17:43:34  2001
           MSVC NT Kernel headers include this lovely line:
           extern const GUID __declspec(selectany) \
            MOUNTDEV_MOUNTED_DEVICE_GUID = { 0x53f5630d, 0xb6bf, 0x11d0, { \
            0x94, 0xf2, 0x00, 0xa0, 0xc9, 0x1e, 0xfb, 0x8b } };
           So we allow "extern" + "initializer" if "const" is
           around. *)
        (* sm: As I read the ISO spec, in particular 6.9.2 and 6.7.8,
           "extern int foo = 3" is exactly equivalent to "int foo = 3";
           that is, if you put an initializer, then it is a definition, and
           "extern" is redundantly giving the name external linkage.
           gcc emits a warning, I guess because it is contrary to usual
           practice, but I think CIL warnings should be about semantic rather
           than stylistic issues, so I see no reason to even emit a warning. *)
        if varinfo.vstorage = Extern
        then varinfo.vstorage <- NoStorage; (* Equivalent and canonical. *)

        Globals.AlreadyDefined.add varinfo (Cil.CurrentLoc.get ());
        Globals.MustTurnIntoDef.remove varinfo;
        Globals.cabsPushGlobal
          (GVar (varinfo, {init = init_option}, Cil.CurrentLoc.get ()));
        varinfo
      end else begin
        if not (Cil.isFunctionType varinfo.vtype)
        && not (Globals.MustTurnIntoDef.mem varinfo)
        then Globals.MustTurnIntoDef.add varinfo;
        if not already_in_env then begin (* Only one declaration. *)
          (* If it has function type it is a prototype. *)
          (* NB: We add the formal prms in the env. *)
          if Cil.isFunctionType varinfo.vtype then begin
            if not varinfo.vdefined
            then Cil.setFormalsDecl varinfo varinfo.vtype;
            let funspec =
              match logic_spec with
              | None -> Cil.empty_funspec ()
              | Some (spec, loc) ->
                Cil.CurrentLoc.set loc;
                try
                  (* It can not have old behavior names, since this is the first
                     time we see the declaration. *)
                  Ltyping.funspec [] varinfo None varinfo.vtype spec
                with LogicTypeError ((source, _), msg) ->
                  Kernel.warning ~source
                    "%s. Ignoring specification of function %a"
                    msg Printer.pp_varinfo varinfo;
                  Cil.empty_funspec ()
            in
            Globals.cabsPushGlobal
              (GFunDecl (funspec, varinfo, Cil.CurrentLoc.get ()));
          end
          else
            Globals.cabsPushGlobal
              (GVarDecl (varinfo, Cil.CurrentLoc.get ()));
          varinfo
        end else begin
          Kernel.debug ~dkey:category_global ~level:2
            " already in env %s" varinfo.vname;
          begin
            match logic_spec with
            | None -> ()
            | Some (spec, loc) ->
              Cil.CurrentLoc.set loc;
              let merge_spec = function
                | GFunDecl(old_spec, _, _) ->
                  let behaviors =
                    List.map
                      (fun behavior -> behavior.b_name)
                      old_spec.spec_behavior
                  in
                  let funspec =
                    try
                      Ltyping.funspec behaviors varinfo None varinfo.vtype spec
                    with LogicTypeError ((source, _), msg) ->
                      Kernel.warning ~source
                        "%s. Ignoring specification of function %a"
                        msg Printer.pp_varinfo varinfo;
                      Cil.empty_funspec ()
                  in
                  Cil.CurrentLoc.set varinfo.vdecl;
                  let merge_verbosely = Logic_utils.MergeVerbosely varinfo in
                  Logic_utils.merge_funspec merge_verbosely old_spec funspec
                | _ -> assert false
              in
              Globals.update_fundec_in_theFile varinfo merge_spec
          end;
          varinfo
        end
      end
    end

(* Must catch the [Static] local variables. Make them global. *)
and createLocal ghost ((_, sto, _, _) as specs)
    ((((n, ndt, a, loc) : Cabs.name),
      (inite : Cabs.init_expression)) as init_name)
  : Chunk.t =
  (* Check if we are declaring a function. *)
  let rec isProto (dt: Cabs.decl_type) : bool =
    match dt with
    | Cabs.PROTO (Cabs.JUSTBASE, _, _) -> true
    | Cabs.PROTO (x, _, _) -> isProto x
    | Cabs.PARENTYPE (_, x, _) -> isProto x
    | Cabs.ARRAY (x, _, _) -> isProto x
    | Cabs.PTR (_, x) -> isProto x
    | _ -> false
  in
  match ndt with
  (* Maybe we have a function prototype in local scope. Make it global. We
     do this even if the storage is Static *)
  | _ when isProto ndt ->
    let vi = create_global ghost None specs init_name in
    (* Add it to the environment to shadow previous decls *)
    add_local_to_env n (EnvVar vi);
    LocalFuncHook.apply vi;
    Chunk.empty

  | _ when sto = Static ->
    Kernel.debug ~dkey:category_global ~level:2
      "create_global (local static): %s" n;
    (* Now alpha convert it to make sure that it does not conflict with existing
       globals or locals from this function. *)
    let newname, _  = newAlphaName true "" n in
    (* Make it global  *)
    let vi = make_varinfo_cabs ~ghost ~is_formal:false
        ~is_global:true
        loc specs (newname, ndt, a) in
    (* However, we have a problem if a real global appears later with the name
       that we have happened to choose for this one. Remember these names for
       later. *)
    Globals.StaticLocals.add vi;
    (* Add it to the environment as a local so that the name goes out of scope
       properly. *)
    add_local_to_env n (EnvVar vi);

    (* Maybe this is an array whose length depends on something with local
       scope, e.g. "static char device[ sizeof(local) ]".
       Const-fold the type to fix this. *)
    Cil.update_var_type vi (constFoldType vi.vtype);

    let init : init option =
      if inite = Cabs.NO_INIT then
        None
      else begin
        let se, ie', et = do_initializer (ghost_local_env ghost) vi inite in
        (* Maybe we now have a better type?  Use the type of the initializer
           only if it really differs from the type of the variable. *)
        if Cil.unrollType vi.vtype != Cil.unrollType et then
          Cil.update_var_type vi et;
        if Chunk.is_not_empty se then
          Kernel.error ~once:true ~current:true "global static initializer";
        (* Check that no locals are refered by the initializer *)
        Check_no_locals.in_initializer ie';
        (* Maybe the initializer refers to the function itself.
           Push a prototype for the function, just in case. *)
        Globals.cabsPushGlobal
          (GFunDecl (Cil.empty_funspec (), !current_fun_fundec.svar,
                     Cil.CurrentLoc.get ()));
        Cil.setFormalsDecl
          !current_fun_fundec.svar !current_fun_fundec.svar.vtype;
        Some ie'
      end
    in
    Globals.cabsPushGlobal (GVar(vi, {init = init}, Cil.CurrentLoc.get ()));
    Chunk.empty

  (* Maybe we have an extern declaration. Make it a global *)
  | _ when sto = Extern ->
    let vi = create_global ghost None specs init_name in
    (* Add it to the local environment to ensure that it shadows previous local
       variables. *)
    add_local_to_env n (EnvVar vi);
    Chunk.empty

  | _ ->
    (* Make a variable of potentially variable size. If [se0 <> empty] then it
       is a variable size variable. *)
    let vi,se0,len,isvarsize =
      make_variable_size_varinfo ghost loc specs (n, ndt, a) in

    let vi = alpha_convert_var_and_add_to_env true vi in (* Replace [vi]. *)
    let se1 =
      if isvarsize then begin (* Variable-sized array *)
        Kernel.warning ~current:true
          "Variable-sized local variable %a" Printer.pp_varinfo vi;
        (* Make a local variable to keep the length *)
        let savelen =
          make_varinfo_cabs
            ~ghost
            ~is_formal:false
            ~is_global:false
            loc
            (Cil.(theMachine.typeOfSizeOf), NoStorage, false, [])
            ("__lengthof_" ^ vi.vname,Cabs.JUSTBASE, [])
        in
        (* Register it *)
        let savelen = alpha_convert_var_and_add_to_env true savelen in
        let se0 = Chunk.local_var_chunk se0 savelen in
        (* Compute the allocation size *)
        let elt_size = Cil.new_exp ~loc
            (SizeOfE
               (Cil.new_exp ~loc
                  (Lval
                     (Mem(Cil.new_exp ~loc (Lval(Cil.var vi))),
                      NoOffset))))
        in
        let alloca_size =
          Cil.new_exp ~loc
            (BinOp(Mult,
                   elt_size,
                   Cil.new_exp ~loc (Lval (Cil.var savelen)),
                   Cil.(theMachine.typeOfSizeOf)))
        in
        (* Register the length *)
        Datatype.Int.Hashtbl.add variable_size_arrays vi.vid alloca_size;
        (* There can be no initializer for this *)
        if inite != Cabs.NO_INIT then
          Kernel.error ~once:true ~current:true
            "Variable-sized array cannot have initializer";
        let se0 =
          (* TODO: Add an assertion to ensure the given size is correcly bound:
             assert alloca_bounds: 0 < elt_size * array_size <= max_bounds *)
          (se0 +++ (
              let castloc = Cil.CurrentLoc.get () in
              let talloca_size =
                let telt_size = Logic_utils.expr_to_term ~cast:false elt_size in
                let tlen = Logic_utils.expr_to_term ~cast:false len in
                Logic_const.term
                  (TBinOp (Mult,telt_size,tlen))
                  telt_size.term_type
              in
              let pos_size =
                let zero =  Logic_const.tinteger ~loc:castloc 0 in
                Logic_const.prel ~loc:castloc (Rlt, zero, talloca_size)
              in
              let max_size =
                let szTo = Cil.bitsSizeOf Cil.(theMachine.typeOfSizeOf) in
                let max_bound =
                  Logic_const.tint ~loc:castloc (Cil.max_unsigned_number szTo)
                in
                Logic_const.prel ~loc:castloc (Rle, talloca_size, max_bound)
              in
              let alloca_bounds =
                Logic_const.pand ~loc:castloc (pos_size, max_size)
              in
              let alloca_bounds =
                { alloca_bounds with name = ["alloca_bounds"] }
              in
              let annot =
                Logic_const.new_code_annotation (AAssert ([], alloca_bounds))
              in
              (Cil.mkStmtOneInstr ~ghost
                 (Code_annot (annot, castloc)),
               Chunk.no_effects)))
        in
        let setlen =
          se0 +++
          (Cil.mkStmtOneInstr
             ~ghost
             (Set (Cil.var savelen,
                   makeCast ~e:len ~newt:savelen.vtype,
                   Cil.CurrentLoc.get ())),
           Chunk.no_effects)
        in
        (* Initialize the variable. *)
        let alloca : varinfo = alloca_fun () in
        if Kernel.DoCollapseCallCast.get () then
          (* Do it in one step. *)
          setlen +++
          (Cil.mkStmtOneInstr ~ghost
             (Call(Some(Cil.var vi), Cil.new_exp ~loc (Lval(Cil.var alloca)),
                   [ alloca_size ], loc)),
           Chunk.make_effects ~modified:[] ~writes:[Cil.var vi] ~reads:[])
        else begin
          (* do it in two *)
          let rt, _, _, _ = Cil.splitFunctionType alloca.vtype in
          let tmp =
            make_new_tmp_var
              (Pretty_utils.sfprintf
                 "alloca(%a)" Printer.pp_exp alloca_size)
              false rt
          in
          (Chunk.local_var_chunk setlen tmp)
          +++ (Cil.mkStmtOneInstr ~ghost
                 (Call(Some(Cil.var tmp),
                       Cil.new_exp ~loc (Lval(Cil.var alloca)),
                       [ alloca_size ],
                       Cil.CurrentLoc.get ())),
               Chunk.no_effects)
          +++ (Cil.mkStmtOneInstr ~ghost
                 (Set
                    ((Cil.var vi),
                     makeCast
                       ~e:(Cil.new_exp ~loc (Lval(Cil.var tmp)))
                       ~newt:vi.vtype,
                     Cil.CurrentLoc.get ())),
               Chunk.make_effects
                 ~modified:[]
                 ~writes:[Cil.var vi]
                 ~reads:[Cil.var tmp])
        end
      end else Chunk.empty
    in
    let se1 = Chunk.local_var_chunk se1 vi in
    if inite = Cabs.NO_INIT then
      se1 (* [skip_chunk] *)
    else begin
      let se4, ie', et = do_initializer (ghost_local_env ghost) vi inite in
      (* Fix the length. *)
      (match vi.vtype, ie', et with
       (* We have a length now. *)
       | TArray (_, None, _, _), _, TArray (_, Some _, _, _) ->
         Cil.update_var_type vi et
       (* Initializing a local array. *)
       | TArray (TInt ((IChar | IUChar | ISChar), _) as bt, None, l, a),
         SingleInit ({ enode = Const CStr s; eloc = loc; _ }), _ ->
         Cil.update_var_type vi
           (TArray(bt,
                   Some (Cil.integer ~loc (String.length s + 1)),
                   l, a))
       | _, _, _ -> ());

      (* Now create assignments instead of the initialization. *)
      (se1 @@ (se4, ghost))
      @@ (assign_init ~ghost (Var vi, NoOffset) ie' et Chunk.empty, ghost)
    end

and doAliasFun vtype (thisname:string) (othername:string)
    (sname:Cabs.single_name) (loc: Cabs.cabsloc) : unit =
  (* This prototype declares that name is an alias for [othername], which must
     be defined in this file. *)
  let rt, formals, isva, _ = Cil.splitFunctionType vtype in
  if isva
  then Kernel.error ~once:true ~current:true "alias unsupported with varargs";
  let args =
    List.map
      (fun (n,_,_) -> { Cabs.expr_loc = loc; expr_node = Cabs.VARIABLE n})
      (Cil.argsToList formals)
  in
  let call =
    Cabs.CALL
      (Cabs.{expr_loc = loc; expr_node = Cabs.VARIABLE othername}, args)
  in
  let stmt =
    { Cabs.stmt_ghost = false;
      stmt_node =
        if Cil.isVoidType rt then
          Cabs.COMPUTATION(Cabs.{expr_loc = loc; expr_node = call}, loc)
        else Cabs.RETURN(Cabs.{expr_loc = loc; expr_node = call}, loc) }
  in
  let body = { Cabs.blabels = []; Cabs.battrs = []; Cabs.bstmts = [stmt] } in
  let fdef = Cabs.FUNDEF (None, sname, body, loc, loc) in
  ignore (doDecl empty_local_env true fdef);
  (* Get the new function. *)
  let v,_ =
    try lookup_global_var thisname
    with Not_found -> Kernel.abort ~current:true "error in doDecl"
  in
  v.vattr <- Cil.dropAttribute "alias" v.vattr


(* Do one declaration. *)
and doDecl local_env (isglobal: bool) : Cabs.definition -> Chunk.t =
  function
  | Cabs.DECDEF (logic_spec, (s, nl), loc) ->
    Cil.CurrentLoc.set loc;
    (* Do the specifiers exactly once. *)
    let sugg =
      match nl with
      | [] -> ""
      | ((n, _, _, _), _) :: _ -> n
    in
    let ghost = local_env.is_ghost in
    let spec_res = do_spec_list ghost sugg s in
    (* Do all the variables and concatenate the resulting statements. *)
    let doOneDeclarator (acc: Chunk.t) (name: Cabs.init_name) =
      let (n,ndt,a,l),_ = name in
      if isglobal then begin
        let bt,_,_,attrs = spec_res in
        let vtype, nattr =
          do_type local_env.is_ghost false
            (Cil.AttrName false) bt (Cabs.PARENTYPE(attrs, ndt, a)) in
        (match Cil.filterAttributes "alias" nattr with
         | [] -> (* ordinary prototype. *)
           ignore (create_global local_env.is_ghost logic_spec spec_res name)
         (*  E.log "%s is not aliased\n" name *)
         | [Attr("alias", [AStr othername])] ->
           if not (Cil.isFunctionType vtype) || local_env.is_ghost then begin
             Kernel.warning ~current:true
               "%a: CIL only supports attribute((alias)) for C functions."
               Printer.pp_location (Cil.CurrentLoc.get ());
             ignore (create_global ghost logic_spec spec_res name)
           end else
             doAliasFun vtype n othername (s, (n,ndt,a,l)) loc
         | _ ->
           Kernel.error ~once:true ~current:true "Bad alias attribute at %a"
             Printer.pp_location (Cil.CurrentLoc.get()));
        acc
      end else
        acc @@ (createLocal ghost spec_res name, ghost)
    in
    let res = List.fold_left doOneDeclarator Chunk.empty nl in
    if isglobal then res
    else begin
      match logic_spec with
      | None -> res
      | Some (spec,loc) ->
        begin
          try
            let spec =
              Ltyping.code_annot loc local_env.known_behaviors
                (Ctype !current_return_type) (AStmtSpec ([],spec))
            in
            append_chunk_to_annot ~ghost
              (Chunk.of_stmt
                 (Cil.mkStmtOneInstr
                    ~ghost:local_env.is_ghost (Code_annot (spec,loc))))
              res
          with LogicTypeError ((source,_),msg) ->
            Kernel.warning ~source
              "%s. Ignoring code annotation" msg;
            res
        end
    end
  | Cabs.TYPEDEF (ng, loc) ->
    Cil.CurrentLoc.set loc; do_typedef local_env.is_ghost ng; Chunk.empty

  | Cabs.ONLYTYPEDEF (s, loc) ->
    Cil.CurrentLoc.set loc; do_only_typedef local_env.is_ghost s; Chunk.empty

  | Cabs.GLOBASM (s,loc) when isglobal ->
    Cil.CurrentLoc.set loc;
    Globals.cabsPushGlobal (GAsm (s, Cil.CurrentLoc.get ())); Chunk.empty

  | Cabs.PRAGMA (a, loc) when isglobal -> begin
      Cil.CurrentLoc.set loc;
      match do_attr local_env.is_ghost ("dummy", [a]) with
      | [Attr("dummy", [a'])] ->
        let a'' =
          match a' with
          | ACons (s, args) ->
            Pragma.process_align_pragma s args;
            Pragma.process_pack_pragma s args
          | _ -> (* Cil.fatal "Unexpected attribute in #pragma" *)
            Kernel.warning ~current:true "Unexpected attribute in #pragma";
            Some (Attr ("", [a']))
        in
        Extlib.may
          (fun a'' ->
             Globals.cabsPushGlobal (GPragma (a'', Cil.CurrentLoc.get ())))
          a'';
        Chunk.empty

      | _ -> Kernel.fatal ~current:true "Too many attributes in pragma"
    end

  | Cabs.FUNDEF (spec,((specs,(n,dt,a, _)) : Cabs.single_name),
                 (body : Cabs.block), loc1, loc2) when isglobal ->
    begin
      let ghost = local_env.is_ghost in
      let idloc = loc1 in
      let funloc = fst loc1, snd loc2 in
      let endloc = loc2 in
      Kernel.debug ~dkey:category_global ~level:2
        "Definition of %s at %a\n" n Printer.pp_location idloc;
      Cil.CurrentLoc.set idloc;
      CallTempVars.clear ();

      (* Make the fundec right away, and we'll populate it later. We  need this
         throughout the code to create temporaries. *)
      current_fun_fundec :=
        { svar = Cil.makeGlobalVar ~temp:false n Cil.voidType;
          slocals = []; (* For now we'll put here both the locals and the
                           formals. Then [endFunction] will separate them. *)
          sformals = []; (* Not final yet. *)
          smaxid = 0;
          sbody = dummy_function.sbody; (* Not final yet. *)
          smaxstmtid = None;
          sallstmts = [];
          sspec = Cil.empty_funspec ()
        };
      !current_fun_fundec.svar.vdecl <- idloc;

      (* Setup the environment. Add the formals to the locals. Maybe they need
         alpha-conv.*)
      enter_scope (); (* Start the scope. *)
      Labels.Manage.gather_labels body;
      Cil.CurrentLoc.set idloc;
      Datatype.Int.Hashtbl.clear variable_size_arrays;

      (* Enter all the function's labels into the alpha conversion table. *)
      Labels.Manage.register_labels body;
      Cil.CurrentLoc.set idloc;

      (* Do not process transparent unions in function definitions.
         We'll do it later. *)
      TransparentUnionArgs.clear ();

      (* Fix the NAME and the STORAGE. *)
      let _ =
        let bt,sto,inl,attrs = do_spec_list local_env.is_ghost n specs in
        !current_fun_fundec.svar.vinline <- inl;

        let ftyp, funattr =
          do_type local_env.is_ghost false
            (Cil.AttrName false) bt (Cabs.PARENTYPE(attrs, dt, a)) in
        Cil.update_var_type !current_fun_fundec.svar ftyp;
        !current_fun_fundec.svar.vattr <- funattr;

        (* Now we have the name and the storage. *)
        !current_fun_fundec.svar.vname <- n;
        !current_fun_fundec.svar.vstorage <- sto
      in
      let vi,has_decl =
        make_global_varinfo true !current_fun_fundec.svar in
      (* Add the function itself to the environment. Add it before you do the
         body because the function might be recursive. Add it also before you
         add the formals to the environment because there might be a formal with
         the same name as the function and we want it to take precedence. *)
      (* Make a variable out of it and put it in the environment. *)
      !current_fun_fundec.svar <- vi;

      (* If it is extern inline then we add it to the global environment for the
         original name as well. This will ensure that all uses of this function
         will refer to the renamed function. *)
      add_global_to_env n (EnvVar !current_fun_fundec.svar);
      if Globals.AlreadyDefined.mem !current_fun_fundec.svar then
        Kernel.abort ~current:true "redefinition of function '%s'" n;

      Globals.AlreadyDefined.add !current_fun_fundec.svar idloc;

      (* [make_global_varinfo] might have changed the type of the function (when
         combining it with the type of the prototype). So get the type only now.
      *)

      (**** Process the TYPE and the FORMALS. ***)
      let _ =
        let (returnType, formals_t, isvararg, funta) =
          Cil.splitFunctionTypeVI !current_fun_fundec.svar
        in
        (* Record the [returnType] for [do_statement]. *)
        current_return_type := returnType;

        (* Return type [va_list] is rejected. *)
        begin
          if Cil.isVariadicListType returnType then
            Kernel.abort ~current:true
              "Function %a returns a va_list object."
              Printer.pp_varinfo !current_fun_fundec.svar;
        end;

        (* Create the formals and add them to the environment. *)
        (* sfg: extract tsets for the formals from dt *)
        let doFormal (loc : location) (fn, ft, fa) =
          let f = Cil.makeVarinfo ~temp:false false true fn ft in
          (f.vdecl <- loc;
           f.vattr <- fa;
           alpha_convert_var_and_add_to_env true f)
        in
        let rec doFormals fl' ll' =
          begin
            match (fl', ll') with
            | [], _ -> []

            | fl, [] -> (* No more locs available. *)
              List.map (doFormal (Cil.CurrentLoc.get ())) fl

            | f::fl, (_,(_,_,_,l))::ll ->
              (* sfg: these lets seem to be necessary to force the right order
                 of evaluation. *)
              let f' = doFormal l f in
              let fl' = doFormals fl ll in
              f' :: fl'
          end
        in
        let fmlocs = (match dt with Cabs.PROTO(_, fml, _) -> fml | _ -> []) in
        let formals = doFormals (Cil.argsToList formals_t) fmlocs in

        (* In case of formals referred to in types of others, [do_type] has put
           dummy varinfos. We need to fix them now that we have proper bindings.
           TODO: Completely refactor the way formals' typechecking is done. *)
        let () = fixFormalsType formals in

        (* Recreate the type based on the formals. *)
        let ftype = TFun(returnType,
                         Some (List.map (fun f ->
                             (f.vname,
                              f.vtype,
                              f.vattr)) formals),
                         isvararg, funta) in

        (* Now fix the names of the formals in the type of the function as well.
        *)
        Cil.update_var_type !current_fun_fundec.svar ftype;
        !current_fun_fundec.sformals <- formals;
        (* We will revisit the spec for the declaration in order to change the
           formals according to the new variables. *)
        if has_decl then begin
          try
            Hashtbl.add alpha_renaming
              vi.vid
              (Cil.create_alpha_renaming
                 (Cil.getFormalsDecl vi) formals)
          with Not_found ->
            (* The declaration comes from an implicit prototype. We do not have
               any spec anyway. However, we will have a declaration in the
               resulting AST, to which we must attach some formals. *)
            Cil.unsafeSetFormalsDecl vi formals
        end;
      in
      (* Now change the type of transparent union args back to what it was so
         that the body type checks. We must do it this late because
         [make_global_varinfo] from above might choke if we give the function a
         type containing transparent unions. *)
      let _ =
        let rec fixbackFormals (idx: int) (args: varinfo list) : unit =
          match args with
          | [] -> ()
          | a :: args' ->
            (* Fix the type back to a transparent union type. *)
            (try
               let origtype = TransparentUnionArgs.get idx in
               Cil.update_var_type a origtype;
             with Not_found -> ());
            fixbackFormals (idx + 1) args'
        in
        fixbackFormals 0 !current_fun_fundec.sformals;
        TransparentUnionArgs.clear ()
      in
      let behaviors =
        Globals.find_existing_behaviors !current_fun_fundec.svar
      in
      (******* Now do the spec. *******)
      begin
        match spec with
        | Some (spec,loc) ->
          Cil.CurrentLoc.set loc;
          (try
             !current_fun_fundec.sspec <-
               Ltyping.funspec behaviors
                 !current_fun_fundec.svar
                 (Some !current_fun_fundec.sformals)
                 !current_fun_fundec.svar.vtype spec
           with LogicTypeError ((source,_),msg) ->
             Kernel.warning ~source
               "%s. Ignoring logic specification of function %a"
               msg Printer.pp_varinfo !current_fun_fundec.svar)
        | None -> ()
      end;
      (* Merge pre-existing spec if needed. *)
      if has_decl then begin
        let merge_spec = function
          | GFunDecl(old_spec,f,loc) as g ->
            if not (Cil.is_empty_funspec old_spec) then begin
              rename_spec g;
              Cil.CurrentLoc.set loc;
              let merge_verbosely = Logic_utils.MergeVerbosely f in
              Logic_utils.merge_funspec merge_verbosely
                !current_fun_fundec.sspec old_spec;
              Logic_utils.clear_funspec old_spec;
            end
          | _ -> assert false
        in
        Globals.update_fundec_in_theFile !current_fun_fundec.svar merge_spec
      end;
      (********** Now do the BODY. *************)
      let _ =
        let stmts =
          doBody
            { empty_local_env with
              known_behaviors =
                (List.map (fun x -> x.b_name)
                   !current_fun_fundec.sspec.spec_behavior)
                @ behaviors;
              is_ghost = local_env.is_ghost
            }
            body in
        (* Finish everything. *)
        exit_scope ();
        (* Now fill in the computed goto statement with cases. Do this before
           [mk_function_body] which resolves the gotos. *)
        (match ComputedGoto.get_target_data () with
         | Some (_switchv, switch) ->
           let switche, loc =
             match switch.skind with
             | Switch (switche, _, _, l) -> switche, l
             | _ ->
               Kernel.fatal ~current:true
                 "the computed goto statement not a switch"
           in
           (* Build a default chunk that segfaults. *)
           let default =
             Chunk.Make.default_chunk ~ghost
               loc
               (Chunk.of_stmt_and_effects
                  (Cil.mkStmtOneInstr
                     ~ghost:local_env.is_ghost
                     (Set ((Mem (makeCast
                                   ~e:(Cil.integer ~loc 0)
                                   ~newt:Cil.intPtrType),
                            NoOffset),
                           Cil.integer ~loc 0, loc)),
                   Chunk.no_effects))
           in
           let bodychunk = ref default in
           ComputedGoto.iter_target_hash
             (fun lname laddr ->
                bodychunk :=
                  Chunk.Make.case_range_chunk ~ghost
                    [Simple (Cil.integer ~loc laddr)] loc
                    (Chunk.Make.goto_chunk ~ghost lname loc @@ (!bodychunk, ghost)));
           (* Now recreate the switch. *)
           let newswitch = Chunk.Make.switch_chunk ~ghost switche !bodychunk loc in
           (* We must still share the old switch statement since we have already
              inserted the gotos. *)
           let newswitchkind =
             match newswitch.Chunk.stmts_with_effects with
             | [ s, _effects, _calls]
               when newswitch.Chunk.cases == [] -> s.skind
             | _ ->
               Kernel.fatal ~current:true
                 "Unexpected result from switch_chunk"
           in
           switch.skind <- newswitchkind

         | None -> ());
        (* Now finish the body and store it. *)
        let body = Chunk.mk_function_body ~ghost stmts in
        (* Need to add temporary variables created at [sbody] level. *)
        body.blocals <- !current_fun_fundec.sbody.blocals @ body.blocals;
        !current_fun_fundec.sbody <- body;
        (* Reset the global parameters. *)
        ComputedGoto.clear_env
      in
      let rec dropFormals formals locals =
        match formals, locals with
        | [], l -> l
        | f :: formals, l :: locals ->
          if f != l then
            Kernel.abort ~current:true
              "formal %a is not in locals (found instead %a)"
              Printer.pp_varinfo f
              Printer.pp_varinfo l;
          dropFormals formals locals
        | _ -> Kernel.abort ~current:true "Too few locals"
      in
      !current_fun_fundec.slocals
      <- dropFormals !current_fun_fundec.sformals
          (List.rev !current_fun_fundec.slocals);
      Cil.setMaxId !current_fun_fundec;

      (* Now go over the types of the formals and pull out the formals with
         transparent union type. Replace them with some shadow parameters and
         then add assignments. *)
      let _ =
        let newformals, newbody =
          List.fold_right (* So that the formals come out in order. *)
            (fun f (accform, accbody) ->
               match isTransparentUnion f.vtype with
               | None -> (f :: accform, accbody)
               | Some fstfield ->
                 (* A new shadow to be placed in the formals. Use [makeTempVar]
                    to update [smaxid] and all others but do not insert as a
                    local variable of [f]. *)
                 let loc = Cil.CurrentLoc.get () in
                 let shadow =
                   Cil.makeTempVar
                     !current_fun_fundec ~insert:false
                     fstfield.ftype
                 in
                 (* Now replace it with the current formal. *)
                 (shadow :: accform,
                  Cil.mkStmtOneInstr ~ghost:local_env.is_ghost
                    (Set ((Var f, Field(fstfield, NoOffset)),
                          Cil.new_exp ~loc (Lval (Cil.var shadow)), loc))
                  :: accbody))
            !current_fun_fundec.sformals
            ([], !current_fun_fundec.sbody.bstmts)
        in
        !current_fun_fundec.sbody.bstmts <- newbody;
        (* To make sure sharing with the type is proper. *)
        Cil.setFormals !current_fun_fundec newformals;
      in

      (* Now see whether we can fall through to the end of the function. *)
      if FallsThroughCanBreak.block_falls_through !current_fun_fundec.sbody
      then begin
        let protect_return, retval =
          (* Guard the [return] instructions we add with an [\assert \false]. *)
          let pfalse = Logic_const.unamed ~loc:endloc Pfalse in
          let pfalse = { pfalse with name = ["missing_return"] } in
          let assert_false () =
            let annot =
              Logic_const.new_code_annotation (AAssert ([], pfalse))
            in
            Cil.mkStmt ~ghost:local_env.is_ghost
              (Instr (Code_annot (annot, endloc)))
          in
          match Cil.unrollType !current_return_type with
          | TVoid _ -> [], None
          | (TInt _ | TEnum _ | TFloat _ | TPtr _) as rt ->
            let res =
              let zero_exp = Cil.zero ~loc:endloc in
              Some (Casts.makeCastT ~e:zero_exp ~oldt:Cil.intType ~newt:rt)
            in
            if !current_fun_fundec.svar.vname = "main" then
              [], res
            else begin
              Kernel.warning ~current:true
                "Body of function %a falls-through. \
                 Adding a return statement"
                Printer.pp_varinfo !current_fun_fundec.svar;
              [assert_false ()], res
            end
          | _ ->
            Kernel.warning ~current:true
              "Body of function %a falls-through and \
               cannot find an appropriate return value"
              Printer.pp_varinfo !current_fun_fundec.svar;
            [assert_false ()], None
        in
        if not (Cil.hasAttribute "noreturn" !current_fun_fundec.svar.vattr)
        then
          !current_fun_fundec.sbody.bstmts <-
            !current_fun_fundec.sbody.bstmts
            @ protect_return @
            [Cil.mkStmt ~ghost:local_env.is_ghost (Return(retval, endloc))]
      end;

      Globals.cabsPushGlobal (GFun (!current_fun_fundec, funloc));

      (* Leave the function definition's scope. *)
      current_fun_fundec := dummy_function;

      Chunk.empty
    end (* FUNDEF *)

  | Cabs.LINKAGE (n, loc, dl) ->
    Cil.CurrentLoc.set loc;
    if n <> "C" then
      Kernel.warning ~current:true
        "Encountered linkage specification \"%s\"" n;
    if not isglobal then
      Kernel.error ~once:true ~current:true
        "Encountered linkage specification in local scope";
    (* For now drop the linkage on the floor !!! *)
    List.iter
      (fun d ->
         let s = doDecl local_env isglobal d in
         if Chunk.is_not_empty s then
           Kernel.abort ~current:true
             "doDecl returns non-empty statement for global")
      dl;
    Chunk.empty

  | Cabs.GLOBANNOT (decl) when isglobal ->
    begin
      List.iter
        (fun decl  ->
           let loc = decl.Logic_ptree.decl_loc in
           Cil.CurrentLoc.set loc;
           try
             let tdecl = Ltyping.annot decl in
             Globals.cabsPushGlobal (GAnnot(tdecl,Cil.CurrentLoc.get ()))
           with LogicTypeError ((source,_),msg) ->
             Kernel.warning ~source "%s. Ignoring global annotation" msg)
        decl;
    end;
    Chunk.empty

  | Cabs.CUSTOM (custom, name, loc) when isglobal ->
    begin
      Cil.CurrentLoc.set loc;
      try
        let tcustom = Ltyping.custom custom in
        Globals.cabsPushGlobal (GAnnot(Dcustom_annot(tcustom, name, loc),loc))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning ~source "%s. Ignoring custom annotation" msg
    end;
    Chunk.empty
  | Cabs.CUSTOM _ | Cabs.GLOBANNOT _ | Cabs.PRAGMA _ | Cabs.GLOBASM _
  | Cabs.FUNDEF _ ->
    Kernel.fatal ~current:true "this form of declaration must be global"

and do_typedef ghost ((specs, nl): Cabs.name_group) =
  (* Do the specifiers exactly once. *)
  let bt, sto, inl, attrs = do_spec_list ghost (suggest_anon_name nl) specs in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true
      "Storage or inline specifier not allowed in typedef";
  let createTypedef ((n,ndt,a,_) : Cabs.name) =
    (*    E.s (error "do_typeDef") *)
    let newTyp, tattr =
      do_type ghost false Cil.AttrType bt (Cabs.PARENTYPE(attrs, ndt, a))  in
    let newTyp' = cabsTypeAddAttributes tattr newTyp in
    let typedefNameAlreadyDefined : (typ * location) option =
      try Some (lookup_type_no_error "type" n)
      with Not_found -> None
    in
    match typedefNameAlreadyDefined with
    | Some (previousTyp, previousTypedefLoc) ->
      (* This typedef name was already defined. Following C11:6.7p3 :
         "(...) a typedef name may be redefined to denote the same type as it
         currently does, provided that type is not a variably modified type;" *)
      let previousTyp' = Cil.unrollTypeDeep previousTyp in
      let newTyp'      = Cil.unrollTypeDeep newTyp' in
      let both_denote_the_same_type =
        Cil_datatype.Typ.equal previousTyp' newTyp'
      in
      if both_denote_the_same_type
      then
        Kernel.feedback ~current:true
          "correct redefinition of typedef name %s as type %a, it was \
           previously defined to denote the same type (at %a)"
          n Printer.pp_typ newTyp'
          Printer.pp_location previousTypedefLoc
      else
        Kernel.abort ~current:true
          "redefinition of typedef name %s as type %a, when previously defined \
           as type %a (at %a)"
          n Printer.pp_typ newTyp' Printer.pp_typ previousTyp'
          Printer.pp_location previousTypedefLoc
    | None ->
      (* This typedef name was not defined yet: proceed as normal. *)
      let n', _  = newAlphaName true "type" n in
      let ti =
        { torig_name = n; tname = n';
          ttype = newTyp'; treferenced = false }
      in
      (* Since we use the same name space, we might later hit a global with the
         same name and we would want to change the name of the global. It is
         better to change the name of the type instead. So, remember all types
         whose names have changed. *)
      Globals.Typedefs.add n' ti;
      let namedTyp = TNamed(ti, []) in
      (* Register the type. register it as local because we might be in a local
         context. *)
      add_local_to_env (kind_plus_name "type" n) (EnvTyp namedTyp);
      Globals.cabsPushGlobal (GType (ti, Cil.CurrentLoc.get ()))
  in
  List.iter createTypedef nl

and do_only_typedef ghost (specs: Cabs.spec_elem list) : unit =
  let bt, sto, inl, attrs = do_spec_list ghost "" specs in
  if sto <> NoStorage || inl then
    Kernel.error ~once:true ~current:true
      "Storage or inline specifier not allowed in typedef";
  let restyp, nattr =
    do_type ghost false Cil.AttrType bt (Cabs.PARENTYPE(attrs, Cabs.JUSTBASE, []))
  in
  if nattr <> [] then
    Kernel.warning ~current:true "Ignoring identifier attribute";
  (* [doSpec] will register the type. *)
  (* See if we are defining a composite or enumeration type, and in that case
     move the attributes from the defined type into the composite type. *)
  let isadef =
    List.exists
      (function
          Cabs.SpecType(Cabs.Tstruct(_, Some _, _)) -> true
        | Cabs.SpecType(Cabs.Tunion(_, Some _, _)) -> true
        | Cabs.SpecType(Cabs.Tenum(_, Some _, _)) -> true
        | _ -> false) specs
  in
  match restyp with
  | TComp(ci, _, al) ->
    if isadef then begin
      ci.cattr <- Utility.cabsAddAttributes ci.cattr al;
      (* The [GCompTag] was already added. *)
    end else (* Add a [GCompTagDecl]. *)
      Globals.cabsPushGlobal (GCompTagDecl(ci, Cil.CurrentLoc.get ()))
  | TEnum(ei, al) ->
    if isadef then begin
      ei.eattr <- Utility.cabsAddAttributes ei.eattr al;
    end else
      Globals.cabsPushGlobal (GEnumTagDecl(ei, Cil.CurrentLoc.get ()))
  | _ ->
    Kernel.warning ~current:true
      "Ignoring un-named typedef that does not introduce a struct or \
       enumeration type"

(* Now define the processors for body and statement. *)
and doBody local_env (blk: Cabs.block) : Chunk.t =
  let ghost = local_env.is_ghost in
  enter_scope ();
  (* Rename the labels and add them to the environment. *)
  List.iter
    (fun l -> ignore (Labels.Manage.gen_new_local_label l))
    blk.Cabs.blabels;
  (* See if we have some attributes. *)
  let battrs = do_attributes ghost blk.Cabs.battrs in

  let bodychunk =
    after_conversion ~ghost
      (snd
         (List.fold_left   (* !!! @ evaluates its arguments backwards. *)
            (fun ((new_behaviors,keep_block),prev) s ->
               let local_env =
                 { local_env with
                   known_behaviors =
                     new_behaviors @ local_env.known_behaviors
                 }
               in

               let res = do_statement local_env s in
               (* Keeps [stmts] originating from the same source statement in a
                  single block when the statement follows a code annotation, so
                  that the annotation will be attached to the whole result and
                  not to the first [Cil] statement. *)
               let new_behaviors, keep_next =
                 match s.Cabs.stmt_node with
                 | Cabs.CODE_ANNOT _  -> [], true
                 | Cabs.CODE_SPEC (s,_) ->
                   List.map (fun x -> x.b_name) s.spec_behavior, true
                 | _ -> [], false
               in
               let chunk =
                 if keep_block then
                   append_chunk_to_annot ~ghost prev res
                 else prev @@ (res, ghost)
               in ((new_behaviors, keep_next), chunk))
            (([],false),Chunk.empty)
            blk.Cabs.bstmts))
  in
  exit_scope ();
  if battrs == [] && bodychunk.Chunk.locals == []
  then begin
    (* Keep block marked with [TIS_KERNEL_KEEP_BLOCK] or that defines local
       variables as independent blocks whatever happens. *)
    bodychunk
  end
  else begin
    let b = Chunk.to_block ~ghost bodychunk in
    b.battrs <- battrs;
    let res = Chunk.of_stmt (Cil.mkStmt ~ghost (Block b)) in
    { res with Chunk.cases = bodychunk.Chunk.cases }
  end

and do_statement local_env (stmt : Cabs.statement) : Chunk.t =
  let make_loop_annot code_annots loc : code_annotation list =
    try
      let do_code_annot : Logic_ptree.code_annot -> Cil_types.code_annotation =
        Ltyping.code_annot
          loc local_env.known_behaviors (Ctype !current_return_type)
      in
      List.map do_code_annot code_annots
    with LogicTypeError ((source, _), msg) ->
      Kernel.warning ~source "%s. Ignoring loop annotation" msg;
      []
  in
  let ghost = stmt.Cabs.stmt_ghost in
  let local_env = { local_env with is_ghost = ghost } in

  match stmt.Cabs.stmt_node with
  | Cabs.NOP loc ->
    let empty_stmt = Cil.mkEmptyStmt ~ghost ~loc () in
    { Chunk.empty with Chunk.stmts_with_effects = [empty_stmt, Chunk.no_effects, []]}
  | Cabs.COMPUTATION (exp, loc) ->
    Cil.CurrentLoc.set loc;
    let (last_stmt, data_ref, drop_body) = !gnu_body_result in
    if last_stmt == stmt
    then
      begin (* This is the last in a [GNU_BODY]. *)
        let action =
          match drop_body with
          | true -> ADrop
          | false -> AExp None
        in
        let (chunk, result_exp, result_typ) =
          do_full_exp local_env DontExpectConst exp action
        in
        data_ref := Some (result_exp, result_typ); (* Record the result. *)
        chunk
      end
    else
      begin
        let (chunk, exp, _typ) =
          do_full_exp local_env DontExpectConst exp ADrop
        in
        (* Drop the side-effect free expression unless the whole computation is
           pure and it contains potential threats (i.e. dereference). *)
        match Chunk.is_empty chunk, Is_dangerous.exp exp with
        | true, true ->
          let make_new_tmp_var = make_new_tmp_var ?prefix:None in
          chunk @@ (Chunk.keep_pure_exp ~make_new_tmp_var ~ghost exp loc, ghost)
        | true, _ ->
          let name = !current_fun_fundec.svar.vorig_name in
          IgnorePureExpHook.apply (name, exp);
          chunk
        | _ -> chunk
      end

  | Cabs.BLOCK (block, loc, _) ->
    Cil.CurrentLoc.set loc;
    let chunk = doBody local_env block in
    let result_chunk =
      let block = Chunk.to_block ~ghost chunk in
      let keep_block_attr = Attr (tis_kernel_keep_block, []) in
      block.battrs <- Cil.addAttributes [keep_block_attr] block.battrs;
      let stmt = Cil.mkStmt ~ghost (Block block) in
      Chunk.of_stmt stmt
    in
    { result_chunk with Chunk.cases = chunk.Chunk.cases }

  | Cabs.SEQUENCE (stmt_1, stmt_2, _) ->
    let chunk_1 = do_statement local_env stmt_1 in
    let chunk_2 = do_statement local_env stmt_2 in
    chunk_1 @@ (chunk_2, ghost)

  | Cabs.IF(conditional_exp, stmt_if_true, stmt_if_false, loc) ->
    let chunk_if_true = do_statement local_env stmt_if_true in
    let chunk_if_false = do_statement local_env stmt_if_false in
    Cil.CurrentLoc.set loc;
    do_condition local_env false conditional_exp chunk_if_true chunk_if_false

  | Cabs.WHILE(loop_invariant, conditional_exp, stmt, loc) ->
    let annotations = make_loop_annot loop_invariant loc in
    Labels.Loops.start_loop true;
    let chunk =
      let chunk = do_statement local_env stmt in
      match !Parameters.doTransformWhile with
      | true -> chunk @@ (Labels.Loops.cons_label_continue ~ghost Chunk.Make.skip_chunk, ghost)
      | false -> chunk
    in
    let break_cond_chunk = Chunk.Make.break_chunk ~ghost loc in
    Labels.Loops.exit_loop ();
    Cil.CurrentLoc.set loc;
    let conditional_chunk =
      do_condition local_env false conditional_exp Chunk.Make.skip_chunk break_cond_chunk
    in
    Chunk.Make.loop_chunk ~ghost annotations (conditional_chunk @@ (chunk, ghost))

  | Cabs.DOWHILE(loop_invariant, exp, stmt, loc) ->
    let annotations = make_loop_annot loop_invariant loc in
    Labels.Loops.start_loop false;
    let chunk = do_statement local_env stmt in
    Cil.CurrentLoc.set loc;
    (* No 'break' instruction can exit the chunk. *)
    let no_break chunk =
      List.for_all
        (fun (stmt, _effects, _calls) ->
           not (FallsThroughCanBreak.stmt_can_break stmt))
        chunk.Chunk.stmts_with_effects
    in
    (* Check if we are translating [do { <s> } while (0)]. If so, translate it
       into [<s>] instead. Only active when [-simplify-trivial-loops] is set
       (default), as it impact plugins that compare the shape of the [Cabs] and
       of the [Cil] files. *)
    if Kernel.SimplifyTrivialLoops.get ()
    && isCabsZeroExp exp (* [exp] is 0 or something equivalent. *)
    && annotations = [] (* No loop annotation. *)
    && not (Labels.Loops.continue_used ()) (* No [continue] inside the chunk. *)
    && no_break chunk (* No [break] inside the chunk. *)
    then begin
      Labels.Loops.exit_loop ();
      chunk
    end else begin
      let chunk' =
        let conditional_chunk =
          do_condition local_env false exp Chunk.Make.skip_chunk (Chunk.Make.break_chunk ~ghost loc)
        in
        Labels.Loops.cons_label_continue ~ghost conditional_chunk
      in
      Labels.Loops.exit_loop ();
      Chunk.Make.loop_chunk ~ghost annotations (chunk @@ (chunk', ghost))
    end

  | Cabs.FOR(loop_invariant, for_clause, exp_2, exp_3, stmt, loc) ->
    Cil.CurrentLoc.set loc;
    enter_scope (); (* Just in case we have a declaration. *)
    ForLoopHook.apply (for_clause, exp_2, exp_3, stmt);
    let (chunk_1, _, _), has_declaration =
      match for_clause with
      | Cabs.FC_EXP exp_1 ->
        do_full_exp local_env DontExpectConst exp_1 ADrop, false
      | Cabs.FC_DECL defn_1 ->
        let chunk = doDecl local_env false defn_1 in
        let exp = Cil.zero ~loc in
        let typ = Cil.voidType in
        (chunk, exp, typ), true
    in
    let annotations = make_loop_annot loop_invariant loc in
    let (chunk_3, _, _) = do_full_exp local_env DontExpectConst exp_3 ADrop in
    Labels.Loops.start_loop false;
    let stmt_chunk = do_statement local_env stmt in
    (*Kernel.debug "Loop body : %a" d_chunk s';*)
    Cil.CurrentLoc.set loc;
    let chunk_3 = Labels.Loops.cons_label_continue ~ghost chunk_3 in
    let break_cond_chunk = Chunk.Make.break_chunk ~ghost loc in
    Labels.Loops.exit_loop ();
    let result_chunk =
      match exp_2.Cabs.expr_node with
      | Cabs.NOTHING -> (* This means [true]. *)
        chunk_1 @@
        (Chunk.Make.loop_chunk ~ghost annotations
           (stmt_chunk @@ (chunk_3, ghost)), ghost)
      | _ ->
        chunk_1 @@
        (Chunk.Make.loop_chunk ~ghost annotations
           (((do_condition local_env false exp_2 Chunk.Make.skip_chunk break_cond_chunk)
             @@ (stmt_chunk, ghost)) @@ (chunk_3, ghost)), ghost)
    in
    exit_scope ();
    if has_declaration then
      let chunk =
        let stmt = Cil.mkStmt ~ghost (Block (Chunk.to_block ~ghost result_chunk)) in
        Chunk.of_stmt stmt
      in
      { chunk with Chunk.cases = result_chunk.Chunk.cases }
    else
      result_chunk

  | Cabs.BREAK loc ->
    Cil.CurrentLoc.set loc;
    Chunk.Make.break_chunk ~ghost loc

  | Cabs.CONTINUE loc ->
    Cil.CurrentLoc.set loc;
    Labels.Loops.continue_or_label_chunk ~ghost loc

  | Cabs.RETURN (Cabs.{ expr_node = Cabs.NOTHING; _ }, loc) ->
    Cil.CurrentLoc.set loc;
    if not (Cil.isVoidType !current_return_type) then
      Kernel.error ~current:true
        "Return statement without a value in function returning %a\n"
        Printer.pp_typ !current_return_type;
    Chunk.Make.return_chunk ~ghost None loc

  | Cabs.RETURN (exp, loc) ->
    Cil.CurrentLoc.set loc;
    (* Sometimes we return the result of a void function call. *)
    if Cil.isVoidType !current_return_type
    then begin
      if not (Cil.gccMode ()) then
        Kernel.abort ~current:true ~once:true
          "Return statement with a value in function returning void only \
           allowed with a GCC machdep.";
      let (chunk, _, _) = do_full_exp local_env DontExpectConst exp ADrop in
      Kernel.warning ~current:true ~once:true
        "Return statement with a value in function returning void.";
      chunk @@ (Chunk.Make.return_chunk ~ghost None loc, ghost)
    end else begin
      let return_typ =
        Cil.typeRemoveAttributes ["warn_unused_result"] !current_return_type
      in
      let (chunk, exp, typ) =
        do_full_exp local_env DontExpectConst exp (AExp (Some return_typ))
      in
      let (_typ, exp) = Casts.castTo typ return_typ exp in
      chunk @@ (Chunk.Make.return_chunk ~ghost (Some exp) loc, ghost)
    end

  | Cabs.SWITCH (exp, stmt, loc) ->
    Cil.CurrentLoc.set loc;
    let (exp_chunk, exp, typ) =
      do_full_exp local_env DontExpectConst exp (AExp None)
    in
    if not (Cil.isIntegralType typ) then
      Kernel.error ~once:true ~current:true
        "Switch on a non-integer expression.";
    let typ' = Cil.integralPromotion typ in
    let exp = Casts.makeCastT ~e:exp ~oldt:typ ~newt:typ' in
    Chunk.enter_break_env ();
    let stmt_chunk = do_statement local_env stmt in
    Chunk.exit_break_env ();
    exp_chunk @@ (Chunk.Make.switch_chunk ~ghost exp stmt_chunk loc, ghost)

  | Cabs.CASE (exp, stmt, loc) ->
    Cil.CurrentLoc.set loc;
    let (exp_chunk, exp, _typ) =
      do_full_exp local_env ExpectConst exp (AExp None)
    in
    if Chunk.is_not_empty exp_chunk || not (Cil.isIntegerConstant exp) then
      Kernel.error ~once:true ~current:true
        "Case statement with a non-constant";
    let stmt_chunk =
      let case =
        Simple
          (if Cil.(theMachine.lowerConstants)
           then Cil.constFold false exp
           else exp)
      in
      Chunk.Make.case_range_chunk ~ghost
        [case] loc (do_statement local_env stmt)
    in
    (* [exp_chunk] has no statements, but can contain local variables, in
       particular in the case of a [sizeof] with side-effects. *)
    exp_chunk @@ (stmt_chunk, ghost)

  | Cabs.CASERANGE (exp_low, exp_high, stmt, loc) ->
    Cil.CurrentLoc.set loc;
    let (exp_low_chunk, exp_low, _) =
      do_full_exp local_env DontExpectConst exp_low (AExp None)
    in
    let (exp_high_chunk, exp_high, _) =
      do_full_exp local_env DontExpectConst exp_high (AExp None)
    in
    if Chunk.is_not_empty exp_low_chunk
    || Chunk.is_not_empty exp_high_chunk then
      Kernel.error ~once:true ~current:true
        "Case statement with a non-constant";
    let int_low, int_high =
      match Cil.constFoldToInt exp_low, Cil.constFoldToInt exp_high with
      | Some integer_low, Some integer_high ->
        Integer.to_int integer_low, Integer.to_int integer_high
      | _ -> Kernel.abort ~current:true
               "Cannot understand the constants in case range"
    in
    if int_low > int_high then
      Kernel.error ~once:true ~current:true "Empty case range";
    let cases =
      (* Expand small ranges. *)
      if int_high - int_low <= 5
      then
        let rec expand_range (i : int) =
          if i <= int_high
          then Simple (Cil.integer ~loc i) :: expand_range (i + 1)
          else []
        in
        expand_range int_low
      else [Range (exp_low, exp_high)]
    in
    (exp_low_chunk @@ (exp_high_chunk, ghost)) @@
    (Chunk.Make.case_range_chunk ~ghost cases loc (do_statement local_env stmt), ghost)

  | Cabs.DEFAULT (stmt, loc) ->
    Cil.CurrentLoc.set loc;
    let chunk = do_statement local_env stmt in
    Chunk.Make.default_chunk ~ghost loc chunk

  | Cabs.LABEL (label, stmt, loc) ->
    Cil.CurrentLoc.set loc;
    C_logic_env.add_current_label label;
    (* Lookup the label because it might have been locally defined. *)
    let chunk =
      let chunk = do_statement local_env stmt in
      let label = Labels.Manage.lookup_label label in
      Labels.Loops.cons_label ~ghost label chunk loc true
    in
    C_logic_env.reset_current_label ();
    chunk

  | Cabs.GOTO (label, loc) ->
    Cil.CurrentLoc.set loc;
    (* Maybe we need to rename this label. *)
    Chunk.Make.goto_chunk ~ghost (Labels.Manage.lookup_label label) loc

  | Cabs.COMPGOTO (exp, loc) -> begin
      Cil.CurrentLoc.set loc;
      (* Do the expression. *)
      let exp_chunk, exp, _ =
        do_full_exp local_env DontExpectConst exp (AExp (Some Cil.voidPtrType))
      in
      match ComputedGoto.get_target_data () with
      | Some (switch_varinfo, switch_stmt) ->
        (* We have already generated this one.  *)
        let set_chunk =
          let set_instr =
            Set (Cil.var switch_varinfo, makeCast ~e:exp ~newt:Cil.intType, loc)
          in
          Chunk.of_stmt_and_effects (Cil.mkStmtOneInstr ~ghost set_instr,
                                     Chunk.no_effects)
        in
        let goto_chunk =
          Chunk.of_stmt (Cil.mkStmt ~ghost (Goto (ref switch_stmt, loc)))
        in
        (exp_chunk @@ (set_chunk, ghost)) @@ (goto_chunk, ghost)

      | None -> begin
          (* Make a temporary variable *)
          let vchunk = createLocal
              local_env.is_ghost
              (Cil.intType, NoStorage, false, [])
              (("__compgoto", Cabs.JUSTBASE, [], loc), Cabs.NO_INIT)
          in
          if not (Chunk.is_empty vchunk) then
            Kernel.fatal ~current:true
              "Non-empty chunk in creating temporary for goto *";
          let switchv, _ =
            try lookup_var "__compgoto"
            with Not_found ->
              Kernel.abort ~current:true "Cannot find temporary for goto *";
          in
          (* Make a switch statement. We'll fill in the statements at the end of
             the function. *)
          let switch =
            Cil.mkStmt ~ghost
              (Switch (Cil.new_exp ~loc (Lval(Cil.var switchv)),
                       Cil.mkBlock [], [], loc))
          in
          (* And make a label for it since we'll goto it. *)
          switch.labels <- [Label ("__docompgoto", loc, false)];
          ComputedGoto.set_target_data (Some (switchv, switch));
          (exp_chunk @@
           (Chunk.of_stmt_and_effects
              (Cil.mkStmtOneInstr ~ghost
                 (Set (Cil.var switchv,
                       makeCast ~e:exp ~newt:Cil.intType, loc)),
               Chunk.no_effects),
            ghost))
          @@ (Chunk.of_stmt switch, ghost)
        end
    end

  | Cabs.DEFINITION d ->
    doDecl local_env false d

  | Cabs.ASM (asmattr, tmpls, details, loc) ->
    (* Make sure all the outs are variables. *)
    let attr' = do_attributes local_env.is_ghost asmattr in
    Cil.CurrentLoc.set loc;
    let stmts : Chunk.t ref = ref Chunk.empty in
    let (tmpls', outs', ins', clobs', labels') =
      match details with
      | None ->
        let tmpls' =
          if Cil.msvcMode () then tmpls
          else
            let pattern = Str.regexp "%" in
            let escape = Str.global_replace pattern "%%" in
            List.map escape tmpls
        in
        (tmpls', [], [], [],[])
      | Some Cabs.{ aoutputs = outs;
                    ainputs = ins;
                    aclobbers = clobs;
                    alabels = labels } ->
        let outs' =
          List.map
            (fun (id, c, e) ->
               let (se, e', _) =
                 do_full_exp local_env DontExpectConst e (AExp None)
               in
               let lv =
                 match e'.enode with
                 | Lval lval
                 | StartOf lval -> lval
                 | _ ->
                   Kernel.abort ~current:true "invalid lvalue in asm output"
               in
               if not (Chunk.is_empty se) then
                 stmts := !stmts @@ (se, ghost);
               (id, c, lv)) outs
        in
        (* Get the side-effects out of expressions *)
        let ins' =
          List.map
            (fun (id, c, e) ->
               let (r, se, e', _) =
                 do_expression local_env DontExpectConst e (AExp None)
               in
               let se = Chunk.add_reads e'.eloc r se in
               if not (Chunk.is_empty se) then
                 stmts := !stmts @@ (se, ghost);
               (id, c, e'))
            ins
        in
        let labels' =
          List.map
            (fun label ->
               let label = Labels.Manage.lookup_label label in
               let gref = ref Cil.dummyStmt in
               Chunk.add_goto label gref;
               gref)
            labels
        in
        (tmpls, outs', ins', clobs, labels')
    in
    !stmts @@
    (Chunk.of_stmt_and_effects
       (Cil.mkStmtOneInstr ~ghost:local_env.is_ghost
          (Asm(attr', tmpls', outs', ins', clobs', labels', loc)),
        Chunk.no_effects),
     ghost)
  | Cabs.THROW (e,loc) ->
    Cil.CurrentLoc.set loc;
    (match e with
     | None -> Chunk.of_stmt (Cil.mkStmt ~ghost (Throw (None,loc)))
     | Some e ->
       let se,e,t = do_full_exp local_env DontExpectConst e (AExp None) in
       se @@
       (Chunk.of_stmt (Cil.mkStmt ~ghost (Throw (Some (e,t),loc))),ghost))
  | Cabs.TRY_CATCH(stry,l,loc) ->
    Cil.CurrentLoc.set loc;
    let chunk_try = do_statement local_env stry in
    let type_one_catch (var,scatch) =
      enter_scope();
      let vi =
        match var with
        | None -> Catch_all
        | Some (t,(n,ndt,a,ldecl)) ->
          let spec = do_spec_list ghost n t in
          let vi =
            make_varinfo_cabs
              ~ghost ~is_formal:false ~is_global:false ldecl spec (n,ndt,a)
          in
          add_local_to_env n (EnvVar vi);
          Catch_exn(vi,[])
      in
      let chunk_catch = do_statement local_env scatch in
      exit_scope();
      (vi,Chunk.to_block ~ghost chunk_catch)
    in
    let catches = List.map type_one_catch l in
    Chunk.of_stmt (Cil.mkStmt ~ghost
                     (TryCatch (Chunk.to_block ~ghost chunk_try,catches,loc)))
  | Cabs.TRY_FINALLY (b, h, loc) ->
    Cil.CurrentLoc.set loc;
    let b': Chunk.t = doBody local_env b in
    let h': Chunk.t = doBody local_env h in
    if b'.Chunk.cases <> [] || h'.Chunk.cases <> [] then
      Kernel.error ~once:true ~current:true
        "Try statements cannot contain switch cases";
    Chunk.of_stmt (Cil.mkStmt ~ghost
                     (TryFinally (Chunk.to_block ~ghost b',
                                  Chunk.to_block ~ghost h',
                                  loc)))

  | Cabs.TRY_EXCEPT (b, e, h, loc) ->
    Cil.CurrentLoc.set loc;
    let b': Chunk.t = doBody local_env b in
    (* Now do [e]. *)
    let ((se: Chunk.t), e', _) =
      do_full_exp local_env DontExpectConst e (AExp None) in
    let h': Chunk.t = doBody local_env h in
    if b'.Chunk.cases <> [] || h'.Chunk.cases <> [] || se.Chunk.cases <> [] then
      Kernel.error ~once:true ~current:true
        "Try statements cannot contain switch cases";
    (* Now take se and try to convert it to a list of instructions. This might
       not be always possible. *)
    let stmt_to_instrs s =
      List.rev_map
        (function (s, _effects, _calls) -> match s.skind with
            | Instr s' -> assert (s.labels = []); s'
            | _ ->
              Kernel.fatal ~current:true
                "Except expression contains unexpected statement")
        s
    in
    let il' = stmt_to_instrs se.Chunk.stmts_with_effects in
    Chunk.of_stmt (Cil.mkStmt ~ghost
                     (TryExcept
                        (Chunk.to_block ~ghost b',
                         (il', e'),
                         Chunk.to_block ~ghost h',
                         loc)))
  | Cabs.CODE_ANNOT (a, loc) ->
    begin
      try
        let typed_annot =
          Ltyping.code_annot
            loc local_env.known_behaviors (Ctype !current_return_type) a
        in
        Chunk.of_stmt (Cil.mkStmtOneInstr ~ghost (Code_annot (typed_annot,loc)))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning ~source "%s. Ignoring code annotation" msg;
        Chunk.empty
    end

  | Cabs.CODE_SPEC (a, loc) ->
    begin
      try
        let spec =
          Ltyping.code_annot loc local_env.known_behaviors
            (Ctype !current_return_type) (AStmtSpec ([],a))
        in
        Chunk.of_stmt (Cil.mkStmtOneInstr ~ghost (Code_annot (spec,loc)))
      with LogicTypeError ((source,_),msg) ->
        Kernel.warning ~source "%s. Ignoring code annotation" msg;
        Chunk.empty
    end

let blockInitializer local_env varinfo init_expression =
  let ghost = local_env.is_ghost in
  let chunk, init, typ = do_initializer local_env varinfo init_expression in
  let block = Chunk.to_block ~ghost chunk in
  block, init, typ

(* Reset the global variables and pick the proper language. *)
let clear_env language =
  startFile ();
  FunsCalledWithoutPrototype.clear ();
  CompInfo.clear_env ();
  CompField.clear_env ();
  Globals.clear_env ();
  CombineTypes.clear_env ();
  Hashtbl.clear alpha_renaming;
  Pragma.clear_env ();
  Logic_env.prepare_tables ();
  CompField.clear_env ();
  CallTempVars.clear ();
  ConstrExprId.reset ();
  current_language := language

(* Translate a file *)
let convFile ?(lang = default_language) (cabs_file : Cabs.file) : file =

  if Kernel.is_debug_key_enabled dkey_cabs then begin
    (* This test should be equivalent to what [Kernel.debug ~dkey] does.
       This is currently done like that because the Log module forces
       indentation which is quite huge when using [~dkey]. *)
    let pp_job fmt =
      Format.fprintf fmt "[kernel:cabs:print] Cabs code:\n";
      Cprint.printFile fmt cabs_file;
      Format.fprintf fmt "\n[kernel:cabs:print] End of code.\n"
    in
    if Filename.basename (fst cabs_file) <> "__fc_builtin_for_normalization.i"
    then Log.print_delayed pp_job
  end;

  (* Remove parentheses from the Cabs. *)
  let (file_name, cabs_definitions) = StripParen.file cabs_file in

  Errorloc.clear_errors ();

  (* Initial clean-up of the global types. *)
  clear_env lang;

  (* Conversion. *)
  Kernel.debug ~level:2 "Converting CABS->CIL" ;
  Cil.Builtin_functions.iter_sorted
    (fun name def -> ignore (setup_builtin name def));
  let do_one_global (ghost, (cabs_definition : Cabs.definition)) =
    let local_env = ghost_local_env ghost in
    let chunk = doDecl local_env true cabs_definition in
    if Chunk.is_not_empty chunk then
      Kernel.abort ~current:true
        "doDecl returns non-empty statement for global";
  in
  List.iter do_one_global cabs_definitions;
  let globals = ref (Globals.fileGlobals ()) in
  List.iter rename_spec !globals;

  (* The final clean-up. *)
  clear_env default_language;

  (* We are done. *)
  { fileName = file_name;
    globals  = !globals;
    globinit = None;
    globinitcalled = false; }


(**** Export parameters to API ****)
let typeForInsertedVar = Parameters.typeForInsertedVar
let setDoTransformWhile = Parameters.setDoTransformWhile
let setDoAlternateConditional = Parameters.setDoAlternateConditional
let typeForInsertedCast = Parameters.typeForInsertedCast

let allow_return_collapse ~tlv ~tf =
  allow_return_collapse ~lval_typ:tlv ~f_typ:tf

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
