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

(** Initialization. *)

(*
  DEPENDS ON:
  + SubObj: Utility.integerArrayLength
  + SubObj: Chunk.t
  + AssignInit: Chunk
*)

(*
  ENVIRONMENT: None.
*)

(*
  EXPORTS: Nothing.
*)

open Cil_types
open Cil

module Utility = Cabs2cil_Utility
module Chunk = Cabs2cil_BlockChunk


module Initializer = struct

  (* USED ONLY IN:
     + do_initializer
     + do_init
  *)

  let category_initializer = Kernel.register_category "cabs2cil:initializers"

  (** Find the fields to initialize in a composite
      This function is used to preprocess the list of structure's fields which
      will be iterated on for initializing. There are basically two options:
      - if not given a designator (a field name), then the list of all the
        non-anonymous fields is returned; this is useful when we initialize a
        structure from the beginning (e.g. [{ 42, 43, 44 }]);
      - if given the a designator (a field name), then the list of all the
        non-anonymous fields starting with the designated one (the one with the
        provided name) is returned; this corresponds to initializing a structure
        using a designator with a field's name (i.e. [{ .x = 42 }]).
      Afterwards, if this is in fact a union and not a structure, then only the
      first element of the list is kept, as only one field of a union should
      be initialized. *)
  let comp_fields_to_init
      (comp : compinfo)
      (field_designator : string) (* Empty string counts as no designator. *)
    : fieldinfo list =
    (* 1. Filter out the anonymous fields. *)
    let non_anonymous_fields =
      List.filter
        (fun fieldinfo -> fieldinfo.fname <> missingFieldName)
        comp.cfields
    in
    (* 2. If a designator (a field name) was provided, start with
          the designated field. *)
    let fields_starting_with_designated_one =
      match field_designator with
      | "" -> non_anonymous_fields
      | field_name ->
        let rec get_fields_starting_with_designated_one = function
          (* TODO: This is a user error message. Shouldn't I rather check that
             before and make a fatal error here? *)
          | [] -> Kernel.abort ~current:true
                    "field designator '%s' does not refer to any field in \
                     type '%s'"
                    field_name (Cil.compFullName comp)
          | (fieldinfo :: _) as fields
            when fieldinfo.fname = field_name ->
            fields
          | _ :: remaining_fields ->
            get_fields_starting_with_designated_one remaining_fields
        in
        get_fields_starting_with_designated_one non_anonymous_fields
    in
    (* 3. If it is a union, take only the first field from the list. *)
    match fields_starting_with_designated_one with
    | [] -> []
    | (fieldinfo :: _) when not comp.cstruct -> [fieldinfo]
    | _ when comp.cstruct -> fields_starting_with_designated_one
    | _ -> assert false (* Impossible match case, compiler does not see it. *)

  module Subobj = struct

    (* NOTE: All the data structures here could be replaced by just a triple:
       two offsets and a type. This would be (of course) less time-efficient
       than now, when we are maintaining all the information on every level, but
       also so much less complicated: we would just traverse the offsets and the
       type in sync, there would be no need to update the data structures and
       make sure that they are consistent. *)

    (* We always remember two offsets: the absolute and the relative one. *)
    type subobj_offsets = {
      absolute_offset: offset; (* Absolute offset from the host which is being
                                  initialized (i.e. the initialized variable).
                                  Useful mostly for error messages.*)
      relative_offset: offset; (* Offset relative to the current object.
                                  For example: if the current object is a
                                  structure [s] of type [{ int x, y; }] and the
                                  subobject is its field [x], then the relative
                                  offset is [.x]. *)
    }

    (* The current object. *)
    type current_obj = {
      host : varinfo;                       (* The host that is being
                                               initialized.
                                               Useful for error messages. *)
      current_obj_typ : typ;                (* Type of the current object. *)
      current_obj_offsets : subobj_offsets; (* Offsets of the current object. *)
    }

    let make_current_obj host typ offset ~relative_is_absolute =
      let offsets = {
        absolute_offset = offset;
        relative_offset = (if relative_is_absolute
                           then offset
                           else NoOffset);
      } in {
        host                = host;
        current_obj_typ     = typ;
        current_obj_offsets = offsets;
      }

    (* Stack of subobjects leading from the current object to the currently
       processed subobject.
       - [subobj_stack] is always the parent element in the stack, i.e. the
         subobject one-offset-up;
       - [subobj_offsets] are always the offsets (absolute and relative) of the
         parent of this subobject; the offset of the subobject itself is always
         computed on demand (using functions [get_absolute_offset] and
         [get_relative_offset]) by taking the appropriate parent offset and
         appending to it one last element:
         - if the subobject is [InArray] then we append an offset element
           pointing to the right cell in the array;
         - if the subobject is [InComp] then we append an offset element
           pointing to the right field of the composite;
       - [in_array] and [in_comp] describe how this subobject relates to its
         parent subobject. *)
    type subobj_stack =
      | AtCurrentObject
      | EndReached (* Set if we have reached the end. *)
      | InArray of subobj_stack * subobj_offsets * in_array
      | InComp  of subobj_stack * subobj_offsets * in_comp

    and in_array = {
      base_type : typ;     (* Array's base type. *)
      length : int;        (* Array's length; if the array's length is
                              unspecified we use [Int.max_int]. *)
      current_index : int; (* Index in array where we are right now. *)
    }

    and in_comp = {
      flds_to_init: fieldinfo list; (* Fields left to initialize in the
                                       composite. The head of the list is
                                       the field where we are right now. *)
    }

    (** A subobject basically corresponds to an offset from the current object.
        The offset of the currently handled subobject can be found reading from
        the end of the list (the bottom of the stack), starting with the current
        object. This is not necessary though, as the offsets are computed and
        stored on every level of the stack ([InArray] and [InComp] stack element
        constructors store the relative and absolute offset of the corresponding
        subobject's parent), so it's enough to look at the first element of the
        stack.
        NOTE: This data structure is basically just a stack (of elements which
        correspond to subobjects), where additionaly we can access the last
        element (i.e. the current object) in constant time instead of being
        forced to traverse the whole the stack in order to reach it. A simpler
        implementation would remove the [current_obj] field here and change the
        [AtCurrentObject] constructor to [AtCurrentObject of current_obj]. *)
    type t = {
      current_obj: current_obj;   (* The current object.  *)
      subobj_stack: subobj_stack; (* The stack of subobjects leading to this
                                     subobject. *)
    }

    (** Make a subobject iterator if you already have a current object. *)
    let make_subobj current_obj = {
      current_obj  = current_obj;
      subobj_stack = AtCurrentObject;
    }

    (** Make a subobject iterator if you don't have the current object. *)
    let make host typ offset ~relative_is_absolute =
      make_subobj (make_current_obj host typ offset ~relative_is_absolute)

    (** Make a subobject iterator where the given variable is the host and the
        current object. *)
    let of_varinfo varinfo =
      make varinfo varinfo.vtype NoOffset ~relative_is_absolute:false

    let was_end_reached so = so.subobj_stack = EndReached

    (* Build the offset corresponding to the array cell. This offset consist
       of two parts:
       - first the (absolute / relative) offset of the array,
       - then the (relative) offset of the cell in the array. *)
    let offset_in_array array_offset (index : int) =
      (* Prepare the (relative) offset pointing to the array's cell. *)
      let array_cell_offset =
        let index_exp = integer ~loc:(CurrentLoc.get()) index in
        Index(index_exp, NoOffset)
      in
      (* Append it to the offset pointing to the array. *)
      addOffset array_cell_offset array_offset

    (* Build the offset corresponding to the given composite's field. This
       offset consist of two parts:
       - first the (absolute / relative) offset of the composite,
       - then the (relative) offset of the given field in the composite. *)
    let offset_in_comp comp_offset (field : fieldinfo) =
      (* Prepare the (relative) offset pointing to the composite's field. *)
      let comp_field_offset = Field(field, NoOffset) in
      (* Append it to the offset pointing to the composite. *)
      addOffset comp_field_offset comp_offset

    (* Subobject is inconsistent in three cases:
       - it corresponds to an array field and the current index is either
         negative or out of the array's bounds,
       - it corresponds to a structure field and the list of fields left to
         initialize is empty.
       This should only be possible inside [set_subobj_stack_and_normalize] and
       [advance_to_next] functions, at all other times the subobject is always
       consistent. *)
    let fatal_inconsistent_subobj () =
      Kernel.fatal ~current:true "Subobj: inconsistent subobj"

    (** Two functions to find the absolute / relative offset of the currently
        handled subobject. *)
    let get_absolute_offset, get_relative_offset =

      let get_offset (extract_offset : subobj_offsets -> offset) so =
        match so.subobj_stack with
        | AtCurrentObject | EndReached ->
          extract_offset so.current_obj.current_obj_offsets
        | InArray (_, parent_offsets, in_array)
          when in_array.current_index >= 0
            && in_array.current_index < in_array.length ->
          let parent_array_offset = extract_offset parent_offsets in
          offset_in_array parent_array_offset in_array.current_index
        | InComp (_, parent_offsets, { flds_to_init = (first_fld :: _) }) ->
          let parent_comp_offset = extract_offset parent_offsets in
          offset_in_comp parent_comp_offset first_fld
        | _ -> fatal_inconsistent_subobj ()
      in

      get_offset (fun offsets -> offsets.absolute_offset),
      get_offset (fun offsets -> offsets.relative_offset)


    let get_typ so = match so.subobj_stack with
      | AtCurrentObject | EndReached -> so.current_obj.current_obj_typ
      | InArray (_, _, in_array)
        when in_array.current_index >= 0
          && in_array.current_index < in_array.length -> in_array.base_type
      | InComp (_, _, { flds_to_init = (first_fld :: _) }) -> first_fld.ftype
      | _ -> fatal_inconsistent_subobj ()

    (* Set the subobj_stack and normalize the subobject iterator.
       Basically: if we are in an array / composite and the end of an array /
       last field of the composite was surpassed then we should descend one
       level (i.e. to the level of this array / composite) and then advance one
       subobject forward. *)
    let rec set_subobj_stack_and_normalize so subobj_stack =

      let set_subobj_stack so subobj_stack' =
        { so with subobj_stack = subobj_stack' }
      in

      match subobj_stack with

      (* A the top level. *)
      | AtCurrentObject | EndReached -> set_subobj_stack so subobj_stack

      (* In an array. The last cell was passed. *)
      | InArray (parent_stack, _, in_array)
        when in_array.current_index = in_array.length ->
        Kernel.debug ~dkey:category_initializer "Past the end of array";
        (* Descend one level. *)
        let so = set_subobj_stack so parent_stack in
        (* Advance forward! *)
        advance_to_next so

      (* In an array. Inconsistent current index. *)
      | InArray (_, _, in_array)
        when in_array.current_index > in_array.length ->
        Kernel.fatal ~current:true
          "Subobj.normalize: current array index > array length"

      | InArray (_, _, in_array)
        when in_array.current_index < 0 ->
        Kernel.fatal ~current:true
          "Subobj.normalize: current array index is negative"

      (* In an array. There are some cells left. *)
      | InArray _ -> set_subobj_stack so subobj_stack

      (* In a structure. The last field was passed. *)
      | InComp (parent_stack, _, { flds_to_init = [] }) ->
        Kernel.debug ~dkey:category_initializer "Past the end of structure";
        (* Descend one level. *)
        let so = set_subobj_stack so parent_stack in
        (* Advance forward! *)
        advance_to_next so

      (* In a structure. There are some fields left. *)
      | InComp _ -> set_subobj_stack so subobj_stack


    (* Advance to the next subobject. *)
    and advance_to_next so =

      match so.subobj_stack with

      (* The end of the current object was already reached before. This should
         not happen! *)
      | EndReached ->
        Kernel.fatal ~current:true
          "Subobj.advance_to_next: Past the end of the current current object"

      (* At the current object. We have thus reached the end, there are no more
         subobjects ahead. *)
      | AtCurrentObject ->
        Kernel.debug ~dkey:category_initializer
          "Subobj.advance_to_next: The end of the current object was reached";
        set_subobj_stack_and_normalize so EndReached

      (* In an array. No more cells to initialize. *)
      | InArray (_, _, in_array)
        when in_array.current_index >= in_array.length ->
        Kernel.fatal ~current:true
          "Subobj.advance_to_next: Advancing past the end of the array"

      (* In an array. The cell index is negative. *)
      | InArray (_, _, in_array)
        when in_array.current_index < 0 ->
        Kernel.fatal ~current:true
          "Subobj.advance_to_next: Current cell's index is negative"

      (* In an array. Advance to the next array index. *)
      | InArray (parent_stack, parent_offsets, in_array) ->
        let new_index = in_array.current_index + 1 in
        Kernel.debug ~dkey:category_initializer
          "Subobj.advance_to_next: Advancing to [%d]" new_index;
        let subobj_stack =
          let in_array' = { in_array with current_index = new_index } in
          InArray (parent_stack, parent_offsets, in_array')
        in
        set_subobj_stack_and_normalize so subobj_stack

      (* In a structure. No more fields left to initialize. *)
      | InComp (_, _, { flds_to_init = [] }) ->
        Kernel.fatal ~current:true
          "Subobj.advance_to_next: Advancing past the end of the structure"

      (* In a structure. Advance to the next field. *)
      | InComp (parent_stack, parent_offsets,
                { flds_to_init = (first_fld :: remaining_flds)}) ->
        Kernel.debug ~dkey:category_initializer
          "Subobj.advance_to_next: Advancing past .%s" first_fld.fname;
        let subobj_stack =
          let in_comp = { flds_to_init = remaining_flds } in
          InComp(parent_stack, parent_offsets, in_comp)
        in
        set_subobj_stack_and_normalize so subobj_stack

    let advance_into_array ?(index=0) so =
      let subobj_stack =
        match unrollType (get_typ so) with
        | TArray(array_base_type, array_len_opt, _, _) ->
          (* Put this on top of the previous subobject stack. *)
          let parent_stack = so.subobj_stack in
          (* The parent offsets are the offsets of the previous subobject. *)
          let parent_offsets = {
            absolute_offset = get_absolute_offset so;
            relative_offset = get_relative_offset so;
          } in
          (* Go into the array. *)
          let in_array =
            let array_len = Utility.integerArrayLength array_len_opt in {
              base_type     = array_base_type;
              length        = array_len;
              current_index = index;
            }
          in
          InArray(parent_stack, parent_offsets, in_array)
        | _ ->
          Kernel.fatal ~current:true
            "Subobj.advance_into_array: the subobject is not an array"
      in
      set_subobj_stack_and_normalize so subobj_stack

    let advance_into_comp ?(field_name="") so =
      let subobj_stack =
        match unrollType (get_typ so) with
        | TComp (comp, _, _) ->
          (* Put this on top of the previous subobject stack. *)
          let parent_stack = so.subobj_stack in
          (* The parent offsets are the offsets of the previous subobject. *)
          let parent_offsets = {
            absolute_offset = get_absolute_offset so;
            relative_offset = get_relative_offset so;
          } in
          (* Go into the composite. *)
          let in_comp = {
            flds_to_init = comp_fields_to_init comp field_name
          } in
          InComp(parent_stack, parent_offsets, in_comp)
        | _ ->
          Kernel.fatal ~current:true
            "Subobj.advance_into_comp: the subobject is not a composite"
      in
      set_subobj_stack_and_normalize so subobj_stack

    let start_with_new_current_object ?(keep_the_relative_offset=false) so =
      (* Prepare the subobject iterator where the current object is the
         currently handled subobject of the provided subobject iterator. *)
      let new_current_object =
        (* The host variable remains the same. *)
        let host = so.current_obj.host in
        (* Use the type is be the type of the currently handled subobject. *)
        let typ = get_typ so in
        (* Use the absolute offset of the currently handled subobject. *)
        let offset = get_absolute_offset so in
        (* Do we want to keep the relative offset of the previous current
           object or start with no offset? *)
        let relative_is_absolute = keep_the_relative_offset in
        make_current_obj host typ offset ~relative_is_absolute
      in
      (* Use the prepared current object to create the new subobject
         iterator. *)
      make_subobj new_current_object

    let reset so = make_subobj so.current_obj

    let get_offset = get_relative_offset

    let get_current_obj_offset so =
      so.current_obj.current_obj_offsets.relative_offset

    let pp_as_lval fmt so =
      let lval = Var so.current_obj.host, get_offset so in
      Format.fprintf fmt "%a" Cil_printer.pp_lval lval

    let pp_current_obj_as_lval fmt so =
      let lval = Var so.current_obj.host, get_current_obj_offset so in
      Format.fprintf fmt "%a" Cil_printer.pp_lval lval

    let get_host_var_name so = so.current_obj.host.vname

  end


  module Preinit = struct

    (* Some utilitites for doing initializers. *)

    let dkey = category_initializer

    (* A designator here is in fact a single element of a designator:
       - either corresponding to an array index "[i]",
       - or corresponding to a composite field ".s".
       Thus lists of designators (called "paths" here) correspond to
       CIL offsets. *)
    (* NOTE: The choice for storing compound preinitializers was:
       - either to have two constructors:
         * CompoundArrayPreinit with an integer map (for mapping array indices
           to preinitializers for the array cells),
         * and CompoundCompositePreinit with a string map (for mapping the
           composite fields' names to preinitializers for the composite's
           fields);
       - or have just one constructor CompoundPreinit with a designator map,
         where a designator can be either an array index or a field name.
       The first solution would create more redundant code (everywhere we would
       need to consider both options), but typing would be stronger. The second
       solution was ultimately chosen, because of the code redundancy reason and
       because it seemed more sensible in this place to consider designators
       more from the syntactic perspective and do not require this kind of
       consistency on the type level, as this issue is already taken care of
       somewhere else.
       Some runtime checks have been added though, just for a good measure of
       defensive programming and for easier debugging. *)
    module Designator : sig

      type t
      type path = t list

      val of_array_index : int -> t
      val of_fieldinfo : fieldinfo -> t

      val compare : t -> t -> int

      val pretty : Format.formatter -> t -> unit

      (* Pretty print a list of designators as if it was a CIL offset. *)
      val pp_path : Format.formatter -> path -> unit

      val as_array_index : t -> int
      val as_field_name : t -> string

    end = struct

      type t =
        | ArrayIndex of int
        | FieldName of string * int

      type path = t list

      let of_array_index i = ArrayIndex i

      let of_fieldinfo fieldinfo =
        let field_name = fieldinfo.fname in
        let field_index : int =
          Extlib.find_index
            (fun field_info' -> field_info'.fname = field_name)
            fieldinfo.fcomp.cfields
        in
        FieldName (field_name, field_index)

      let failure_mixed_designators i s =
        Kernel.failure ~current:true
          "Preinit.Designator: array index designator %d and field name \
           designator %s compared" i s

      let compare x y =
        match x, y with
        | ArrayIndex     i1,  ArrayIndex     i2  -> compare i1 i2
        | FieldName  (_, i1), FieldName  (_, i2) -> compare i1 i2
        | ArrayIndex i, FieldName (s, _) -> failure_mixed_designators i s;  1
        | FieldName (s, _), ArrayIndex i -> failure_mixed_designators i s; -1

      let pretty fmt = function
        | ArrayIndex  i     -> Format.fprintf fmt "[%d]" i
        | FieldName  (s, _) -> Format.fprintf fmt ".%s" s

      let pp_path fmt path =
        Format.fprintf fmt "%a"
          (Pretty_utils.pp_list pretty) (List.rev path)

      let as_array_index = function
        | ArrayIndex  i     -> i
        | FieldName  (s, _) -> Kernel.fatal ~current:true
                                 "designator was expected to be an array \
                                  index, it is a composite type field name %s" s

      let as_field_name = function
        | FieldName  (s, _) -> s
        | ArrayIndex  i     -> Kernel.fatal ~current:true
                                 "designator was expected to be a composite \
                                  type field name, it is an array index %d" i

    end

    module DesignatorMap = Map.Make(Designator)

    (* This type stores basically everything possible that came out of the
       [do_expression] performed on the initialization expression in CABS
       (including the CABS expression itself). Thanks to this it is not
       necessary to call [do_expression] for a second time (the first call must
       happen quite early, because we make decisions based on the expression's
       type). Also, these details are useful for printing warnings (e.g. when an
       initalizer got overridden) and debugging. *)
    type init_exp_with_details = {
      cabs_exp: Cabs.expression;
      reads:    lval list;
      se:       Chunk.t;
      exp:      exp;
      typ:      typ;
    }

    let make_init_exp_with_details cabs_exp reads se exp typ = {
      cabs_exp = cabs_exp;
      reads    = reads;
      se       = se;
      exp      = exp;
      typ      = typ;
    }

    (* TODO: Comment! *)
    type preinit =
      | NoPreinit
      | SinglePreinit of single_preinit
      | CompoundPreinit of preinits * bool
      | SingleThenCompoundPreinit of single_preinit * preinits

    and single_preinit = init_exp_with_details

    and preinits = preinit DesignatorMap.t

    (* Preinitializer type for the API. *)
    type t = preinit

    let preinits_empty = DesignatorMap.empty

    let get_max_index (preinits : preinits) : Designator.t =
      fst (DesignatorMap.max_binding preinits)

    let preinit_fold
        (f : Designator.path -> single_preinit -> 'a -> 'a)
        (preinit : preinit)
        (acc : 'a) =
      let rec preinit_fold' f preinit (path : Designator.path) acc =
        match preinit with
        | NoPreinit -> acc
        | SinglePreinit init_exp -> f path init_exp acc
        | CompoundPreinit (preinits, _complete) ->
          let f' designator preinit acc =
            preinit_fold' f preinit (designator :: path) acc
          in
          DesignatorMap.fold f' preinits acc
        | SingleThenCompoundPreinit (init_exp, preinits) ->
          let acc' = preinit_fold' f (SinglePreinit init_exp) path acc in
          preinit_fold' f (CompoundPreinit (preinits, false)) path acc'
      in
      preinit_fold' f preinit [] acc

    let preinit_iter
        (f : Designator.path -> single_preinit -> unit)
        (preinit : preinit) : unit =
      let f' path init_exp () = f path init_exp; () in
      preinit_fold f' preinit ()

    (* Pretty-printing. *)
    let pretty fmt preinit =
      (* If there is a remaining offset, then print a full designator using that
         offset. If there is no remaining offset, then there is no need to print
         anything. *)
      let pp_initializer_relative_path fmt = function
        | [] -> ()
        | relative_path ->
          Format.fprintf fmt "%a = " Designator.pp_path relative_path
      in

      (* Print preinits that are correspond to contents of an actual compound
         initializer. *)
      let rec pp_complete_preinits fmt preinits =
        Format.fprintf fmt "%a"
          (Pretty_utils.pp_list ~sep:",@ "
             (fun fmt (key, value) ->
                Format.fprintf fmt "@[%a = %a@]"
                  Designator.pretty key
                  (pretty' [] true) value))
          (DesignatorMap.bindings preinits)

      (* Print preinits that are just a subset of initializers of a compound
         initializer: they refine a previous single initializer. *)
      and pp_refining_preinits fmt (relative_path, preinits) =
        Format.fprintf fmt "%a"
          (Pretty_utils.pp_list ~sep:",@ "
             (fun fmt (key, value) ->
                Format.fprintf fmt "@[%a@]"
                  (pretty' (key::relative_path) false) value))
          (DesignatorMap.bindings preinits)

      (* This is a bit complex, because we have to take into account what
         happens when a single initializer is refined using other initializers,
         e.g.

           struct sub { int x, y, z; };
           struct sub s0 = { 1, 2, 3 };
           struct { struct sub s; } s1 = { .s = s0, .s.x = 4, .s.y. = 5 };

         In order to print this correctly we must distinguish between:
         1) the situation when given CompoundPreinit corresponds to an actual
            complete initializer and thus we can write it, for instance, as:

              .s = { .x = 4, .y = 5 }

         2) and the when it corresponds to several individual initializers for
            subobjects and thus we should write it as:

              .s.x = 4, .s.y = 5

         The semantics of these two is different because the field .s.z will get
         a different value and thus we must use the right notation.

         Invariant: the [relative_path] always holds the offset from the current
         object, i.e. the part of offset concerning the given preinit which was
         not printed yet on previous levels. *)
      and pretty' relative_path overriding fmt preinit =
        match preinit with
        | NoPreinit ->
          Format.fprintf fmt "@[%a<no_initializer>@]"
            pp_initializer_relative_path relative_path
        | SinglePreinit e ->
          Format.fprintf fmt "@[%a%a@]"
            pp_initializer_relative_path relative_path
            Cil_printer.pp_exp e.exp
        | CompoundPreinit (preinits, complete) ->
          if overriding || complete then
            (* We should print it as a complete compound initializer. *)
            Format.fprintf fmt "@[%a{@ @[%a@]@ }@]"
              pp_initializer_relative_path relative_path
              pp_complete_preinits preinits
          else
            (* We should print it as several individual initializers. *)
            (* NOTE: This will never happen at the top-level. *)
            Format.fprintf fmt "@[%a@]"
              pp_refining_preinits (relative_path, preinits)
        | SingleThenCompoundPreinit (e, preinits) ->
          Format.fprintf fmt "@[%a{@ @[%a@],@ @[%a@]@ }@]"
            pp_initializer_relative_path relative_path
            Cil_printer.pp_exp e.exp
            pp_refining_preinits (relative_path, preinits)
      in

      Format.fprintf fmt "\n%a\n"
        (pretty' [] true) preinit


    (** Building and manipulating preinitializers. *)

    let none = NoPreinit

    let is_single preinit = match preinit with
      | SinglePreinit _ -> true
      | _               -> false

    (* Special case for treating GNU extension on empty compound
       initializers. *)
    let make_compound_empty_preinit () =
      if Cil.gccMode () || Cil.msvcMode ()
      then CompoundPreinit (preinits_empty, true)
      else Kernel.abort "empty initializers only allowed for GCC/MSVC"

    (* Set a preinitializer at the given offset. *)
    let rec add_preinit existing_preinit remaining_offset preinit so =

      let host_name = Subobj.get_host_var_name so in
      let absolute_offset = Subobj.get_absolute_offset so in

      match remaining_offset with

      (* The right place to initialize was reached as the offset ends here.
         Just put the pre-initializer here, no matter if it is a new one or if
         we override an existing pre-initializer. *)
      | NoOffset ->
        begin
          if existing_preinit <> NoPreinit then
            Kernel.feedback ~current:true
              "initializer overrides prior initialization of %s at offset %a"
              host_name Cil_printer.pp_offset absolute_offset;
          preinit_iter
            (fun path init_exp ->
               Kernel.debug ~dkey ~current:true
                 "overriding initializer at offset %a%a"
                 Cil_printer.pp_offset absolute_offset
                 Designator.pp_path path;
               (* Following C11:footnote 151 :
                  "Any initializer for the subobject which is overridden and so
                  not used to initialize that subobject might not be evaluated
                  at all." *)
               (* TODO: Improve checking if there are actually some side
                  effects. Now, for example, "k ? 0 : 1" is translated to a
                  non-empty chunk, and thus a feedback is emitted. If we were
                  sure, this could be changed to a warning. *)
               if Chunk.is_not_empty init_exp.se then
                 Kernel.feedback ~current:true
                   "this expression from the overriden initializer may or may \
                    not be evaluated:@ @[%a@]"
                   Cprint.print_expression init_exp.cabs_exp)
            existing_preinit;
          preinit
        end

      (* Otherwise traverse the pre-initializer structure (if it is already
         there) or build / modify the pre-initializer structure (if it is not
         there yet or if it is just a single pre-initializer at this point and
         we need to override) following the given offset. *)
      | _ ->
        (* Find the designator using the head of the offset. *)
        let (designator, remaining_offset, so)
          : (Designator.t * offset * Subobj.t) =
          match remaining_offset with
          | NoOffset -> assert false (* Previous match excludes this option. *)

          | Index ({ enode = Const (CInt64 (i, _, _)); _ }, remaining_offset) ->
            let index = Integer.to_int i in
            let designator = Designator.of_array_index index in
            let so = Subobj.advance_into_array ~index so in
            designator, remaining_offset, so

          | Index _ ->
            Kernel.fatal ~current:true
              "add_preinit: non-constant index in offset"

          | Field (fieldinfo, remaining_offset) ->
            let designator = Designator.of_fieldinfo fieldinfo in
            let so =
              let field_name = fieldinfo.fname in
              Subobj.advance_into_comp ~field_name so
            in
            designator, remaining_offset, so
        in

        let (single_exp_opt, preinits, complete)
          : single_preinit option * preinits * bool =
          match existing_preinit with

          (* If there was no pre-initializer corresponding to this designation,
             a new compound pre-initialier will be built. *)
          | NoPreinit ->
            None, preinits_empty, false

          (* There is already a compound pre-initialier here: we will modify
             or refine it. *)
          | CompoundPreinit (preinits, complete) ->
            None, preinits, complete

          (* There is already a single pre-initialier here: we will refine it
             (override only partially). *)
          | SinglePreinit init_exp ->
            Some init_exp, preinits_empty, false

          (* There is alread both a single and a compound pre-initializer here:
             we will modify or refine the comound part. *)
          | SingleThenCompoundPreinit (init_exp, preinits) ->
            Some init_exp, preinits, false
        in

        begin
          match single_exp_opt with
          | Some _ ->
            Kernel.feedback ~current:true
              "subobject initialization partially overrides prior single \
               initialization of %s at offset %a"
              host_name Cil_printer.pp_offset absolute_offset
          | None -> ()
        end;

        let refined_preinits : preinits =
          let preinit : preinit =
            let existing_preinit =
              try DesignatorMap.find designator preinits
              with Not_found -> NoPreinit
            in
            add_preinit existing_preinit remaining_offset preinit so
          in
          DesignatorMap.add designator preinit preinits
        in

        match single_exp_opt with
        | None ->
          CompoundPreinit (refined_preinits, complete)
        | Some init_exp ->
          SingleThenCompoundPreinit (init_exp, refined_preinits)

    let set_init_exp_for_subobject existing_preinit so init_exp =
      let preinit = SinglePreinit init_exp in
      let set_at_offset = Subobj.get_offset so in
      add_preinit existing_preinit set_at_offset preinit (Subobj.reset so)

    let set_preinit_for_subobject existing_preinit so preinit =
      let set_at_offset = Subobj.get_offset so in
      (* This preinitializer overrides any previous initializer there, so it is
         marked as complete. *)
      let preinit = match preinit with
        | CompoundPreinit (preinits, _) -> CompoundPreinit (preinits, true)
        | _ -> preinit
      in
      add_preinit existing_preinit set_at_offset preinit (Subobj.reset so)

    (* This function is never used now. It is kept around just in case if we
       were one day to revise our decision on the interpretation of the Defect
       Report #413. *)
    let _set_compound_empty_preinit_for_subobject existing_preinit so =
      let set_at_offset = Subobj.get_offset so in
      let preinit = make_compound_empty_preinit () in
      add_preinit existing_preinit set_at_offset preinit (Subobj.reset so)

    (* Helper function with a sanity check included: get the length of
       a complete array type as integer, print an internal error if the length
       is invalid. *)
    let int_of_complete_array_length (array_len_exp : exp) =
      match constFoldToInt array_len_exp with
      | Some length when Integer.ge length Integer.zero ->
        Integer.to_int length
      | Some length ->
        Kernel.fatal ~current:true
          "invalid array length: %d is negative"
          (Integer.to_int length)
      | None ->
        Kernel.fatal ~current:true
          "invalid array length: %a is not a constant expression"
          Cil_printer.pp_exp array_len_exp

    let are_types_equal t1 t2 =
      Cil_datatype.Typ.equal (unrollType t1) (unrollType t2)

    (* A sanity check function used to make sure that the type of a subobject
       after initialization is a correct refinement of its type before
       initialization. *)
    let check_if_type_change_is_correct ~at_toplevel t_before t_after : unit =
      (* If the types are equal, everything is surely OK. *)
      if not (are_types_equal t_before t_after) then
        (* The types are not equal. The only valid case of type refinement is if
           an unspecified-size array type was refined to a corresponding (i.e.
           with the same base type) known-size array type at the initializer
           top-level (i.e. not in an array cell nor a composite field). *)
        begin
          let is_type_refinement_correct =
            (match unrollType t_before, unrollType t_after with
             | TArray(base_type_1, None  , _, _),
               TArray(base_type_2, Some _, _, _) ->
               Cil_datatype.Typ.equal
                 (unrollType base_type_1)
                 (unrollType base_type_2)
             | _ -> false)
          in
          if not is_type_refinement_correct then
            Kernel.failure ~current:true
              "initializer has changed the type of the initialized object in \
               an invalid way, the type before initialization was %a, the type \
               after initialization is %a"
              Cil_printer.pp_typ t_before
              Cil_printer.pp_typ t_after;
          if not at_toplevel then
            Kernel.failure ~current:true
              "initializer has changed the type of the initialized subobject \
               which is not at the top-level of the initialization, the type \
               before initialization was %a, the type after initialization is \
               %a"
              Cil_printer.pp_typ t_before
              Cil_printer.pp_typ t_after;
        end


    (* A sanity check function used to make sure that two initializers for the
       same subobject have the same type.

       This situation occurs when we have more than one initializer concerning
       the same subobject, which is the case when at some point there is a
       single intializer for a subobject which is refined afterwards.
       Basically the only case when types here could be different would be if
       we had an unspecified-size array and two initializers with a different
       number of elements. However, as we cannot have incomplete types inside
       structures nor arrays, this would be possible only on the top-level.
       This function is never called at the top-level though, as it is
       impossible to specify more than one initializer for the host object. *)
    let check_if_init_types_are_equal t1 t2 : unit =
      if not (are_types_equal t1 t2) then
        Kernel.fatal ~current:true
          "two initializers for the same object have different types, %a vs %a"
          Cil_printer.pp_typ t1
          Cil_printer.pp_typ t2

    type accumulate_side_effects_t =
      Chunk.t ->
      (lval list * Chunk.t * exp * typ) ->
      (Chunk.t * exp)

    (** Collect a CIL initializer for an object of a certain type given the
        corresponding preinitializer.
        @param accumulate_f     This function takes as arguments the existing
                                side effects accumulator (which is a chunk) and
                                all the necessary information about the new side
                                effects that occur when evaluating the given
                                subobject's initialization expression, and it
                                returns an updated side effects accumulator and
                                an updated expression that should be assigned to
                                the concerned subobject.
        @param preinit          The [preinitializer] to translate.
        @param typ              The type of the object that is being initialized
                                with this [preinitializer].
        @param is_fam           Is the current initialized object a flexible
                                array member? This is [false] by default.
        @param at_toplevel      Is this the top-level initializer? This
                                information is useful for debugging and error
                                reporting.
        @param override         Are we in the {i override mode} (i.e. the
                                encountered compound initializers should be
                                treated as complete initializers: every
                                subobject not initialized by them explicitly
                                should be initialized implicitly) or in the
                                {i refining mode} (i.e. the encountered compound
                                initializers should be treated as sets of
                                individual initializers refining prior
                                initialization and thus no implicit
                                initialization should occur).
        @param side_effects_acc Accumulator of the side effects produced by the
                                encountered initialization expressions.
        @return The function returns a triple
                [(inits, final_typ, side_effects)]: {ul
                {- [inits] is a list of CIL initializers that (together, when
                   applied in the given order) will initialize the object as
                   described by preinit;}
                {- [final_typ] is the type of the object after initialization
                   (it may differ from the provided type [typ] only if the
                   provided type is an unspecified-size array and the final type
                   is a known-size array (with the same base type);}
                {- [side_effects] is a chunk that contains all the side effects
                   of the initializers and should be executed before the
                   initialization.} }
                NOTE: [inits] is a list, as it is possible for a single object
                to be first initialized integrally by a single initializer and
                then some of its subobjects' initialization can be overriden by
                other initializers, which must be translated into several CIL
                initializers, e.g.: {[
                  struct sub { int x, y; };
                    struct sub s0 = { 1, 2 };
                      struct { struct sub s; } s1 = { .s = s0, .s.x = 4 };
                  ]} *)
    let rec init_of_preinit
        ~(accumulate_f : accumulate_side_effects_t)
        (preinit : preinit)
        (typ : typ)
        ?(at_toplevel=false)
        ?(is_fam=false)
        ~(override : bool)
        (side_effects_acc : Chunk.t)
      : init list * typ * Chunk.t =

      let loc = CurrentLoc.get () in

      (* Invariant:
         1) if it is a flexible array member, then its type is an
            unspecified-size array type,
         2) if the type is an unknown size array type then either:
            a) it is a flexible array member or
            b) we are at the top level. *)
      assert (not is_fam || Cil.isUnspecifiedSizeArrayType typ);
      if Cil.isUnspecifiedSizeArrayType typ && not is_fam && not at_toplevel
      then
        Kernel.fatal ~current:true
          "init_of_preinit: unspecified size array encountered not at the \
           toplevel nor in the flexible array member case";
      (* NOTE: Condition 1) is assured locally, so it is can be an internal
         assertion; condition 2) may actually occur if an invalid type was
         passed here, so it's a fatal error instead. *)

      Kernel.debug ~dkey ~current:true
        "init_of_preinit called: type = %a, preinit = %a"
        Cil_printer.pp_typ (unrollType typ)
        pretty preinit;

      match unrollType typ, preinit with

      (* Anything <- NoPreinit *)
      (* This is a standard implicit initialization. *)
      | _, NoPreinit when override && not is_fam ->
        Kernel.debug ~dkey
          "IMPLICIT INITIALIZATION: zero-initializing object of type %a"
          Cil_printer.pp_typ typ;
        [makeZeroInit ~loc typ], typ, side_effects_acc

      (* Anything <- NoPreinit *)
      (* We recognise a flexible array member when processing a structure
         intitialization on the level before, so we can ignore it here. *)
      | _, NoPreinit when is_fam ->
        Kernel.debug ~dkey ~current:true
          "Implicit initialization of a flexible array member is ignored";
        [], typ, side_effects_acc

      (* Anything <- NoPreinit *)
      (* This would be an implicit initialization, but we are not overriding,
         just refining a previous initialization of a given subobject, so it
         should be ignored, as this subobject was already initialized
         explicitly. *)
      | _, NoPreinit when not override ->
        Kernel.debug ~dkey ~current:true
          "Implicit initialization of an object of type %a ignored, we are \
           refining a previous complete initialization"
          Cil_printer.pp_typ typ;
        [], typ, side_effects_acc

      (* Anything <- SinglePreinit *)
      (* This is a standard explicit single initialization. *)
      | _ , SinglePreinit init_exp ->
        Kernel.debug ~dkey
          "EXPLICIT INITIALIZATION: Initializing object of type %a to %a"
          Cil_printer.pp_typ typ
          Cil_printer.pp_exp init_exp.exp;
        let side_effects_acc, exp =
          accumulate_f side_effects_acc
            (init_exp.reads, init_exp.se, init_exp.exp, init_exp.typ)
        in
        [SingleInit exp], typ, side_effects_acc

      (* Array <- CompoundPreinit *)
      (* This is a standard array initialization. *)
      | TArray (array_base_type, array_len_opt, _, attributes),
        CompoundPreinit (preinits, complete) ->

        (* Override either if we were already in the overriding mode or if this
           compound initializer is complete (i.e. everything which is not
           explicitly initialized by it should be initialized implicitly). *)
        let override = override || complete in

        let last_initialized_index =
          try Designator.as_array_index (get_max_index preinits)
          with Not_found -> -1
        in

        Kernel.debug ~dkey
          "Initializing an array object of type %a"
          Cil_printer.pp_typ typ;

        let array_len, initializer_len_used =
          match array_len_opt with
          | Some length ->
            (* Complete array type case. Use array's declared length. *)
            int_of_complete_array_length length, false
          | None ->
            (* Incomplete array type case. The length of the array is determined
               by the initializer. *)
            (* Following C11:6.7.9p22 :
               "If an array of unknown size is initialized, its size is
               determined by the largest indexed element with an explicit
               initializer. The array type is completed at the end of its
               initializer list." *)
            assert (is_fam || at_toplevel); (* This was checked before. *)
            (* TODO: Should we move the FAM treatment here? *)
            last_initialized_index + 1, true
        in

        (* Sanity check. *)
        if last_initialized_index >= array_len then
          Kernel.failure ~current:true
            "init_of_preinit: too many initializers for an array or a \
             designator out of bounds (last initialized index is %d while \
             array's lenght is %d)"
            (last_initialized_index + 1) array_len;

        let new_type =
          (* TODO: This would be better with some reorganization, but I'm not
             sure abot the whole typ'' assignment later, so I leave it be. *)
          (* Detect flexible array member initialization. *)
          assert (array_len >= 0); (* Checked just before. *)
          if is_fam && array_len > 0 then begin
            (* This is a flexible array member with a non-empty initializer:
               we have a problem. *)
            Kernel.debug ~dkey:category_initializer
              "Detected initialization of a flexible array member \
               (length %d)"
              array_len;
            Kernel.abort ~once:true ~current:true
              "static initialization of flexible array members is an \
               unsupported GNU extension"
          end else begin
            (* Here one of three options is possible:
               1) this is     a flexible array member AND array_len = 0;
               2) this is not a flexible array member AND array_len = 0;
               3) this is not a flexible array member AND array_len > 0.
               Cases 1) and 2) are only allowed if we support compiler
               extension concerning zero-sized arrays. Case 3) is a standard
               unspecified-size array refinement to a known-array size. *)
            if array_len = 0 && not (Cil.gccMode () || Cil.msvcMode ()) then
              Kernel.error ~once:true ~current:true
                "arrays of size zero not supported in C99@ \
                 (only allowed as compiler extensions)";
            let array_len_exp_opt =
              if is_fam
              (* The flexible array member's type remains
                 an unspecified-size array.
                 NOTE: As it is a compiler extension it is not entirely clear
                 if this is what should happen, but it seems pretty reasonable
                 not to change the involved structure's type from
                 unspecified-size array to zero-size array. *)
              then None
              (* In all normal cases, the array's size becomes specified and
                 depends on the last explicitly initialized cell's index. *)
              else Some (integer ~loc array_len)
            in
            TArray (array_base_type, array_len_exp_opt,
                    empty_size_cache (), attributes)
          end
        in

        (* Treat recursively the array's cells. *)
        let (init, side_effects_acc) =
          let collect_one_cell_inits
              (designator : Designator.t) (preinit : preinit)
              (inits_acc, side_effects_acc) =
            match preinit, Designator.as_array_index designator with
            | NoPreinit, _ ->
              (* There is no explicit initializer for the given array cell.
                 As an exception we don't initialize implicitly every cell of an
                 array here: this will be done later and more efficitently using
                 bzero. *)
              (* TODO: Can this actually happen, as we are folding on
                 initializers (not on array cells!), here? *)
              inits_acc, side_effects_acc
            | _ , index ->
              (* Recursively prepare the initializer for this array cell. *)
              let cell_inits, typ', side_effects_acc =
                init_of_preinit ~accumulate_f ~override
                  preinit array_base_type side_effects_acc
              in
              (* The type should not change as we cannot have unknown size
                 arrays inside an array. *)
              check_if_type_change_is_correct ~at_toplevel array_base_type typ';
              (* Add a designator to each of the cell's initializers. *)
              let cell_inits' =
                let cell_offset = Index (integer ~loc index, NoOffset) in
                List.map (fun cell_init -> cell_offset, cell_init) cell_inits
              in
              (* Accumulate with the initializers for previously treated
                 array cells. *)
              let inits_acc = (List.rev cell_inits') @ inits_acc in
              inits_acc, side_effects_acc
          in
          let init, side_effects_acc =
            DesignatorMap.fold
              collect_one_cell_inits
              preinits
              ([], side_effects_acc)
          in
          (List.rev init, side_effects_acc)
        in

        (* TODO: WTF does this comment mean? *)
        (* If the sizes of the initializers have not been used anywhere,
           we can fold back an eventual typedef. Otherwise, push the
           attributes to the elements of the array. *)
        let typ' =
          if initializer_len_used
          then new_type
          else typ
        in

        [CompoundInit (new_type, init)], typ', side_effects_acc

      (* Structure <- CompoundPreinit *)
      (* This is a standard structure initialization. *)
      | TComp (comp, _, _),
        CompoundPreinit (preinits, complete) when comp.cstruct ->

        (* Override either if we were already in the overriding mode or if this
           compound initializer is complete (i.e. everything which is not
           explicitly initialized by it should be initialized implicitly). *)
        let override = override || complete in

        Kernel.debug ~dkey
          "init_of_preinit: Initializing a structure of type %a"
          Cil_printer.pp_typ typ;

        let (inits, side_effects_acc) =
          List.fold_left
            (fun (init_acc, side_effects_acc) fi ->
               (* Is the field unnamed? *)
               let is_unnamed = (fi.fname = missingFieldName) in
               (* Is the field a flexible array member? *)
               let is_fam = Cil.isUnspecifiedSizeArrayType fi.ftype in
               if is_fam then begin
                 (* Sanity check: there is no way that a structure field with an
                    incomplete array type appears here if it is not FAM, the
                    structure's type would be recognised much before as invalid.
                    We verify it just for the sake of defensive programming. *)
                 assert (comp.cfields <> []); (* Impossible as we entered the
                                                 folding function. *)
                 let last_fi = (Extlib.last comp.cfields) in
                 if fi.fname <> last_fi.fname then
                   Kernel.fatal ~current:true
                     "init_of_preinit: flexible array member not at the end of \
                      a structure"
               end;
               (* Unnamed fields are not initialized. *)
               if is_unnamed
               then init_acc, side_effects_acc
               else
                 (* Recursively prepare the initializer for this structure
                    field. *)
                 let (field_inits, typ', side_effects_acc)
                   : init list * typ * Chunk.t =
                   let field_preinit =
                     let designator = Designator.of_fieldinfo fi in
                     try DesignatorMap.find designator preinits
                     with Not_found -> NoPreinit
                   in
                   let field_type = fi.ftype in
                   init_of_preinit
                     ~accumulate_f ~override ~is_fam
                     field_preinit field_type side_effects_acc
                 in
                 (* The type should not change. The only way to have an unknown
                    size arrays inside a structure's field is the flexible
                    array member (which can only be initialized with and empty
                    initializer) and in this case the type stays the same. *)
                 check_if_type_change_is_correct ~at_toplevel fi.ftype typ';

                 (* Add a designator to each of the field's initializers. *)
                 let field_inits' =
                   let offset = Field(fi, NoOffset) in
                   List.map (fun init -> offset, init) field_inits
                 in

                 (* Accumulate with the initializers for previously treated
                    structure cells. *)
                 let init_acc' = (List.rev field_inits') @ init_acc in

                 init_acc', side_effects_acc)
            ([], side_effects_acc)
            comp.cfields
        in
        let inits = List.rev inits in

        [CompoundInit (typ, inits)], typ, side_effects_acc

      (* Union <- CompoundPreinit *)
      (* This is a standard union initialization. *)
      | TComp (comp, _, _),
        CompoundPreinit (preinits, _overriding)
        when not comp.cstruct ->

        Kernel.debug ~dkey
          "Initializing a union of type %a"
          Cil_printer.pp_typ typ;

        let inits, side_effects_acc =
          begin
            let number_of_initializers = DesignatorMap.cardinal preinits in
            match number_of_initializers with
            (* One initializer: correct. *)
            | 1 ->
              let designator, preinit = DesignatorMap.choose preinits in
              let field_name = Designator.as_field_name designator in
              let fi =
                try getCompField comp field_name
                with Not_found ->
                  Kernel.fatal ~current:true
                    "init_of_preinit: Initialized field does not exist"
              in

              begin
                let is_first_field =
                  match comp.cfields with
                  | fst_field :: _ -> fst_field.fname = field_name
                  | [] -> assert false (* At least the initialized field exists,
                                          so the list of the union's fields
                                          cannot be empty. *)
                in
                if Cil.msvcMode () && not is_first_field then
                  Kernel.warning ~current:true
                    "On MSVC we can initialize only the first field of a union"
              end;

              (* Recursively prepare the initializer for the union's field. *)
              let field_inits, typ', side_effects_acc =
                let field_type = fi.ftype in
                init_of_preinit ~accumulate_f ~override
                  preinit field_type side_effects_acc
              in

              (* The type should not change. *)
              check_if_type_change_is_correct ~at_toplevel fi.ftype typ';

              (* Add a designator to each of the field's initializers. *)
              let field_inits' =
                let offset = Field(fi, NoOffset) in
                List.map (fun init -> offset, init) field_inits
              in

              field_inits', side_effects_acc

            (* No initializers: error. *)
            | 0 ->
              Kernel.fatal ~current:true
                "init_of_preinit: An empty initializer for a union"

            (* More than one initializer. *)
            | _ when number_of_initializers > 1 ->
              (* TODO: abort or fatal? *)
              Kernel.abort ~current:true
                "only one field can be initialized in a union"

            | _ ->
              (* The only case left are negative numbers. Cardinal of a map
                 cannot be negative. *)
              assert false
          end
        in

        [CompoundInit (typ, inits)], typ, side_effects_acc

      (* Anything <- SingleThenCompoundPreinit *)
      (* This is a single initialization of an object followed by a series of
         initializers refining the initialization of its subobjects. *)
      | _, SingleThenCompoundPreinit (init_exp, refining_preinits) ->

        Kernel.debug ~dkey
          "Initializing and then refining object of type %a"
          Cil_printer.pp_typ typ;

        (* Recursively process the single initializer for the object. *)
        let single_inits, single_init_typ, side_effects_acc =
          let single_preinit = SinglePreinit init_exp in
          init_of_preinit ~accumulate_f ~override ~is_fam
            single_preinit typ side_effects_acc
        in

        (* Recursively process the refining compound initializer for the
           object. *)
        let refining_inits, refining_init_typ, side_effects_acc =
          let refining_preinit = CompoundPreinit (refining_preinits, false) in
          let override = false (* If we were in "overriding mode" we pass to
                                  the "refining mode". *)
          in
          init_of_preinit ~accumulate_f ~override ~is_fam
            refining_preinit typ side_effects_acc
        in

        (* The type should not change. *)
        check_if_init_types_are_equal single_init_typ refining_init_typ;

        single_inits @ refining_inits, single_init_typ, side_effects_acc

      (* Any other thing: impossible! *)
      | _ ->
        Kernel.fatal ~current:true
          "init_of_preinit: type = %a, preinit = %a"
          Cil_printer.pp_typ (unrollType typ) pretty preinit

    (* The top-level call to init_of_preinit. *)
    let to_init ~accumulate_f preinit typ =
      let at_toplevel = true in
      let inits, typ', side_effects_acc =
        let side_effects_acc = Chunk.(unspecified_chunk empty) in
        let override = true (* We start in the "overriding mode". *) in
        init_of_preinit ~accumulate_f ~at_toplevel ~override
          preinit typ side_effects_acc
      in
      (* Check if the type was unchanged or changed correctly. *)
      check_if_type_change_is_correct ~at_toplevel typ typ';
      match inits with
      | [init] -> init, typ', side_effects_acc
      | _ -> Kernel.fatal ~current:true
               "to_init: got both a single initializer for a whole object and \
                some initializers overriding its subobjects on the initializer \
                top-level"

  end

end


module AssignInit = struct

  (* USED ONLY IN:
     + assign_init
  *)

  module BlockChunk = Cabs2cil_BlockChunk
  let (+++) = BlockChunk.(+++)
  let (@@) = BlockChunk.(@@)

  let ensures_init varinfo offset init_exp =
    let cast = false in
    let init_exp_term = Logic_utils.expr_to_term ~cast init_exp in
    let loc = init_exp_term.term_loc in
    let lval_term =
      let base =
        let logic_var = Cil.cvar_to_lvar varinfo in
        let term_offset = Logic_utils.offset_to_term_offset ~cast offset in
        (TVar logic_var, term_offset)
      in
      let term_node = TLval base in
      let logic_type = Cil.typeOfTermLval base in
      Logic_const.term ~loc term_node logic_type
    in
    Logic_const.prel ~loc (Req, lval_term, init_exp_term)

  let zero_enum ~loc enuminfo =
    try
      let zero_enumitem =
        List.find
          (fun enumitem -> Cil.isZero enumitem.eival)
          enuminfo.eitems
      in
      Cil.new_exp ~loc (Const (CEnum zero_enumitem))
    with Not_found ->
      Cil.kinteger ~loc enuminfo.ekind 0

  (* memset to 0 an entire array. *)
  let set_to_zero ~ghost vi off typ =
    let loc = vi.vdecl in
    let bzero =
      try
        Cil.Frama_c_builtins.find "Frama_C_bzero"
      with Not_found ->
        Kernel.fatal
          "Incorrect Cil initialization: cannot find Frama_C_bzero builtin"
    in
    let zone =
      Cil.new_exp ~loc
        (CastE(TPtr(TInt (IUChar,[]),[]),
               Cil.new_exp ~loc (StartOf(Var vi,off))))
    in
    let size =
      Cil.new_exp ~loc
        (CastE (TInt(IULong,[]),
                Cil.new_exp ~loc (SizeOf typ)))
    in
    Cil.mkStmt ~ghost
      (Instr
         (Call
            (None,Cil.evar ~loc bzero,
             [zone; size], loc)))


  (* Initialize the first cell of an array, and call Frama_C_copy_block to
     propagate this initialization to the rest of the array.
     Array is located at vi.off, of length len, and cells are of type base_type.
  *)
  let rec zero_init ~ghost vi off len base_typ =
    let loc = vi.vdecl in
    let copy =
      try
        Cil.Frama_c_builtins.find "Frama_C_copy_block"
      with Not_found ->
        Kernel.fatal
          "Incorrect Cil initialization: cannot find Frama_C_copy_block builtin"
    in
    let zone =
      Cil.new_exp ~loc
        (CastE(TPtr(TInt (IUChar,[]),[]),
               Cil.new_exp ~loc (StartOf(Var vi,off))))
    in
    let size =
      Cil.new_exp ~loc
        (CastE (TInt(IULong,[]),
                Cil.new_exp ~loc (SizeOf base_typ)))
    in
    let len = Cil.kinteger ~loc IULong len in
    let off = Cil.addOffset (Index (Cil.integer ~loc 0, NoOffset)) off in
    let zero_init = zero_init_cell ~ghost vi off base_typ in
    zero_init +++
    (Cil.mkStmt ~ghost
       (Instr
          (Call
             (None, Cil.evar ~loc copy, [zone; size; len], loc))),
     Chunk.make_effects ~modified:[] ~writes:[] ~reads:[(Var vi, off)])

  and zero_init_cell ~ghost vi off typ =
    let loc = vi.vdecl in
    match Cil.unrollType typ with
    | TVoid _ -> BlockChunk.empty
    | TInt(ikind,_) ->
      let lv = (Var vi,off) in
      Chunk.of_stmt
        (Cil.mkStmt ~ghost (Instr (Set (lv, (Cil.kinteger ~loc ikind 0),loc))))

    | TFloat (fkind,_) ->
      let lv = (Var vi,off) in
      Chunk.of_stmt
        (Cil.mkStmt ~ghost (Instr (Set (lv, (Cil.kfloat ~loc fkind 0.),loc))))

    | TPtr _ ->
      let lv = (Var vi,off) in
      let exp = Cil.new_exp ~loc (CastE(typ,Cil.zero ~loc)) in
      Chunk.of_stmt (Cil.mkStmt ~ghost (Instr (Set (lv, exp,loc))))

    | TArray(_,None,_,_) ->
      Kernel.fatal ~source:(fst loc)
        "Trying to zero-initialize variable with incomplete type"

    | TArray(typ,Some e,_,_) ->
      let len =
        match Cil.constFoldToInt e with
        | Some i -> Integer.to_int i
        | _ ->
          Kernel.fatal ~source:(fst loc)
            "Trying to zero-initialize variable with incomplete type"
      in
      zero_init ~ghost vi off len typ

    | TFun _ -> Kernel.fatal "Trying to zero-initialize a function"

    | TNamed _ -> assert false (* guarded by unrollType *)

    | TComp (ci,_,_) ->
      let treat_one_field acc fi =
        let off = Cil.addOffset (Field (fi,NoOffset)) off in
        acc @@
        (zero_init_cell ~ghost vi off fi.ftype, ghost)
      in
      if ci.cstruct then
        List.fold_left treat_one_field BlockChunk.empty ci.cfields
      else begin
        (* Standard says that zero initializing an union is done by setting
           its first field to 0
        *)
        match ci.cfields with
        | [] -> Kernel.fatal "Union type without fields"
        | fst :: _ -> treat_one_field BlockChunk.empty fst
      end

    | TEnum (ei,_) ->
      let lv = (Var vi,off) in
      let zero = zero_enum ~loc ei in
      Chunk.of_stmt (mkStmt ~ghost (Instr (Set (lv,zero,loc))))

    | TBuiltin_va_list _ ->
      Kernel.fatal "Found builtin varargs in zero-initialization"

  let get_implicit_indexes loc vi len known_idx =
    let split_itv i itvs =
      let i = Integer.to_int i in
      let rec aux processed remaining =
        match remaining with
        | [] ->
          Kernel.warning ~current:true
            "Unexpected index in array initialization (bad computed length?)";
          List.rev processed
        | (low,high) as itv :: tl ->
          if i < low then begin
            (* should have been captured by earlier interval*)
            Kernel.warning ~current:true
              "Unexpected index in array initialization \
               (double initialization?)";
            List.rev_append processed remaining
          end
          else if i > high then aux (itv::processed) tl
          else (* split the interval *)
          if i = low then
            if high = low then (* interval is a singleton, just remove it*)
              List.rev_append processed tl
            else (* remove first elt of interval *)
              List.rev_append processed ((low+1,high)::tl)
          else if i = high then (* remove last elt of interval,
                                   which is not singleton *)
            List.rev_append processed ((low,high-1)::tl)
          else (* split interval in two, non empty intervals.  *)
            List.rev_append processed ((low,i-1)::(i+1,high)::tl)
      in
      aux [] itvs
    in
    let unknown_idx =
      Datatype.Integer.Set.fold split_itv known_idx [0,pred len]
    in
    let one_range acc (low,high) =
      Logic_const.pand ~loc
        (acc,Logic_const.pand ~loc
           (Logic_const.prel ~loc
              (Rle, Logic_const.tinteger ~loc low, Logic_const.tvar vi),
            Logic_const.prel ~loc
              (Rle, Logic_const.tvar vi, Logic_const.tinteger ~loc high)))
    in
    List.fold_left one_range Logic_const.ptrue unknown_idx

  let ensures_is_zero_offset loc term typ =
    let rec aux nb_idx term typ =
      let mk_term () =
        Logic_const.term ~loc (TLval term) (Cil.typeOfTermLval term)
      in
      match Cil.unrollType typ with
      | TVoid _ ->
        Kernel.warning "trying to zero-initialize a void value"; Logic_const.ptrue
      | TInt _ ->
        Logic_const.prel(Req,mk_term (),Logic_const.tinteger ~loc 0)
      | TFloat _ ->
        Logic_const.prel (Req,mk_term (),Logic_const.treal ~loc 0.)
      | TPtr _ ->
        Logic_const.prel (Req, mk_term (), Logic_const.term ~loc Tnull (Ctype typ))
      | TArray (t,e,_,_) ->
        let name = "__i" ^ string_of_int nb_idx in
        let vi = Cil_const.make_logic_var_quant name Linteger in
        let idx = Logic_const.tvar ~loc vi in
        let max =
          match e with
          | None -> Logic_const.ptrue
          | Some e ->
            Logic_const.prel ~loc
              (Rlt, idx, Logic_utils.expr_to_term ~cast:false e)
        in
        let pre =
          Logic_const.pand ~loc
            (Logic_const.prel ~loc (Rle, Logic_const.tinteger ~loc 0, idx),max)
        in
        let subterm =
          Logic_const.addTermOffsetLval (TIndex (idx,TNoOffset)) term
        in
        let cond = aux (nb_idx + 1) subterm t in
        Logic_const.pforall ~loc ([vi], Logic_const.pimplies ~loc (pre, cond))
      | TFun _ -> Kernel.fatal "Trying to zero-initialize a function"
      | TNamed _ -> assert false (* protected by unrollType *)
      | TComp (c,_,_) ->
        let treat_one_field acc fi =
          let subterm =
            Logic_const.addTermOffsetLval (TField (fi,TNoOffset)) term
          in
          let cond = aux nb_idx subterm fi.ftype in
          Logic_const.pand ~loc (acc,cond)
        in
        if c.cstruct then
          List.fold_left treat_one_field Logic_const.ptrue c.cfields
        else
          (match c.cfields with
           | [] -> Kernel.fatal "zero-initialize a union with no members"
           | f :: _ -> treat_one_field Logic_const.ptrue f)
      | TEnum (e,_) ->
        let zero = Logic_utils.expr_to_term ~cast:false (zero_enum ~loc e) in
        Logic_const.prel ~loc (Req,mk_term (),zero)
      | TBuiltin_va_list _ ->
        Kernel.fatal "Trying to zero-initialize a vararg list"
    in
    aux 0 term typ

  (* Make a contract for a block that performs partial initialization of a local,
     relying on bzero for implicit zero-initialization.
  *)
  let make_implicit_ensures vi off base_typ len known_idx =
    let loc = vi.vdecl in
    let i = Cil_const.make_logic_var_quant "__i" Linteger in
    let pre = get_implicit_indexes loc i len known_idx in
    let lv = Cil.cvar_to_lvar vi in
    let lo = Logic_utils.offset_to_term_offset ~cast:false off in
    let base = (TVar lv, lo) in
    let term =
      Logic_const.addTermOffsetLval (TIndex (Logic_const.tvar i, TNoOffset)) base
    in
    let res = ensures_is_zero_offset loc term base_typ in
    let cond = Logic_const.pimplies ~loc (pre, res) in
    Logic_const.pforall ~loc ([i],cond)

end
