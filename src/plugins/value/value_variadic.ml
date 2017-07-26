(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2015-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil
open Cil_datatype
open Cvalue
open Value_util
open Eval_exprs


(* == Debug printing settings and helpers. == *)

let dkey = Value_parameters.register_category "variadic"

let register_subcategory subcategory =
  Value_parameters.register_category ("variadic:" ^ subcategory)

(* In the helper functions. *)
let dkey_reduce_ptr_to_valid_values       = register_subcategory "reduce_ptr_to_valid_values"
let dkey_reduce_arg_ptr_to_valid_values   = register_subcategory "reduce_arg_ptr_to_valid_values"
let dkey_convert_arg_ptr_values_to_a_type = register_subcategory "convert_arg_ptr_values_to_a_type"

(* In the variadic macros. *)
let dkey_va_start = register_subcategory "va_start"
let dkey_va_copy  = register_subcategory "va_copy"
let dkey_va_arg   = register_subcategory "va_arg"
let dkey_va_end   = register_subcategory "va_end"

(* Helpers. *)
let d_kf_with_call_site fmt (kf, call_site) =
  let d_call_site fmt call_site =
    match call_site with
    | Kstmt stmt -> Format.fprintf fmt "[STATEMENT %a]" Stmt.pretty stmt
    | Kglobal    -> Format.pp_print_string fmt "[GLOBAL]"
  in
  Format.fprintf fmt
    "function %a at call site %a"
    Kernel_function.pretty kf
    d_call_site call_site


(* == Managing the va_args variables. == *)

(*
   Each variadic function has an associated va_args variable, which is used to
   store and then access the values of the variadic arguments passed to this
   function. More exactly: a separate memory location is created for storing the
   value of each argument and the va_args variable is bound to the array of
   pointers to the arguments.

   A new such variable is created and associated to a function when it's called
   (at the same time when its arguments are treated). It is accessed in the
   invocations of the va_start macro, in order to give an initial value to the
   arg_prt_ptr field of the concerned va_list object. Finally, it is removed at
   the end of the function call, after the array of pointers to the variadic
   arguments and all the variadic arguments themselves have been cleaned up.
*)
module Va_args : sig

  val has_function_va_args_varinfo : Kernel_function.t -> bool
  (** [has_va_args_varinfo kf] returns [true] if the function [kf] already has a
      va_args variable attributed and [false] otherwise. *)

  val add_va_args_varinfo_for_function : Kernel_function.t -> varinfo -> unit
  (** [add_va_args_varinfo_for_function kf varinfo] establishes the given
      [varinfo] as the va_args variable of the function [kf]. *)

  val get_va_args_varinfo_of_function : Kernel_function.t -> varinfo
  (** [get_va_args_varinfo_of_function kf] returns the [varinfo] corresponding
      to the va_args variable of a given funtion [kf]; if for some reason no
      va_args variable is associated with the function [kf], it aborts value
      analysis. *)

  val remove_va_args_varinfo_for_function : Kernel_function.t -> unit
  (** [remove_va_args_varinfo_for_function kf] removes the existing association
      of the va_args variable with the function [kf]. *)

  (* NOTE: Currently the implementation is a stack behaviour: we establish a
     new va_args variable for a given variadic function every time when this
     function is called (we push a new varinfo on a stack corresponding to the
     given function, covering the previous association if it exists) and we
     remove the association with this variable when the function call returns
     (i.e. we pop the varinfo, so if there was more than one pushed there,
     the previous one will be uncovered). This, of course, may make a difference
     only if the same function is called recursively; otherwise there is always
     at most one variable associated with each function at any given moment. *)

end = struct

  (* == The projectified hashtable: Kernel_function.t --> varinfo  == *)

  module KFHashtblInfo = struct
    let size = 100
    let dependencies = [Db.Value.self]
    let name = "Value.Variadic.Va_args"
  end

  module KFHashtbl =
    State_builder.Hashtbl
      (Kernel_function.Hashtbl) (* the key hashtable module *)
      (Varinfo)                 (* the value module *)
      (KFHashtblInfo)           (* the data structure parameters *)

  let () = Ast.add_monotonic_state KFHashtbl.self

  (* == Functions implementation == *)

  let has_function_va_args_varinfo = KFHashtbl.mem

  let add_va_args_varinfo_for_function kf varinfo = KFHashtbl.add kf varinfo

  let remove_va_args_varinfo_for_function kf = KFHashtbl.remove kf

  let get_va_args_varinfo_of_function kf =
    try
      KFHashtbl.find kf
    with Not_found ->
      Value_parameters.fatal ~current:true "get_va_args_varinfo_of_function: \
                                            va_args variable for function \"%a\" does not exist!%t"
        Kernel_function.pretty kf
        pp_callstack

end

(* == Managing the variadic arguments varinfos per function per call site. == *)

module Variadic_arguments : sig
  (** Store the varinfos corresponding to the variadic arguments passed to the
      given function at the given call site. *)
  val set : Kernel_function.t -> kinstr -> varinfo list -> unit

  (** Get the varinfos corresponding to variadic arguments for the given
      function at the given call site. *)
  val get : Kernel_function.t -> kinstr -> varinfo list option
end = struct

  (* == Preparing the data structure for storing varinfos == *)

  (* A new datatype: a pair consisting of a function and a call site.
     (This is the "H" parameter for our projectified Hashtbl.) *)
  module FunctionAndCallSite =
    Datatype.Pair_with_collections(Kernel_function)(Kinstr)
      (struct let module_name = "FunctionAndCallSite" end)

  (* A new datatype: a list of varinfos.
     (This is the "Data" parameter for our projectified Hashtbl.) *)
  module VarinfoList = Datatype.List(Varinfo)

  (* The ArgsHashtbl data structure parameters. *)
  module ArgsHashtblInfoWithSize = struct
    let name         = "Value.Variadic.Variadic_arguments"
    let size         = 50
    let dependencies = [Db.Value.self]
  end

  (* The variadic arguments hashtable: it associates pairs consisting of
     a function and a call site with lists of varinfos. *)
  module ArgsHashtbl =
    State_builder.Hashtbl
      (FunctionAndCallSite.Hashtbl) (* the key hashtable module *)
      (VarinfoList)                 (* the value module *)
      (ArgsHashtblInfoWithSize)     (* the data structure parameters *)

  let () = Ast.add_monotonic_state ArgsHashtbl.self

  (* == Functions implementation == *)

  let set kf call_site varinfos =
    try
      (* If any variables were previously stored for this function and call site
         then everything is fine as long as they are equal to the new ones which
         we are attempting to store. *)
      let previously_stored_varinfos = ArgsHashtbl.find (kf, call_site) in
      if not
          (try List.for_all2 Varinfo.equal previously_stored_varinfos varinfos
           with Invalid_argument msg when msg = "List.for_all2" -> false)
      then
        Value_parameters.fatal ~current:true
          "Variadic_arguments.set: variadic variables for %a were already set \
           differently (i.e. the new varinfos are not the same as the \
           previously stored ones)!"
          d_kf_with_call_site (kf, call_site)
    with Not_found -> ArgsHashtbl.add (kf, call_site) varinfos

  let get kf call_site =
    try Some (ArgsHashtbl.find (kf, call_site))
    with Not_found -> None

end


(* == Some helper functions. == *)


(* = Handling call depth. = *)

(* The call depth is used to determine if a given va_list object is
   deinitialized (using the va_end macro) in the same function where it is
   initialized (using the va_start or va_copy macro).  *)

let get_current_call_depth () : int = List.length (call_stack ())

let get_current_call_depth_v () : V.t =
  let call_depth_ival =
    let call_depth_int = get_current_call_depth () in
    Ival.inject_singleton (Abstract_interp.Int.of_int call_depth_int)
  in
  V.inject_ival call_depth_ival


(* = Handling initializedness. = *)

(* Two basic initializedness-related predicates on Cvalue.V_Or_Uninitialized.t values. *)

let is_initialized : V_Or_Uninitialized.t -> bool =
  V_Or_Uninitialized.is_initialized

let is_bottom (v_or_uninit : V_Or_Uninitialized.t) : bool =
  V.is_bottom (V_Or_Uninitialized.get_v v_or_uninit)

(* For clarity we introduce a separate predicate for each initializedness case. *)
let is_definitely_init   v_or_uninit =      is_initialized v_or_uninit

let is_possibly_init     v_or_uninit = not (is_bottom      v_or_uninit)

let is_definitely_uninit v_or_uninit = not (is_initialized v_or_uninit) &&
                                       (is_bottom      v_or_uninit)

let is_possibly_uninit   v_or_uninit = not (is_initialized v_or_uninit)



(* = Handling locations and pointers (in form of cvalues). = *)

(*
  These small helper functions are used around for handling cvalues representing
  pointers (i.e. such that the bases and offsets stored in a cvalue are
  representing memory addresses pointed by the pointer).
*)

(* [loc_of_ptr size ptr] gets the value of the pointer [ptr] as a location
   (where [size] should be the size of this location, i.e. the size of the
   value pointed). *)
let loc_of_ptr (size : Int_Base.t) (ptr : V.t) : (Locations.location) =
  let loc_bits = Locations.loc_bytes_to_loc_bits ptr in
  Locations.make_loc loc_bits size


(* [is_ptr_valid size ptr] checks if the pointer is entirely valid, i.e. if all
   the addresses pointed by the pointer are valid. *)
let is_ptr_valid (size : Int_Base.t) (ptr : V.t) : bool =
  let ptr_loc = loc_of_ptr size ptr in
  Locations.is_valid ~for_writing:false ptr_loc


(* [has_ptr_a_valid_part size ptr] checks if at least some addresses pointed
   by the pointer are valid. *)
let has_ptr_a_valid_part (size : Int_Base.t) (ptr : V.t) : bool =
  let ptr_loc = loc_of_ptr size ptr in
  not (Locations.(is_bottom_loc (valid_part ~for_writing:false ptr_loc)))


(* [get_v_or_uninit_under_ptr state size ptr] retrieves the value written under
   the given pointer in the given abstract state. *)
let get_v_or_uninit_under_ptr state (size : Int_Base.t) (ptr : V.t) : V_Or_Uninitialized.t =
  let ptr_loc = loc_of_ptr size ptr in
  let (_, ptr_v_or_uninit : bool * V_Or_Uninitialized.t) =
    Model.find_unspecified state ptr_loc
  in
  ptr_v_or_uninit

(* [get_v_under_ptr state size ptr] retrieves the *initialized* value written
   under the given pointer in the given abstract state. *)
let get_v_under_ptr state (size : Int_Base.t) (ptr : V.t) : V.t =
  let ptr_v_or_uninit = get_v_or_uninit_under_ptr state size ptr in
  V_Or_Uninitialized.get_v ptr_v_or_uninit



(* = Sizes. = *)

(*
   The size of a pointer to an argument. A pointer to an argument has the
   type "void *", because arguments can have different types and sizes.

   The value pointed by arg_ptr_ptr is a pointer to an argument. Also, of
   course, this is the size of each cell of the argument pointers array.
*)

let void_ptr_size () =
  let void_ptr_typ = Cil.voidPtrType in
  Bit_utils.sizeof void_ptr_typ

let arg_ptr_size     = void_ptr_size
let arg_ptr_ptr_size = void_ptr_size


(* == The va_list object. == *)

(*
  Each va_list variable has an associated va_list object structure. More
  exactly: a va_list variable contains a pointer to such a structure.

  Every va_list variable's life-cycle is managed on the block level. All the
  variables local to a block are initialized (i.e. the underlying structure is
  created and the variable is set to point to this structure) when the block is
  opened and then deinitialized (i.e. the underlying structure is removed and
  the variable itself is set to the uninitialized value) when the block is
  closed.
*)

module Va_list_object_struct : sig

  (* The abstract type. *)
  type t

  (* - The fields of the va_list object structure. - *)
  type struct_field =
    | F_arg_ptr_ptr     (* arg_ptr_ptr:     The pointer to the pointer to the next argument. *)
    | F_init_call_depth (* init_call_depth: The call depth on which this object has been initialized. *)


  (* - Building and destroying the structure. - *)
  val create        : string -> Model.t -> t * Model.t
  val remove        : t      -> Model.t ->     Model.t


  (* - Whole structure operations. - *)

  (* Set the value of all the object's fields to UNINITIALIZED. *)
  val deinitialize : with_alarms:CilE.warn_mode -> t -> Model.t        -> Model.t

  (* Set the object's arg_ptr_ptr field to the provided value and initialize the
     object's init_call_depth to the current call_depth. *)
  val initialize   : with_alarms:CilE.warn_mode -> t -> Model.t -> V.t -> Model.t

  (* Copy the object's arg_ptr_ptr's field's value from the given source va_list
     object and initialize the object's init_call_depth to the current
     call_depth. *)
  val copy         : with_alarms:CilE.warn_mode -> t -> Model.t -> t   -> Model.t


  (* - Transformations. - *)
  val get_loc        : t -> Model.t -> Locations.location
  val of_ptr         : V.t                -> Model.t -> t
  val of_va_list_loc : Locations.location -> Model.t -> t


  (* - Operations on fields. - *)

  (* Get the location corresponding to a given va_list object's field. *)
  val field_get_loc          : struct_field -> Model.t -> t -> Locations.location

  (* Get the value of a given va_list object's field. *)
  val field_find_unspecified : struct_field -> Model.t -> t -> V_Or_Uninitialized.t
  val field_find             : struct_field -> Model.t -> t -> V.t

  (* Set the value of a given va_list object's field. *)
  val field_add_binding      : struct_field -> with_alarms:CilE.warn_mode -> Model.t -> t -> V.t -> Model.t

end = struct

  type t = Locations.location

  type struct_field =
    | F_arg_ptr_ptr
    | F_init_call_depth

  let name_of_field struct_field =
    match struct_field with
    | F_arg_ptr_ptr      -> "arg_ptr_ptr"
    | F_init_call_depth  -> "init_call_depth"

  let mk_single_attr attribute_name =
    let attribute = Attr (attribute_name, []) in
    Cil.addAttribute attribute []

  let arg_ptr_ptr_typ =
    let attributes = mk_single_attr "variadic_arg_ptr_ptr" in
    Cil.setTypeAttrs Cil.voidPtrType attributes

  let integer_counter_typ =
    let attributes = mk_single_attr "integer_counter" in
    Cil.setTypeAttrs Cil.intType attributes

  let obj_typ () =
    let compinfo =
      let is_a_struct = true in
      let struct_type_name = "va_list_obj" in
      let mkfspec =
        let loc = Cil.builtinLoc in (* Cil.builtinLoc is the location of
                                       the prototypes of builtin functions. *)
        (fun _ ->
           [
             (*field's name       field's type,        key?, type's attributes,                    loc *)
             "arg_ptr_ptr",     arg_ptr_ptr_typ,     None, mk_single_attr "va_list_arg_ptr_ptr", loc;
             "init_call_depth", integer_counter_typ, None, mk_single_attr "va_list_call_depth",  loc
           ]
        ) in
      let compinfo_attributes =
        mk_single_attr "va_list_obj_structure_compinfo"
      in
      Cil.mkCompInfo
        is_a_struct         (* It is a structure. *)
        struct_type_name    (* The name of the structure type. *)
        mkfspec             (* Description of the structure's fields. *)
        compinfo_attributes (* Attributes of the type of the structure. *)
    in
    let bitsSizeofTypCache = Cil.empty_size_cache () in
    let tcomp_attributes = mk_single_attr "va_list_obj" in
    TComp (compinfo, bitsSizeofTypCache, tcomp_attributes)

  let obj_size () = Bit_utils.sizeof (obj_typ ())

  let is_va_list_obj_loc va_list_obj_loc _state =
    (*
       Check:
       1. all bases in the location should be of the right type,
       2. all offsets should be equal zero.
    *)
    let va_list_obj_loc_bytes =
      Locations.loc_to_loc_without_size va_list_obj_loc
    in
    let () =
      Locations.Location_Bytes.fold_i (fun base offset _acc ->
          (* Check the offset. *)
          begin
            if(not (Ival.is_zero offset)) then
              Value_parameters.fatal ~current:true "invalid va_list location: \
                                                    offset is not a singleton zero!%t" pp_callstack
          end;
          (* Check the type of the base. *)
          begin
            let typ_option = Base.typeof base in
            match typ_option with
            | None ->
              Value_parameters.fatal ~current:true "invalid va_list location: \
                                                    base with no type!%t" pp_callstack
            | Some typ ->
              if(not (Cabs2cil.compatibleTypesp (obj_typ ()) typ)) then
                Value_parameters.fatal ~current:true
                  "invalid va_list location: base with wrong type! \
                   type = %a; expected type = %a%t"
                  Cil_printer.pp_typ typ
                  Cil_printer.pp_typ (obj_typ ())
                  pp_callstack
          end;
          ()
        ) va_list_obj_loc_bytes ()
    in
    true

  let of_loc va_list_obj_loc state =
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    va_list_obj_loc

  let of_ptr va_list_ptr state =
    let va_list_object_loc = loc_of_ptr (obj_size ()) va_list_ptr in
    of_loc va_list_object_loc state

  let of_va_list_loc va_list_loc state =
    let (_, va_list_value_or_uninit : bool * V_Or_Uninitialized.t) =
      Model.find_unspecified state va_list_loc
    in

    (* This structure has been created by us thus it must be coherent. *)
    begin
      if not (is_definitely_init va_list_value_or_uninit) ||
         (is_bottom va_list_value_or_uninit) then
        Value_parameters.abort ~current:true ~dkey
          "va_list object's structure incoherent! either it is a bug or you \
           are using variadic features in a way that is not handled correctly \
           yet (e.g. va_list objects inside arrays or structs)"
    end;
    assert (is_definitely_init va_list_value_or_uninit);
    assert (not (is_bottom va_list_value_or_uninit));

    (* Apparently this assertion does not necessarily hold: it may happen then
       a va_list is retuned conditionally, then the return value is imprecise. *)
    (* assert (V_Or_Uninitialized.cardinal_zero_or_one va_list_value_or_uninit); *)

    let va_list_value = V_Or_Uninitialized.get_v va_list_value_or_uninit in
    of_ptr va_list_value state

  let get_loc va_list_obj_loc state =
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    va_list_obj_loc

  let get_field_offset field =
    match obj_typ () with
    | TComp (compinfo, _bitsSizeofTypCache, _attributes) ->
      let fieldinfo = getCompField compinfo (name_of_field field) in
      Field(fieldinfo, NoOffset)
    | _ -> assert false (* We've constructed the type ourselves. *)

  let field_get_loc field state va_list_obj_loc =
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    let location_offset, location_size =
      let offset_bits, size_bits =
        let typ    = obj_typ () in
        let offset = get_field_offset field in
        bitsOffset typ offset
      in
      let offset_bytes = offset_bits / 8 in
      Ival.inject_singleton (Abstract_interp.Int.of_int offset_bytes),
      Int_Base.inject       (Abstract_interp.Int.of_int size_bits)
    in
    let field_loc_bits =
      let field_loc_bytes =
        let va_list_obj_loc_bytes =
          Locations.loc_to_loc_without_size va_list_obj_loc
        in
        Locations.Location_Bytes.fold_i (fun base offset loc_bytes_acc ->
            assert (Ival.is_zero offset); (* Checked just before in is_va_list_obj_loc, so it must hold. *)
            Locations.Location_Bytes.add base location_offset loc_bytes_acc
          ) va_list_obj_loc_bytes Locations.Location_Bytes.bottom
      in
      Locations.loc_bytes_to_loc_bits field_loc_bytes
    in
    Locations.make_loc field_loc_bits location_size

  let field_find_unspecified field state va_list_obj_loc =
    (* return va_list_obj.field *)
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    let field_loc = field_get_loc field state va_list_obj_loc in
    let (_, v_or_uninit : bool * V_Or_Uninitialized.t) =
      Model.find_unspecified state field_loc
    in
    v_or_uninit

  let field_find field state va_list_obj_loc =
    (* return va_list_obj.field *)
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    let field_loc = field_get_loc field state va_list_obj_loc in
    let (_, v : bool * V.t) = Model.find state field_loc in
    v

  let field_add_binding field ~with_alarms state va_list_obj_loc v =
    (* va_list_obj.field := v *)
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    let field_loc = field_get_loc field state va_list_obj_loc in
    let exact = Locations.(Location_Bits.cardinal_zero_or_one field_loc.loc) in
    Eval_op.add_binding ~with_alarms ~exact state field_loc v

  let field_add_binding_unspecified field ~with_alarms state va_list_obj_loc v_or_uninit =
    (* va_list_obj.field := v_or_uninit *)
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    let field_loc = field_get_loc field state va_list_obj_loc in
    let exact = Locations.(Location_Bits.cardinal_zero_or_one field_loc.loc) in
    Eval_op.add_binding_unspecified ~with_alarms ~exact state field_loc v_or_uninit

  let deinitialize ~with_alarms va_list_obj_loc state : Model.t =
    (* va_list_obj.arg_ptr_ptr      := UNINITIALIZED *)
    (* va_list_obj.init_call_depth  := UNINITIALIZED *)
    let uninitialized = V_Or_Uninitialized.uninitialized in
    let field_value_pairs =
      [
        (F_arg_ptr_ptr,      uninitialized);
        (F_init_call_depth,  uninitialized)
      ]
    in
    let bind_field_to_v_or_uninit state (field, v_or_uninit) =
      field_add_binding_unspecified field ~with_alarms state
        va_list_obj_loc v_or_uninit
    in
    List.fold_left bind_field_to_v_or_uninit state field_value_pairs

  let initialize ~with_alarms va_list_obj_loc state arg_ptr_ptr_v =
    (* va_list_obj.arg_ptr_ptr      := arg_ptr_ptr_v *)
    (* va_list_obj.init_call_depth  := [current call dept] *)
    let call_depth_v : V.t = get_current_call_depth_v ()
    in
    let field_value_pairs =
      [
        (F_arg_ptr_ptr,      arg_ptr_ptr_v);
        (F_init_call_depth,  call_depth_v)
      ]
    in
    let bind_field_to_value state (field, v) =
      field_add_binding field ~with_alarms state va_list_obj_loc v
    in
    List.fold_left bind_field_to_value state field_value_pairs

  let copy ~with_alarms va_list_dest_obj_loc state va_list_src_obj_loc =
    (* dest_va_list_obj.arg_ptr_ptr      := src_va_list_obj.arg_ptr_ptr *)
    (* dest_va_list_obj.init_call_depth  := [current call dept] *)
    let src_field_find_unspecified field =
      field_find_unspecified field state va_list_src_obj_loc
    in
    let call_depth_v : V.t = get_current_call_depth_v ()
    in
    let field_value_pairs =
      [
        (F_arg_ptr_ptr,      src_field_find_unspecified F_arg_ptr_ptr);
        (F_init_call_depth,  V_Or_Uninitialized.initialized call_depth_v)
      ]
    in
    let bind_dest_field_to_v_or_uninit state (field, v_or_uninit) =
      field_add_binding_unspecified field ~with_alarms state
        va_list_dest_obj_loc v_or_uninit
    in
    List.fold_left bind_dest_field_to_v_or_uninit state field_value_pairs

  let create vname state =
    let name = Printf.sprintf "%s_va_list_obj" vname in
    let typ = obj_typ () in
    (* Prepare the base. *)
    let base =
      let var_name = name in
      let var_name_desc = "*" ^ name in
      let varinfo = Value_util.create_new_var var_name typ in
      varinfo.vdescr <- Some var_name_desc;
      let validity = Base.validity_from_type varinfo in
      Base.register_allocated_var varinfo validity
    in
    (* Write the uninitialized value to the appropriate location. *)
    let loc = Locations.loc_of_base base in
    let _, state =
      Model.add_binding_unspecified ~exact:true state
        loc (V_Or_Uninitialized.uninitialized)
    in
    (* Return the location and the new abstract state. *)
    (loc, state)

  let remove va_list_obj_loc state =
    let _ = is_va_list_obj_loc va_list_obj_loc state in
    (* Remove the variables. *)
    let module VarinfoSet = Varinfo.Set in
    let va_list_obj_loc_bytes =
      Locations.loc_to_loc_without_size va_list_obj_loc
    in
    let varinfos_to_remove =
      Locations.Location_Bytes.fold_i (fun base offset varinfos ->
          assert (Ival.is_zero offset); (* Checked just before in is_va_list_obj_loc, so it must hold. *)
          let varinfo = Base.to_varinfo base in
          VarinfoSet.add varinfo varinfos
        ) va_list_obj_loc_bytes VarinfoSet.empty
    in
    let varinfos_to_remove = VarinfoSet.elements varinfos_to_remove in
    Model.remove_variables varinfos_to_remove state

end

module Va_list_object = Va_list_object_struct

let arg_loc_of_va_list_loc state va_list_loc =
  (* return **(va_list_obj.arg_ptr_ptr) *)
  let va_list_obj : Va_list_object.t =
    Va_list_object.of_va_list_loc va_list_loc state
  in
  (* Value of the pointer to the pointer to the next argument. *)
  let arg_ptr_ptr : V.t =
    Va_list_object.(field_find F_arg_ptr_ptr state va_list_obj)
  in
  (* Value of the pointer to the next argument. *)
  let arg_ptr : V.t =
    get_v_under_ptr state (arg_ptr_ptr_size ()) arg_ptr_ptr
  in
  (* Location corresponding to the next argument. *)
  loc_of_ptr (arg_ptr_size ()) arg_ptr


(* == Helper functions mainly for the va_arg macro. == *)

(** [reduce_ptr_to_valid_values size ptr] reduces the values of the pointer
    [ptr], which points at something of size [size], to only valid values. *)
let reduce_ptr_to_valid_values (size : Int_Base.t) (ptr : V.t) : V.t =
  let dkey = dkey_reduce_ptr_to_valid_values in

  (* Get the pointer as location. *)
  let loc : Locations.location = loc_of_ptr size ptr in
  Value_parameters.debug ~dkey "ptr loc = %a" Locations.pretty loc;

  (* Reduce the this location to the valid part. *)
  let reduced_loc : Locations.location =
    Locations.valid_part ~for_writing:false loc
  in
  Value_parameters.debug ~dkey "REDUCED ptr loc = %a"
    Locations.pretty reduced_loc;

  (* Convert to a cvalue. *)
  let reduced_value : V.t =
    Locations.loc_to_loc_without_size reduced_loc
  in
  Value_parameters.debug ~dkey "REDUCED ptr = %a" V.pretty ptr;

  (* Return the reduced value. *)
  reduced_value

(* We reduce the value of the pointer to a pointer to an argument in the
   abstract state to the cases where we do not go past the last argument. *)
let reduce_arg_ptr_ptr_to_valid_values : V.t -> V.t =
  reduce_ptr_to_valid_values (arg_ptr_size ())


(* A base and offset pair pretty printer. *)
let pp_base_and_offset fmt ((base, offset) : (Base.t * Ival.t)) =
  let validity = Base.validity base in
  Format.fprintf fmt
    "base: %a -> offset: %a; validity: %a"
    Base.pretty base
    Ival.pretty offset
    Base.pretty_validity validity

(* A typ option pretty printer. *)
let pp_typ_option fmt (typ_option : Cil_types.typ option) =
  match typ_option with
  | None     -> Format.fprintf fmt "(None)"
  | Some typ -> Format.fprintf fmt "%a" Printer.pp_typ typ


(* Helper to check argument pointer's validity. *)
let are_arg_ptr_are_base_and_offset_valid ~dkey base offset =
  Value_parameters.debug ~dkey "%a"
    pp_base_and_offset (base, offset);

  (* Check the base's validity properties. *)
  let is_base_valid_from_the_beginning, _base_validity_size =
    let base_validity = Base.validity base in
    Abstract_interp.Int.(
      match base_validity with
      | Base.Empty                -> (false, zero)
      | Base.Known    (fst, lst)  -> (is_zero fst, add (sub lst fst) one)
      | Base.Unknown  (fst, _, _) -> (is_zero fst, zero)
      | Base.Variable  _          -> (false, zero) (* TODO: Handle this properly? *)
      | Base.Invalid              -> (false, zero)
    )
  in
  Value_parameters.debug ~dkey ":> %s"
    (if is_base_valid_from_the_beginning
     then "the base is valid from the beginning. OK."
     else "the base is NOT valid from the beginning! WRONG!");

  (* The pointer's offset should be a singleton zero, as the argument
     pointer should always point to the beginning of the argument. *)
  let is_offset_a_singleton_zero = Ival.is_zero offset in
  Value_parameters.debug ~dkey ":> %s"
    (if is_offset_a_singleton_zero
     then "the offset is a singleton zero. OK."
     else "the offset is NOT a singleton zero! WRONG!");

  (* The argument's location is valid if its base is valid from the
     beginning and the offset is zero. *)
  is_base_valid_from_the_beginning &&
  is_offset_a_singleton_zero


(* Helper to check argument pointer's value's type. *)
let is_type_of_the_arg_ptr_value_as_expected ~dkey state base expected_arg_typ =

  (* Get the type of the value from the base. *)
  let arg_typ_option = Base.typeof base in

  (* Debug printing... *)
  Value_parameters.debug ~dkey "expected type: %a, actual type %a"
    Printer.pp_typ expected_arg_typ
    pp_typ_option arg_typ_option;

  match arg_typ_option with
  | Some arg_typ ->
      (*
         From C11 standard (7.16.1.1.2):

         "(...) if type is not compatible with the type of the actual next
         argument (as promoted according to the default argument
         promotions), the behavior is undefined, except for the following
         cases:
         - one type is a signed integer type, the other type is the
           corresponding unsigned integer type, and the value is
           representable in both types;
         - one type is pointer to void and the other is a pointer to a
           character type."
       *)

    (* Check the compatibility. *)
    let are_types_compatible = lazy (
      (* Promote the argument type. *)
      let promoted_arg_typ =
        if Cil.isIntegralType arg_typ
        then Cil.integralPromotion arg_typ
        else arg_typ
      in
      (* Compare the unrolled argument types. *)
      Bit_utils.equal_type_no_attribute
        (Cil.unrollTypeDeep promoted_arg_typ)
        (Cil.unrollTypeDeep expected_arg_typ)
        (* Cabs2cil.compatibleTypesp promoted_arg_typ expected_arg_typ *)
    ) in

    Value_parameters.debug ~dkey ":> %s"
      (if Lazy.force are_types_compatible
       then "the expected and actual types are the same."
       else "the expected and actual types are NOT the same.");

    (* Special case 1:
       signed and unsigned versions of the same integer type. *)
    let are_corresponding_signed_and_unsigned_integers = lazy (
      match arg_typ, expected_arg_typ with
      | TInt((IBool)                as actual_arg_ikind,   _),
        TInt((IBool)                as expected_arg_ikind, _)
      | TInt((IChar|ISChar|IUChar)  as actual_arg_ikind,   _),
        TInt((IChar|ISChar|IUChar)  as expected_arg_ikind, _)
      | TInt((IInt|IUInt)           as actual_arg_ikind,   _),
        TInt((IInt|IUInt)           as expected_arg_ikind, _)
      | TInt((IShort|IUShort)       as actual_arg_ikind,   _),
        TInt((IShort|IUShort)       as expected_arg_ikind, _)
      | TInt((ILong|IULong)         as actual_arg_ikind,   _),
        TInt((ILong|IULong)         as expected_arg_ikind, _)
      | TInt((ILongLong|IULongLong) as actual_arg_ikind,   _),
        TInt((ILongLong|IULongLong) as expected_arg_ikind, _)
      | TInt((IInt128|IUInt128)     as actual_arg_ikind,   _),
        TInt((IInt128|IUInt128)     as expected_arg_ikind, _) ->
        begin

          Value_parameters.debug ~dkey
            ":> signed - unsigned same integral type case.";

          (* Check which one is signed an which one unsigned. *)
          let is_expected_arg_typ_signed = isSigned expected_arg_ikind in
          let is_actual_arg_typ_signed   = isSigned actual_arg_ikind in

          (* One should be signed and the other one unsigned,
             but just in case we check that... *)
          if is_expected_arg_typ_signed == is_actual_arg_typ_signed
          then begin
            Value_parameters.debug ~dkey ":> both signed / both unsigned!";
            true
          end else begin

            (* Get the minimal and maximal value of the argument. *)
            let actual_min, actual_max =
              (* Get all the possible argument values. *)
              let base, ival =
                let _, arg_value =
                  let loc = Locations.loc_of_base base in
                  Model.find state loc
                in
                try V.find_lonely_key arg_value
                with Not_found -> assert false (* We've just created the location and it has only one base. *)
              in
              assert (Base.is_null base); (* The value is of an integral type, thus the base must be null. *)
              Value_parameters.debug ~dkey ":> actual ival = %a"
                Ival.pretty ival;

              (* Get the max and min. *)
              Ival.min_and_max ival
            in

            (* Check if the all / some values fit into the expected type. *)
            if is_expected_arg_typ_signed
            then begin
              assert (not is_actual_arg_typ_signed);
              (* Expected type signed, actual type unsigned:
                 -> Values greater that max signed value representable don't fit! *)
              Value_parameters.debug ~dkey
                ":> expected type SIGNED, actual type UNSIGNED!";

              (* Get the max signed value representable. *)
              let max_signed_value : Abstract_interp.Int.t =
                let size_in_bits = Cil.bitsSizeOf expected_arg_typ in
                Cil.max_signed_number size_in_bits
              in

              (* Does actual max is greater that max signed value? *)
              let some_values_do_not_fit =
                match actual_max with
                | None            -> true
                | Some actual_max -> Abstract_interp.Int.gt actual_max max_signed_value
              in
              Value_parameters.debug ~dkey ":> %s"
                (if some_values_do_not_fit
                 then "there are SOME values that do not fit!"
                 else "there are NO values that do not fit");

              (* Does actual min is lower or equal to the max signed value? *)
              let some_values_fit =
                match actual_min with
                | None            -> true
                | Some actual_min -> Abstract_interp.Int.le actual_min max_signed_value
              in
              Value_parameters.debug ~dkey ":> %s"
                (if some_values_do_not_fit
                 then "there are SOME values that fit!"
                 else "there are NO values that fit!");

              begin
                (* If SOME values do NOT fit: emit a warning. *)
                if some_values_do_not_fit then
                  Value_parameters.warning ~current:true
                    "although the type provided to the va_arg macro (%a) \
                     and the actual type of the next variadic argument \
                     (%a) are corresponding signed - unsigned variants of \
                     the same integer type, the actual value of the \
                     argument (unsigned) is too big to be represented in \
                     the provided (signed) type; assert the (unsigned) \
                     value of the argument fits in the (signed) type \
                     provided to the va_arg macro%t"
                    Printer.pp_typ expected_arg_typ
                    Printer.pp_typ arg_typ
                    pp_callstack
              end;

              (* If ALL values do NOT fit: false. Otherwise: true. *)
              some_values_fit || (not some_values_do_not_fit)

            end else begin
              assert is_actual_arg_typ_signed;
              Value_parameters.debug ~dkey
                ":> expected type UNSIGNED, actual type SIGNED!";

              (* Expected type is unsigned, actual type is signed:
                 -> Negative values don't fit! *)

              let zero = Abstract_interp.Int.zero in

              (* Does actual min is lower that 0? *)
              let some_values_do_not_fit =
                match actual_min with
                | None            -> true
                | Some actual_min -> Abstract_interp.Int.lt actual_min zero
              in
              Value_parameters.debug ~dkey ":> %s"
                (if some_values_do_not_fit
                 then "there are SOME values that do not fit!"
                 else "there are NO values that do not fit");

              (* Does actual max is greater or equal to 0?*)
              let some_values_fit =
                match actual_max with
                | None            -> true
                | Some actual_max -> Abstract_interp.Int.ge actual_max zero
              in
              Value_parameters.debug ~dkey ":> %s"
                (if some_values_do_not_fit
                 then "there are SOME values that fit!"
                 else "there are NO values that fit!");

              begin
                (* If SOME values do NOT fit: emit a warning. *)
                if some_values_do_not_fit then
                  Value_parameters.warning ~current:true
                    "although the type provided to the va_arg macro (%a) \
                     and the actual type of the next variadic argument \
                     (%a) are corresponding unsigned - signed variants of \
                     the same integer type, the actual value of the \
                     argument (signed) is negative thus it cannot be \
                     represented in the provided (unsigned) type; assert \
                     the (signed) value of the argument fits in the \
                     (unsigned) type provided to the va_arg macro%t"
                    Printer.pp_typ expected_arg_typ
                    Printer.pp_typ arg_typ
                    pp_callstack
              end;

              (* If ALL values do NOT fit: false. Otherwise: true. *)
              some_values_fit || (not some_values_do_not_fit)

            end
          end
        end

      | _ -> false
    ) in

    (* Special case 2:
       one is a void pointer and the other a char pointer. *)
    let are_both_void_or_char_pointer_types = lazy (
      let isVoidOrCharPointerType typ =
        Cil.((isVoidPtrType typ) || (isCharPtrType typ))
      in
      (isVoidOrCharPointerType          arg_typ) &&
      (isVoidOrCharPointerType expected_arg_typ)
    ) in

    (* Check if the types are compatible or if one of the special cases applies. *)
    Lazy.force are_types_compatible
    || Lazy.force are_both_void_or_char_pointer_types
    || Lazy.force are_corresponding_signed_and_unsigned_integers

  | None ->
    (* What does it mean if we cannot get the argument base's type?
       I'm not sure, but this is definitely a type mismatch. *)
    false

(* [reduce_arg_ptr_to_valid_values arg_ptr expected_arg_typ] reduces the
   pointer to an argument to valid values, expecially we verify here if the type
   of the value of the pointed argument is coherent with the expected type
   provided to the va_arg macro. *)
let reduce_arg_ptr_to_valid_values state arg_ptr expected_arg_typ =
  let dkey = dkey_reduce_arg_ptr_to_valid_values in

  (* Iterate on all the base and offset pairs. *)
  try
    V.fold_i (fun (base : Base.t) (offset : Ival.t) cvalue_acc ->
        (* The argument's location is valid if its base is valid from the
           beginning and the offset is zero. *)
        let are_base_and_offset_valid =
          are_arg_ptr_are_base_and_offset_valid ~dkey base offset
        in
        Value_parameters.debug ~dkey ":> %s"
          (if are_base_and_offset_valid
           then "the base and the offset are valid. OK."
           else "the base and the offset are NOT valid! WRONG!");

        (* The type of the value under the pointer should be the  *)
        let is_type_of_the_value_as_expected =
          is_type_of_the_arg_ptr_value_as_expected ~dkey state base expected_arg_typ
        in
        Value_parameters.debug ~dkey ":> %s"
          (if is_type_of_the_value_as_expected
           then "the type is as expected. OK."
           else "the type is NOT as expected! WRONG!");

        begin
          if not are_base_and_offset_valid then
            Value_parameters.warning ~current:true
              "next variadic argument is invalid; assert all variadic \
               arguments are valid%t" pp_callstack;

          if not is_type_of_the_value_as_expected then
            begin
              if Value_parameters.WarnVaArgTypeMismatch.get () then
                begin
                  Value_parameters.warning ~current:true
                    "the actual type of the next variadic argument (%a) does not \
                     match the type provided to the va_arg macro (%a); assert the \
                     type of each variadic arguments provided to a function matches \
                     the type given to the corresponding call to the va_arg macro%t"
                    pp_typ_option (Base.typeof base)
                    Printer.pp_typ expected_arg_typ
                    pp_callstack
                end else begin
                Value_parameters.feedback ~current:true
                  "continuing past mismatch in the actual type of the next \
                   variadic argument (%a) and the type provided to the va_arg \
                   macro (%a)"
                  pp_typ_option (Base.typeof base)
                  Printer.pp_typ expected_arg_typ
              end;

              (* Also: If the size of the expected argument type is smaller than
                 the size of an integer then it may be a problem related to
                 integer promotion. *)
              let do_integer_promotion_warning =
                try
                  (* Expected size of the argument. *)
                  let arg_expected_size_int =
                    let arg_expected_size = Bit_utils.sizeof expected_arg_typ in
                    Int_Base.project arg_expected_size
                  in
                  (* Size of an integer. *)
                  let integer_size_int =
                    let integer_size = Bit_utils.sizeof intType in
                    Int_Base.project integer_size
                  in
                  (* Compare. *)
                  Abstract_interp.Int.lt arg_expected_size_int integer_size_int
                with Int_Base.Error_Top -> false
              in
              if do_integer_promotion_warning then
                Value_parameters.warning ~current:true
                  "the type provided to the va_arg macro is smaller than \
                   integer; because of the integer promotion performed \
                   automatically on all function arguments this is almost \
                   definitely an error%t"
                  pp_callstack
            end;
        end;

        (* Decide to keep or skip this value. *)
        let is_type_of_the_value_as_expected =
          (* Should we ignore the type mismatch? *)
          is_type_of_the_value_as_expected ||
          (not (Value_parameters.WarnVaArgTypeMismatch.get ()))
        in
        if are_base_and_offset_valid && is_type_of_the_value_as_expected then
          (* Everything is ok (or we ignore the type mismatch on purpose)! *)
          (* This value stays. *)
          let cvalue = V.inject base offset in
          V.join cvalue cvalue_acc
        else
          (* Something is wrong! Print the appropriate warnings and skip this value. *)
          (* Skip the value! *)
          cvalue_acc

      ) arg_ptr V.bottom
  with V.Error_Top ->
    (* NOTE: I think it's not possible for an argument pointer to be top, as I see
       no way of making that happen: it is a value of a cell of the argument
       pointers array that we build ourselves. However, in order to get there we
       pass through two pointer levels: the va_list variable and the arg_ptr_ptr
       of the underlying va_list object. I think the va_list variable cannot be
       imprecise without using direct conditional assignments, and assigning a
       value to a va_list variable directly is forbidden. When we use the va_start
       and va_copy macros only the arg_ptr_ptr field is touched, not the variable
       itself. And I don't know how we could make arg_ptr_ptr top, as we can
       modify it only using the variadic macros. *)
    Value_parameters.fatal ~current:true "a variadic argument pointer with \
                                          a top value encountered!%t" pp_callstack

(* [convert_arg_ptr_values_to_a_type ~with_alarms state dst_typ arg_size arg_ptr]
   builds the properly typed argument value by converting the type of all the
   possible argument values to the [dst_typ] one by one. *)
let convert_arg_ptr_values_to_a_type ~with_alarms state dst_typ arg_typ arg_ptr =
  let dkey = dkey_convert_arg_ptr_values_to_a_type in

  (* Flags used to mark if any argument value is potentially uninitialized
     or contains escaping addresses. *)
  let possibly_uninit = ref false in
  let possibly_esc    = ref false in

  (* Convert all the possible argument values from their type to
     the destination type. *)
  let cvalue : V.t =

    (* Iterate on all possible base and offset pairs. *)
    try
      V.fold_i (fun (base : Base.t) (offset : Ival.t) cvalue_acc ->

          (* This should hold for each valid argument pointer. *)
          assert (Ival.is_zero offset);

          (* Get the type of the value from the base. *)
          let src_typ_option = Base.typeof base in
          match src_typ_option with
          | Some src_typ ->
            Value_parameters.debug ~dkey "type conversion: \
                                          dst_typ = %a, src_typ = %a"
              Cil_printer.pp_typ dst_typ
              Cil_printer.pp_typ src_typ;

            (* Prepare a singleton argument value using the base and offset. *)
            let singleton_v =
              let v_or_uninit =
                (* Create a singleton pointer cvalue. *)
                let ptr_singleton = V.inject base offset in
                (* Prepare the size. *)
                let arg_size = Bit_utils.sizeof arg_typ in
                (* Get the value under this pointer. *)
                get_v_or_uninit_under_ptr state arg_size ptr_singleton
              in
              (* If it might be uninitialized, set the flag. *)
              begin
                if not (V_Or_Uninitialized.is_initialized v_or_uninit)
                then possibly_uninit := true
              end;
              (* If it might contain escaping addresses, set the flag. *)
              begin
                if not (V_Or_Uninitialized.is_noesc v_or_uninit)
                then possibly_esc := true
              end;
              (* Use the initialized version of the value. *)
              V_Or_Uninitialized.get_v v_or_uninit
            in
            (* Convert this singleton value to the right type. *)
            let v' =
              begin
                (* Prepare conversion parameters. *)
                let rounding_mode = get_rounding_mode () in
                let msg fmt = Format.fprintf fmt "%a" V.pretty singleton_v in
                (* Perform the type conversion. *)
                Eval_op.do_promotion ~with_alarms
                  rounding_mode ~src_typ ~dst_typ singleton_v msg
              end
            in
            V.join v' cvalue_acc

          (* What does it mean if we cannot get the argument's type?
             I'm not sure, but we skip this value. *)
          | None ->
            Value_parameters.debug ~dkey
              "src_typ = NONE; no conversion possible!";
            cvalue_acc

        ) arg_ptr V.bottom

    with V.Error_Top ->
      Value_parameters.fatal ~current:true "a variadic argument pointer with \
                                            a top value encountered!%t" pp_callstack
  in

  (* Use the flags in order to create the appropriate variant of the possibly
     uninitialized and/or containing escaping addresses cvalue. *)
  V_Or_Uninitialized.(
    match !possibly_uninit, !possibly_esc with
    | true, true   -> C_uninit_esc   cvalue
    | true, false  -> C_uninit_noesc cvalue
    | false, true  -> C_init_esc     cvalue
    | false, false -> C_init_noesc   cvalue
  )


(* Shift the given pointer (in this case the pointer to a cell in
   the array of pointers to arguments) by one array cell forward. *)
let get_next_arg_ptr_ptr (arg_ptr_ptr : V.t) : V.t =
  let shift_by : Ival.t =
    let shift_by_bits  = Integer.to_int (Int_Base.project (arg_ptr_size ())) in
    let shift_by_bytes = shift_by_bits / 8 in
    let shift_by_int   = Abstract_interp.Int.of_int shift_by_bytes in
    Ival.inject_singleton shift_by_int
  in
  V.shift shift_by arg_ptr_ptr


(* == Variadic macros implementation. == *)

(* Exception used in the va_* functions to return the bottom state immediately. *)
exception Reduce_to_bottom


(* Implementation of the va_start macro. *)
let va_start' ~with_alarms current_kf va_list_lv state =

  Valarms.set_syntactic_context (Valarms.SyMem va_list_lv);
  let dkey = dkey_va_start in
  Value_parameters.debug ~dkey "__builtin_va_start called!";

  (*
    Conditions to check when calling va_start macro:
    1. The function is variadic.
    2. The function is not the entry point.
    3. The va_list object is uninitialized (which means that neither va_start
       nor va_copy were called on it before without a subsequent call to va_end
       macro).

    Note: If somehow there is no va_args variable associated with the function
    even though it is variadic and it is not an entry point, then something must
    have gone wrong as the variadic arguments passed to the function have not
    been properly treated. This should not happen.

    Things to do:
    1. Modify the va_list to point to the beginning of our variadic arguments
       pointers array (this array is available in the va_args variable
       associated with the current function).
  *)

  (* == We start by checking if va_start can be called in this function. == *)

  (* First, we check if the function is variadic. *)
  if not (Value_util.is_function_variadic current_kf) then
    begin
      warning_once_current "va_start macro \
                            called in a non-variadic function %a; assert function %a is variadic%t"
        Kernel_function.pretty current_kf
        Kernel_function.pretty current_kf
        pp_callstack;
      raise Reduce_to_bottom
    end;

  (* Second, we check if the function is not the entry point. *)
  if Kernel_function.is_entry_point current_kf then
    begin
      warning_once_current "va_start macro \
                            called in function %a which is the entry point (i.e. the program's main \
                            function); assert va_start macro is not called in the entry point \
                            functions%t"
        Kernel_function.pretty current_kf
        pp_callstack;
      raise Reduce_to_bottom
    end;

  (* Then, just in case, we check there is a va_args variable associated with
     the function. If there is not, then something must have gone wrong... We
     prefer to abort in a clean way than to assert this though. *)
  if not (Va_args.has_function_va_args_varinfo current_kf) then
    begin
      Value_parameters.fatal ~current:true "va_start macro \
                                            called in function %a, whose variadic arguments have not been correctly \
                                            handled and prepared!%t"
        Kernel_function.pretty current_kf pp_callstack
    end;

  (* == Now we handle the va_start call. == *)

  (* Get the va_list from the provided va_list lvalue. *)
  let va_list =
    let va_list_loc = Eval_exprs.lval_to_loc ~with_alarms state va_list_lv in
    Va_list_object.of_va_list_loc va_list_loc state
  in

  begin
    (* If the arg_ptr_ptr is initialized then this is an error, as it can be
       the case only if va_start or va_copy macros were called on the provided
       va_list variable before without a subsequent va_end call. *)
    let arg_ptr_ptr_v_or_uninit =
      Va_list_object.(field_find_unspecified F_arg_ptr_ptr state va_list)
    in
    Value_parameters.debug ~dkey "arg_ptr_ptr value = %a"
      V_Or_Uninitialized.pretty arg_ptr_ptr_v_or_uninit;

    (* If our arg_ptr_ptr MIGHT BE initialized: emit a warning. *)
    if is_possibly_init arg_ptr_ptr_v_or_uninit then
      warning_once_current "va_start macro \
                            called on a va_list variable %a that has been already initialized \
                            using va_start or va_copy macro without of a subsequent call to the \
                            va_end macro; assert each call to va_start or va_copy macro \
                            preceding this call to va_start macro has a matching va_end call%t"
        Printer.pp_lval va_list_lv pp_callstack;

    (* If our arg_ptr_ptr is DEFINITELY initialized: the abstract state
       becomes bottom. *)
    if is_definitely_init arg_ptr_ptr_v_or_uninit then
      raise Reduce_to_bottom
  end;

  (* Prepare the pointer to the beginning of the array of pointers to the
     variadic arguments. *)
  let arg_ptr_ptr_new_value : V.t =
    let va_args_varinfo : varinfo =
      assert (Value_util.is_function_variadic current_kf);
      assert (Va_args.has_function_va_args_varinfo current_kf);
      Va_args.get_va_args_varinfo_of_function current_kf
    in
    let va_args_loc = Locations.loc_of_varinfo va_args_varinfo in
    Locations.loc_to_loc_without_size va_args_loc
  in
  Value_parameters.debug ~dkey "arg_ptr_ptr new value = %a"
    V.pretty arg_ptr_ptr_new_value;

  (* Initialized the the provided va_list with this pointer written to the
     arg_ptr_ptr field. *)
  Va_list_object.(initialize ~with_alarms va_list state arg_ptr_ptr_new_value)


(* Implementation of the va_copy macro. *)
let va_copy' ~with_alarms _current_kf (va_list_dest_lv, va_list_src_exp) state =

  Valarms.set_syntactic_context (Valarms.SyMem va_list_dest_lv);
  let dkey = dkey_va_copy in
  Value_parameters.debug ~dkey "__builtin_va_copy called!";

  (*
    Conditions to check when calling va_copy macro:
    1. The va_list destination object is uninitialized (which means that
       neither va_start nor va_copy were called on it before without a
       subsequent va_end call).

    Things to do:
    1. Modify the destination va_list object to point to the same place as the
       source va_list object (i.e. to the same argument pointer in the same
       variadic argument pointers array).
  *)

  Value_parameters.debug ~dkey "destination va_list_dest_lv = (%a) %a"
    Printer.pp_typ (Cil.typeOfLval va_list_dest_lv)
    Printer.pp_lval va_list_dest_lv;

  (* Get the va_list from the provided destination va_list lvalue. *)
  let va_list_dest =
    let va_list_dest_loc = Eval_exprs.lval_to_loc ~with_alarms state va_list_dest_lv in
    Va_list_object.of_va_list_loc va_list_dest_loc state
  in

  begin
    (* If the destination arg_ptr_ptr is initialized then this is an error, as
       it can be the case only if va_start or va_copy macros were called on the
       provided destination va_list variable before without a subsequent va_end
       call. *)
    let arg_ptr_ptr_dest_v_or_uninit =
      Va_list_object.(field_find_unspecified F_arg_ptr_ptr state va_list_dest)
    in
    Value_parameters.debug ~dkey "destination arg_ptr_ptr value = %a"
      V_Or_Uninitialized.pretty arg_ptr_ptr_dest_v_or_uninit;

    (* If our destination arg_ptr_ptr MIGHT BE initialized: emit a warning. *)
    if is_possibly_init arg_ptr_ptr_dest_v_or_uninit then
      warning_once_current "va_copy macro \
                            called on a destination va_list variable %a that has been already \
                            initialized using va_start or va_copy macro without of a subsequent \
                            call to the va_end macro; assert each call to va_start or va_copy \
                            macro preceding this call to va_copy macro has a matching va_end call%t"
        Printer.pp_lval va_list_dest_lv pp_callstack;

    (* If our destination arg_ptr_ptr is DEFINITELY initialized: the abstract
       state becomes bottom. *)
    if is_definitely_init arg_ptr_ptr_dest_v_or_uninit then
      raise Reduce_to_bottom
  end;

  Value_parameters.debug ~dkey "va_list_src_exp: (%a) %a"
    Printer.pp_typ (Cil.typeOf va_list_src_exp)
    Printer.pp_exp va_list_src_exp;

  (* Get the value of the arg_ptr_ptr of the provided source va_list. *)
  let va_list_src =
    let va_list_src_exp_value = eval_expr ~with_alarms state va_list_src_exp in
    Va_list_object.of_ptr va_list_src_exp_value state
  in

  let arg_ptr_ptr_src_value : V.t =
    Va_list_object.(field_find F_arg_ptr_ptr state va_list_src)
  in
  Value_parameters.debug ~dkey "arg_ptr_ptr_src value = %a"
    V.pretty arg_ptr_ptr_src_value;

  (* Set the value of the destination va_list's arg_ptr_ptr to the value of the
     source va_list's arg_ptr_ptr. *)
  Va_list_object.(copy ~with_alarms va_list_dest state va_list_src)


(* Implementation of the va_end macro. *)
let va_end' ~with_alarms _current_kf va_list_lv state =

  Valarms.set_syntactic_context (Valarms.SyMem va_list_lv);
  let dkey = dkey_va_end in
  Value_parameters.debug ~dkey "__builtin_va_end called!";

  (*
    Conditions to check when calling va_end macro:
    1. The va_list object is already initialized (which means that either
       va_start or va_copy macro was called on it before without a subsequent
       call of va_end macro).
    2. The function in which the va_list object was initialized (i.e. where
       va_start or va_copy macro was called on it) is the same as the current
       function.

    Things to do:
    1. Uninitialize the va_list object.
  *)

  (* Get the va_list from the provided va_list lvalue. *)
  let va_list =
    let va_list_loc = Eval_exprs.lval_to_loc ~with_alarms state va_list_lv in
    Va_list_object.of_va_list_loc va_list_loc state
  in

  begin
    (* If the arg_ptr_ptr is uninitialized then this is an error, as it can be
       the case only if neither va_start nor va_copy macros were called on the
       provided va_list variable yet without a subsequent va_end macro call. *)
    let arg_ptr_ptr_v_or_uninit =
      Va_list_object.(field_find_unspecified F_arg_ptr_ptr state va_list)
    in
    Value_parameters.debug ~dkey "arg_ptr_ptr value = %a"
      V_Or_Uninitialized.pretty arg_ptr_ptr_v_or_uninit;

    (* If our arg_ptr_ptr MIGHT BE uninitialized: emit a warning. *)
    if is_possibly_uninit arg_ptr_ptr_v_or_uninit then
      warning_once_current "va_end macro \
                            called on a va_list variable %a that is not initialized (using \
                            va_start or va_copy macro); assert va_start or va_copy macro has \
                            been called before calling va_end macro%t"
        Printer.pp_lval va_list_lv pp_callstack;

    (* If our arg_ptr_ptr is DEFINITELY uninitialized: the abstract state
       becomes bottom. *)
    if is_definitely_uninit arg_ptr_ptr_v_or_uninit then
      raise Reduce_to_bottom
  end;

  begin
    (* If the initial call depth is not equal to the current call depth then
       this is an error, as it can be the case only if va_start or va_copy macro
       was called on the provided va_list variable in a different function than
       the current one. *)
    let init_call_depth_v = Va_list_object.(field_find F_init_call_depth state va_list) in
    let current_call_depth_v = get_current_call_depth_v () in
    Value_parameters.debug ~dkey "call_depth value = %a; current call depth = %a"
      V.pretty init_call_depth_v V.pretty current_call_depth_v;

    (* If our init_call_depth MIGHT BE different that the current one:
       emit a warning. *)
    let call_depth_difference = V.diff init_call_depth_v current_call_depth_v in
    if not (V.is_bottom call_depth_difference) then
      warning_once_current "va_end macro \
                            called on a va_list variable %a that was initialized (using va_start \
                            or va_copy macro) in a different function; assert va_start or va_copy \
                            macro has been called in the same function that the va_end macro%t"
        Printer.pp_lval va_list_lv pp_callstack;

    (* If our init_call_depth is DEFINITELY different than the current one:
       the abstract state becomes bottom. *)
    if not (V.intersects current_call_depth_v init_call_depth_v) then
      raise Reduce_to_bottom
  end;

  (* Deinitialize the provided va_list (all fields become uninitialized). *)
  Va_list_object.(deinitialize ~with_alarms va_list state)


(* Implementation of the va_arg macro. *)
let va_arg' ~with_alarms _current_kf (va_list_lv, expected_arg_typ, dest_lv) state =

  Valarms.set_syntactic_context (Valarms.SyMem va_list_lv);
  let dkey = dkey_va_arg in
  Value_parameters.debug ~dkey "__builtin_va_arg called!";

  (*
    Conditions to check when calling va_arg macro:
    1. The provided va_list object is initialized (which means that either
       va_start or va_copy macro was called on it before and va_end macro was
       not called in the meantime).
    2. We are not going past the last argument (by iterating the va_arg macro
       too many times).

    Things to do:
    1. Write the value of the next argument to the destination.
    2. Modify the va_list object so that on the next invocation of va_arg the
       successive argument will be returned.
  *)

  (* General comment:
     The implementation of the va_arg macro is quite long and complex as
     we have to treat multiple nuances concerning the argument which is read.

     Globally there are three separate things to do here:
     1. preparing the value of the next variadic argument,
     2. writing the value of this argument to the variable which is on the
        left side of the assignment [x = va_arg(...)],
     3. modifying the provided va_list object so that next time we will read
        the following argument.

     Especially preparing the value of the next variadic argument is
     a complicated sequence of operations. In order to properly prepare
     this value we need to:
     1.1. take into account the possible uninitializedness of the provided
          va_list object,
     1.2. check if we do not go past the last variadic argument,
     1.3. check if the type of the returned argument corresponds well to
          the expected type given to the va_arg macro,
     1.4. cast the value of the argument to the type of the lvalue where we
          will write it (as the result assignment part of the va_arg macro
          does not work separately but must be implemented here,
          i.e. when [x = va_arg(ap, type)] is written in the code, we cannot
          just return the value of the next argument, we need to write this
          value to [x] inside here, it will not happen automatically).

      Everywhere where we can we try to reduce the value in different
      locations (e.g. the va_list variable) in order to make further analysis
      more exact. Also if something is possibly wrong we emit appropriate
      warnings, and if something is certainly wrong (i.e. there is no
      execution in which it would work correctly), we return the bottom
      abstract state immediately.
  *)

  (* == (1.) Preparing the value of the next variadic argument. == *)

  (* Get the va_list from the provided va_list lvalue. *)
  let va_list =
    let va_list_loc = Eval_exprs.lval_to_loc ~with_alarms state va_list_lv in
    Va_list_object.of_va_list_loc va_list_loc state
  in

  (* Get the value of arg_ptr_ptr of the provided va_list. *)
  let arg_ptr_ptr_v_or_uninit =
    Va_list_object.(field_find_unspecified F_arg_ptr_ptr state va_list)
  in
  Value_parameters.debug ~dkey "arg_ptr_ptr value = %a"
    V_Or_Uninitialized.pretty arg_ptr_ptr_v_or_uninit;

  (* (1.1.) Checking the va_list's arg_ptr_ptr initializedness. *)
  begin
    (* If the arg_ptr_ptr is uninitialized then this is an error, as it can be
       the case only if neither va_start nor va_copy macros were called on the
       provided va_list yet without a subsequent va_end macro call. *)

    (* If our arg_ptr_ptr MIGHT BE uninitialized: emit a warning. *)
    if is_possibly_uninit arg_ptr_ptr_v_or_uninit then
      warning_once_current "va_arg macro \
                            called on a va_list variable %a that is not initialized (using \
                            va_start or va_copy macro); assert va_start or va_copy macro has \
                            been called before calling va_arg macro%t"
        Printer.pp_lval va_list_lv pp_callstack;

    (* If our arg_ptr_ptr is DEFINITELY uninitialized: the abstract state
       becomes bottom. *)
    if is_definitely_uninit arg_ptr_ptr_v_or_uninit then
      raise Reduce_to_bottom
  end;

  (* Either way reduce the value of the arg_ptr_ptr of the provided va_list
     (also in the abstract state) to the cases where it has been initialized. *)
  let arg_ptr_ptr =
    V_Or_Uninitialized.get_v arg_ptr_ptr_v_or_uninit
  in
  let state =
    let arg_ptr_ptr_loc =
      Va_list_object.(field_get_loc F_arg_ptr_ptr state va_list)
    in
    Model.reduce_previous_binding state arg_ptr_ptr_loc arg_ptr_ptr
  in

  (* (1.2.) We have now (in arg_ptr_ptr) the pointer to the pointer to
            the next argument which is surely initialized. We need to
            handle the possibility of surpassing the last argument and then
            we need to reduce it to valid values. *)

  (* Check the validity properties of the pointer. *)
  let is_arg_ptr_ptr_valid =
    is_ptr_valid (arg_ptr_size ()) arg_ptr_ptr
  in
  let is_arg_ptr_ptr_bottom =
    not (has_ptr_a_valid_part (arg_ptr_size ()) arg_ptr_ptr)
  in

  begin
    (* If the pointer to the pointer to the argument is out of bounds of the
       argument pointers array then this is an error, as we have gone past
       the last argument and behaviour is undefined. *)
    (* TODO: May also happen if arg_ptr_ptr was modified by hand and is now
       simply invalid (for example NULL)! *)

    (* If going past the last argument is POSSIBLE: emit a warning. *)
    if not is_arg_ptr_ptr_valid then
      warning_once_current "va_arg macro \
                            called when all the variadic arguments have been already used up; \
                            assert enough arguments%t" pp_callstack;

    (* If going past the last argument is SURE: the abstract state becomes
       bottom. *)
    if is_arg_ptr_ptr_bottom then
      raise Reduce_to_bottom
  end;

  (* We reduce the value of the va_list variable in the abstract state to the
     valid cases (i.e. where we do not go past the last argument). *)
  let arg_ptr_ptr = reduce_arg_ptr_ptr_to_valid_values arg_ptr_ptr in

  (* Two sanity checks... *)

  (* As arg_ptr_ptr had a valid part before, thus it is certainly not
     bottom after performing the reduction to valid values. *)
  assert (not (V.is_bottom arg_ptr_ptr));

  (* Also right now, after the reduction, it should be entirely valid. *)
  assert (is_ptr_valid (arg_ptr_size ()) arg_ptr_ptr);

  (* Now we get the value of the pointer to the argument and reduce it
     to valid values. *)
  let arg_ptr : V.t =

    (* Get the value of the pointer to the argument. *)
    let arg_ptr : V.t =
      let v_or_uninit : V_Or_Uninitialized.t =
        get_v_or_uninit_under_ptr state (arg_ptr_size ()) arg_ptr_ptr
      in
      Value_parameters.debug ~dkey "arg_ptr = %a"
        V_Or_Uninitialized.pretty v_or_uninit;

      begin
        (* This pointer should have always been created by us, thus it should
           always be initialized. However for now we cannot assert that, as
           somebody might have also created this array by hand, which should be
           forbidden... *)
        let is_arg_ptr_initialized =
          V_Or_Uninitialized.is_initialized v_or_uninit
        in
        if not is_arg_ptr_initialized then
          warning_once_current
            "the array of pointers to variadic arguments contains an \
             uninitialized pointer, it must have been modified or created by \
             hand which is forbidden; assert values are assigned to \
             variables of type va_list only using the va_start and va_copy \
             macros%t" pp_callstack
      end;

      (* Get the initialized version of the pointer's value. *)
      V_Or_Uninitialized.get_v v_or_uninit
    in

    (* (1.3.) Reducing the pointer to the argument to valid values.
              Expecially we verify here if the type of the value of
              the pointed argument is coherent with the expected type
              provided to the va_arg macro. *)
    reduce_arg_ptr_to_valid_values state arg_ptr expected_arg_typ

  in
  Value_parameters.debug ~dkey "REDUCED arg_ptr = %a" V.pretty arg_ptr;

  (* If the pointer DEFINITELY doesn't point to any valid argument:
     the abstract state becomes bottom. *)
  if V.is_bottom arg_ptr then
    raise Reduce_to_bottom;

  (* (1.4.) We have the proper value of the argument pointer, now we need to
            prepare the value of the argument itself.
            Especially we need to cast all the possible values to the right
            type, so that the assignment (i.e. x = va_arg(...)) works well. *)

  (* Debug printing... *)
  let arg_v_or_uninit_before_type_cast =
    let arg_size = Bit_utils.sizeof expected_arg_typ in
    get_v_or_uninit_under_ptr state arg_size arg_ptr
  in
  Value_parameters.debug ~dkey "arg_value (before type cast) = %a"
    V_Or_Uninitialized.pretty arg_v_or_uninit_before_type_cast;

  (* Build the argument value by converting the type for all possible argument
     values one by one. *)
  let arg_v_or_uninit =
    (* The destination type: we need to convert to this. *)
    let dst_typ = Cil.typeOfLval dest_lv in

    (* Convert. *)
    convert_arg_ptr_values_to_a_type ~with_alarms
      state dst_typ expected_arg_typ arg_ptr
  in
  Value_parameters.debug ~dkey "arg_value = %a"
    V_Or_Uninitialized.pretty arg_v_or_uninit;

  (* == (2.) Putting the value of the argument into the destination. == *)

  let state =
    (* Prepare the destination (where the next argument's value will
       be written). The destination corresponds to the [x] variable
       in [x = va_arg(...)]. *)
    Value_parameters.debug ~dkey "destination lval = %a"
      Cil_printer.pp_lval dest_lv;

    let dest_loc =
      Eval_exprs.lval_to_loc ~with_alarms state dest_lv
    in
    Value_parameters.debug ~dkey "destination loc = %a"
      Locations.pretty dest_loc;

    (* Write the argument's value to the destination. *)
    let state' =
      let exact =
        Locations.(Location_Bits.cardinal_zero_or_one dest_loc.loc)
      in
      Eval_op.add_binding_unspecified ~with_alarms ~exact state
        dest_loc arg_v_or_uninit
    in
    Value_parameters.debug ~dkey
      "argument value %a written to destination location %a!"
      V_Or_Uninitialized.pretty arg_v_or_uninit
      Locations.pretty dest_loc;

    state'
  in

  (* == (3.) Updating the va_list object. == *)
  (* (In order to read the following variadic argument the next time
     when the va_arg macro is called.) *)

  (* We write a new value to pointer to the arg_ptr_ptr of the provided va_list. *)
  let state =
    (* We advance the existing pointer by one. *)
    let state' =
      let arg_ptr_ptr_new_value : V.t = get_next_arg_ptr_ptr arg_ptr_ptr in
      Va_list_object.(field_add_binding
                        F_arg_ptr_ptr ~with_alarms state va_list arg_ptr_ptr_new_value)
    in
    Value_parameters.debug ~dkey "va_list's arg_ptr_ptr incremented!";

    state'
  in

  (* Everything is done! *)
  state


(* All the va_* macros implementations are wrapped in order to intercept
   the Reduce_to_bottom exception and reduce the state to bottom. *)
let va_start, va_end, va_copy, va_arg =
  let wrap_va_macro va_macro ~with_alarms current_kf macro_args state =
    try
      va_macro ~with_alarms current_kf macro_args state
    with
    | Reduce_to_bottom -> Model.bottom
  in
  wrap_va_macro va_start',
  wrap_va_macro va_end',
  wrap_va_macro va_copy',
  wrap_va_macro va_arg'


(* == Adding, removing and checking variadic variables == *)

let add_variadic_arguments_to_state kf call_site actuals state =

  (*
    1. We write the surplus arguments into memory:
       1.1. We get argument varinfos (we reuse existing ones if possible).
       1.2. We add the arguments one by one to the abstract state.
    2. We create the array of pointers to the variadic arguments.
       2.1. We prepare the array's type and values.
       2.2. We initialize the array.
       2.3. We fill the array with values (i.e. pointers to the arguments).
  *)


  (* 1. We write the surplus arguments into memory. *)

  (* 1.1. We get argument varinfos (we reuse existing ones if possible). *)
  let va_args_varinfos =
    match Variadic_arguments.get kf call_site with
    | Some varinfos -> varinfos
    | None ->
      let varinfos =
        List.mapi (fun i (exp, _) ->
            (* Get the type of the argument's value. *)
            let typ = Cil.typeOf exp in
            (* Prepare a hidden variable. *)
            let varinfo =
              let name =
                Format.sprintf "%s_va_arg_%02d" (Kernel_function.get_name kf) i
              in
              Value_util.create_new_var name typ
            in
            (* Prepare the base. *)
            let _base =
              let validity = Base.validity_from_type varinfo in
              Base.register_memory_var varinfo validity
            in
            varinfo
          ) actuals
      in
      (* Save the varinfos for later use if we are at this call site again. *)
      Variadic_arguments.set kf call_site varinfos;
      varinfos
  in

  (* 1.2. We add the arguments one by one to the abstract state. *)
  let state =
    let varinfos_and_actuals_do_not_match () =
      Value_parameters.fatal ~current:true
        "add_variadic_arguments_to_state (%a): variadic variables for \
         arguments and actual argument values pass do not match"
        d_kf_with_call_site (kf, call_site)
    in
    try
      List.fold_left2 (fun state varinfo (exp, offsetmap) ->
          (* Assert it's not a dummy expression. *)
          assert (not (Exp.equal exp Exp.dummy));
          (* Prepare the location. *)
          let loc = Locations.loc_of_varinfo varinfo in
          (* Get the type of the argument's value. *)
          let typ = Cil.typeOf exp in
          if not (Typ.equal typ varinfo.vtype)
          then varinfos_and_actuals_do_not_match ();
          (* Prepare the argument's value. *)
          let (v_or_uninit : V_Or_Uninitialized.t) =
            let with_alarms = CilE.warn_none_mode in (* TODO: Right warning mode? *)
            Eval_op.v_uninit_of_offsetmap ~with_alarms ~typ offsetmap
          in
          Value_parameters.debug ~dkey
            "processing variadic argument %a: type: %a, value: %a"
            Cil_printer.pp_varinfo varinfo
            Cil_printer.pp_typ typ
            V_Or_Uninitialized.pretty v_or_uninit;
          let _, state' =
            Value_parameters.debug ~dkey
              "-> writing value %a@\n   at location %a"
              V_Or_Uninitialized.pretty v_or_uninit
              Locations.pretty loc;
            Model.add_binding_unspecified ~exact:true state loc v_or_uninit
          in
          state'
        ) state va_args_varinfos actuals
    with
      Invalid_argument msg when msg = "List.fold_left2" ->
      varinfos_and_actuals_do_not_match ()
  in

  (* 2. We create the array of pointers to the variadic arguments. *)
  let state =

    (* 2.1. We prepare the array's type and values. *)

    let decl_loc = Kernel_function.get_location kf in

    (* The contents of the array of pointers are the bases of the arguments. *)
    let values =
      let bases = List.map Base.of_varinfo va_args_varinfos in
      (* The actual value of each cell is the address of the corresponding
         argument (i.e. the base) with an offset zero. *)
      List.map (fun base -> V.inject base Ival.zero) bases
    in

    (* The array's type. *)
    let array_typ =
      (* The values are generic void* pointers. *)
      let value_typ =
        Cil.voidPtrType
      in
      (* The array's length. *)
      let length : exp option =
        let values_count = List.length values in
        Some (Cil.kinteger64 ~loc:decl_loc (Abstract_interp.Int.of_int values_count))
      in
      (* The attribute identifying the array as a variadic argument array. *)
      let array_attributes =
        let attribute = Attr ("variadic_arguments_array", []) in
        Cil.addAttribute attribute []
      in
      let bitsSizeofTypCache = Cil.empty_size_cache () in
      TArray (value_typ, length, bitsSizeofTypCache, array_attributes)
    in

    (* 2.2. We initialize the array. *)
    let array_base, state =

      (* The name of the variable corresponding to the array contains
         the function's name. *)
      let name =
        Format.sprintf "__va_args_array_%s" (Kernel_function.get_name kf)
      in
      let typ = array_typ in

      (* Prepare the variable. *)
      let varinfo =
        let var_name = name in
        let var_name_desc = "*" ^ name in
        let varinfo = Value_util.create_new_var var_name typ in
        varinfo.vdescr <- Some var_name_desc;
        varinfo
      in

      (* Prepare the base. *)
      let base =
        let validity = Base.validity_from_type varinfo in
        Base.register_memory_var varinfo validity
      in

      (* Write the uninitialized value to the appropriate location. *)
      let loc = Locations.loc_of_base base in
      let _, state =
        Model.add_binding_unspecified ~exact:true state
          loc (V_Or_Uninitialized.uninitialized)
      in

      (* Set the variable as the va_args variable of the current function. *)
      Va_args.add_va_args_varinfo_for_function kf varinfo;

      base, state
    in

    (* 2.3. We fill the array with values. *)
    let state =
      (* Attribute an index (i.e. the index of its cell in the array) to each value. *)
      let indexed_values = List.mapi (fun i value -> (i, value)) values in

      (* Write the values one by one in the abstract state. *)
      List.fold_left (fun state (i, ith_value) ->

          (* Prepare the location corresponding to the offset i. *)
          let ith_location =
            let offset =
              let offset_exp = Cil.kinteger64 ~loc:decl_loc (Abstract_interp.Int.of_int i) in
              Index(offset_exp, NoOffset)
            in
            Locations.loc_of_typoffset array_base array_typ offset
          in

          (* Write the value at the location. *)
          let _, state =
            Model.add_binding ~exact:true state ith_location ith_value
          in

          (* Pass on the new abstract state. *)
          state

        ) state indexed_values

    in

    (* Return the new abstract state. *)
    state

  in

  (* Done! *)
  state


let variadic_arg_varinfos_of_arg_ptr_ptr arg_ptr_ptr state =

  (* First prepare the list of pointers to arguments. *)
  let arg_ptrs : V.t list =

    (* [is_arg_ptr_ptr_valid arg_ptr_ptr] returns true if the
       pointer [arg_ptr_ptr] (which is a pointer to a pointer to
       an argument) is pointing to something valid (i.e. adress still in
       the array of the pointers to arguments). *)
    let is_arg_ptr_ptr_valid : V.t -> bool =
      is_ptr_valid (arg_ptr_size ())
    in

    (* [get_arg_ptr state arg_ptr_ptr] returns the value under the
       pointer [arg_ptr_ptr] (which is a pointer to a pointer to
       an argument), that is, it returns the pointer to that argument. *)
    let get_arg_ptr state (arg_ptr_ptr : V.t) : V.t =
      assert (is_arg_ptr_ptr_valid arg_ptr_ptr);
      let arg_ptr_loc = loc_of_ptr (arg_ptr_size ()) arg_ptr_ptr in
      let ((_, arg_ptr_value) : (bool * V.t)) =
        Model.find state arg_ptr_loc
      in
      arg_ptr_value
    in

    (* Iterate on the pointers to the arguments until reaching
       the end of the array. *)
    let current_arg_ptr_ptr : V.t ref = ref arg_ptr_ptr in
    let arg_ptrs : V.t list ref = ref [] in

    while is_arg_ptr_ptr_valid !current_arg_ptr_ptr
    do
      (* Get the value of this pointer (i.e. this value is a pointer
         to the current argument). *)
      let current_arg_ptr : V.t = get_arg_ptr state !current_arg_ptr_ptr in

      (* The current argument pointer should be a single valid address. *)
      assert (V.cardinal_zero_or_one current_arg_ptr);
      assert (not (V.is_bottom current_arg_ptr));

      (* Add the current pointer value (which is a pointer to the
         current argument) to the list of results. *)
      arg_ptrs := current_arg_ptr :: !arg_ptrs;

      (* Get the next pointer (i.e. the pointer to the pointer to the
         next argument. *)
      current_arg_ptr_ptr := get_next_arg_ptr_ptr !current_arg_ptr_ptr
    done;

    (* Return the accumulated pointers to the arguments. *)
    !arg_ptrs
  in

  (* Now get the list of variables corresponding to arguments.*)
  let arg_varinfos =
    List.rev_map
      (fun arg_ptr_value ->
         let (arg_base, _arg_ival) =
           try V.find_lonely_key arg_ptr_value
           with Not_found ->
             (* It was checked by assertions before that the cardinality
                of each arg_ptr_value is exactly one. *)
             assert false
         in
         Base.to_varinfo arg_base)
      arg_ptrs
  in

  arg_varinfos

let map_varinfos_from_va_list va_list state f =
  let arg_ptr_ptr =
    let va_list_obj = Va_list_object_struct.of_ptr va_list state in
    Va_list_object_struct.(field_find F_arg_ptr_ptr state va_list_obj)
  in
  List.map f (variadic_arg_varinfos_of_arg_ptr_ptr arg_ptr_ptr state)

let get_variadic_arguments_varinfos kf call_site =
  match Variadic_arguments.get kf call_site with
  | Some varinfos -> varinfos
  | None -> []

let remove_variadic_arguments_from_state kf state =

  if not (Va_args.has_function_va_args_varinfo kf)
  (* If there is no va_args variable then we do not need to remove anything. *)
  then state
  else begin

    (* Find the va_args variable associated with the function. *)
    let va_args_varinfo = Va_args.get_va_args_varinfo_of_function kf in
    (* Remove the association of the va_args variable with the function. *)
    Va_args.remove_va_args_varinfo_for_function kf;

    (* Prepare the pointer to the array of pointers to the
       variadic arguments. *)
    let va_args_value : V.t =
      (* Get the value of the va_args variable. *)
      let va_args_loc = Locations.loc_of_varinfo va_args_varinfo in
      Locations.loc_to_loc_without_size va_args_loc
    in

    (* Sanity check... *)
    assert (V.cardinal_zero_or_one va_args_value);
    assert (not (V.is_bottom va_args_value));

    (* Prepare the variable corresponding to the array of pointers to
       arguments (if there were any variadic arguments passed). *)
    let arg_ptrs_array_varinfo_option : varinfo option =
      let (arg_array_base, _arg_ival) =
        try
          V.find_lonely_key va_args_value
        with Not_found ->
          Value_parameters.abort ~current:true
            "The value of the va_args variable of function %a is not \
             a singleton. Aborting"
            Kernel_function.pretty kf
      in
      if Base.is_null arg_array_base
      then None
      else Some (Base.to_varinfo arg_array_base)
    in

    (* If there are any variadic arguments, we treat them. *)
    let arg_varinfos_to_remove : varinfo list =
      match arg_ptrs_array_varinfo_option with
      | None -> []
      | Some args_array_varinfo ->
        let arg_varinfos =
          let arg_ptr_ptr = va_args_value in
          variadic_arg_varinfos_of_arg_ptr_ptr arg_ptr_ptr state
        in
        args_array_varinfo :: arg_varinfos
    in

    let state =
      (* Remove both the variable corresponding to the array of pointers to
         the arguments and the variables corresponding to each argument. *)
      let varinfos_to_remove = va_args_varinfo :: arg_varinfos_to_remove in

      (* Pretty printer for lists of variables. *)
      let pp_varinfos = Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n+ ")
          Printer.pp_varinfo
      in

      Value_parameters.debug ~dkey "Removing hidden variadic variables \
                                    in function \"%a\":@\n+ %a"
        Kernel_function.pretty kf
        pp_varinfos varinfos_to_remove;

      (* Proceed with the removal. *)
      Model.remove_variables varinfos_to_remove state
    in

    (* The returned state is free of variadic variables of the given function. *)
    state

  end


let create_structure_for_a_variadic_variable state va_list_varinfo =

  if not (Cil.isVariadicListType va_list_varinfo.vtype)
  then state
  else begin

    (* Debug printing... *)
    Value_parameters.debug ~dkey
      "creating the underlying structure for the va_list variable %a"
      Cil_printer.pp_varinfo va_list_varinfo;

    (* Create the underlying va_list object structure. *)
    (* TODO: If already has structure, something went wrong... *)
    let va_list_object, state =
      Va_list_object.create va_list_varinfo.vname state
    in

    (* Prepare the pointer to the va_list object, so we can put it in
       the va_list variable. *)
    let va_list_object_ptr : V.t =
      let base, offset =
        let loc_bytes =
          let loc = Va_list_object.get_loc va_list_object state in
          Locations.loc_to_loc_without_size loc in
        try
          Cvalue.V.find_lonely_binding loc_bytes
        with Not_found ->
          (* The object was just created by us and it should be precise. *)
          assert false;
      in
      (* The object was just created by us and the offset should be zero. *)
      assert (Ival.is_zero offset);
      V.inject base Ival.zero
    in

    (* Bind: va_list := &va_list_object *)
    let _, state =
      let va_list_loc   = Locations.loc_of_varinfo va_list_varinfo in
      Value_parameters.debug ~dkey
        "-> writing value %a@\n   at location %a"
        V.pretty va_list_object_ptr
        Locations.pretty va_list_loc;
      Model.add_binding ~exact:true state va_list_loc va_list_object_ptr

    in

    state

  end


let remove_structure_for_a_variadic_variable state va_list_varinfo =

  if not (Cil.isVariadicListType va_list_varinfo.vtype)
  then state
  else begin

    (* Debug printing... *)
    Value_parameters.debug ~dkey
      "removing the underlying structure for the va_list variable %a"
      Cil_printer.pp_varinfo va_list_varinfo;

    (* Remove the underlying va_list object structure. *)
    let va_list_loc = Locations.loc_of_varinfo va_list_varinfo in
    let state =
      let va_list_object = Va_list_object.of_va_list_loc va_list_loc state in
      Va_list_object.remove va_list_object state
    in

    (* Uninitialize the va_list variable. *)
    let _, state =
      let uninitialized = V_Or_Uninitialized.uninitialized in
      Value_parameters.debug ~dkey
        "-> writing value %a@\n   at location %a"
        V_Or_Uninitialized.pretty uninitialized
        Locations.pretty va_list_loc;
      Model.add_binding_unspecified ~exact:true state va_list_loc uninitialized
    in

    state

  end


(* Filter only variables of type va_list. *)
let filter_va_list_variables : varinfo list -> varinfo list =
  let is_va_list_type varinfo = Cil.isVariadicListType varinfo.vtype in
  List.filter is_va_list_type


let create_structure_for_variadic_variables varinfos state =
  let va_list_varinfos = filter_va_list_variables varinfos in
  List.fold_left create_structure_for_a_variadic_variable state va_list_varinfos

let remove_structure_for_variadic_variables varinfos state =
  let va_list_varinfos = filter_va_list_variables varinfos in
  List.fold_left remove_structure_for_a_variadic_variable state va_list_varinfos


let check_variadic_variables current_kf state varinfos =

  (* Prepare lists of definitely and possibly initialized variables. *)
  let definitely_initialized, possibly_initialized =

    (* Filter only variables of type va_list. *)
    let (va_list_varinfos : varinfo list) = filter_va_list_variables varinfos in

    (* Get the initializedness information of each variable. *)
    let (va_list_varinfos_with_initializedness : (varinfo * bool * bool) list) =
      begin
        if va_list_varinfos <> [] then
          Value_parameters.debug ~dkey "Checking if local va_list \
                                        variables in function \"%a\" are all uninitialized \
                                        (this check may be at the end of the function \
                                        or one of its\blocks):"
            Kernel_function.pretty current_kf
      end;

      List.map (fun va_list_varinfo ->

          (* Get the value of the arg_ptr_ptr corresponding to the given va_list. *)
          let arg_ptr_ptr_v_or_uninit =
            let va_list_object =
              let va_list_loc = Locations.loc_of_varinfo va_list_varinfo in
              Va_list_object.of_va_list_loc va_list_loc state
            in
            Va_list_object.(field_find_unspecified F_arg_ptr_ptr state va_list_object)
          in

          (* Get the initializedness information of the arg_ptr_ptr. *)
          let is_initialized = is_initialized arg_ptr_ptr_v_or_uninit in
          let is_bottom      = is_bottom      arg_ptr_ptr_v_or_uninit in

          (* Debug printing... *)
          Value_parameters.debug ~dkey "+ variable %a (%s, %s) = %a"
            Printer.pp_varinfo va_list_varinfo
            (if is_initialized then "initialized" else "unintialized")
            (if is_bottom      then "bottom"      else "not bottom")
            V_Or_Uninitialized.pretty arg_ptr_ptr_v_or_uninit;

          (* The result triple. *)
          (va_list_varinfo, is_initialized, is_bottom)

        ) va_list_varinfos
    in

    (* A helper for the calls to Extlib.filter_map that follow. *)
    let extract_varinfo varinfo_with_initializedness =
      let (varinfo, _, _) = varinfo_with_initializedness in
      varinfo
    in

    (* Variables with DEFINITELY initialized arg_ptr_ptr. *)
    let definitely_initialized =
      Extlib.filter_map (fun (_varinfo, is_initialized, is_bottom) ->
          match is_initialized, is_bottom with
          | true, false -> true
          (* | true, true  -> true *)
          (* TODO: We ignore this case on purpose, because I'm quite sure it only
                   happens when a variable is out of scope. *)
          | _           -> false
        ) extract_varinfo va_list_varinfos_with_initializedness
    in

    (* Variables with POSSIBLY initialized arg_ptr_ptr. *)
    let possibly_initialized =
      Extlib.filter_map (fun (_varinfo, is_initialized, is_bottom) ->
          match is_initialized, is_bottom with
          | _, false -> true
          | _        -> false
        ) extract_varinfo va_list_varinfos_with_initializedness
    in

    (* The two lists are ready. *)
    definitely_initialized,
    possibly_initialized

  in

  (* Print warnings for each variable which arg_ptr_ptr is POSSIBLY uninitialized. *)
  List.iter (fun varinfo ->
      warning_once_current
        "local variable %a of type va_list in function %a has been initialized \
         using va_start or va_copy macro and has not been deinitialized by a \
         matching va_end macro; assert va_list variable %a has been \
         uninitialized using the va_end macro;"
        Printer.pp_varinfo varinfo
        Kernel_function.pretty current_kf
        Printer.pp_varinfo varinfo
    ) possibly_initialized;

  (* If any variable is DEFINITELY initialized we return false. *)
  definitely_initialized = []



(* Two functions managing the variadic variables when entering / exiting a block. *)

(* NOTE:

   These two are direct add-ons on [Value_util.bind_block_locals] and
   [Cvalue.Model.uninitialize_blocks_locals].

   They are both used only in [Eval_slevel].

   Implementing it this way is not very elegant. We would prefer to modify the
   concerned functions directly. Unfortunately the two add-ons depend on this
   module, i.e. [Value_variadic], more exactly on two functions:
   - [create_structure_for_a_variadic_variable] and
   - [remove_structure_for_variadic_variables].
   And this module depends both on [Value_util] and [Cvalue]. Thus we have a
   dependency loop.

   Possible solutions:
   1. Current status.
   2. Use references.
   3. Use hooks.
   4. Move these functions to [Value_util] and [Cvalue] modules, pass functions
      [create_structure_for_a_variadic_variable] and [remove_structure_for_variadic_variables]
      as arguments.

*)

(* Create the underlying structure for all the block's local va_list variables. *)

let bind_variadic_local_state varinfo state =
  if Cil.isVariadicListType varinfo.vtype then
    begin
      create_structure_for_a_variadic_variable state varinfo
    end
  else state

let bind_block_variadic_locals states block =
  (* Bind [varinfo] in [states] *)
  let bind_local_stateset states varinfo =
    State_set.map (bind_variadic_local_state varinfo) states
  in
  List.fold_left bind_local_stateset states block.blocals

let bind_variadic_locals_state st b =
  List.fold_left
    (fun st vi -> bind_variadic_local_state vi st)
    st b.blocals


(* Remove the underlying structure for all the block's local va_list variables. *)
let uninitialize_blocks_variadic_locals blocks state =
  List.fold_left (fun state block ->
      remove_structure_for_variadic_variables block.blocals state
    ) state blocks


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
