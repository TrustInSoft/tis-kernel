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
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- No-Aliasing Memory Model                                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Ctypes

open Lang
open Memory

type param = NotUsed | ByValue | ByRef | InContext | InArray | InHeap
type separation = Separation.clause

let pp_param fmt = function
  | NotUsed -> Format.pp_print_string fmt "not used"
  | ByValue -> Format.pp_print_string fmt "by value"
  | ByRef -> Format.pp_print_string fmt "by ref."
  | InContext -> Format.pp_print_string fmt "in context"
  | InArray -> Format.pp_print_string fmt "in array"
  | InHeap -> Format.pp_print_string fmt "in heap"

module type VarUsage =
sig
  val datatype : string
  val param : varinfo -> param
  val separation : unit -> separation
end

module Make(V : VarUsage)(M : Memory.Model) =
struct

  (* -------------------------------------------------------------------------- *)
  (* ---  Model                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let datatype = "MemVar." ^ V.datatype ^ M.datatype
  let configure = M.configure

  let separation () = V.separation () :: M.separation ()

  (* -------------------------------------------------------------------------- *)
  (* ---  Chunk                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  type chunk =
    | CVar of varinfo
    | CAlloc of varinfo
    | CMem of M.Chunk.t

  let is_framed_var x =
    not x.vglob &&
    match V.param x with
    | NotUsed | ByValue -> true
    | ByRef | InHeap | InContext | InArray -> false

  module VAR =
  struct
    type t = varinfo
    let self = "var"
    let hash = Varinfo.hash
    let equal = Varinfo.equal
    let compare = Varinfo.compare
    let pretty = Varinfo.pretty
    let typ_of_chunk x =
      match V.param x with
      | ByRef -> Cil.typeOf_pointed x.vtype
      | _ -> x.vtype
    let tau_of_chunk x = Lang.tau_of_ctype (typ_of_chunk x)
    let is_framed = is_framed_var
    let basename_of_chunk = LogicUsage.basename
  end

  module VALLOC =
  struct
    type t = varinfo
    let self = "alloc"
    let hash = Varinfo.hash
    let compare = Varinfo.compare
    let equal = Varinfo.equal
    let pretty = Varinfo.pretty
    let tau_of_chunk _x = Qed.Logic.Bool
    let basename_of_chunk x =
      match V.param x with
      | ByRef ->
        "ra_" ^ LogicUsage.basename x
      | NotUsed | ByValue | InHeap | InContext | InArray ->
        "ta_" ^ LogicUsage.basename x
    let is_framed = is_framed_var
  end

  module Chunk =
  struct
    type t = chunk
    let self = "varmem"
    let hash = function
      | CVar x -> 3 * Varinfo.hash x
      | CAlloc x -> 5 * Varinfo.hash x
      | CMem m -> 7 * M.Chunk.hash m
    let compare c1 c2 =
      if c1 == c2 then 0 else
        match c1 , c2 with
        | CVar x , CVar y
        | CAlloc x , CAlloc y -> Varinfo.compare x y
        | CMem p , CMem q -> M.Chunk.compare p q
        | CVar _ , _ -> (-1)
        | _ , CVar _ -> 1
        | CAlloc _  , _ -> (-1)
        | _ , CAlloc _ -> 1
    let equal c1 c2 = (compare c1 c2 = 0)
    let pretty fmt = function
      | CVar x -> Varinfo.pretty fmt x
      | CAlloc x -> Format.fprintf fmt "alloc(%a)" Varinfo.pretty x
      | CMem m -> M.Chunk.pretty fmt m
    let tau_of_chunk = function
      | CVar x -> VAR.tau_of_chunk x
      | CAlloc x -> VALLOC.tau_of_chunk x
      | CMem m -> M.Chunk.tau_of_chunk m
    let basename_of_chunk = function
      | CVar x -> VAR.basename_of_chunk x
      | CAlloc x -> VALLOC.basename_of_chunk x
      | CMem m -> M.Chunk.basename_of_chunk m
    let is_framed = function
      | CVar x -> VAR.is_framed x
      | CAlloc x -> VALLOC.is_framed x
      | CMem m -> M.Chunk.is_framed m
  end

  (* -------------------------------------------------------------------------- *)
  (* ---  Sigma                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  module HEAP = Qed.Collection.Make(VAR)
  module TALLOC = Qed.Collection.Make(VALLOC)
  module SIGMA = Sigma.Make(VAR)(HEAP)
  module ALLOC = Sigma.Make(VALLOC)(TALLOC)
  module Heap = Qed.Collection.Make(Chunk)

  type sigma = {
    mem : M.Sigma.t ;
    vars : SIGMA.t ;
    alloc : ALLOC.t ;
  }

  module Sigma =
  struct
    type t = sigma
    type chunk = Chunk.t
    type domain = Heap.set
    let empty = Heap.Set.empty
    let union = Heap.Set.union

    let create () = {
      vars = SIGMA.create () ;
      alloc = ALLOC.create () ;
      mem = M.Sigma.create () ;
    }
    let copy s = {
      vars = SIGMA.copy s.vars ;
      alloc = ALLOC.copy s.alloc ;
      mem = M.Sigma.copy s.mem ;
    }
    let merge s1 s2 =
      let s,pa1,pa2 = SIGMA.merge s1.vars s2.vars in
      let a,ta1,ta2 = ALLOC.merge s1.alloc s2.alloc in
      let m,qa1,qa2 = M.Sigma.merge s1.mem s2.mem in
      { vars = s ; alloc = a ; mem = m } ,
      Passive.union (Passive.union pa1 ta1) qa1 ,
      Passive.union (Passive.union pa2 ta2) qa2
    let join s1 s2 =
      Passive.union
        (Passive.union
           (SIGMA.join s1.vars s2.vars)
           (ALLOC.join s1.alloc s2.alloc))
        (M.Sigma.join s1.mem s2.mem)

    let get s = function
      | CVar x -> SIGMA.get s.vars x
      | CAlloc x -> ALLOC.get s.alloc x
      | CMem m -> M.Sigma.get s.mem m
    let mem s = function
      | CVar x -> SIGMA.mem s.vars x
      | CAlloc x -> ALLOC.mem s.alloc x
      | CMem m -> M.Sigma.mem s.mem m
    let value s c = Lang.F.e_var (get s c)
    let iter f s =
      begin
        SIGMA.iter (fun x -> f (CVar x)) s.vars ;
        ALLOC.iter (fun x -> f (CAlloc x)) s.alloc ;
        M.Sigma.iter (fun m -> f (CMem m)) s.mem ;
      end
    let iter2 f s t =
      begin
        SIGMA.iter2 (fun x a b -> f (CVar x) a b) s.vars t.vars ;
        ALLOC.iter2 (fun x a b -> f (CAlloc x) a b) s.alloc t.alloc ;
        M.Sigma.iter2 (fun m p q -> f (CMem m) p q) s.mem t.mem ;
      end

    let domain_partition r =
      begin
        let xs = ref HEAP.Set.empty in
        let ts = ref TALLOC.Set.empty in
        let ms = ref M.Heap.Set.empty in
        Heap.Set.iter
          (function
            | CVar x -> xs := HEAP.Set.add x !xs
            | CAlloc x -> ts := TALLOC.Set.add x !ts
            | CMem c -> ms := M.Heap.Set.add c !ms
          ) r ;
        !xs , !ts , !ms
      end

    let domain_var xs =
      HEAP.Set.fold (fun x s -> Heap.Set.add (CVar x) s) xs Heap.Set.empty

    let domain_alloc ts =
      TALLOC.Set.fold (fun x s -> Heap.Set.add (CAlloc x) s) ts Heap.Set.empty

    let domain_mem ms =
      M.Heap.Set.fold (fun m s -> Heap.Set.add (CMem m) s) ms Heap.Set.empty

    let assigned s1 s2 w =
      let w_vars , w_alloc , w_mem = domain_partition w in
      let h_vars = SIGMA.assigned s1.vars s2.vars w_vars in
      let h_alloc = ALLOC.assigned s1.alloc s2.alloc w_alloc in
      let h_mem = M.Sigma.assigned s1.mem s2.mem w_mem in
      Bag.ulist [h_vars;h_alloc;h_mem]

    let havoc s r =
      let rvar , ralloc , rmem = domain_partition r
      in {
        vars = SIGMA.havoc s.vars rvar ;
        alloc = ALLOC.havoc s.alloc ralloc ;
        mem = M.Sigma.havoc s.mem rmem ;
      }

    let havoc_chunk s = function
      | CVar x -> { s with vars = SIGMA.havoc_chunk s.vars x }
      | CAlloc x -> { s with alloc = ALLOC.havoc_chunk s.alloc x }
      | CMem m -> { s with mem = M.Sigma.havoc_chunk s.mem m }

    let havoc_any ~call s = {
      alloc = s.alloc ;
      vars = SIGMA.havoc_any ~call s.vars ;
      mem = M.Sigma.havoc_any ~call s.mem ;
    }

    let domain s =
      Heap.Set.union
        (Heap.Set.union
           (domain_var (SIGMA.domain s.vars))
           (domain_alloc (ALLOC.domain s.alloc)))
        (domain_mem (M.Sigma.domain s.mem))

    let pretty fmt s =
      Format.fprintf fmt "@[<hov 2>{X:@[%a@]@ T:@[%a@]@ M:@[%a@]}@]"
        SIGMA.pretty s.vars
        ALLOC.pretty s.alloc
        M.Sigma.pretty s.mem

  end

  let get_var s x = SIGMA.get s.vars x
  let get_term s x = Lang.F.e_var (get_var s x)

  (* -------------------------------------------------------------------------- *)
  (* ---  Location                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  type mem =
    | CVAL (* By-Value variable *)
    | CREF (* By-Ref variable *)
    | CTXT (* In-context pointer *)
    | CARR (* In-context array *)
    | HEAP (* In-heap variable *)


  type loc =
    | LRef of varinfo
    | LVal of mem * varinfo * ofs list (* The varinfo has {i not} been contextualized yet *)
    | LLoc of M.loc (* Generalized In-Heap pointer *)

  and ofs =
    | OField of fieldinfo
    | OShift of c_object * Lang.F.term

  type segment = loc rloc

  let rec ofs_vars xs = function
    | [] -> xs
    | OField _ :: ofs -> ofs_vars xs ofs
    | OShift (_, k) :: ofs -> ofs_vars (Lang.F.Vars.union xs (F.vars k)) ofs

  let vars = function
    | LRef _ -> Lang.F.Vars.empty
    | LLoc l -> M.vars l
    | LVal (_, _, ofs) -> ofs_vars Lang.F.Vars.empty ofs

  let rec ofs_occurs x = function
    | [] -> false
    | OField _ :: ofs -> ofs_occurs x ofs
    | OShift (_, k) :: ofs -> Lang.F.Vars.mem x (F.vars k) || ofs_occurs x ofs

  let occurs x = function
    | LRef _ -> false
    | LLoc l -> M.occurs x l
    | LVal (_, _, ofs) -> ofs_occurs x ofs

  (* -------------------------------------------------------------------------- *)
  (* ---  Variable and Context                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let vtype m x =
    match m with
    | CVAL | HEAP -> x.vtype
    | CTXT | CREF -> Cil.typeOf_pointed x.vtype
    | CARR -> Ast_info.array_type (Cil.typeOf_pointed x.vtype)

  let vobject m x = Ctypes.object_of (vtype m x)

  let vbase m x =
    match m with
    | CVAL | HEAP -> x
    | _ -> { x with vglob = true ; vtype = vtype m x }

  (* -------------------------------------------------------------------------- *)
  (* ---  Pretty                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let rec pp_offset ~obj fmt = function
    | [] -> ()
    | OField f :: ofs ->
      Format.fprintf fmt ".%s" f.fname ;
      pp_offset ~obj:(object_of f.ftype) fmt ofs
    | OShift (elt, k) :: ofs ->
      if Ctypes.is_array obj ~elt then
        ( Format.fprintf fmt ".(%a)" F.pp_term k ;
          pp_offset ~obj:elt fmt ofs )
      else
        ( Format.fprintf fmt ".(%a : %a)" F.pp_term k Ctypes.pretty elt ;
          pp_offset ~obj:elt fmt ofs )

  let pp_mem fmt = function
    | CVAL -> Format.pp_print_string fmt "var"
    | CREF -> Format.pp_print_string fmt "ref"
    | CTXT -> Format.pp_print_string fmt "ptr"
    | CARR -> Format.pp_print_string fmt "arr"
    | HEAP -> Format.pp_print_string fmt "mem"

  let pp_var_model fmt = function (* re-uses strings that are used into the description of -wp-xxx-vars *)
    | ByValue | NotUsed -> Format.pp_print_string fmt "non-aliased" (* cf.  -wp-unalias-vars *)
    | ByRef -> Format.pp_print_string fmt "by reference" (* cf. -wp-ref-vars *)
    | InContext | InArray -> Format.pp_print_string fmt "in an isolated context" (* cf. -wp-context-vars *)
    | InHeap -> Format.pp_print_string fmt "aliased" (* cf. -wp-alias-vars *)

  let pretty fmt = function
    | LRef x -> VAR.pretty fmt x
    | LLoc l -> M.pretty fmt l
    | LVal (m, x, ofs) ->
      let obj = vobject m x in
      Format.fprintf fmt "@[%a:%a%a@]"
        pp_mem m VAR.pretty x
        (pp_offset ~obj) ofs

  let noref ~op var =
    Warning.error
      "forbidden %s variable '%a' considered %a.@\n\
       Use model 'Typed' instead or specify '-wp-unalias-vars %a'"
      op Varinfo.pretty var
      pp_var_model (V.param var)
      Varinfo.pretty var

  (* -------------------------------------------------------------------------- *)
  (* ---  Basic Constructors                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let null = LLoc M.null

  let literal ~eid cst = LLoc (M.literal ~eid cst)

  let cvar x = match V.param x with
    | NotUsed | ByValue -> LVal (CVAL, x, [])
    | InHeap -> LVal (HEAP, x, [])
    | InContext | InArray | ByRef -> LRef x

  (* -------------------------------------------------------------------------- *)
  (* ---  Lifting                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let moffset l = function
    | OField f -> M.field l f
    | OShift (e, k) -> M.shift l e k

  let mseq_of_seq seq = { pre = seq.pre.mem ; post = seq.post.mem }

  let mloc_of_path m x ofs =
    List.fold_left moffset (M.cvar (vbase m x)) ofs

  let mloc_of_loc = function
    | LLoc l -> l
    | LRef x -> M.cvar x
    | LVal (m, x, ofs) -> mloc_of_path m x ofs

  let pointer_loc p = LLoc (M.pointer_loc p)
  let pointer_val l = M.pointer_val (mloc_of_loc l)

  let field l f = match l with
    | LLoc l -> LLoc (M.field l f)
    | LRef x -> noref ~op:"field access to" x
    | LVal (m, x, ofs) -> LVal (m, x, ofs @ [ OField f ])

  let rec ofs_shift obj k = function
    | [] -> [ OShift (obj, k) ]
    | [ OShift (elt, i) ] when Ctypes.equal obj elt ->
      [ OShift (elt, F.e_add i k) ]
    | f :: ofs -> f :: ofs_shift obj k ofs

  let shift l obj k = match l with
    | LLoc l -> LLoc (M.shift l obj k)
    | LRef x -> noref ~op:"array access to" x
    | LVal (m, x, ofs) -> LVal (m, x, ofs_shift obj k ofs)

  let base_addr = function
    | LLoc l -> LLoc (M.base_addr l)
    | LRef x -> noref ~op:"base address of" x (* ??? ~suggest:ByValue *)
    | LVal (m, x, _) -> LVal (m, x, [])

  let block_length sigma obj = function
    | LLoc l -> M.block_length sigma.mem obj l
    | LRef x -> noref ~op:"block-length of" x
    | LVal (m, x, _) ->
      let obj = Ctypes.object_of (vtype m x) in
      let size =
        if Ctypes.sizeof_defined obj
        then Ctypes.sizeof_object obj
        else if Wp_parameters.ExternArrays.get ()
        then max_int
        else Warning.error ~source:"MemVar" "Unknown array-size"
      in F.e_int size

  let cast obj l = LLoc (M.cast obj (mloc_of_loc l))
  let loc_of_int e a = LLoc (M.loc_of_int e a)
  let int_of_loc i l = M.int_of_loc i (mloc_of_loc l)

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Load                                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let rec access a = function
    | [] -> a
    | OField f :: ofs -> access (Lang.F.e_getfield a (Cfield f)) ofs
    | OShift (_, k) :: ofs -> access (Lang.F.e_get a k) ofs

  let rec update a ofs v = match ofs with
    | [] -> v
    | OField f :: ofs ->
      let phi = Cfield f in
      let a_f = F.e_getfield a phi in
      let a_f_v = update a_f ofs v in
      F.e_setfield a phi a_f_v
    | OShift (_, k) :: ofs ->
      let a_k = F.e_get a k in
      let a_k_v = update a_k ofs v in
      F.e_set a k a_k_v

  let load sigma obj = function
    | LRef x ->
      begin match V.param x with
        | ByRef -> Memory.Loc (LVal (CREF, x, []))
        | InContext -> Memory.Loc (LVal (CTXT, x, []))
        | InArray -> Memory.Loc (LVal (CARR, x, []))
        | InHeap | NotUsed | ByValue -> assert false
      end
    | LVal ((CREF | CVAL), x, ofs) ->
      Memory.Val (access (get_term sigma x) ofs)
    | LLoc l ->
      Cvalues.map_value
        (fun l -> LLoc l)
        (M.load sigma.mem obj l)
    | LVal((CTXT|CARR|HEAP) as m,x,ofs) ->
      Cvalues.map_value
        (fun l -> LLoc l)
        (M.load sigma.mem obj (mloc_of_path m x ofs))

  (* -------------------------------------------------------------------------- *)
  (* ---  Memory Store                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let stored seq obj l v = match l with
    | LRef x -> noref ~op:"write to" x
    | LVal ((CREF | CVAL), x, ofs) ->
      let v1 = get_term seq.pre x in
      let v2 = get_term seq.post x in
      [ F.p_equal v2 (update v1 ofs v) ]
    | LVal ((CTXT | CARR | HEAP) as m, x, ofs) ->
      M.stored (mseq_of_seq seq) obj (mloc_of_path m x ofs) v
    | LLoc l ->
      M.stored (mseq_of_seq seq) obj l v

  let copied seq obj l1 l2 =
    let v = match load seq.pre obj l2 with
      | Memory.Val r -> r
      | Memory.Loc l -> pointer_val l
    in stored seq obj l1 v

  (* -------------------------------------------------------------------------- *)
  (* ---  Pointer Comparison                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let is_null = function
    | LLoc l -> M.is_null l
    | LRef _
    | LVal _ -> F.p_false

  let rec offset = function
    | [] -> Lang.F.e_zero
    | OField f :: ofs ->
      Lang.F.e_add (Lang.F.e_int (Ctypes.field_offset f)) (offset ofs)
    | OShift (obj, k)::ofs ->
      Lang.F.e_add (Lang.F.e_fact (Ctypes.sizeof_object obj) k) (offset ofs)

  let loc_diff obj a b =
    match a, b with
    | LLoc l1, LLoc l2 -> M.loc_diff obj l1 l2
    | LRef x, LRef y when Varinfo.equal x y -> Lang.F.e_zero
    | LVal (_, x, p), LVal (_, y, q) when Varinfo.equal x y ->
      Lang.F.e_div
        (Lang.F.e_sub (offset p) (offset q))
        (Lang.F.e_int (Ctypes.sizeof_object obj))
    | _ ->
      Warning.error ~source:"Reference Variable Model"
        "Uncomparable locations %a and %a" pretty a pretty b

  let loc_compare lcmp icmp same a b =
    match a, b with
    | LLoc l1, LLoc l2 -> lcmp l1 l2
    | LRef x, LRef y ->
      if Varinfo.equal x y then same else Lang.F.p_not same
    | LVal (_, x, p), LVal (_, y, q) ->
      if Varinfo.equal x y then icmp (offset p) (offset q)
      else Lang.F.p_not same
    | (LVal _ | LLoc _), (LVal _ | LLoc _) ->
      lcmp (mloc_of_loc a) (mloc_of_loc b)
    | LRef _, (LVal _ | LLoc _) | (LVal _ | LLoc _), LRef _ ->
      Lang.F.p_not same

  let loc_eq = loc_compare M.loc_eq F.p_equal F.p_true
  let loc_lt = loc_compare M.loc_lt F.p_lt F.p_false
  let loc_leq = loc_compare M.loc_leq F.p_leq F.p_true
  let loc_neq = loc_compare M.loc_neq F.p_neq F.p_false

  (* -------------------------------------------------------------------------- *)
  (* ---  Validity                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  exception ShiftMismatch

  type alloc = c_object * int

  let is_heap_allocated = function
    | CREF | CVAL -> false | HEAP | CTXT | CARR -> true

  let shift_mismatch l =
    Wp_parameters.fatal "Invalid shift : %a" pretty l

  let unsized_array () = Warning.error ~severe:false
      "Validity of unsized-array not implemented yet"

  (* Append conditions to [cond] for [range=(elt,a,b)],
     consiting of [a..b] elements with type [elt] to fits inside the block,
     provided [a<=b]. *)
  let rec fits cond (block,size) ((elt,a,b) as range) =
    if Ctypes.equal block elt then
      Lang.F.p_leq Lang.F.e_zero a :: Lang.F.p_lt b (Lang.F.e_int size) :: cond
    else
      match Ctypes.get_array block with
      | Some( e , Some n ) -> fits cond (e , n * size) range
      | Some( _ , None ) -> unsized_array ()
      | None -> raise ShiftMismatch

  (* Append conditions for [offset] to fits [object], provided [a<=b]. *)
  let rec offset_fits cond obj offset =
    match offset with
    | [] -> cond
    | OField fd :: ofs ->
      offset_fits cond (Ctypes.object_of fd.ftype) ofs
    | OShift (te, k) :: ofs ->
      match Ctypes.get_array obj with
      | Some( e , Some n ) when Ctypes.equal e te ->
        let cond =
          Lang.F.p_leq Lang.F.e_zero k :: Lang.F.p_lt k (Lang.F.e_int n) :: cond
        in
        offset_fits cond e ofs
      | Some( _ , None ) -> unsized_array ()
      | _ -> offset_fits (fits cond (obj,1) (te,k,k)) te ofs

  (* Append conditions to [cond] for [range=(elt,a,b)], starting at [offset],
     consiting of [a..b] elements with type [elt] to fits inside the block,
     provided [a<=b]. *)
  let rec range_fits cond alloc offset ((elt,a,b) as range) =
    match offset with
    | [] -> fits cond alloc range
    | OField fd :: ofs ->
      range_fits cond (Ctypes.object_of fd.ftype,1) ofs range
    | OShift (te, k) :: ofs ->
      if Ctypes.equal te elt then
        range_fits cond alloc ofs (elt,Lang.F.e_add a k,Lang.F.e_add b k)
      else
        match Ctypes.get_array (fst alloc) with
        | Some( e , Some n ) when Ctypes.equal e te ->
          let cond =
            Lang.F.p_leq Lang.F.e_zero k
            :: Lang.F.p_lt k (Lang.F.e_int n)
            :: cond
          in
          range_fits cond (e,n) ofs range
        | Some( _ , None ) -> unsized_array ()
        | _ ->
          range_fits (fits cond alloc (te,k,k)) (te,1) ofs range

  let valid_offset obj ofs =
    F.p_conj (offset_fits [] obj ofs )

  let valid_range obj ofs range =
    F.p_conj (range_fits [] (obj,1) ofs range)

  (* varinfo *)

  let valid_base sigma acs mem x =
    if x.vglob then
      if acs = RW && Cil.typeHasQualifier "const" x.vtype
      then Lang.F.p_false
      else Lang.F.p_true
    else
      match mem with
      | CVAL | HEAP -> Lang.F.p_bool (ALLOC.value sigma.alloc x)
      | CREF | CTXT | CARR -> Lang.F.p_true

  (* segment *)

  let valid_offset_path sigma acs mem x ofs =
    Lang.F.p_and
      (valid_base sigma acs mem x)
      (valid_offset (vobject mem x) ofs)

  let valid_range_path sigma acs mem x ofs rg =
    Lang.F.p_and
      (valid_base sigma acs mem x)
      (valid_range (vobject mem x) ofs rg)

  (* in-model validation *)

  let valid sigma acs = function
    | Rloc(obj,l) ->
      begin match l with
        | LRef _ -> Lang.F.p_true
        | LLoc l -> M.valid sigma.mem acs (Rloc(obj,l))
        | LVal (m, x, p) ->
          try valid_offset_path sigma acs m x p
          with ShiftMismatch ->
            if is_heap_allocated m then
              M.valid sigma.mem acs (Rloc(obj,mloc_of_loc l))
            else
              shift_mismatch l
      end
    | Rrange(l,elt,a,b) ->
      begin match l with
        | LRef x -> noref ~op:"valid sub-range of" x
        | LLoc l -> M.valid sigma.mem acs (Rrange (l, elt, a, b))
        | LVal (m, x, p) ->
          match a,b with
          | Some ka,Some kb ->
            begin
              try
                F.p_imply (F.p_leq ka kb)
                  (valid_range_path sigma acs m x p (elt,ka,kb))
              with ShiftMismatch ->
                if is_heap_allocated m then
                  let l = mloc_of_loc l in
                  M.valid sigma.mem acs (Rrange(l,elt,a,b))
                else shift_mismatch l
            end
          | _ ->
            Warning.error "Validity of infinite range @[%a.(%a..%a)@]"
              pretty l Vset.pp_bound a Vset.pp_bound b
      end

  (* -------------------------------------------------------------------------- *)
  (* ---  Scope                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let is_mem x = match V.param x with
    | InHeap -> true
    | _ -> false

  let is_mvar_alloc x =
    match V.param x with
    | ByRef | InContext | InArray | NotUsed -> false
    | ByValue | InHeap -> true

  let alloc_var ta xs v =
    TALLOC.Set.fold
      (fun x hs -> Lang.F.p_equal (ALLOC.value ta x) v :: hs)
      xs []

  let allocates ta_out xs valid (* of introduced variables *) =
    let xs = List.filter (fun x -> is_mvar_alloc x) xs in
    if xs = [] then ta_out , []
    else
      let xs_all = List.fold_right TALLOC.Set.add xs TALLOC.Set.empty in
      let ta_in = ALLOC.havoc ta_out xs_all in
      let h_out =
        alloc_var
          ta_out
          xs_all
          (if valid then Lang.F.e_false else Lang.F.e_true)
      in
      let h_in =
        alloc_var ta_in xs_all (if valid then Lang.F.e_true else Lang.F.e_false)
      in
      ta_in , h_in @ h_out

  let framed sigma =
    let pool = ref [] in
    SIGMA.iter
      (fun x p ->
         if (x.vglob || x.vformal) && Cil.isPointerType (VAR.typ_of_chunk x)
         then pool := M.global sigma.mem (Lang.F.e_var p) :: !pool
      ) sigma.vars ;
    !pool

  let scope_vars sigma sc xs =
    match sc with
    | Mcfg.SC_Global | Mcfg.SC_Function_in -> sigma.alloc , framed sigma
    | Mcfg.SC_Function_frame | Mcfg.SC_Block_in ->
      allocates sigma.alloc xs false
    | Mcfg.SC_Function_out | Mcfg.SC_Block_out -> allocates sigma.alloc xs true

  let scope sigma sc xs =
    let xmem = List.filter is_mem xs in
    let smem , hmem = M.scope sigma.mem sc xmem in
    let ta , hvars = scope_vars sigma sc xs in
    { vars = sigma.vars ; alloc = ta ; mem = smem } , hvars @ hmem

  let global sigma p = M.global sigma.mem p

  (* -------------------------------------------------------------------------- *)
  (* ---  Havoc allong a ranged-path                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let rec assigned_path
      (hs : Lang.F.pred list) (* collector of properties *)
      (xs : Lang.F.var list)  (* variable quantifying the assigned location *)
      (ys : Lang.F.var list)  (* variable quantifying others locations *)
      (a : Lang.F.term)  (* pre-term for root + current offset *)
      (b : Lang.F.term)  (* post-term for root + current offset *)
    = function
      | [] -> hs

      (*TODO: optimized version for terminal [Field _] and [Index _] *)

      | OField f :: ofs ->
        let cf = Cfield f in
        let af = Lang.F.e_getfield a cf in
        let bf = Lang.F.e_getfield b cf in
        let hs = assigned_path hs xs ys af bf ofs in
        List.fold_left
          (fun hs g ->
             if Fieldinfo.equal f g then hs else
               let cg = Cfield g in
               let ag = Lang.F.e_getfield a cg in
               let bg = Lang.F.e_getfield b cg in
               let eqg = Lang.F.p_forall ys (Lang.F.p_equal ag bg) in
               eqg :: hs
          ) hs f.fcomp.cfields

      | OShift (_, e) :: ofs ->
        let y = Lang.freshvar ~basename:"k" Qed.Logic.Int in
        let k = Lang.F.e_var y in
        let ak = Lang.F.e_get a k in
        let bk = Lang.F.e_get b k in
        if List.exists (fun x -> F.occurs x e) xs then
          (* index [e] is covered by [xs]:
             must explore deeper the remaining path. *)
          assigned_path hs xs (y::ys) ak bk ofs
        else
          (* index [e] is not covered by [xs]:
             any indice different from e is disjoint.
             explore also deeply with index [e]. *)
          let ae = Lang.F.e_get a e in
          let be = Lang.F.e_get b e in
          let ek = Lang.F.p_neq e k in
          let eqk =
            Lang.F.p_forall (y::ys) (Lang.F.p_imply ek (Lang.F.p_equal ak bk))
          in
          assigned_path (eqk :: hs) xs ys ae be ofs

  let assigned_descr s xs mem x ofs p =
    let valid = valid_offset_path s.post Memory.RW mem x ofs in
    let a = get_term s.pre x in
    let b = get_term s.post x in
    let a_ofs = access a ofs in
    let b_ofs = access b ofs in
    let p_sloc =
      Lang.F.p_forall xs
        (Lang.F.p_hyps [valid;Lang.F.p_not p] (Lang.F.p_equal a_ofs b_ofs))
    in
    assigned_path [p_sloc] xs [] a b ofs

  (* -------------------------------------------------------------------------- *)
  (* ---  Assigned                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let assigned_loc seq obj = function
    | LRef x -> noref ~op:"assigns to" x
    | LVal ((CVAL | CREF), _, []) -> [] (* full update *)
    | LVal ((CVAL | CREF), _, _) as vloc ->
      let v = Lang.freshvar ~basename:"v" (Lang.tau_of_object obj) in
      stored seq obj vloc (Lang.F.e_var v)
    | LVal ((HEAP | CTXT | CARR) as m, x, ofs) ->
      M.assigned (mseq_of_seq seq) obj (Sloc (mloc_of_path m x ofs))
    | LLoc l ->
      M.assigned (mseq_of_seq seq) obj (Sloc l)

  let assigned_array seq obj l elt n =
    match l with
    | LRef x -> noref ~op:"assigns to" x
    | LVal ((CVAL | CREF), _, []) -> [] (* full update *)
    | LVal ((CVAL | CREF), _, _) as vloc ->
      let te = Lang.tau_of_object elt in
      let v = Lang.freshvar ~basename:"v" Qed.Logic.(Array(Int,te)) in
      stored seq obj vloc (Lang.F.e_var v)
    | LVal ((HEAP | CTXT | CARR) as m, x, ofs) ->
      let l = mloc_of_path m x ofs in
      M.assigned (mseq_of_seq seq) obj (Sarray(l,elt,n))
    | LLoc l ->
      M.assigned (mseq_of_seq seq) obj (Sarray(l,elt,n))

  let assigned_range seq obj l elt a b =
    match l with
    | LRef x -> noref ~op:"assigns to" x
    | LLoc l ->
      M.assigned (mseq_of_seq seq) obj (Srange(l,elt,a,b))
    | LVal ((HEAP | CTXT | CARR) as m, x, ofs) ->
      M.assigned (mseq_of_seq seq) obj (Srange(mloc_of_path m x ofs,elt,a,b))
    | LVal ((CVAL | CREF) as m, x, ofs) ->
      let k = Lang.freshvar ~basename:"k" Qed.Logic.Int in
      let p = Vset.in_range (Lang.F.e_var k) a b in
      let ofs = ofs_shift elt (Lang.F.e_var k) ofs in
      assigned_descr seq [k] m x ofs p

  let assigned_descr seq obj xs l p =
    match l with
    | LRef x -> noref ~op:"assigns to" x
    | LLoc l ->
      M.assigned (mseq_of_seq seq) obj (Sdescr(xs,l,p))
    | LVal ((HEAP | CTXT | CARR) as m, x, ofs) ->
      M.assigned (mseq_of_seq seq) obj (Sdescr(xs,mloc_of_path m x ofs,p))
    | LVal ((CVAL | CREF) as m, x, ofs) ->
      assigned_descr seq xs m x ofs p

  let assigned seq obj = function
    | Sloc l -> assigned_loc seq obj l
    | Sarray(l,elt,n) -> assigned_array seq obj l elt n
    | Srange(l,elt,a,b) -> assigned_range seq obj l elt a b
    | Sdescr(xs,l,p) -> assigned_descr seq obj xs l p

  (* -------------------------------------------------------------------------- *)
  (* --- Segments                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  type seq =
    | Rseg of varinfo
    | Fseg of varinfo * delta list
    | Mseg of M.loc rloc * varinfo * delta list
    | Lseg of M.loc rloc
  and delta =
    | Dfield of fieldinfo
    | Drange of Lang.F.term option * Lang.F.term option

  let dofs = function
    | OField f -> Dfield f
    | OShift (_, k) -> let u = Some k in Drange(u,u)

  let delta ofs = List.map dofs ofs

  let rec range ofs obj a b =
    match ofs with
    | [] -> [ Drange(a,b) ]
    | [ OShift (elt, k) ] when Ctypes.equal elt obj ->
      [ Drange (Vset.bound_shift a k, Vset.bound_shift b k) ]
    | d :: ofs -> dofs d :: range ofs obj a b

  let dsize s = Drange(Some (Lang.F.e_int 0) , Some (Lang.F.e_int (s-1)))
  let rsize ofs s = delta ofs @ [ dsize s ]

  let locseg = function
    | Rloc (_,LRef x) -> Rseg x
    | Rrange (LRef x, _, _, _) -> noref ~op:"sub-range of" x

    | Rloc (obj, LLoc l) -> Lseg (Rloc (obj, l))
    | Rloc (_, LVal ((CVAL | CREF), x, ofs)) -> Fseg (x, delta ofs)

    | Rrange (LLoc l, obj, a, b) -> Lseg (Rrange (l, obj, a, b))
    | Rrange (LVal ((CVAL | CREF), x, ofs), obj, a, b) ->
      Fseg (x, range ofs obj a b)

    (* in M: *)
    | Rloc (obj, LVal ((CTXT | CARR | HEAP) as m, x, ofs)) ->
      Mseg (Rloc (obj, mloc_of_path m x ofs), x, delta ofs)
    | Rrange (LVal ((CTXT | CARR | HEAP) as m, x, ofs), obj, a, b) ->
      Mseg (Rrange (mloc_of_path m x ofs, obj, a, b), x, range ofs obj a b)

  (* -------------------------------------------------------------------------- *)
  (* ---  Segment Inclusion                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  let rec included_delta d1 d2 =
    match d1 , d2 with
    | _ , [] -> Lang.F.p_true
    | [] , _ -> Lang.F.p_false
    | u :: d1 , v :: d2 ->
      match u , v with
      | Dfield f , Dfield g when Fieldinfo.equal f g ->
        included_delta d1 d2
      | Dfield _ , _ | _ , Dfield _ -> Lang.F.p_false
      | Drange(a1,b1) , Drange(a2,b2) ->
        Lang.F.p_conj [ Vset.ordered ~strict:false ~limit:true a2 a1 ;
                 Vset.ordered ~strict:false ~limit:true b1 b2 ;
                 included_delta d1 d2 ]

  let included s1 s2 =
    match locseg s1 , locseg s2 with
    | Rseg x , Rseg y ->
      if Varinfo.equal x y then Lang.F.p_true else Lang.F.p_false
    | Rseg _ , _ | _ , Rseg _ -> Lang.F.p_false

    | Fseg(x1,d1) , Fseg(x2,d2)
    | Mseg(_,x1,d1) , Mseg(_,x2,d2) ->
      if Varinfo.equal x1 x2 then included_delta d1 d2 else Lang.F.p_false

    | Fseg _ , _ | _ , Fseg _ -> Lang.F.p_false

    | (Lseg s1|Mseg(s1,_,_)) , (Lseg s2|Mseg(s2,_,_)) -> M.included s1 s2

  (* -------------------------------------------------------------------------- *)
  (* ---  Segment Separation                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let rec separated_delta d1 d2 =
    match d1 , d2 with
    | [] , _ | _ , [] -> Lang.F.p_false
    | u :: d1 , v :: d2 ->
      match u , v with
      | Dfield f , Dfield g when Fieldinfo.equal f g
        -> separated_delta d1 d2
      | Dfield _ , _ | _ , Dfield _ -> Lang.F.p_true
      | Drange(a1,b1) , Drange(a2,b2) ->
        Lang.F.p_disj [ Vset.ordered ~strict:true ~limit:false b1 a2 ;
                 Vset.ordered ~strict:true ~limit:false b2 a1 ;
                 separated_delta d1 d2 ]

  let separated r1 r2 =
    match locseg r1 , locseg r2 with
    | Rseg x , Rseg y ->
      if Varinfo.equal x y then Lang.F.p_false else Lang.F.p_true
    | Rseg _ , _ | _ , Rseg _ -> Lang.F.p_true

    | Fseg(x1,d1) , Fseg(x2,d2)
    | Mseg(_,x1,d1) , Mseg(_,x2,d2) ->
      if Varinfo.equal x1 x2 then separated_delta d1 d2 else Lang.F.p_true
    | Fseg _ , _ | _ , Fseg _ -> Lang.F.p_true

    | (Lseg s1|Mseg(s1,_,_)) , (Lseg s2|Mseg(s2,_,_)) -> M.separated s1 s2

  (* -------------------------------------------------------------------------- *)
  (* ---  Domain                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let domain obj l =
    match l with
    | LRef x | LVal ((CVAL | CREF), x, _) ->
      Heap.Set.singleton (CVar x)
    | LLoc _ | LVal ((CTXT | CARR | HEAP), _, _) ->
      M.Heap.Set.fold
        (fun m s -> Heap.Set.add (CMem m) s)
        (M.domain obj (mloc_of_loc l)) Heap.Set.empty

  (* -------------------------------------------------------------------------- *)

end
