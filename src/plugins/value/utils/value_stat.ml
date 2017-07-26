(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*  TrustInSoft Kernel is a fork of Frama-C. All the differences are:     *)
(*    Copyright (C) 2014-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

type key =
  | Kernel_function of kernel_function
  | Stmt of stmt
  | Mem_exec of bool
  | Builtin
  | Other of int

let n_other = 0

let i_other i c t =
  let x = Array.make n_other (0, 0.) in
  x.(i) <- (c, t); x

module Key = struct
  include Datatype.Make_with_collections(struct
      include Datatype.Serializable_undefined
      type t = key
      let name = "key"
      let reprs =
        List.fold_left (fun acc kf -> Kernel_function kf :: acc)
          (List.fold_left (fun acc s -> Stmt s :: acc)
             [Mem_exec true; Mem_exec false; Builtin]
             Stmt.reprs)
          Kernel_function.reprs
      let structural_descr =
        let open Structural_descr in
        t_sum [| [| Kernel_function.packed_descr |];
                 [| Stmt.packed_descr |];
                 [| p_bool |];
                 (* Builtin *)
                 [| p_int |];
              |]
      let equal = Datatype.from_compare
      let compare x y =
        match x, y with
        | Kernel_function kf1, Kernel_function kf2 ->
          Kernel_function.compare kf1 kf2
        | Kernel_function _, _ -> -1
        | _, Kernel_function _ -> 1
        | Stmt s1, Stmt s2 -> Stmt.compare s1 s2
        | Stmt _, _ -> -1
        | _, Stmt _ -> 1
        | Mem_exec b1, Mem_exec b2 -> Pervasives.compare b1 b2
        | Mem_exec _, _ -> -1
        | _, Mem_exec _ -> 1
        | Other i1, Other i2 -> Pervasives.compare i1 i2
        | Other _, _ -> -1
        | _, Other _ -> 1
        | Builtin, Builtin -> 0
      let hash x =
        (* TODO: what would be a better hash function? *)
        match x with
        | Kernel_function kf -> 3 * Kernel_function.hash kf + 5
        | Stmt s -> 3 * Stmt.hash s + 4
        | Other i -> 3 * i + 3
        | Mem_exec false -> 2
        | Mem_exec true -> 1
        | Builtin -> 0
      let pretty fmt x =
        match x with
        | Kernel_function kf -> Kernel_function.pretty fmt kf
        | Stmt s -> Format.fprintf fmt "s%d" s.sid
        | Mem_exec false -> Format.fprintf fmt "m"
        | Mem_exec true -> Format.fprintf fmt "M"
        | Other i -> Format.fprintf fmt "I%d" i
        | Builtin -> Format.fprintf fmt "B"
    end)
  let size = 17
end

module Stat = Statistics.Make(Key)

type statement_data = {
  mutable s_overhead : float;
  mutable s_total : float;
}
module Statement_data = Datatype.Make(struct
    (* TODO: is this module correct? *)
    type t = statement_data
    let name = "statement_data"
    let reprs = [ { s_overhead = 0.;
                    s_total = 0.;
                  } ]
    let structural_descr =
      let open Structural_descr in
      t_record [| (* s_overhead *) p_float;
                                (* s_total *) p_float;
               |]
    let equal = Pervasives.(=)
    let compare = Pervasives.compare
    let hash = FCHashtbl.hash
    let copy { s_overhead; s_total; } = { s_overhead; s_total; }
    let rehash = Datatype.identity
    let internal_pretty_code prec_caller fmt x =
      Type.par prec_caller Type.Basic fmt (fun fmt ->
          Format.fprintf fmt "{ ";
          Format.fprintf fmt "s_overhead = %f; " x.s_overhead;
          Format.fprintf fmt "s_total = %f; " x.s_total;
          Format.fprintf fmt "}";
        )
    let pretty fmt x = internal_pretty_code Type.Basic fmt x
    let varname _x = "sd"
    let mem_project = Datatype.never_any_project
  end)
let statement_data_default () = {
  s_overhead = 0.;
  s_total = 0.;
}
let statement_data_zero = statement_data_default ()
let statement_data_incr x d =
  x.s_overhead <- x.s_overhead +. d.s_overhead;
  x.s_total <- x.s_total +. d.s_total;
;;

module Statement =
  Cil_state_builder.Stmt_hashtbl(Statement_data)
    (struct
      let name = "Value_stat.Statement"
      let size = 17
      let dependencies = [ Db.Value.self ]
    end)
let () = Ast.add_monotonic_state Statement.self

let statement_incr s delta =
  let old =
    try Statement.find s
    with Not_found ->
      let default = statement_data_default () in
      Statement.add s default;
      default
  in
  statement_data_incr old delta

type function_data = {
  mutable f_count : int;
  mutable f_overhead : float;
  mutable f_other : (int * float) array;
  mutable f_total : float;
  mutable f_memexec_time : float;
  mutable f_memexec_hit : int;
  mutable f_stmt_all : float;
  mutable f_stmt_call : float;
}
module Function_data = Datatype.Make(struct
    (* TODO: is this module correct? *)
    type t = function_data
    let name = "function_data"
    let reprs = [ { f_count = 0;
                    f_overhead = 0.;
                    f_other = Array.make n_other (0, 0.);
                    f_total = 0.;
                    f_memexec_time = 0.;
                    f_memexec_hit = 0;
                    f_stmt_all = 0.;
                    f_stmt_call = 0.;
                  } ]
    let structural_descr =
      let open Structural_descr in
      t_record [| p_int; (* f_count *)
                  p_float; (* f_overhead *)
                  pack (t_array (t_tuple [|p_int; p_float|])); (* f_other *)
                  p_float; (* f_total *)
                  p_float; (* f_memexec_time *)
                  p_int; (* f_memexec_hit *)
                  p_float; (* f_stmt_all *)
                  p_float; (* f_stmt_call *)
               |]
    let equal = Pervasives.(=)
    let compare = Pervasives.compare
    let hash = FCHashtbl.hash
    let copy { f_count; f_overhead; f_other; f_total; f_memexec_time;
               f_memexec_hit; f_stmt_all; f_stmt_call; }
    (* TODO: What is the semantics of copy? It did not unshare the
       pairs of the f_other array. *)
      = { f_count; f_overhead; f_other = Array.copy f_other; f_total;
          f_memexec_time; f_memexec_hit; f_stmt_all; f_stmt_call; }
    let rehash = Datatype.identity
    let internal_pretty_code prec_caller fmt x =
      Type.par prec_caller Type.Basic fmt (fun fmt ->
          Format.fprintf fmt "{ ";
          Format.fprintf fmt "f_count = %d; " x.f_count;
          Format.fprintf fmt "f_overhead = %f; " x.f_overhead;
          Format.fprintf fmt "f_other = %a; "
            (Pretty_utils.pp_array ~pre:"[| " ~sep:"; " ~suf:"|]"
               (fun fmt _ (x, y) -> Format.fprintf fmt "(%d, %f)" x y))
            x.f_other;
          Format.fprintf fmt "f_total = %f; " x.f_total;
          Format.fprintf fmt "f_memexec_time = %f; " x.f_memexec_time;
          Format.fprintf fmt "f_memexec_hit = %d; " x.f_memexec_hit;
          Format.fprintf fmt "f_stmt_all = %f; " x.f_stmt_all;
          Format.fprintf fmt "f_stmt_call = %f; " x.f_stmt_call;
          Format.fprintf fmt "}";
        )
    let pretty fmt x = internal_pretty_code Type.Basic fmt x
    let varname _x = "fd"
    let mem_project = Datatype.never_any_project
  end)
let function_data_default () = {
  f_count = 0;
  f_overhead = 0.;
  f_other = Array.make n_other (0, 0.);
  f_total = 0.;
  f_memexec_time = 0.;
  f_memexec_hit = 0;
  f_stmt_all = 0.;
  f_stmt_call = 0.;
}
let function_data_zero = function_data_default ()
let function_data_incr x d =
  x.f_count <- x.f_count + d.f_count;
  x.f_overhead <- x.f_overhead +. d.f_overhead;
  for i = 0 to Array.length x.f_other - 1 do
    let x1, x2 = x.f_other.(i) in
    let d1, d2 = d.f_other.(i) in
    x.f_other.(i) <- (x1 + d1, x2 +. d2);
  done;
  x.f_total <- x.f_total +. d.f_total;
  x.f_memexec_time <- x.f_memexec_time +. d.f_memexec_time;
  x.f_memexec_hit <- x.f_memexec_hit + d.f_memexec_hit;
  x.f_stmt_all <- x.f_stmt_all +. d.f_stmt_all;
  x.f_stmt_call <- x.f_stmt_call +. d.f_stmt_call;
;;

module Function =
  State_builder.Hashtbl(Kernel_function.Hashtbl)(Function_data)
    (struct
      let name = "Value_stat.Function"
      let size = 17
      let dependencies = [ Db.Value.self ]
    end)
let () = Ast.add_monotonic_state Function.self

let function_incr kf delta =
  let old =
    try Function.find kf
    with Not_found ->
      let default = function_data_default () in
      Function.add kf default;
      default
  in
  function_data_incr old delta

let measure_statement stmt thunk =
  Stat.measure (Stmt stmt) thunk

let measure_function kf thunk =
  Stat.measure (Kernel_function kf) thunk

let measure_memexec thunk =
  Stat.measure (Mem_exec false) thunk

let measure_memexec_hit () =
  Stat.measure (Mem_exec true) (fun () -> ())

let measure_builtin thunk =
  Stat.measure Builtin thunk

let measure_other i thunk =
  assert (0 <= i && i < n_other);
  Stat.measure (Other i) thunk

let measure_entry_point kf thunk =
  Stat.start (Value_parameters.ValStatistics.get ());
  measure_function kf thunk

module Flamegraph = State_builder.Option_ref
    (Statistics.Flamegraph(Kernel_function))
    (struct
      let name = "Value_stat.Flamegraph"
      let dependencies = [ Db.Value.self ]
    end)
let () = Ast.add_monotonic_state Flamegraph.self

let process () =
  let measures = Stat.stop () in
  if Stat.Measures.enabled measures
  then begin
    let keep = function
      | Kernel_function kf -> Some kf
      | Stmt _ | Mem_exec _ | Builtin | Other _ -> None
    in
    Flamegraph.set (Stat.Measures.flamegraph measures keep);
  end;
  let conv = Stat.Measures.conv measures in
  let stack = ref [] in
  let kf_stack = ref [] in  (* used to get the current kf *)
  let push key { Stat.count; Stat.self; } =
    stack := key :: !stack;
    match key with
    | Kernel_function kf ->
      kf_stack := kf :: !kf_stack;
      function_incr kf { function_data_zero with
                         f_count = count;
                         f_overhead = conv self;
                       };
    | Stmt s ->
      statement_incr s { statement_data_zero with s_overhead = conv self; };
    | Mem_exec b ->
      function_incr
        (List.hd !kf_stack)
        { function_data_zero with
          f_memexec_time = conv self;
          f_memexec_hit = if b then count else 0; }
    | Builtin -> ()
    | Other i ->
      function_incr
        (List.hd !kf_stack)
        { function_data_zero with
          f_other = i_other i count (conv self); };
  in
  let pop total =
    begin match List.hd !stack with
      | Kernel_function kf ->
        kf_stack := List.tl !kf_stack;
        function_incr kf { function_data_zero with f_total = conv total; };
      | Stmt s ->
        let total = conv total in
        statement_incr s { statement_data_zero with s_total = total; };
        function_incr
          (List.hd !kf_stack)
          { function_data_zero with
            f_stmt_all = total;
            f_stmt_call =
              (match s.skind with
               | Instr (Call _) -> total
               | _ -> 0.); };
      | Mem_exec _ -> ()
      | Builtin -> ()
      | Other _ -> ()
    end;
    stack := List.tl !stack;
  in
  Statement.clear ();
  Function.clear ();
  Stat.Measures.iter measures push pop;
  Statement.mark_as_computed ();
  Function.mark_as_computed ();
;;
