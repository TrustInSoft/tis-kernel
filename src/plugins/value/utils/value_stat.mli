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

(** For performance debugging purpose. See end of file. *)
val n_other: int

(*
 *  Final Statistics
 *  ----------------
 *
 * These statistics are used by the GUI.
 *)

(** Statement data. *)
type statement_data = private {
  (** Overhead analysis time. *)
  mutable s_overhead : float;
  (** Total analysis time. *)
  mutable s_total : float;
}

(** Function data. *)
type function_data = private {
  (** Number of analyzed calls. *)
  mutable f_count : int;
  (** Overhead analysis time. *)
  mutable f_overhead : float;
  (** For performance debugging purpose. See end of file. *)
  mutable f_other : (int * float) array;
  (** Total analysis time. *)
  mutable f_total : float;
  (** Memexec analysis time. *)
  mutable f_memexec_time : float;
  (** Number of memexec hits (analysis skipped). *)
  mutable f_memexec_hit : int;
  (** Total analysis time for all statements. *)
  mutable f_stmt_all : float;
  (** Total analysis time for call statements only. *)
  mutable f_stmt_call : float;
}

(** Final consolidated statement statistics. *)
module Statement : State_builder.Hashtbl
  with type key = stmt
   and type data = statement_data

(** Final consolidated function statistics. *)
module Function : State_builder.Hashtbl
  with type key = kernel_function
   and type data = function_data

(** Final flamegraph informations. *)
module Flamegraph : State_builder.Option_ref
  with type data = kernel_function Statistics.flamegraph


(*
 *  Measurement API
 *  ---------------
 *
 * These measurement functions are meant to be used by Value and have
 * very low overhead (except [process]).
 *)

(** [measure_entry_point kf thunk] starts statistics and measures
    [thunk] for [kf].
*)
val measure_entry_point : kernel_function -> (unit -> 'a) -> 'a

(** [measure_function kf thunk] measures [thunk] for [kf]. *)
val measure_function : kernel_function -> (unit -> 'a) -> 'a

(** [measure_memexec thunk] measures [thunk] as memexec. *)
val measure_memexec : (unit -> 'a) -> 'a

(** [measure_memexec_hit ()] increments memexec hit. *)
val measure_memexec_hit : unit -> unit

(** [measure_builtin thunk] measures [thunk] as builtin. *)
val measure_builtin : (unit -> 'a) -> 'a

(** [measure_other slot thunk] measures [thunk] for [slot].
    For performance debugging purpose. See end of file. *)
val measure_other : int -> (unit -> 'a) -> 'a

(** [measure_statement stmt thunk] measures [thunk] as [stmt]. *)
val measure_statement : stmt -> (unit -> 'a) -> 'a

(** [process ()] ends statistics and fills the [Statement] and
    [Function] modules accordingly.
*)
val process : unit -> unit


(*

  How to debug time performance issues
  ------------------------------------

  First, you need to decide how many calls you want to measure. For
  example, let's say you want to measure 5 calls. You must set the
  n_other variable in value_stat.ml to this number. In our example,
  you would update the existing line:

      let n_other = 0

  To:

      let n_other = 5

  Then, you can wrap the suspicious calls with unique slot identifier
  between 0 and `n_other - 1`. In our example, a slot identifier is a
  number between 0 and 4. For example, if you want to measure the
  following call in the third slot:

      f x y z

  You would change the call to:

      Value_stat.measure_other 2 (fun () -> f x y z)

  Finally, after you compiled everything and run your example showing
  the problem, you can view the results with the following script:

      let dump name =
        let open Value.Value_stat in
        let x = Function.find (Globals.Functions.find_by_name name) in
        Format.printf "%16s %d %.3f %.3f"
          name x.f_count x.f_overhead (x.f_total -. x.f_overhead);
        for i = 0 to n_other - 1 do
          let y, z = x.f_other.(i) in
          Format.printf " %d:%d:%.3f" i y z;
        done;
        Format.printf "@."

      let main () =
        dump "C_function_1_showing_the_problem";
        dump "C_function_2_showing_the_problem";
        dump "C_function_3_showing_the_problem";
      ;;

      let () = Db.Main.extend main

  And the following command:

      tis-kernel -load save.state -load-script script.ml

*)
