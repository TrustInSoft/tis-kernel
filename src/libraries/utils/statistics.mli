(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2014-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(* See end of file for a HOWTO. *)

type 'a flamegraph = ((int * 'a) list * float) list

module Flamegraph(Key : Datatype.S) :
  Datatype.S with type t = Key.t flamegraph

module type S = sig

  type key
  type data = private {
    mutable count : int;
    mutable self : int;
  }
  type status =
    | Broken
    | Stopped
    | Started of bool

  module Measures : sig

    type t

    (** [final measures] tells if [measures] are final or partial. *)
    val final : t -> bool

    (** [enabled measures] tells if [measures] were enabled. *)
    val enabled : t -> bool

    (** [conv measures] converts from RDTSC to seconds. *)
    val conv : t -> int -> float

    (** [iter measures push pop] iterates over [measures]. The
        function [push] is called each time we go deeper in the stack,
        while [pop] is called with the total time when we leave a
        frame.
        Does nothing if measurement were disabled.
    *)
    val iter : t -> (key -> data -> unit) -> (int -> unit) -> unit

    (** [fullprint measures] prints [measures] for reading.
        Does nothing if measurement were disabled.
    *)
    val fullprint : t -> Format.formatter -> unit

    (** [flamegraph measures keep] prints [measures] for FlameGraph
        keeping only a subset of keys according to [keep].
        Does nothing if measurement were disabled.
        FlameGraph: https://github.com/brendangregg/FlameGraph
    *)
    val flamegraph : t -> (key -> 'a option) -> 'a flamegraph

  end

  (** [status ()] returns the current status. The possible values are:
      [Broken]: Someone broke the API.
      [Stopped]: The measurement is stopped.
      [Started true]: The measurement is started and enabled.
      [Started false]: The measurement is started but disabled. *)
  val status : unit -> status

  (** [start enabled] starts the measurement.
      Requires [Stopped]. Sets [Started enabled]. *)
  val start : bool -> unit

  (** [measure key thunk] behaves as [thunk ()] (even if [Broken] or
      [Stopped]) and adds measurement to [key] if [Started true].
      Requires [Started _]. *)
  val measure : key -> (unit -> 'a) -> 'a

  (** [dump ()] dumps current measurements.
      Should not be called too often.
      Requires [Started _]. *)
  val dump : unit -> Measures.t

  (** [stop ()] stops the measurement and returns the final measures.
      Requires [Started _]. *)
  val stop : unit -> Measures.t

end

module type Key = sig
  type t
  module Hashtbl : sig
    type key
    type 'a t
    val create : int -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val find : 'a t -> key -> 'a
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val length : 'a t -> int
  end with type key = t
  val pretty : Format.formatter -> t -> unit
  val size : int
end

(** Create a measurement module from a Datatype with collections. *)
module Make(Key : Key) : S with type key = Key.t

module Helpers : sig

  (** [FastString] provides abstract keys:
      - internally represented as [int]
      - externally printed as [string]
  *)
  module FastString : sig
    include Key
    val register : string -> t
  end

  (** [to_file printer filename] writes the content of [printer] in
      [filename] (which is truncated if it exists).
      Returns [None] for success and [Some e] otherwise.
  *)
  val to_file : (Format.formatter -> unit) -> string -> exn option

  (** [pp_flamegraph pp_key fmt flamegraph] prints [flamegraph]. *)
  val pp_flamegraph : (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a flamegraph -> unit

end

(*
 * HOWTO
 * =====
 *
 * How to measure
 * --------------
 *
 * You first need to create the measurement module:
 *
 *     module MyStat = Statistics.Make(Statistics.Helpers.FastString)
 *
 *
 * You then need to start and stop your the measurement period:
 *
 *   +   MyStat.start (EnableStatOption.get ());
 *   ...
 *   +   let final_measures = MyStat.stop () in
 *
 *
 * You can then measure each function you are interested in:
 *
 *   - let f x y z =
 *   + let do_f x y z () =
 *        <body>
 *        ..
 *        <body>
 *
 *   + let key_f = Statistics.Helpers.FastString.register "f"
 *   + let f x y z =
 *   +   MyStat.measure key_f (do_f x y z)
 *   +
 *
 *
 * During the measurement period, you may dump the current
 * measurements:
 *
 *   + let partial_measures = MyStat.dump () in
 *
 *
 * You may print the measurements (partial or final) with:
 *
 *   + MyStat.Measures.fullprint Format.std_formatter measures;
 *
 *
 * How it works
 * ------------
 *
 * The measures are well-parenthesized by construction and all
 * measures are between the [start] and [stop] functions:
 *
 *  start(....................................................)stop
 *          (k1.....)    (k2...............)(k1...)  (k3.....)
 *             (k3.)       (k1....) (k2.)              (k4..)
 *                         (k4)(k4)
 *
 * For each callstack, the measures contain:
 * - the number of calls
 * - the cumulative self time
 *
 *)
