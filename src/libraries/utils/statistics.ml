(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2014-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(** [rdtsc ()] returns RDTSC divided by two (called rdtsc). *)
let rdtsc = Extlib.getperfcount

module Ratio : sig
  val get : unit -> float
  val conv : float -> int -> float
end = struct
  let checkpoint () =
    let t = Unix.gettimeofday () in
    let x = rdtsc () in
    t, x
  let ta, xa = checkpoint ()
  let get () =
    let tb, xb = checkpoint () in
    (tb -. ta) /. float_of_int (xb - xa)
  let conv r x = r *. float_of_int x
end

type 'a flamegraph = ((int * 'a) list * float) list

module Flamegraph(Key : Datatype.S) =
  Datatype.List
    (Datatype.Pair
       (Datatype.List(Datatype.Pair(Datatype.Int)(Key)))
       (Datatype.Float))

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
    val final : t -> bool
    val enabled : t -> bool
    val conv : t -> int -> float
    val iter : t -> (key -> data -> unit) -> (int -> unit) -> unit
    val fullprint : t -> Format.formatter -> unit
    val flamegraph : t -> (key -> 'a option) -> 'a flamegraph
  end
  val status : unit -> status
  val start : bool -> unit
  val measure : key -> (unit -> 'a) -> 'a
  val dump : unit -> Measures.t
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

module Make(Key : Key) = struct

  type key = Key.t
  type data = (* private *) {
    mutable count : int;
    mutable self : int;
  }
  type row = {
    data : data;
    children : row Key.Hashtbl.t;
  }
  type status =
    | Broken
    | Stopped
    | Started of bool

  module Measures = struct

    type t = {
      final : bool;
      ratio : float;
      state : (key list * row Key.Hashtbl.t) option;
    }

    let create final state =
      let ratio = Ratio.get () in
      { final; ratio; state; }

    let final { final; _ } = final
    let enabled { state; _ } = state <> None
    let conv { ratio; _ } = fun x -> Ratio.conv ratio x

    let iter { state; _ } push pop =
      let rec aux table =
        let total = ref 0 in
        Key.Hashtbl.iter (fun key { data; children; } ->
            push key data;
            let sub_total = data.self + aux children in
            total := !total + sub_total;
            pop sub_total;
          ) table;
        !total
      in
      match state with
      | None -> ()
      | Some (_, root) -> ignore (aux root)

    let fullprint { final; ratio; state; } fmt =
      match state with
      | None -> ()
      | Some (keys, root) ->
        let start = rdtsc () in
        Format.fprintf fmt "Status: %s\n"
          (if final then "Final measurements" else "Partial dump");
        if not final then begin
          Format.fprintf fmt "Current stack:";
          Pretty_utils.pp_list ~pre:"" ~sep:" -> " ~suf:""
            Key.pretty fmt keys;
          Format.fprintf fmt "\n";
        end;
        let conv x = Ratio.conv ratio x in
        let print indent key { count; self; } =
          let self = conv self in
          Format.fprintf fmt "%*s%a: %d %f\n"
            indent "" Key.pretty key count self
        in
        let rec aux indent table =
          Key.Hashtbl.iter (fun key { data; children; } ->
              print indent key data;
              aux (succ indent) children;
            ) table;
        in
        aux 0 root;
        let time = conv (rdtsc () - start) in
        Format.fprintf fmt "Printing time: %f\n" time

    let flamegraph { ratio; state; _ } keep =
      match state with
      | None -> []
      | Some (_, root) ->
        let conv x = Ratio.conv ratio x in
        let result = ref [] in
        let rec aux stack table =
          let extra = ref 0 in
          Key.Hashtbl.iter (fun key { data; children; } ->
              match keep key with
              | None ->
                let self = data.self + aux stack children in
                extra := !extra + self;
              | Some key ->
                let stack = (data.count, key) :: stack in
                let self = data.self + aux stack children in
                result := (stack, conv self) :: !result;
            ) table;
          !extra
        in
        ignore (aux [] root);
        !result

  end

  module Stack : sig

    (** [first_push ()] initiates the stack. *)
    val first_push : unit -> unit

    (** [last_pop ()] cleans the stack. *)
    val last_pop : unit -> row Key.Hashtbl.t

    (** [push key] pushes [key] on the stack. *)
    val push : key -> unit

    (** [pop ()] pops one element from the stack. *)
    val pop : unit -> unit

    (** [snapshot ()] takes a snapshot. *)
    val snapshot : unit -> key list * row Key.Hashtbl.t

    (** [clear ()] clears internal state. *)
    val clear : unit -> unit

  end = struct

    type stack =
      | Empty
      | Root of row
      | Frame of key * row * stack

    let stack = ref Empty
    let last = ref 0

    (** [new_row ()] returns a new empty row. *)
    let new_row () =
      let data = { count = 0; self = 0; } in
      let children = Key.Hashtbl.create Key.size in
      { data; children; }

    let first_push () =
      (* Init last and stack. *)
      stack := Root (new_row ());
      last := rdtsc ();
    ;;

    let last_pop () =
      let { children = root; _ } =
        match !stack with
        | Root root -> root
        | Empty | Frame _ -> assert false
      in
      (* Reset last and stack. *)
      last := 0;
      stack := Empty;
      root

    (** [find table key] returns the row associated with [key] in
        [table] and creates it if necessary. *)
    let find p_table key =
      try Key.Hashtbl.find p_table key
      with Not_found ->
        let c_row = new_row () in
        Key.Hashtbl.add p_table key c_row;
        c_row

    let push key =
      let now = rdtsc () in
      let parent =
        match !stack with
        | Root row -> row
        | Frame (_, row, _) -> row
        | Empty -> assert false
      in
      let child = find parent.children key in
      (* Increment parent self. *)
      parent.data.self <- parent.data.self + now - !last;
      (* Increment child count. *)
      child.data.count <- child.data.count + 1;
      (* Update last and stack. *)
      stack := Frame (key, child, !stack);
      last := rdtsc ();
    ;;

    let pop () =
      let now = rdtsc () in
      match !stack with
      | Frame (_, { data; _ }, rest) ->
        (* Increment current self. *)
        data.self <- data.self + now - !last;
        (* Update last and stack. *)
        stack := rest;
        last := rdtsc ();
      | Empty | Root _ -> assert false

    let snapshot () =
      let rec copy table =
        let result = Key.Hashtbl.(create (length table)) in
        Key.Hashtbl.iter
          (fun key { data = { count; self; }; children; } ->
             let data = { count; self; } in
             let children = copy children in
             Key.Hashtbl.add result key { data; children; }
          ) table;
        result
      in
      let rec aux keys = function
        | Root root -> keys, copy root.children
        | Frame (key, _, stack) -> aux (key :: keys) stack
        | Empty -> assert false
      in
      let now = rdtsc () in
      match !stack with
      | Root { data; _ }
      | Frame (_, { data; _ }, _) ->
        data.self <- data.self + now - !last;
        let result = aux [] !stack in
        last := rdtsc ();
        result
      | Empty -> assert false

    let clear () =
      last := 0;
      stack := Empty;
    ;;

  end

  let status = ref Stopped

  (** [api_broken ()] permanently disables measurements. *)
  let api_broken () =
    status := Broken;
    Stack.clear ();
  ;;

  let start enabled =
    match !status with
    | Broken -> ()
    | Started _ -> api_broken ()
    | Stopped ->
      status := Started enabled;
      if enabled then Stack.first_push ();
  ;;

  let stop () =
    let state =
      match !status with
      | Broken -> None
      | Stopped -> api_broken (); None
      | Started enabled ->
        status := Stopped;
        if enabled then Some ([], Stack.last_pop ()) else None
    in
    Measures.create true state

  let measure key thunk =
    match !status with
    | Broken -> thunk ()
    | Stopped -> api_broken (); thunk ()
    | Started false -> thunk ()
    | Started true ->
      Stack.push key;
      try
        let result = thunk () in
        Stack.pop ();
        result
      with e ->
        Stack.pop ();
        raise e (* WARNING! Backtraces may be truncated here. *)
  (* Disable statistics with [start false].     *)

  let dump () =
    let state =
      match !status with
      | Broken -> None
      | Stopped -> api_broken (); None
      | Started false -> None
      | Started true -> Some (Stack.snapshot ())
    in
    Measures.create false state

  let status () = !status

end

module Helpers = struct

  module FastString = struct
    include Datatype.Int
    let table = Hashtbl.create 1
    let last = ref 0
    let register name =
      let key = !last in
      incr last;
      assert (not (Hashtbl.mem table key));
      Hashtbl.add table key name;
      key
    let pretty fmt key =
      let name =
        try Hashtbl.find table key
        with Not_found -> assert false
      in
      Format.fprintf fmt "%s" name
    let size = 1
  end

  let to_file printer filename =
    try
      let oc = open_out filename in
      try
        let fmt = Format.formatter_of_out_channel oc in
        printer fmt;
        close_out oc;
        None
      with e ->
        close_out_noerr oc;
        Some e
    with e -> Some e

  let pp_flamegraph pp_key fmt flamegraph =
    let pp_stack =
      Pretty_utils.pp_list ~pre:"" ~sep:";" ~suf:""
        (fun fmt (count, key) ->
           Format.fprintf fmt "%d.%a" count pp_key key)
    in
    let print (stack, self) =
      Format.fprintf fmt "%a %f\n" pp_stack (List.rev stack) self
    in
    List.iter print flamegraph;
    Format.fprintf fmt "@?"

end
