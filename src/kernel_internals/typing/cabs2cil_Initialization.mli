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

open Cil_types
module Chunk = Cabs2cil_BlockChunk

module Initializer : sig

  (* USED ONLY IN:
     + do_initializer
     + do_init
  *)

  val category_initializer : Log.category

  module Subobj : sig

    type t

    (** Create a subobject iterator whose host and whose current object both
        correspond to the given variable. *)
    val of_varinfo : varinfo -> t


    (** Move around. *)

    (** Advance to the next subobject of the current object. *)
    val advance_to_next : t -> t

    (** Advance into the first array cell of the subobject OR into the cell with
        the provided index. This corresponds to a situation either:
        1) the subobject is an array and the encountered initializer is single
           (in this case we should attempt to use the encountered initializer to
           initialize the first cell of the array);
        2) OR the subobject is an array and an explicit designator is pointing
           to one of its cells. *)
    val advance_into_array : ?index:int -> t -> t

    (** Advance into the first field of a composite type OR into the field with
        the provided name. This corresponds to a situation when either:
        1) - the subobject is of composite type
           - and the encountered initializer is single,
           - and the provided initialization expression does not have a type
             compatible with the composite type
           (in this case we should attempt to use the encountered initializer to
           initialize the first field of the composite);
        2) OR the subobject is of composite type and an explicit designator is
           pointing to one of its fields. *)
    val advance_into_comp : ?field_name:string -> t -> t

    (** Set the subobject to the current object. This is used when we encounter
        a designator, subsequently we use the two above functions (i.e.
        [advance_into_array] and [advance_into_comp]) to advance into
        array cells / composite type fields as the designator specifies. *)
    val reset : t -> t

    (** Make a new subobject iterator whose current object is the currently
        handled subobject. This is used when a compound initalizer is
        encountered: we attempt to recursively initialize the subobject with
        encountered compound initializer.
        @param keep_the_relative_offset This argument is never used now, so it
        always defaults to [false]. However, it is kept just in case if we were
        one day to revise our decision on the interpretation of the Defect
        Report #413, concerning how compound initializers override previous
        initialization of the same subobject. *)
    val start_with_new_current_object : ?keep_the_relative_offset:bool -> t -> t


    (** Access to the subobject. *)

    (** The type of the subobject. *)
    val get_typ : t -> typ

    (** The relative offset of the subobject from the current object. *)
    val get_offset : t -> offset

    (** The absolute offset of the subobject from the initialized host (i.e.
        the variable that is being initialized). *)
    val get_absolute_offset : t -> offset

    (** Returns [true] if there is nothing left to initialize in the current
        object. At this point an attempt to advance in the subobject causes
        an error. *)
    val was_end_reached : t -> bool

    (** Pretty printing. *)
    val pp_as_lval : Format.formatter -> t -> unit
    val pp_current_obj_as_lval : Format.formatter -> t -> unit

  end

  module Preinit : sig

    (** Preinitializer is an intermediate step in the initializer translation
        process. It is necessary in order to correctly take into account details
        such as overriding of initializers.

        Basically, in the intializer translation and normalization process first
        we build a preinitializer by traversing a CABS initializer: we keep
        track of the current object and subobject and we fill the constructed
        preinitializer accordingly to encountered nested compound and single
        initializers. Then we can translate the preinitializer to the CIL
        initializer form. *)
    type t

    (** A single init expression corresponding to a single initializer. It
        contains all the necessary details for performing the initialization. *)
    type init_exp_with_details

    (** Make a single init expression from some inputs and all results of
        a [do_expression] call. *)
    val make_init_exp_with_details :
      Cabs.expression -> lval list -> Chunk.t -> exp -> typ ->
      init_exp_with_details

    (** No preintializer. *)
    val none : t

    (** Does the preinitializer contain just a single expression?
        This is a simple and useful way to determine if we can just directly
        ignore the problems related to the order of evaluation of expressions in
        the initializer: if there is only one expression then obviously it is the
        only one to evaluate and thus no further manipulation is needed. *)
    val is_single : t -> bool

    (** Create a preinitializer corresponding to an empty compound initializer,
        i.e. [{}]. This is different that just no preinitializer, because
        a preinitializer for a certain subobject exists, but it is empty.
        This is a GNU extension. *)
    val make_compound_empty_preinit : unit -> t

    (** Set a preinitializer for given subobject. *)
    val set_init_exp_for_subobject : t -> Subobj.t -> init_exp_with_details -> t
    val set_preinit_for_subobject : t -> Subobj.t -> t -> t

    (** Type for the function accumulating side effects for initializers, used
        in [to_init].
        Basically:
        - it takes as arguments the existing side effects accumulator and
          the results of calling [do_expression] on a certain subobject's
          initialization expression,
        - and it returns an updated side effects accumulator and updated
          expression that should be assigned to the concerned subobject. *)
    type accumulate_side_effects_t =
      Chunk.t ->
      (lval list * Chunk.t * exp * typ) ->
      (Chunk.t * exp)

    (** Collect a CIL initializer for an object of a certain type given the
        corresponding preinitializer. It returns the CIL initializer, the new
        type (the type might have been refined because of initialization), and
        the chunk of side effects caused by evaluation of expressions in the
        initializer. *)
    val to_init : accumulate_f:accumulate_side_effects_t ->
      t -> typ -> init * typ * Chunk.t

    val pretty : Format.formatter -> t -> unit

  end

end

module AssignInit : sig

  (* USED ONLY IN:
     + assign_init
  *)

  (** TODO: ??? *)
  val ensures_init : varinfo -> offset -> exp -> predicate named

  (** Memset to 0 an entire array. *)
  val set_to_zero : ghost:bool -> varinfo -> offset -> typ -> stmt

  (** [zero_init ~ghost vi off len base_typ] Initialize the first cell of an
      array, and call Frama_C_copy_block to propagate this initialization to the
      rest of the array. Array is located at [vi.off], of length [len],
      and cells are of type [base_type]. *)
  val zero_init : ghost:bool -> varinfo -> offset -> int -> typ -> Chunk.t

  (** Make a contract for a block that performs partial initialization of
      a local, relying on bzero for implicit zero-initialization. *)
  val make_implicit_ensures : varinfo -> offset -> typ -> int ->
    Datatype.Integer.Set.t -> predicate named
end
