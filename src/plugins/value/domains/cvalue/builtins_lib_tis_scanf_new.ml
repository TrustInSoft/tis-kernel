(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(*TODO LIST:
  - finish the '%p' conversion specifier (if needed/desirable/doable)
  - NAN and INFINITY values are correctly parsed but not written to memory.
    Once the abstract values to represent them are implemented, complete the
    corresponding code (there is a TODO pointing to the exact place).
*)

open Abstract_interp
open Locations
open Cil_types
open Floating_point

exception Invalid_character

(* Exceptions used to handle reading input and formats *)
exception End_of_input
exception Finished
exception Finished_strto of int * string
exception Interpret_format_partial
exception Interpret_format_finished
exception Interpret_format_not_finished
exception Reached_width_limit
exception Read_String_Done
exception Not_yet_implemented of string

(* types for strto* and ato* functions *)
type float_type = Double | Float | LDouble
type int_type = Int | Long | LLong | ULong | ULLong

let string_of_float_type = function
  | Float -> "float"
  | Double -> "double"
  | LDouble -> "long double"

let string_of_int_type = function
  | Int -> "int"
  | Long -> "long"
  | LLong -> "long long"
  | ULong -> "unsigned long"
  | ULLong -> "unsigned long long"

type overflow = {integer : Cvalue.V.t ; typ : Cil_types.typ}
exception Integer_overflow of overflow

(* Exceptions translated from the ISO/IEC 9899:201x N1570 specification *)
exception Input_failure
exception Matching_failure
exception Undefined_behavior
exception Invalid_conversion_specification

(* value of certain macros defined in header files *)
let eof = -1
let erange = 3

(* Types and functions to process conversion directives *)
type bracket_fields =
  {caret: bool; (* if true, the directive matches chars not in the scanlist *)
   scanlist: string (* chars matched by the directive if caret is false *) }
type ncs =
  {suppress: bool; (* assignment suppress character *)
   width: int option; (* maximum field width *)
   length_modifier: string option; (* specifies size of the receiving objet *)
   conversion_specifier: char (* the type of conversion to be applied *) }
type bcs = {normal_fields: ncs; bracket_fields: bracket_fields}
type cs = Normal of ncs | Bracket of bcs
type directive = Whitespace | Char of char | CS of cs
               | ReadStrto_int of int
               | ReadStrto_float

(* Functions to build values of type 'directive' *)
let make_normal_cs_directive
    ?(suppress = false) ~width ?length_modifier ~conversion_specifier () =
  CS(Normal {suppress; width; length_modifier; conversion_specifier})

let make_bracket_cs_directive
    ?(suppress = false) ~width ?length_modifier ~conversion_specifier
    ~caret ~scanlist () =
  CS(Bracket{
      normal_fields = {suppress; width; length_modifier; conversion_specifier};
      bracket_fields = {caret; scanlist}
    })

(* define helper functions *)
module Aux = Builtins_lib_tis_aux
let register_builtin = Aux.register_builtin
let dkey = Value_parameters.register_category "tis_builtins_debug"
let debug_msg = Value_parameters.debug ~dkey

open Aux.StringAndArrayUtilities

let check_string_validity state (exp, cvalue, _) =
  let strlen = Strlen.get_abstract_strlen () in
  let character_bits = get_character_bits Character in
  let emit_alarm =
    let raise_alarm = ref true in
    let strlen_alarm_invalid_string () =
      if !raise_alarm then begin
        Value_parameters.warning ~current:true
          "@[assert(\\valid_string(%a))@]%t"
          Cil_printer.pp_exp exp
          Value_util.pp_callstack;
        raise_alarm := false;
      end
    in
    { Strlen.strlen_alarm_invalid_string }
  in
  ignore (strlen ~character_bits ~max:str_infinity ~emit_alarm cvalue state)

let bottom_result =
  { Value_types.c_values =
      [ Value_types.StateOnly(None, Cvalue.Model.bottom) ] ;
    c_clobbered = Base.SetLattice.bottom;
    c_cacheable = Value_types.NoCache;
    c_from = None; (* TODO?*)
    c_sureouts = None;
  }

let alarm_behavior_raise_problem =
  {CilE.a_ignore with CilE.a_call = fun _ -> raise Interpret_format_partial}

let with_alarms =
  { CilE.defined_logic = alarm_behavior_raise_problem;
    unspecified        = alarm_behavior_raise_problem;
    others             = alarm_behavior_raise_problem;
    imprecision_tracing = CilE.a_ignore}

let with_alarms_from_problem = fun problem ->
  let set_problem =
    {CilE.a_ignore with CilE.a_call = fun _ -> problem := true}
  in
  { CilE.defined_logic  = set_problem;
    unspecified         = set_problem;
    others              = set_problem;
    imprecision_tracing = CilE.a_ignore }



type seen_percent =
    Not_seen
  | Seen of bool * Integer.t option * string
  | InsideBrackets of string
  (* suppressing *, width, length modifier, bracket expression *)

let abort () =
  Value_parameters.error ~current:true
    "assert(match format and arguments)%t. Aborting."
    Value_util.pp_callstack;
  raise Db.Value.Aborted

let warn_destination_possibly_invalid () =
  Value_parameters.warning ~current:true
    "Destination possibly invalid. assert(match format and arguments)%t"
    Value_util.pp_callstack

let get_char_size_from_modifier = function
  | ""  -> Bit_utils.sizeofchar()
  | "l" -> (Int.of_int (Cil.bitsSizeOf Cil.theMachine.Cil.wcharType))
  | _   ->
    Value_parameters.error ~current:true "Invalid length modifier";
    raise Undefined_behavior

(* This function returns the types allowed for a variable used to store values
 * processed by a given directive *)
(* Return value: a pair of lists, the first one with the expected types and
 * the second one with the allowable types for the variable in
 * which the input resulting from the directive should be saved *)
let get_allowable_types conv_specifier length_modifier =

  let shrtu_typ = TInt(IUShort, []) in
  let shrt_typ = TInt(IShort, []) in

  (* TODO: We assume intmax_t = long long and uintmax_t = unsigned long long
   * as for the moment there is not an intmax_t in Frama-C *)
  let ptr_to_intmax_t = Cil_types.(TPtr(Cil.longLongType, [])) in
  let ptr_to_uintmax_t = Cil_types.(TPtr(Cil.ulongLongType,[])) in

  let ptr_to_ptrdiff_t = Cil_types.(TPtr(Cil.theMachine.Cil.ptrdiffType, [])) in
  let ptr_to_size_t = Cil_types.(TPtr(Cil.theMachine.Cil.typeOfSizeOf, [])) in

  (* Signed integer (pointer) types *)
  let ptr_to_schar = Cil.scharPtrType in
  let ptr_to_int = Cil.intPtrType in
  let ptr_to_short = Cil_types.(TPtr(shrt_typ, [])) in
  let ptr_to_long = Cil_types.(TPtr(Cil.longType, [])) in
  let ptr_to_long_long = Cil_types.(TPtr(Cil.longLongType, [])) in

  (* Unsigned integer (pointer) types *)
  let ptr_to_uchar = Cil.ucharPtrType in
  let ptr_to_ushort = Cil_types.(TPtr(shrtu_typ, [])) in
  let ptr_to_uint = Cil.uintPtrType in
  let ptr_to_ulong = Cil_types.(TPtr(Cil.ulongType, [])) in
  let ptr_to_ulong_long = Cil_types.(TPtr(Cil.ulongLongType, [])) in

  (* Float (pointer) types *)
  let ptr_to_float = Cil_types.(TPtr(Cil.floatType, [])) in
  let ptr_to_double = Cil_types.(TPtr(Cil.doubleType, [])) in
  let ptr_to_long_double = Cil_types.(TPtr(Cil.longDoubleType, [])) in

  try
    ( match conv_specifier with
      (* The standard says:
       * "If this object does not have an appropriate type, or if the result of
       * the conversion cannot be represented in the object,
       * the behavior is undefined". *)

      | 'd' | 'i' -> (* all the types are signed int *)
        (match length_modifier with
         | "h"  -> [ptr_to_short], []
         | "hh" -> [ptr_to_schar] , []
         | ""   -> [ptr_to_int]  , []
         | "l"  -> [ptr_to_long] , []
         | "ll" -> [ptr_to_long_long], []
         | "j"  -> [ptr_to_intmax_t], []
         | "z"  -> [ptr_to_size_t], []
         | "t"  -> [ptr_to_ptrdiff_t], []
         | _ -> raise Invalid_conversion_specification)

      | 'x' | 'X' | 'u' | 'o' | 'n' -> (* all the types are unsigned int *)
        (match length_modifier with
         | "h"  -> [ptr_to_ushort], []
         | "hh" -> [ptr_to_uchar], []
         | ""   -> [ptr_to_uint], []
         | "l"  -> [ptr_to_ulong], []
         | "ll" -> [ptr_to_ulong_long], []
         | "j"  -> [ptr_to_uintmax_t], []
         | "z"  -> [ptr_to_size_t], []
         | "t"  -> [ptr_to_ptrdiff_t], []
         |  _   -> raise Invalid_conversion_specification)

      | 'a' | 'e' | 'f' | 'g' | 'A' | 'E' | 'F' | 'G' ->
        (match length_modifier with
         | ""  -> [ptr_to_float], []
         | "l" -> [ptr_to_double], []
         | "L" -> [ptr_to_long_double], []
         | _ -> raise Invalid_conversion_specification )

      | 'c' | 's' | '[' ->
        (match length_modifier with
         | ""  -> [Cil.charPtrType; Cil.ucharPtrType; Cil.scharPtrType], []
         | "l" -> [Cil_types.(TPtr(Cil.theMachine.Cil.wcharType, []))], []
         | _   -> raise Invalid_conversion_specification )

      | _ -> raise Invalid_conversion_specification)
  with Invalid_conversion_specification ->
    (Value_parameters.error ~current:true
       "invalid length modifier %s for conversion specifier %c."
       length_modifier conv_specifier;
     raise Undefined_behavior)

(* This function checks if the value that we are trying to write fits into
 * the variable, or if it will result in an overflow *)
let handle_overflow typ interpreted_e =

  match Cil.unrollType typ with
  | TInt(kind, _) ->
    let signed = Cil.isSigned kind in
    let size = Cil.bitsSizeOfInt kind in
    let mn, mx =
      if signed then
        let b = Int.two_power_of_int (size-1) in
        Int.neg b, Int.pred b
      else
        Int.zero, Int.pred (Int.two_power_of_int size)
    in
    let warn_under, warn_over =
      try
        let i = Cvalue.V.project_ival interpreted_e in
        let imn, imx = Ival.min_and_max i in
        let u =
          match imn with
          | Some bound when Int.ge bound mn -> None
          | _ -> Some mn
        in
        let o =
          match imx with
          | Some bound when Int.le bound mx -> None
          | _ -> Some mx
        in
        u, o
      with Cvalue.V.Not_based_on_null ->
        (* Catch bottom case here: there is no overflow in this case. *)
        if Cvalue.V.is_bottom interpreted_e then
          None, None
        else
          Some mn, Some mx
    in
    ( match warn_under, warn_over with
      | None, None ->
        interpreted_e
      | _ -> raise (Integer_overflow {integer=interpreted_e ; typ = typ}))
  | t ->
    Value_parameters.error
      "Destination pointer is not of integer type, but of type \"%a\""
      Cil_printer.pp_typ t;
    raise Undefined_behavior

(* Detect overlap of destination variables with either source or
   format strings *)
let src_loc_bits = ref Locations.Location_Bits.bottom
let src_len = ref Ival.zero
let fmt_loc_bits = ref Locations.Location_Bits.bottom
let fmt_len = ref Ival.zero

let string_zero = "0"

(* **************************** REGEX ************************************** *)
(* WARNING: when using OR ("\\|") in a regexp string, always put the strongest
 * condition first, otherwise the matched string will be that of the weakeast
 * condition, leading to unexpected results.
 *
 * For example, the regex built from the strings
 * "\\(INF\\))\\|\\(INFINITY\\)" and "\\(INFINITY\\))\\|\\(INF\\)"
 * are both matched by "INFINITY", but in the first case the matching
 * substring will be "INF", leading to a partial read, while in the second it
 * will be "INFINITY" which gives a complete read by the functions used for
 * parsing input. *)

(* Regex strings *)
let reg_or = "\\|"
let reg_decimal_point = "\\."
let reg_decimal_point_opt = reg_decimal_point ^"?"
let reg_sign_opt = "[+-]?"

let reg_zero = string_zero
let reg_decimal_digit = "[0-9]"
let reg_non_zero_digit = "[1-9]"
let reg_octal_digit = "[0-7]"
let reg_hex_digit = "[0-9a-f]"

let digits_from_base = function
  | b when 2 <= b && b <= 10 ->
    "[0-" ^ (string_of_int (b-1))  ^ "]"
  | b when 11 <= b && b <= 36 ->
    let last_lowercase = Char.chr (Char.code 'a' + (b - 11)) in
    let last_uppercase = Char.chr (Char.code 'A' + (b - 11)) in
    Printf.sprintf "[0-9a-%cA-%c]" last_lowercase last_uppercase
  | _ -> assert false

let reg_base_digit = fun base -> digits_from_base base
let reg_base_digit_sequence = fun base -> (reg_base_digit base) ^ "+"


let reg_digit_sequence = reg_decimal_digit ^ "+"
let reg_digit_sequence_opt = reg_decimal_digit ^"*"

let reg_octal_digit_sequence = reg_octal_digit ^"+"
let reg_octal_digit_sequence_opt = reg_octal_digit ^"*"

let reg_hex_prefix = "\\(0x\\)"
let reg_hex_prefix_opt = reg_hex_prefix ^ "?"
let reg_hex_digit_sequence = reg_hex_digit ^ "+"
let reg_hex_digit_sequence_opt = reg_hex_digit ^ "*"

let reg_infinity =
  reg_sign_opt ^ "\\(" ^ "\\(INFINITY\\)" ^ reg_or ^ "\\(INF\\)" ^ "\\)"
let reg_nan =
  reg_sign_opt ^
  "\\(" ^ "\\(NAN" ^ "([0-9a-z]+)\\)" ^ reg_or ^ "\\(NAN\\)" ^ "\\)"

(* Integer regex strings *)
(* For '%d' conversion specifier - format like `strtol` with base = 10 *)
(* For '%u' conversion specifier - format like `strtoul` with base = 10 *)
let reg_decimal_constant = reg_sign_opt ^ reg_digit_sequence

(* For '%i' conversion specifier  - format like `strtol` with base = 0 *)
let reg_decimal_constant_base_zero =
  reg_sign_opt ^ reg_non_zero_digit ^ reg_digit_sequence_opt
let reg_octal_constant_base_zero =
  reg_sign_opt ^ reg_zero ^ reg_octal_digit_sequence_opt
let reg_hex_constant_base_zero =
  reg_sign_opt ^ reg_hex_prefix ^ reg_hex_digit_sequence

let reg_integer_base_zero =
  "\\(" ^ reg_decimal_constant_base_zero ^ "\\)" ^ reg_or ^
  "\\(" ^ reg_hex_constant_base_zero ^ "\\)" ^ reg_or ^
  "\\(" ^ reg_octal_constant_base_zero ^ "\\)"

(* For '%o' conversion specifier - format like `strtoul` with base = 8 *)
let reg_octal_constant = reg_sign_opt ^ reg_octal_digit_sequence

(* For '%x' conversion specifier - format like `strtoul` with base = 16 *)
(* '0x' or '0X' prefix is optional *)
let reg_hex_constant = reg_sign_opt ^
                       reg_hex_prefix_opt ^ reg_hex_digit_sequence

(* Regex for strto{u,l}{l,l} functions *)
let reg_base_constant ~base = reg_sign_opt ^ (reg_base_digit_sequence base)

(* Floating point regex strings *)
(* The format of a floating-point number is the same as expected for the subject
 * sequence of the `strtod` function *)

(* Decimal float Regex *)
let reg_exponent_part = "e" ^ reg_sign_opt ^ reg_digit_sequence
let reg_exponent_part_opt = "\\(" ^ reg_exponent_part ^"\\)?"
let reg_decimal_floating_constant =
  reg_sign_opt ^
  "\\(" ^
  "\\(" ^ reg_decimal_point ^ reg_digit_sequence ^ "\\)" ^
  reg_or ^
  "\\(" ^ reg_digit_sequence ^ reg_decimal_point_opt ^
  reg_digit_sequence_opt ^ "\\)" ^
  "\\)" ^ reg_exponent_part_opt

(* Hexadecimal float Regex *)
let reg_binary_exponent_part = "p" ^ reg_sign_opt ^ reg_digit_sequence
let reg_binary_exponent_part_opt = "\\(" ^ reg_binary_exponent_part ^ "\\)?"

let reg_hex_floating_constant =
  reg_sign_opt ^ reg_hex_prefix ^
  "\\(" ^
  "\\(" ^ reg_decimal_point ^ reg_hex_digit_sequence ^ "\\)" ^
  reg_or ^
  "\\(" ^ reg_hex_digit_sequence ^ reg_decimal_point_opt ^
  reg_hex_digit_sequence_opt ^ "\\)" ^
  "\\)" ^
  reg_binary_exponent_part_opt

(* Float Regex *)
let reg_floating_constant =
  "\\(" ^ reg_hex_floating_constant ^ "\\)" ^
  reg_or ^
  "\\(" ^ reg_decimal_floating_constant ^ "\\)" ^
  reg_or ^
  "\\(" ^ reg_nan ^ "\\)" ^
  reg_or ^
  "\\(" ^ reg_infinity ^ "\\)"

(* Test Regex Match *)
(* Build a function to test matching a prefix of a regex *)
let is_prefix_fun regexp =
  let r = Str.regexp_case_fold regexp in
  fun s ->
    if Str.string_partial_match r s 0 then
      begin
        let matched = Str.matched_string s in
        matched = s
      end
    else false

(* Build a function to test matching a regex *)
let is_match_fun regexp =
  let r = Str.regexp_case_fold regexp in
  fun s ->
    if Str.string_match r s 0 then
      begin
        let matched = Str.matched_string s in
        matched = s
      end
    else false

(* Function to test floating-point match *)
let is_decimal_float_prefix = is_prefix_fun reg_decimal_floating_constant
let is_decimal_float = is_match_fun reg_decimal_floating_constant

let is_hex_float_prefix = is_prefix_fun reg_hex_floating_constant
let is_hex_float = is_match_fun reg_hex_floating_constant

let is_nan_float = is_match_fun reg_nan
let is_infinity_float = is_match_fun reg_infinity

(* This inlcudes NaN and INFINITY in the regex *)
let is_float_extended_prefix = is_prefix_fun reg_floating_constant
let is_float_extended = is_match_fun reg_floating_constant

let with_nan_and_infinity = true (* set to false to exclude NAN and INF float *)
let is_float_prefix =
  let normal = fun s -> is_decimal_float_prefix s || is_hex_float_prefix s in
  let extended = is_float_extended_prefix in
  if with_nan_and_infinity then extended else normal
let is_float =
  let normal = fun s -> is_decimal_float s || is_hex_float s in
  let extended = is_float_extended in
  if with_nan_and_infinity then extended else normal

(* Function to test integer match *)
let is_integer_prefix ~base =
  let regex = match base with
    |  0 -> reg_integer_base_zero
    |  8 -> reg_octal_constant
    | 10 -> reg_decimal_constant
    | 16 -> reg_hex_constant
    | b when 2 <= b && b <= 36 -> (reg_base_constant ~base)
    | _ -> assert false
  in
  is_prefix_fun regex

let is_integer ~base =
  let regex = match base with
    |  0 -> reg_integer_base_zero
    |  8 -> reg_octal_constant
    | 10 -> reg_decimal_constant
    | 16 -> reg_hex_constant
    | b when 2 <= b && b <= 36 -> (reg_base_constant ~base)
    | _ -> assert false
  in
  is_match_fun regex

let is_integer_base_zero_prefix = is_integer_prefix ~base:0
let is_octal_prefix = is_integer_prefix ~base:8
let is_decimal_prefix = is_integer_prefix ~base:10
let is_hex_prefix = is_integer_prefix ~base:16

let is_integer_base_zero_string = is_integer ~base:0
let is_octal_string = is_integer ~base:8
let is_decimal_string = is_integer ~base:10
let is_hex_string = is_integer ~base:16

let is_hex_base_zero = is_match_fun reg_hex_constant_base_zero
let is_octal_base_zero = is_match_fun reg_octal_constant_base_zero
let is_decimal_base_zero = is_match_fun reg_decimal_constant_base_zero

let is_base_prefix ~base = is_integer_prefix ~base
let is_base_string ~base = is_integer ~base
(* ******************* END OF REGEX STUFF ********************************** *)


(* Functions to recognize digits *)
let is_decimal_digit d = '0' <= d && d <= '9'

let is_whitespace c =
  let whitespace = [' '; '\n'; '\t'; '\r'] in
  List.mem c whitespace
let is_not_whitespace = fun c -> not (is_whitespace c)
let is_sign c = c = '-' || c = '+'

(* ********************** Build functions to parse input ******************** *)
(* Function built for scanf - s is a string read from the command line
 * and not a location *)
let build_functions_from_string s =
  let i = ref 0 in
  let len = String.length s in
  let get_char () = if !i = len then raise Read_String_Done else s.[!i] in
  let move_to_next () = incr i in
  (get_char, move_to_next)

(* It reads only the input, not the format.
   Since alarms are caught by the abstract_strlen before executing the builtin,
   it not necessary to catch alarms here. *)
let build_functions_from_state ~character_width l state =
  let new_l = ref l in
  let sizeofchar_size = Int_Base.inject character_width in
  let sizeofchar_ival = Ival.inject_singleton character_width in
  let get_char () =
    try
      let loc = Locations.make_loc !new_l sizeofchar_size in
      let c = Eval_op.find ~with_alarms:CilE.warn_none_mode state loc in
      let c = Cvalue.V.project_ival c in
      let c = Ival.project_int c in
      let c = Int.to_int c in
      assert (-128 <= c && c <= 255);
      let format_ends = c = 0 in
      if format_ends then raise End_of_input;
      let code = if c >= 0 then c else c + 256 in
      char_of_int code
    with
    | Cvalue.V.Not_based_on_null
    | Ival.Not_Singleton_Int
    | Int.Too_big -> raise Invalid_character
  in
  let move_to_next () =
    new_l := Location_Bits.shift sizeofchar_ival !new_l
  in
  (get_char, move_to_next)
(* ************************************************************************* *)

(* function get_input:
 *
 * Description: reads from the input stream using the functions specified to
 *              consume chars according to the conversion specifier provided.
 *
 * Return value: a triple (count,chars,status)
 *               count: number of chars read from the stream
 *               chars: a string containing the chars matched by the directive
 *               status: true if the read was completed, false otherwise
*)
let get_input (get_char, move_to_next) directive  =

  let read_chars_count = ref 0 in

  (* Wrapper around the move_to_next function to count read chars *)
  let move_to_next () =
    incr read_chars_count;
    move_to_next ()
  in

  let bufferRes = Buffer.create 17 in

  (* max number of chars to be read. -1 means no limit *)
  let field_width = ref (-1) in

  (* **************** helper functions to read input *********************** *)
  let push_char_and_get_next a =
    if !field_width <> 0
    then  begin
      Buffer.add_char bufferRes a;
      move_to_next();
      decr field_width;
      get_char()
    end
    else raise Reached_width_limit

  and push_char cha =
    if !field_width <> 0 then begin
      Buffer.add_char bufferRes cha;
      move_to_next ();
      decr field_width
    end
    else raise Reached_width_limit
  in

  let read_while condition =
    let initial_value = get_char () in
    if condition initial_value then push_char initial_value;
    while (condition (get_char ()))
    do
      let next = get_char () in
      push_char next;
    done;
  in

  let skip_whitespace () =
    while is_whitespace (get_char ()) do
      move_to_next ()
    done
  in

  let check_width = function
    | Some w when w = 0 ->
      Value_parameters.error ~current:true
        "invalid conversion specification (illegal zero field width).";
      raise Undefined_behavior
    | _ -> ()
  in

  (* Read input while it is a prefix of a string matching a regexp and push
   * it to 'buffer'.
   * When finished reading, verify that the final buffer obtained matches
   * the regexp, and raise a Matching_failure if not.
   * The functions 'is_match' and 'is_prefix' may be built from a regexp
   * with the functions 'is_match_fun' and 'is_prefix_fun' *)
  let read_while_matches ?(is_strto = false) ~is_match ~is_prefix ~buffer () =
    (* The strto* and ato* functions allow more than one pushback, so
     * the return value might be different than the one of scanf on strings
     * such as "0x" or "100e ". The 'is_strto' variable allows to treat the
     * two cases differently *)

    let buff = Buffer.create 17 in
    let last = ref (get_char ()) in
    Buffer.add_char buff !last;
    let contents = ref (Buffer.contents buff) in
    let last_total_match = ref "" in
    let last_total_match_count = ref 0 in

    (try
       while is_prefix !contents do
         last := push_char_and_get_next !last;
         if is_match !contents then begin
           (* this information is for the strto* and ato* functions *)
           last_total_match := !contents;
           last_total_match_count := !read_chars_count;
         end;
         Buffer.add_char buff !last;
         contents := Buffer.contents buff;
       done;
     with Reached_width_limit | End_of_input -> ());


    if is_match (Buffer.contents buffer) then
      raise Finished
    else if is_strto then
      (* The strto* and ato* functions return the last matching sequence read,
       * even if more than one pushback is necessary to do so *)
      raise (Finished_strto (!last_total_match_count, !last_total_match))
    else
      (* if one pushback is not enough to have a matching sequence, the
       * scanf function has a Matching failure for the directive *)
      raise Matching_failure
  in
  (* *************** end of helper functions ******************************* *)

  (* Parse input according to the directive received *)
  let read_complete =
    (try
       match directive with
       | ReadStrto_float ->
         (* The strto* and ato* functions allow more than one pushback, so
          * the return value might be different than the one of scanf on strings
          * such as "0x" or "100e " *)
         skip_whitespace ();
         read_while_matches ~buffer:bufferRes ~is_strto:true
           ~is_match:is_float
           ~is_prefix:is_float_prefix ()
       | ReadStrto_int base ->
         (* The strto* and ato* functions allow more than one pushback, so
          * the return value might be different than the one of scanf on strings
          * such as "0x" or "100e " *)
         skip_whitespace ();
         read_while_matches ~buffer:bufferRes ~is_strto:true
           ~is_match:(is_base_string ~base)
           ~is_prefix:(is_base_prefix ~base) ()
       | Whitespace ->
         (try
            while is_whitespace (get_char ()) do
              move_to_next ()
            done
          with End_of_input -> ());
         raise Finished (* This directive never fails *)
       | Char ch ->
         (* Don't skip whitespace! *)
         let c = try get_char () with End_of_input -> raise Input_failure in
         if c = ch then begin
           push_char c;
           raise Finished
         end
         else raise Matching_failure
       | CS cs ->
         (* Input white-space characters are skipped, unless the specification
          * includes a '[', 'c' or 'n' specifier *)
         (match cs with
          | Normal
              { suppress = _;
                width;
                length_modifier = _;
                conversion_specifier } ->
            check_width width;
            field_width := (match width with None -> -1 | Some w -> w);
            ( match conversion_specifier with
              | 'd' | 'u' ->
                skip_whitespace ();
                read_while_matches ~buffer:bufferRes
                  ~is_match:is_decimal_string
                  ~is_prefix:is_decimal_prefix ()
              | 'i' ->
                (* 'i' can take either octal, decimal or hex depending on
                   the form of the input ("0.." for octal, "0x..." for
                   hex and leading non-zero for decimal) *)
                (* The format is like expected by `strtol` with base = 0 *)
                skip_whitespace ();
                read_while_matches ~buffer:bufferRes
                  ~is_match:is_integer_base_zero_string
                  ~is_prefix:is_integer_base_zero_prefix ()
              | 'o' ->
                skip_whitespace ();
                read_while_matches ~buffer:bufferRes
                  ~is_match:is_octal_string
                  ~is_prefix:is_octal_prefix ()
              |'x'| 'X'->
                skip_whitespace ();
                read_while_matches ~buffer:bufferRes
                  ~is_match:is_hex_string
                  ~is_prefix:is_hex_prefix ()
              | 'a' | 'e' | 'f' | 'g' | 'A' | 'E' | 'F' | 'G' ->
                skip_whitespace ();
                read_while_matches ~buffer:bufferRes
                  ~is_match:is_float
                  ~is_prefix:is_float_prefix ()
              | 'c' ->
                (* don't skip whitespace! *)
                let c = get_char () in
                (match width with
                 | None ->
                   push_char c;
                   raise Finished
                 | Some _ ->
                   (* read as many chars as specified by the field width *)
                   read_while (fun _ -> true))
              | 's' ->
                skip_whitespace ();
                read_while is_not_whitespace
              | '%' ->
                skip_whitespace ();
                let c =
                  try get_char () with End_of_input -> raise Input_failure
                in
                if c = '%' then begin
                  push_char c;
                  raise Finished
                end
                else raise Matching_failure
              | _ as other ->
                Value_parameters.error ~current:true
                  "Unknown specifier %c" other;
                raise Undefined_behavior)
          | Bracket
              {normal_fields =
                 {suppress=_; width; length_modifier=_; conversion_specifier=_};
               bracket_fields = {caret; scanlist}} ->

            (* don't skip whitespace! *)
            check_width width;

            field_width := (match width with None -> -1 | Some w -> w);
            (* The 'scanlist' is composed by the characters between
             * brackets.
             * The characters matched by the directive are those in
             * the 'scanset'. If there is no '^' after the first
             * bracket, the 'scanset' is equal to the scanlist,
             * otherwise the 'scanset' is composed of the chars
             * that are not in the 'scanlist'. *)
            let is_in_scanlist ch = String.contains scanlist ch in
            let is_in_scanset ch =
              (is_in_scanlist ch && (not caret)) ||
              (not (is_in_scanlist ch) && caret)
            in
            read_while is_in_scanset
         );
         raise Finished
     with
     | Finished -> true
     | Finished_strto (count, matched_string) ->
       read_chars_count := count;
       let matched_string =
         (* If no conversion could be performed, strto* and ato* return zero *)
         if matched_string = "" then begin
           read_chars_count := 0;
           string_zero
         end
         else matched_string
       in

       Buffer.clear bufferRes;
       Buffer.add_string bufferRes matched_string;
       true
     | Reached_width_limit | Read_String_Done ->
       Buffer.length bufferRes <> 0
     | End_of_input ->
       if Buffer.length bufferRes = 0 then raise Input_failure;
       (* The Matching_failure exception should be caught by the caller *)
       (* The Input_failure exception should be caught by the caller and result
        * in returning EOF value. *)
       true
    )
  in
  (* return value for function `get_input` *)
  !read_chars_count, Buffer.contents bufferRes, read_complete


(** [integer_func] executes different arithmetic calculations
    depending on the conversion specifier [cs], in order to return
    an integer value to be written to the memory state *)
let rec integer_func length string_s acc initial_value ?base cs =
  if acc >= length then initial_value
  else
    begin
      let ch = string_s.[acc] in
      let base =
        match cs with
        | 'x' | 'X' -> 16
        | 'o' -> 8
        | 'd'| 'u' | 'n' -> 10
        | 'i' ->
          if is_hex_base_zero string_s then 16
          else if is_octal_base_zero string_s then 8
          else if is_decimal_base_zero string_s then 10
          else assert false
        (* The '-' cs is used only for strtol, atoi and similar functions,
         * not for scanf *)
        | '-' when base = Some 0 ->
          if is_hex_base_zero string_s then 16
          else if is_octal_base_zero string_s then 8
          else if is_decimal_base_zero string_s then 10
          else assert false
        | '-' ->
          (match base with Some b -> b | None -> assert false)
        (* ******************** *)
        | _ ->
          Value_parameters.error ~current:true
            "invalid conversion specifier '%c'" cs;
          raise Undefined_behavior
      in

      if (acc <= 1 && base = 16) && is_hex_base_zero string_s then
        (* Skip hex prefix *)
        integer_func length string_s (acc+1) initial_value cs ~base
      else begin
        (* Multiply initial_value by base and add value of current digit *)
        let digit =
          (* in ASCII '0' < 'A' < 'a' *)
          if ch >= 'a' then (Char.code ch - (Char.code 'a')) + 10
          else if ch >= 'A' then (Char.code ch - (Char.code 'A')) + 10
          else Char.code ch - (Char.code '0')
        in
        assert (0 <= digit && digit < base);
        let digit = Integer.of_int digit in
        let current_value =
          Integer.add
            (Integer.mul (Integer.of_int base) initial_value)
            digit
        in
        integer_func length string_s (acc+1) current_value cs ~base
      end
    end


let write_string_to_memory char_size dst_addr_bytes state input_str ~is_char_cs=
  let exact = Location_Bytes.cardinal_zero_or_one dst_addr_bytes in
  if not exact then
    begin
      Value_parameters.warning ~current:true
        "Destination is not precise%t"
        Value_util.pp_callstack;
    end;

  let dst_addr_bits = ref (Locations.loc_bytes_to_loc_bits dst_addr_bytes) in
  let state = ref state in
  let sizeofchar_size = Int_Base.inject char_size in
  let sizeofchar_ival = Ival.inject_singleton char_size in
  let problem = ref false in
  let s = input_str in
  let length = String.length input_str in
  let with_alarms = with_alarms_from_problem problem in
  let fmt_len_bits = Ival.mul sizeofchar_ival !fmt_len
  and src_len_bits = Ival.mul sizeofchar_ival !src_len
  in

  (* Write char by char in the memory state *)
  for i=0 to pred length
  do
    let v = Cvalue.V.inject_int (Int.of_int (int_of_char s.[i])) in

    (* check if destination overlaps format string *)
    let overlap_status =
      Builtins_lib_tis_aux.overlap_status_loc_bits
        !dst_addr_bits sizeofchar_ival !fmt_loc_bits fmt_len_bits
    in
    begin match overlap_status
      with
      | Builtins_lib_tis_aux.Overlap ->
        Value_parameters.error ~current:true
          "variable %a overlaps format string in call to function sscanf; \
           assert(no overlap between format string and destination)."
          Locations.Location_Bytes.pretty dst_addr_bytes;
        raise Undefined_behavior
      | Builtins_lib_tis_aux.MayOverlap ->
        Value_parameters.warning ~current:true
          "variable %a  may overlap format string in call to function sscanf; \
           assert(no overlap between format string and destination)."
          Locations.Location_Bytes.pretty dst_addr_bytes
      | Builtins_lib_tis_aux.Separated -> ()
    end;

    let overlap_status =
      Builtins_lib_tis_aux.overlap_status_loc_bits
        !dst_addr_bits sizeofchar_ival !src_loc_bits src_len_bits
    in
    (* check if destination overlaps src string *)
    begin match overlap_status
      with
      | Builtins_lib_tis_aux.Overlap ->
        Value_parameters.error ~current:true
          "variable %a overlaps source string in call to function sscanf; \
           assert(no overlap between source and destination)."
          Locations.Location_Bytes.pretty dst_addr_bytes;
        raise Undefined_behavior
      | Builtins_lib_tis_aux.MayOverlap ->
        Value_parameters.warning ~current:true
          "variable %a may overlap source string in call to function sscanf; \
           assert(no overlap between source and destination)."
          Locations.Location_Bytes.pretty dst_addr_bytes
      | Builtins_lib_tis_aux.Separated -> ()
    end;

    let loc = Locations.make_loc !dst_addr_bits sizeofchar_size in
    state := Eval_op.add_binding ~exact ~with_alarms !state loc v;

    if length > i then
      dst_addr_bits :=  Location_Bits.shift sizeofchar_ival !dst_addr_bits
  done;

  (*Adding terminator character '\0' to the string*)
  if not is_char_cs then
    begin
      let v = Cvalue.V.singleton_zero in
      let loc = Locations.make_loc !dst_addr_bits sizeofchar_size in
      state := Eval_op.add_binding ~exact ~with_alarms !state loc v;
    end;

  (*The value for 'problem' is changed if an error occurs during the instruction
    'Eval_op.add_binding'*)
  if !problem then
    begin
      warn_destination_possibly_invalid ();
      raise Undefined_behavior
    end
  else
    !state

let parse_float_from_string ~length_modifier float_string =
  let underflow = ref false in

  (* check input string *)
  if not (is_float float_string) then raise Matching_failure;

  (* TODO: to implement NAN and INFINITY value write, just
   * return the correct abstract values below and remove the exceptions *)
  if is_nan_float float_string then
    (* return abstract NAN value HERE and remove exception *)
    raise (Not_yet_implemented "NAN and INFINITY float values")
  else if is_infinity_float float_string then
    (* return abstract NAN value HERE and remove exception *)
    raise (Not_yet_implemented "NAN and INFINITY float values")
  else (* regular float (not NAN or INFINITY) *) begin
    (* Split a float with no decimal point into an integer and an
       exponent part *)
    let get_int_and_exp_parts float_string ~hex =
      (* This regexp is enough to separate mantissa from exponent,  considering
       * that 'float_string' has already been processed and is thus known to
       * have valid format *)
      let decimal_re_str = "\\([+-]?[0-9]+\\)\\(e[+-]?[0-9]+\\)" in
      let hex_re_str = "\\([+-]?0x[0-9a-f]+\\)\\(p[+-]?[0-9]+\\)" in
      let re_str = if hex then hex_re_str else decimal_re_str in
      let float_with_exponent_nodot_re = Str.regexp_case_fold re_str in

      (* This allows to catch some corner cases where there is no digit after
       * the exponent, such as '100e' *)
      if not (Str.string_match float_with_exponent_nodot_re float_string 0)
      then raise Matching_failure;

      let int_part = Str.matched_group 1 float_string in
      let exp_part = Str.matched_group 2 float_string in
      int_part,exp_part
    in

    (* The single_precision_of_string function available in the
       Floating_point.ml file does not deal with negative floats or
       integers, nor with floats without exponent part or
       decimal-point.
       Because of this, we need to modify the string read before
       passing it *)
    let float_string =
      if String.contains float_string '.' then float_string
      else
        (* float doesn't contain a decimal-dot: add it in the right place *)
        begin
          let is_hex_float = is_hex_float float_string in
          let exp_symbol = if is_hex_float then 'p' else 'e' in
          if not String.(contains (lowercase_ascii float_string) exp_symbol)
          then (* float has no exponent nor dot, add the dot at the end *)
            float_string ^ "."
          else
            (* float has exponent but no dot, add it before the exponent *)
            let int_part,exp_part =
              get_int_and_exp_parts float_string ~hex:is_hex_float
            in
            (int_part ^ ".0") ^ exp_part
        end
    in

    (* For negative numbers we create a new string without the minus sign, then
     * we call the function and add the sign afterwards *)
    let ch = float_string.[0] in
    let len = String.length float_string in
    let is_negative = ch = '-' in

    (* suppress leading +/- *)
    let float_string =
      if is_sign ch then String.sub float_string 1 (len-1)
      else float_string
    in

    let floating_result = try match length_modifier with
      | ""  ->
        let f = Floating_point.single_precision_of_string float_string in
        if (f.f_upper <> f.f_lower) && (f.f_nearest < 1.17549435082228751e-38)
        then underflow := true;
        f
      | "l" | "L" ->
        let f = Floating_point.double_precision_of_string float_string in
        if (f.f_lower <> f.f_upper) && (f.f_nearest < min_float)
        then underflow := true;
        f
      | _ -> assert false
      with _ ->
        Value_parameters.error ~current:true
          "could not parse floating point number \"%s\"" float_string;
        raise Matching_failure
    in

    let f =
      let f_abs = floating_result.f_nearest in
      if is_negative then ~-. f_abs else f_abs
    in
    (* value if not NAN or INFINITY *)
    f, !underflow
  end


let write_float_to_memory ~length_modifier ~typ dst_addr state float_string =
  let exact = Location_Bytes.cardinal_zero_or_one dst_addr in
  if not exact then
    begin
      Value_parameters.warning ~current:true
        "Destination is not precise%t"
        Value_util.pp_callstack;
    end;

  let float_size = Bit_utils.sizeof_pointed (List.hd typ) in
  let problem = ref false in
  let with_alarms = with_alarms_from_problem problem in

  let f, _ = parse_float_from_string ~length_modifier float_string in

  let v =   Cvalue.V.inject_float (Fval.F.of_float f)
  in

  let locat =
    Locations.make_loc (Locations.loc_bytes_to_loc_bits dst_addr) float_size
  in
  let state = Eval_op.add_binding ~exact ~with_alarms state locat v in

  if !problem then
    Value_parameters.warning ~current:true
      "destination possibly invalid. \
       Write_float assert(match format and arguments)%t"
      Value_util.pp_callstack;
  state

let write_int_to_memory
    dst_addr_bytes state str_to_write base ctype =
  let exact = Location_Bytes.cardinal_zero_or_one dst_addr_bytes in
  if not exact
  then begin
    Value_parameters.warning ~current:true
      "Destination is not precise%t"
      Value_util.pp_callstack;
  end;

  let cil_size = Cil.bytesSizeOf ctype in

  let dst_addr_bits = Locations.loc_bytes_to_loc_bits dst_addr_bytes in
  let sizeofType = 8 * cil_size in
  let sizeofType = Integer.of_int sizeofType in
  let sizeofType_size = Int_Base.inject sizeofType in
  let problem = ref false in
  let is_negative = str_to_write.[0] = '-' in
  let with_alarms = with_alarms_from_problem problem in

  (* strip leading sign and calculate length *)
  let s, length =
    let l = String.length str_to_write in
    if is_sign str_to_write.[0] then
      let pred_l = pred l in
      String.sub str_to_write 1 pred_l, pred_l
    else
      str_to_write, l
  in
  let integer =
    integer_func length s 0 Integer.zero base
  in
  let integer = if is_negative then Integer.neg integer else integer in
  let v = handle_overflow ctype (Cvalue.V.inject_int integer) in
  let loc = Locations.make_loc dst_addr_bits sizeofType_size in
  let state = Eval_op.add_binding ~exact ~with_alarms state loc v in

  if !problem then warn_destination_possibly_invalid ();
  state

let ten = Integer.of_int 10

let interpret_format ~character_width state format args (get_source, advance) =
  (* This variable keeps track of the number of chars that have been read from
   * the input stream. This is NOT always the same as the number of chars
   * written by the directives after conversion, as some directives might ignore
   * some characters. For instance, whitespace present in the stream might never
   * be converted (the 'white-space' directive reads the input up to the first
   * non-whitespace character, but this whitespace is discarded and not
   * written to a variable *)
  let char_count = ref 0 in

  (* Wrapper around get_input to count the number of read chars *)
  let get_input  (f,g) directive =
    let count, contents, status =
      get_input (f,g) directive in
    char_count := !char_count + count;
    (contents, status)
  in

  let warn_too_many_args = function () ->
    Value_parameters.warning ~current:true
      "Too many arguments for format. This is technically allowed.%t"
      Value_util.pp_callstack;
  in

  if not (Location_Bytes.cardinal_zero_or_one format) then
    begin
      Value_parameters.error ~current:true
        "Format string could not be resolved%t" Value_util.pp_callstack;
      raise Db.Value.Aborted
    end;

  let state = ref state in
  let format = ref (loc_bytes_to_loc_bits format) in
  let sizeofchar_size = Int_Base.inject character_width in
  let sizeofchar_ival = Ival.inject_singleton character_width in
  let seen_percent = ref Not_seen in
  let args = ref args in
  let counter = ref 0 in
  let num_ch = ref 0 in

  (* The following 3 variables are dedicated to store the values of suppress,
   * width and modifier in case of dealing with a bracket expression *)
  let suppress_bracket = ref false in
  let width_bracket = ref None in
  let length_mod_bracket = ref "" in

  (* this variable indicates whether the execution must stop *)
  let read_ok_aux = ref true in
  (* this will be true if ^ appears after a left bracket *)
  let caret = ref false in
  (* this variable will read the number of characters after '[' *)
  let counter_bracket = ref 0 in
  (* To store the elements in the buffer *)
  let bracket_buff = Buffer.create 17 in
  (* keeps the value to be stored in the variable *)
  let bufferMod = Buffer.create 17 in
  try
    (* Reading the format character by character *)

    let get_format_char = fun () ->
      let loc = Locations.make_loc !format sizeofchar_size in
      let c = Eval_op.find ~with_alarms !state loc in
      let c = Cvalue.V.project_ival c in
      let c = Ival.project_int c in
      let c = Int.to_int c in
      assert (-128 <= c && c <= 255);
      let format_ends = (c = 0) in
      if format_ends
      then raise Interpret_format_finished;
      let code =
        if c >= 0
        then c
        else c + 256
      in
      char_of_int code
    in

    let move_to_next () =
      format := Location_Bits.shift sizeofchar_ival !format
    in

    while true do
      let c = get_format_char () in

      (* Expected_typ is a list with the possible and expected types of the arg
       * The structure of this function is equal to the one you can find in
       * printf builtin
      *)
      let eat_arg_and_reset_seen_percent expected_typ allowable_typ =
        match !args with
        | (arg_exp, arg_v, _) :: rest_args ->
          let arg_typ = Cil.typeOf arg_exp in
          let compare_typ = Cabs2cil.compatibleTypesp arg_typ in
          let v =
            if not (List.exists compare_typ expected_typ) then
              begin
                Value_parameters.warning ~current:true
                  "argument %a has type %a but format indicates %s%a"
                  Printer.pp_exp arg_exp
                  Printer.pp_typ arg_typ
                  (match expected_typ with
                   | [_] -> ""
                   | [] -> assert false
                   | _ -> "one of ")
                  (Pretty_utils.pp_list ~sep:", " Printer.pp_typ) expected_typ;
                if List.exists compare_typ allowable_typ
                then begin
                  Value_parameters.warning ~current:true
                    "Continuing analysis because this seems innocuous";
                  let typ = List.hd expected_typ in
                  let signed = Bit_utils.is_signed_int_enum_pointer typ in
                  let size = Integer.of_int (Cil.bitsSizeOf typ) in
                  let v, _ = Cvalue.V.cast ~size ~signed arg_v in
                  v
                end
                else
                  raise Undefined_behavior
              end
            else
              arg_v
          in
          args := rest_args;
          seen_percent := Not_seen;
          v
        | [] ->
          Value_parameters.error ~current:true "Too few arguments for format";
          raise Undefined_behavior
      in

      let current_seen_percent = !seen_percent in
      ( match current_seen_percent with
        (*It means we have a format like "%[...."*)
        | InsideBrackets scanlist ->
          ( match c with
            | '^' ->
              (*if we have a pattern like "[^"*)
              if !counter_bracket = 0 then
                caret := true
              else
                begin
                  Buffer.add_char bracket_buff '^';
                  let str = Buffer.contents bracket_buff in
                  seen_percent := InsideBrackets str;
                end;
            | ']' ->
              (*With a format of "[]" we add ']' to the buffer of chars
                and we continue searching for the second ']' that will
                end the sequence of chars to be read*)
              if !counter_bracket = 0 then
                begin
                  Buffer.add_char bracket_buff ']';
                  let str = Buffer.contents bracket_buff in
                  seen_percent := InsideBrackets str;
                end
              else
                (* When we find a ']' that corresponds to the end of the bracket
                   expression, we start reading the matching input *)
                begin
                  let typ, allowable_typ =
                    get_allowable_types '[' !length_mod_bracket
                  in
                  let source_char_size = match !length_mod_bracket with
                    | "" -> Bit_utils.sizeofchar ()
                    | "l" ->
                      let wchar = Cil.theMachine.Cil.wcharType in
                      Int.of_int (Cil.bitsSizeOf wchar)
                    | _ as other ->
                      Value_parameters.error ~current:true
                        "Invalid length modifier %s" other;
                      raise Undefined_behavior
                  in
                  let width = match !width_bracket with
                    | None -> None
                    | Some wnum -> Some (Int.to_int wnum)
                  in
                  let caret = !caret in

                  let directive =
                    make_bracket_cs_directive
                      ~suppress:!suppress_bracket ~width
                      ~conversion_specifier:'[' ~caret ~scanlist ()
                  in
                  let s1, read_ok = get_input (get_source, advance) directive in

                  let s1_len = String.length s1 in
                  read_ok_aux := read_ok;
                  num_ch := !num_ch + s1_len;

                  if s1_len = 0 && not !suppress_bracket then
                    begin
                      Value_parameters.error ~current:true
                        "Variable will remain not initialized";
                      raise Undefined_behavior;
                    end;

                  if not !suppress_bracket then
                    begin
                      let arg =
                        eat_arg_and_reset_seen_percent typ allowable_typ in
                      state :=
                        write_string_to_memory source_char_size arg
                          !state s1 ~is_char_cs:false;
                      incr counter;
                    end;

                  Buffer.reset bracket_buff;
                end
            | _ as c ->
              Buffer.add_char bracket_buff c;
              let str = Buffer.contents bracket_buff in
              seen_percent := InsideBrackets str;
              incr counter_bracket;
          )
        | Seen (suppress, width, length_modifier) ->
          ( let conversion_specifier = c in
            match conversion_specifier with
            | '*' when width = None && length_modifier = "" ->
              seen_percent := Seen(true, None, "")
            | digit when is_decimal_digit digit && length_modifier = "" ->
              begin
                let catc i =
                  Integer.add
                    (Integer.mul ten i)
                    (Integer.of_int (int_of_char c - (Char.code '0'))) in
                let width =
                  match width with
                  | None -> Integer.zero
                  | Some l -> l in
                let width = catc width in
                seen_percent := Seen (suppress, Some width, "")
              end;
            | '%' ->
              begin
                if (length_modifier <> "") || (width <> None) || suppress then
                  begin
                    Value_parameters.error ~current:true "invalid directive";
                    raise Undefined_behavior;
                  end;

                let directive =
                  make_normal_cs_directive
                    ~width:None ~suppress ~conversion_specifier ()
                in
                let _, read_ok = get_input (get_source,advance) directive in

                read_ok_aux :=  read_ok;
                incr num_ch;

                if not !read_ok_aux then
                  raise Interpret_format_not_finished;

                seen_percent := Not_seen
              end
            | 'd' | 'i' | 'x' | 'X' | 'u' | 'o' | 'c' | 's' ->
              begin
                let typ, allowable =
                  get_allowable_types conversion_specifier length_modifier in
                let ctype = Cil.typeOf_pointed (List.hd typ) in
                let width =
                  match width with
                  | None -> None
                  | Some width_a -> Some (Int.to_int width_a)
                in
                let directive =
                  make_normal_cs_directive
                    ~width ~suppress ~length_modifier ~conversion_specifier ()
                in
                let s1,read_ok = get_input (get_source,advance) directive in

                num_ch := !num_ch + (String.length s1);
                if not read_ok then raise Interpret_format_not_finished;

                if not suppress then
                  begin
                    let arg = eat_arg_and_reset_seen_percent typ allowable in
                    state :=
                      ( match conversion_specifier with
                        | 'c' | 's' ->
                          let source_char_size =
                            get_char_size_from_modifier length_modifier
                          in
                          write_string_to_memory source_char_size arg !state s1
                            ~is_char_cs:(conversion_specifier = 'c')
                        | _ ->
                          write_int_to_memory
                            arg !state s1 conversion_specifier ctype);
                    incr counter;
                  end
                else
                  begin
                    seen_percent := Not_seen;
                  end;
              end;
              Buffer.clear bufferMod;
            | '[' ->
              suppress_bracket := suppress;
              width_bracket := width;
              length_mod_bracket := length_modifier;
              (*Now, our seen_percent assume is defined as InsideBrackets to
               * keep the execution in a separated state...*)
              seen_percent := InsideBrackets "";
            | 'n' ->
              begin
                let typ,allowable =
                  get_allowable_types conversion_specifier length_modifier
                in
                let ctype = Cil.typeOf_pointed (List.hd typ) in

                begin match width with
                  | None -> ()
                  | Some _ ->
                    begin
                      Value_parameters.error ~current:true
                        "@[the 'n' conversion specifier can't include \
                         a field width.@]@.";
                      raise Undefined_behavior;
                    end
                end;

                if suppress then
                  begin
                    Value_parameters.error ~current:true
                      "@[the 'n' conversion specifier can't include \
                       an assignment-suppressing character.@]@.";
                    raise Undefined_behavior;
                  end;

                let arg = eat_arg_and_reset_seen_percent typ allowable in
                (* let num_ch_s = string_of_int !num_ch in *)
                let num_ch_s = string_of_int !char_count in

                let new_state =
                  write_int_to_memory
                    arg
                    !state
                    num_ch_s
                    conversion_specifier
                    ctype
                in
                state := new_state
              end;
              Buffer.clear bufferMod;
            | 'p' -> raise (Not_yet_implemented "%p modifier")
            (* TODO: implement '%p' conversion specifier handling *)

            (* Warning: Only accepts an empty modifier otherwise the
             * behavior is undefined *)
            | 'a' | 'e' | 'f' | 'g' | 'A' | 'E' | 'F' | 'G' ->
              begin
                let width = match width with
                  | None -> None
                  | Some width_a -> Some (Int.to_int width_a)
                in
                let typ,allowable =
                  get_allowable_types conversion_specifier length_modifier
                in
                let directive =
                  make_normal_cs_directive
                    ~width ~suppress ~length_modifier ~conversion_specifier ()
                in
                let s1, read_ok = get_input (get_source,advance) directive in

                if not read_ok then
                  raise Interpret_format_not_finished;
                (* check if the input string is a valid match *)
                if not (is_float s1) then raise Matching_failure;

                if not suppress then
                  begin
                    let arg = eat_arg_and_reset_seen_percent typ allowable in
                    state :=
                      write_float_to_memory ~length_modifier ~typ arg !state s1;
                    incr counter;
                  end
                else
                  seen_percent := Not_seen;
              end;
              Buffer.clear bufferMod;
            | 'h' | 'l' | 'j' | 'z' | 't' | 'L' as c ->
              Buffer.add_char bufferMod c;
              let len_mod = Buffer.contents bufferMod in
              seen_percent := Seen (suppress, width, len_mod)
            | _ as other ->
              begin
                Value_parameters.warning ~current:true
                  "Format '%c' undefined or not supported (yet)" other;
                abort()
              end;
          )
        | Not_seen ->
          ( match c with
            | '%' ->
              seen_percent := Seen(false, None, "")
            | w when is_whitespace w ->
              (* A directive composed of white-space character(s) is executed by
               * reading input up to the first non-white-space character (which
               * remains unread), or until no more characters can be read *)
              let directive = Whitespace in
              let _, read_ok = get_input (get_source,advance) directive in
              if not read_ok then
                raise Interpret_format_not_finished;
            | _ ->
              (* A directive that is an ordinary multibyte character (neither
               * '%' nor a white-space character), is executed by reading an
               * input character if it matches the one in the directive *)
              let directive = Char c in
              let _, read_ok = get_input (get_source,advance) directive in

              if not read_ok then
                raise Interpret_format_not_finished;
          ));
      move_to_next ();
    done;
    assert false
  with
  | Integer_overflow o ->
    Value_parameters.error ~current:true
      "integer overflow trying to write %a to a variable of type \"%a\"; \
       assert (result of conversion is representable in the object)"
      Cvalue.V.pretty o.integer Cil_printer.pp_typ o.typ;
    raise Undefined_behavior
  | Cvalue.V.Not_based_on_null -> assert false (* TODO? *)
  | Interpret_format_not_finished | Interpret_format_finished
  | Matching_failure ->
    if !args <> [] then warn_too_many_args ();
    !state,!counter
  | Input_failure ->
    (* The function returns the EOF macro if an input failure occurs before the
     * first conversion (if any) has compleated *)
    if !args <> [] then warn_too_many_args ();
    if !counter = 0 then counter := eof;
    !state,!counter
  | Not_yet_implemented feature ->
    Value_parameters.warning ~current:true
      "%s not supported yet. Returning current state" feature;
    !state,!counter


let char_width = Bit_utils.sizeofchar()


let interpret_format_char x = interpret_format ~character_width:char_width x


let define_danger_zone src fmt state =
  (* Trying to use the strlen builtin to get lengths *)
  let len str =
    let open Aux.StringAndArrayUtilities.Strlen in
    let abstract_strlen = get_abstract_strlen () in
    let emit_alarm = {
      strlen_alarm_invalid_string = (fun () -> ())
    } in (* TODO: use a real alarm like in strlen builtin *)
    let character_bits = char_width in
    abstract_strlen ~character_bits ~emit_alarm ~max:str_infinity str state
  in
  src_len := len src;
  fmt_len := len fmt;
  src_loc_bits := Locations.loc_bytes_to_loc_bits src;
  fmt_loc_bits := Locations.loc_bytes_to_loc_bits fmt


let tis_sscanf state args =
  try
    match args with
    |  (_, src, _) as source :: ((_, format, _) as fmt) :: rest ->
      (* define a zone in which it is not allowed to write *)
      check_string_validity state source;
      check_string_validity state fmt;
      define_danger_zone src format state;

      let src = Locations.loc_bytes_to_loc_bits src in
      let state, counter =
        let functions =
          build_functions_from_state
            (* String validity already checked by check_string_validity *)
            ~character_width:(Bit_utils.sizeofchar ())
            src state
        in
        interpret_format_char state format rest functions
      in
      (* The variable 'count' is incremented every time an input is assigned
       * to an argument.  The correspondent value is the one returned by sscanf
       * and scanf *)
      let count = Ival.of_int counter in
      let count = Cvalue.V.inject_ival count in
      let count = Eval_op.wrap_int count in
      { Value_types.c_values = [ Value_types.StateOnly(count, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | [] ->
      Value_parameters.error ~current:true
        "sscanf() needs at least one argument. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
    | _ ->
      Value_parameters.error ~current:true
        "Incorrect Format. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Invalid_character -> bottom_result
  | Interpret_format_partial | Undefined_behavior ->
    Value_parameters.error ~current:true
      "assert(match format and arguments)%t. \
       Ending execution trace here" Value_util.pp_callstack;
    bottom_result


let tis_scanf state args =
  try
    (* The input is read through this simple instruction, but right instead of
     * a location, we deal with a string. Please, notice the difference of
     * 'build_function_from_string src' and build_functions_from_state.
     * This was the strategy implemented in order to use the same function
     * (interpret_format) for both sscanf and scanf.  *)

    let src = input_line stdin in
    match args with
    | (_, format, _) as fmt :: rest ->
      check_string_validity state fmt;
      let state,counter =
        interpret_format_char state format rest
          (build_functions_from_string src)
      in
      let count = Ival.of_int counter in
      let count = Cvalue.V.inject_ival count in
      let count = Eval_op.wrap_int count in
      { Value_types.c_values = [ Value_types.StateOnly(count, state) ] ;
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | [] ->
      Value_parameters.error ~current:true
        "scanf() needs at least one argument. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted

  with
  | Invalid_character -> bottom_result
  | Interpret_format_partial | Undefined_behavior ->
    Value_parameters.error ~current:true
      "assert(match format and arguments)%t. \
       Ending execution trace here" Value_util.pp_callstack;
    bottom_result


(* **** strto{l,ll,ul,ull} builtins **************************************** *)

let get_min_max_int_from_type ~typ =
  let bitsize = Cil.bitsSizeOf typ in
  let is_signed = Bit_utils.is_signed_int_enum_pointer typ in
  let max, min =
    if is_signed then
      Cil.max_signed_number bitsize, Cil.min_signed_number bitsize
    else
      Cil.max_unsigned_number bitsize, Cil.max_unsigned_number bitsize
  in
  min, max


(* This abstract version handles uniformly all the strto{l,ll,ul,ull}
 * and ato{i,l,ll} builtins according to their expected return type *)
let abstract_strto_int ~is_ato ~state ~nptr ~endptr ~base ~ret_type =
  let ctype = match ret_type with
    | Int ->  Cil.intType
    | Long ->  Cil.longType
    | LLong ->  Cil.longLongType
    | ULong ->  Cil.ulongType
    | ULLong ->  Cil.ulongLongType
  in

  let get_char, advance =
    build_functions_from_state
      (* String validity already checked by check_string_validity *)
      ~character_width:(Bit_utils.sizeofchar ())
      (Locations.loc_bytes_to_loc_bits nptr)
      state
  in

  let directive =
    if (base = 0) || (2 <= base && base <= 36) then ReadStrto_int base
    else begin
      Value_parameters.warning ~current:true
        "wrong base for strto* function. assert (base == 0 || 2 <= base <= 36)";
      raise Undefined_behavior
    end
  in

  let read_chars_count, parsed_int_string, _ =
    try
      get_input (get_char, advance) directive
    with Input_failure ->
      0, string_zero, false
  in

  let is_negative = parsed_int_string.[0] = '-' in

  (* strip leading sign and calculate length *)
  let s, length =
    let l = String.length parsed_int_string in
    if is_sign parsed_int_string.[0] then
      String.sub parsed_int_string 1 (l - 1), (l - 1)
    else
      parsed_int_string, l
  in
  let integer =
    let tmp = (* integer without optional sign *)
      integer_func length s 0 Integer.zero '-' ~base
    in
    if is_negative then begin
      (* for strtoul (strtoull), if there was a leading sign, the function
         returns the opposite of the result of the conversion represented as
         an unsigned value, unless the original (nonnegated) value would
         overflow; in the latter case return ULONG_MAX (ULLONG_MAX) *)
      match ret_type with
      | ULong | ULLong ->
        let _, max = get_min_max_int_from_type ~typ:ctype in
        Integer.succ (Integer.sub max tmp)
      | Int | Long | LLong ->
        Integer.neg tmp
    end
    else (* positive values *)
      tmp
  in

  (* Integer return value *)
  let ret, errno =
    try handle_overflow ctype (Cvalue.V.inject_int integer), Ival.zero
    with
      Integer_overflow o ->
      Value_parameters.warning ~current:true
        "integer overflow when writing %a to a location \
         of type \"%a\""
        Cvalue.V.pretty o.integer Cil_printer.pp_typ o.typ;
      if is_ato then
        (* From c11, p7.22.1: The functions atof, atoi, atol, and atoll need not
           affect the value of the integer expression errno on an error. If the
           value of the result cannot be represented, the behavior is
           undefined. *)
        raise Undefined_behavior
      else begin (* strto{l,ll,ul,ull} functions *)
        (* If the correct value is outside the range of representable values,
           LONG_MIN, LONG_MAX, LLONG_MIN, LLONG_MAX, ULONG_MAX or ULLONG_MAX
           is returned (according to the return type and sign of the value,
           if any), and the value of the ERANGE macro is stored in 'errno' *)
        let is_negative =
          Int.lt (Ival.project_int (Cvalue.V.project_ival o.integer)) Int.zero
        in
        let v =
          let min,max = get_min_max_int_from_type ~typ:ctype in
          if is_negative then
            (* for unsigned types min equals ULONG_MAX or ULLONG_MAX,
               so the following line will yield the expected return value *)
            min
          else
            max
        in
        Cvalue.V.inject_int v, Ival.inject_singleton (Int.of_int erange)
      end
  in

  let state =
    if is_ato
    then state
    else
      let state = Aux.optionally_set_errno errno state in
      if Cvalue.V.is_zero endptr
      then state
      else
        let size = Int_Base.inject (Integer.of_int (Bit_utils.sizeofpointer ()))
        in
        let exact = Location_Bytes.cardinal_zero_or_one endptr in
        let endptr = Locations.loc_bytes_to_loc_bits endptr in
        let endptr = Locations.make_loc endptr size in
        let v = Location_Bytes.shift (Ival.of_int read_chars_count) nptr in
        (* Write pointer to final string (only for strto* builtins) *)
        Eval_op.add_binding ~exact ~with_alarms state endptr v
  in

  (* Wrap the return value *)
  let wrapped_int = Some (Eval_op.offsetmap_of_v ~typ:ctype ret) in

  state, wrapped_int


(* *********  strto{l,ll,ul,ull} builtins ********************************** *)
let tis_strto_int state args ~ret_type ~fname =
  debug_msg "Call to %s" fname;
  try
    match args with
    |  [(_, nptr, _) as source; (_, endptr, _); (_, base, _)] ->
      check_string_validity state source;
      let base = match Ival.min_int (Cvalue.V.project_ival base) with
        | Some m -> Int.to_int m
        | None -> 0
      in
      let state, ret =
        abstract_strto_int ~is_ato:false ~state ~nptr ~endptr ~base ~ret_type
      in

      { Value_types.c_values = [ Value_types.StateOnly(ret, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | _ ->
      Value_parameters.error ~current:true
        "function %s, assert(call with correct arguments)%t"
        fname Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Invalid_character -> bottom_result
  | Interpret_format_partial | Undefined_behavior ->
    Value_parameters.error ~current:true
      "function %s, assert(valid %s representation)%t. \
       Ending execution trace here"
      fname (string_of_int_type ret_type) Value_util.pp_callstack;
    bottom_result


(* Define the strto{l,ll,ul,ull} functions by specializing the generic
   version *)
let tis_strtol   state args =
  tis_strto_int state args ~ret_type:Long ~fname:"strtol"
let tis_strtoll  state args =
  tis_strto_int state args ~ret_type:LLong ~fname:"strtoll"
let tis_strtoul  state args =
  tis_strto_int state args ~ret_type:ULong ~fname:"strtoul"
let tis_strtoull state args =
  tis_strto_int state args ~ret_type:ULLong ~fname:"strtoull"


(* *********  ato{i,l,ll} builtins ***************************************** *)
let tis_ato_int state args ~ret_type ~fname =
  try
    match args with
    |   [(_, nptr, _) as source] ->
      check_string_validity state source;
      let base = 10 in
      let endptr = Cvalue.V.singleton_zero in
      let is_ato = true in

      let _, ret =
        abstract_strto_int ~state ~is_ato ~nptr ~endptr ~base ~ret_type
      in
      { Value_types.c_values = [ Value_types.StateOnly(ret, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | _ ->
      Value_parameters.error ~current:true
        "function %s, assert(call with correct arguments)%t"
        fname Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Invalid_character -> bottom_result
  | Interpret_format_partial | Undefined_behavior ->
    Value_parameters.error ~current:true
      "function %s, assert(valid %s representation)%t. \
       Ending execution trace here"
      fname (string_of_int_type ret_type) Value_util.pp_callstack;
    bottom_result


(* Define the ato{i,u,l} functions by specializing the generic version *)
let tis_atoi  state args = tis_ato_int state args ~ret_type:Int ~fname:"atoi"
let tis_atol  state args = tis_ato_int state args ~ret_type:Long ~fname:"atol"
let tis_atoll state args = tis_ato_int state args ~ret_type:LLong ~fname:"atoll"


(* *********  strto{d,f,ld} builtins ********************************** *)

let get_min_max_float_from_type ~ret_type =
  let max_float = Floating_point.max_single_precision_float in
  let min_float = -. max_single_precision_float in
  let max_double = Pervasives.max_float in
  let min_double = -. Pervasives.max_float in
  let max_long_double =
    max_float (* TODO: long double not supported by Frama-C? *)
  in
  let min_long_double = min_float in

  match ret_type with
  | Float -> min_float, max_float
  | Double -> min_double, max_double
  | LDouble -> min_long_double, max_long_double

(* We assume HUGE_VAL* to be equal to the max of the type.
 * TODO: this matches current Frama-C headers. If changes in the
 * headers occur, update accordingly *)
let huge_val_from_type ~ret_type =
  let _min,max = get_min_max_float_from_type ~ret_type in
  max

(* This abstract version handles uniformly all the strto{d,f,lf}
 *  and the atof builtins according to their expected return type *)
let abstract_strto_float ?(is_ato = false) ~state ~nptr ~endptr ~ret_type () =
  debug_msg ">>>+ abstract_strto_float: nptr=%a endptr=%a"
    Cvalue.V.pretty nptr Cvalue.V.pretty endptr;

  let ctype, length_modifier = match ret_type with
    | Double ->  Cil.doubleType, "l"
    | Float  ->  Cil.floatType, ""
    | LDouble -> Cil.longDoubleType, "L"
  in

  let min,max = get_min_max_float_from_type ~ret_type:ret_type in
  let get_char, advance =
    build_functions_from_state
      (* String validity already checked by check_string_validity *)
      ~character_width:(Bit_utils.sizeofchar ())
      (Locations.loc_bytes_to_loc_bits nptr)
      state
  in

  let directive = ReadStrto_float in

  (* Float return value *)
  let ret, read_chars_count, errno =
    try
      let read_chars_count, parsed_float_string, _ =
        get_input (get_char, advance) directive
      in
      let f, underflow =
        parse_float_from_string ~length_modifier parsed_float_string in

      debug_msg "Underflow = %b" underflow;

      if min <= f && f <= max && not underflow then
        Cvalue.V.inject_float (Fval.F.of_float f), read_chars_count, Ival.zero
      else begin
        Value_parameters.warning ~current:true
          "overflow or underflow trying to write %s to a \
           variable of type \"%a\""
          parsed_float_string  Cil_printer.pp_typ ctype;
        if is_ato then
          (* For the atof function, if the value of the result cannot be
             represented, the behavior is undefined (and it needs no affect the
             value of the 'errno' variable on an error. *)
          raise Undefined_behavior
        else
          (* strto{d,f,ld} functions *)

          (* - If the correct value *overflows* and default rounding
             is in effect, plus or minus HUGE_VAL, HUGE_VALF or
             HUGE_VALL is returned (according to the return type and
             sign of the value), and the value of the ERANGE macro is
             stored in errno.
             - If the result *underflows*, the functions return a value whose
             magnitude is no greater than the smallest normalized positive
             number in the return type; whether errno acquires the value ERANGE
             is implementation-defined.
             * REMARK: our implementation does set errno to ERANGE in
             case of an undeflow. *)

          let huge_val = huge_val_from_type ~ret_type in
          let v =
            if max < f then huge_val
            else if f < min then -. huge_val
            else if underflow then f
            else raise Undefined_behavior
          in
          let erange = Ival.inject_singleton (Int.of_int erange) in
          Cvalue.V.inject_float (Fval.F.of_float v), read_chars_count, erange
      end
    with
    | Matching_failure ->
      Cvalue.V.inject_float (Fval.F.of_float 0.0), 0, Ival.zero
  in

  let state =
    if is_ato
    then state
    else
      let state = Aux.optionally_set_errno errno state in
      if Cvalue.V.is_zero endptr
      then state
      else
        let size = Int_Base.inject (Integer.of_int (Bit_utils.sizeofpointer ()))
        in
        let exact = Location_Bytes.cardinal_zero_or_one endptr in
        let endptr = Locations.loc_bytes_to_loc_bits endptr in
        let endptr = Locations.make_loc endptr size in
        let v = Location_Bytes.shift (Ival.of_int read_chars_count) nptr in
        (* Write pointer to final string (only for strto* builtins) *)
        Eval_op.add_binding ~exact ~with_alarms state endptr v
  in

  (* Wrap the return value *)
  let wrapped_float = Some (Eval_op.offsetmap_of_v ~typ:ctype ret) in

  state, wrapped_float

let tis_strto_float state args ~ret_type ~fname =
  debug_msg "Call to %s" fname;
  try
    match args with
    |  [(_, nptr, _) as source; (_, endptr, _)] ->
      check_string_validity state source;
      let state,ret =
        abstract_strto_float ~state ~nptr ~endptr  ~ret_type () in

      { Value_types.c_values = [ Value_types.StateOnly(ret, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | _ ->
      Value_parameters.error ~current:true
        "function %s, assert(call with correct arguments)%t"
        fname Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Invalid_character -> bottom_result
  | Interpret_format_partial | Undefined_behavior ->
    Value_parameters.error ~current:true
      "function %s, assert(valid %s representation)%t. \
       Ending execution trace here"
      fname (string_of_float_type ret_type) Value_util.pp_callstack;
    bottom_result


(* Define the strto{d,f,ld} functions by specializing the generic version *)
let tis_strtod  state args =
  tis_strto_float state args ~ret_type:Double ~fname:"strtod"
let tis_strtof  state args =
  tis_strto_float state args ~ret_type:Float ~fname:"strtof"
let tis_strtold  state args =
  tis_strto_float state args ~ret_type:LDouble ~fname:"strtold"

(* atof builtin *)
let tis_atof state args =
  try
    match args with
    |  [(_, nptr, _) as e] ->
      debug_msg "Call to atof(%a)" Cvalue.V.pretty nptr;
      check_string_validity state e;
      let null = Cvalue.V.singleton_zero in
      let state,ret =
        abstract_strto_float
          ~state ~nptr ~endptr:null  ~ret_type:Double () ~is_ato:true
      in

      { Value_types.c_values = [ Value_types.StateOnly(ret, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | _ ->
      Value_parameters.error ~current:true
        "function atof, assert(call with correct arguments)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Invalid_character -> bottom_result
  | Interpret_format_partial | Undefined_behavior ->
    Value_parameters.error ~current:true
      "function atof, assert(valid double representation)%t. \
       Ending execution trace here" Value_util.pp_callstack;
    bottom_result

(* Register the builtins *)
let () =
  register_builtin "tis_sscanf" tis_sscanf;
  register_builtin "tis_scanf" tis_scanf;

  register_builtin "tis_strtol_interpreter" tis_strtol;
  register_builtin "tis_strtoll_interpreter" tis_strtoll;
  register_builtin "tis_strtoul_interpreter" tis_strtoul;
  register_builtin "tis_strtoull_interpreter" tis_strtoull;

  register_builtin "tis_atoi_interpreter" tis_atoi;
  register_builtin "tis_atol_interpreter" tis_atol;
  register_builtin "tis_atoll_interpreter" tis_atoll;

  register_builtin "tis_strtof_interpreter" tis_strtof;
  register_builtin "tis_strtod_interpreter" tis_strtod;
  register_builtin "tis_strtold_interpreter" tis_strtold;

  register_builtin "tis_atof_interpreter" tis_atof;

(*
  Local Variables:
  compile-command: "make -C ../../../../.."
  End:
*)
