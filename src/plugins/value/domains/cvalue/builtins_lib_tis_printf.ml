(**************************************************************************)
(*                                                                        *)
(*  This file is part of TrustInSoft Kernel.                              *)
(*                                                                        *)
(*    Copyright (C) 2016-2017 TrustInSoft                                 *)
(*                                                                        *)
(*  TrustInSoft Kernel is released under GPLv2                            *)
(*                                                                        *)
(**************************************************************************)

(* Formatter to use to dump printing functions results.
   Use the builtins "standard" way to print if [None] *)

let default_formatter =
  Format.make_formatter
    (fun s i l -> Format.printf "@\n%s@." (String.sub s i l))
    (Format.pp_print_flush Format.std_formatter)

let formatter_stdout: Format.formatter ref = ref default_formatter
let formatter_stderr: Format.formatter ref = ref default_formatter

let set_output_formatter ?(stderr=default_formatter) ?(stdout=stderr) () =
  formatter_stdout := stdout;
  formatter_stderr := stderr


(* Builtins for printing functions. *)

open Abstract_interp
open Locations
open Cil_types


(* Parameters *)
module PrintfShowTooManyArguments =
  Value_parameters.True
    (struct
      let option_name = "-val-printf-show-too-many-args"
      let help = "show a message when too many arguments are given to printf \
                  function."
    end)



exception Interpret_format_finished
exception Emit_alarm
exception Return_bottom

(* define helper functions *)
module Aux = Builtins_lib_tis_aux

let bottom_result =
  { Value_types.c_values = [ Value_types.StateOnly(None, Cvalue.Model.bottom) ];
    c_clobbered = Base.SetLattice.bottom;
    c_cacheable = Value_types.NoCache;
    c_from = None; (* TODO?*)
    c_sureouts = None;
  }

let raise_problem _ = raise Emit_alarm

let alarm_behavior_raise_problem =
  {CilE.a_ignore with CilE.a_call = raise_problem }

let with_alarms =
  { CilE.defined_logic = alarm_behavior_raise_problem;
    unspecified        = alarm_behavior_raise_problem;
    others             = alarm_behavior_raise_problem;
    imprecision_tracing = CilE.a_ignore}

type formatting_result =
  { string: string;
    partial: bool }

exception Copy_string_done

type result = Unlocked of Buffer.t | LockedImprecise of String.t

let lock_imprecise result =
  match !result with
  | Unlocked buffer -> result := LockedImprecise(Buffer.contents buffer)
  | LockedImprecise _ -> ()

let add_char result c =
  match !result with
  | Unlocked buffer -> Buffer.add_char buffer c
  | LockedImprecise _ -> ()

let add_string result s =
  match !result with
  | Unlocked buffer -> Buffer.add_string buffer s
  | LockedImprecise _ -> ()

let copy_string ~source_char_size result state l ~precision ~width flags =
  let l = ref (loc_bytes_to_loc_bits l) in
  let sizeofchar_size = Int_Base.inject source_char_size in
  let sizeofchar_ival = Ival.inject_singleton source_char_size in
  let count = ref 0 in
  let word = Buffer.create 17 in
  try
    begin
      while true do
        ( match precision with
          | Some p when !count >= p -> raise Copy_string_done
          | _ -> ());
        let loc = Locations.make_loc !l sizeofchar_size in
        let c = Eval_op.find ~with_alarms state loc in
        let c = Cvalue.V.project_ival c in
        let c = Ival.project_int c in
        let c = Int.to_int c in
        assert (-128 <= c && c <= 255);
        let string_ends = c = 0 in
        if string_ends
        then
          raise Copy_string_done;
        let c =
          if c >= 0
          then c
          else c + 256
        in
        let c = char_of_int c in
        Buffer.add_char word c;
        l := Location_Bits.shift sizeofchar_ival !l;
        incr count;
      done;
    end
  with
  | Ival.Not_Singleton_Int -> lock_imprecise result
  | Cvalue.V.Not_based_on_null -> raise_problem ()
  | Copy_string_done ->
    begin
      let count = !count in
      let diff =
        match width with
        | None -> 0
        | Some width when width <= count -> 0
        | Some width -> width - count
      in
      (* add padding before of after the string depending on the
         alignment flag *)
      let padding = String.make diff ' ' in
      match flags with
      | "-" ->
        add_string result (Buffer.contents word);
        add_string result padding;
      | "" ->
        add_string result padding;
        add_string result (Buffer.contents word);
      | _ -> assert false
    end

let copy_char result c =
  match !result with
  | LockedImprecise _ -> ()
  | Unlocked _ ->
    begin
      try
        let c = Cvalue.V.project_ival c in
        let c = Ival.project_int c in
        let c = Int.logand c (Int.of_int 255) in
        add_char result (char_of_int (Int.to_int c));
      with
      | Ival.Not_Singleton_Int -> lock_imprecise result
      | Cvalue.V.Not_based_on_null -> raise_problem ()
    end

type 'a readysetgo = NotYet | Ready | Started of 'a | Ignored
(* If precision is negative it should be ignored *)

type seen_percent =
    Not_seen
  | Seen of string * Integer.t option * Integer.t readysetgo * string * bool
  (* flags, width, precision, length modifier, last_character_was_a_star*)

let format_of_seen_percent seen_percent length_modifier conversion_specifier =
  match seen_percent with
  | Not_seen -> assert false
  | Seen (flags, width, precision, _, _) ->
    let precision = match precision with
        NotYet | Ignored -> ""
      | Ready -> "."
      | Started n -> "." ^ (Int.to_string n)
    in
    let width = match width with
      | None -> ""
      | Some w -> Int.to_string w
    in
    Printf.sprintf "%%%s%s%s%s%c"
      flags width precision length_modifier conversion_specifier

(* this function in only needed to work around:
   http://caml.inria.fr/mantis/view.php?id=6938
   remove when the oldest supported version is fixed.
   Or maybe don't if the warning is useful. *)
let ignore_zero_if_precision seen_percent =
  match seen_percent with
  | Seen (flags, width, (Ready | Started _ as precision), modi, was_star)
    when String.contains flags '0' ->
    Value_parameters.warning ~current:true ~once:true
      "both '0' flag and precision when printing integer. Ignoring '0' flag";
    let i = ref 0 in
    let rec f x =
      let ii = !i in
      let flag = flags.[ii] in
      i := succ ii;
      if flag = '0' then f x else flag
    in
    let flags = String.init (pred (String.length flags)) f in
    Seen(flags, width, precision, modi, was_star)
  | _ -> seen_percent

let get_precision_value precision =
  match precision with
  | NotYet | Ignored -> None
  | Ready -> Some 0
  | Started len -> Some (Int.to_int len)

exception Printing_zero_with_precision_zero

let copy_int seen_percent conversion_specifier result arg =
  match !result with
  | LockedImprecise _ -> ()
  | Unlocked buffer ->
    begin
      try
        let i = Cvalue.V.project_ival arg in
        let i = Ival.project_int i in
        if Integer.is_zero i then begin
          match seen_percent with
          | Seen (_, _, precision, _, _) when
              get_precision_value precision = Some 0 ->
            raise Printing_zero_with_precision_zero
          | _ -> ()
        end;
        let i =
          if Int.gt i Int.max_int64 then Int.sub i Int.two_power_64 else i
        in
        let i = Integer.to_int64 i in
        let seen_percent = ignore_zero_if_precision seen_percent in
        let fmt = format_of_seen_percent seen_percent "L" conversion_specifier in
        let fmt = Scanf.format_from_string fmt "%Ld" in
        let i = Format.sprintf fmt i in
        Buffer.add_string buffer i
      with
        Ival.Not_Singleton_Int | Cvalue.V.Not_based_on_null ->
        lock_imprecise result
      | Printing_zero_with_precision_zero -> ()
    end

(* TODO: catch exception of Integer.to_int64  *)
(* TODO: add test case for an unsigned long int n > 2^63 (gives neg value?) *)
(* TODO: What happens if long int is 128 bits long? *)

let copy_float seen_percent conversion_specifier result arg =
  match !result with
  | LockedImprecise _ -> ()
  | Unlocked buffer ->
    begin
      try
        let i = Cvalue.V.project_ival arg in
        let lf, uf = Ival.min_and_max_float i in
        if Fval.F.equal lf uf
        then
          let f = Fval.F.to_float lf in
          let format =
            format_of_seen_percent seen_percent "" conversion_specifier
          in
          let result = Floating_point.unsafe_printf_float format f in
          Buffer.add_string buffer result
        else
          raise Fval.Non_finite
      with
        Fval.Non_finite | Cvalue.V.Not_based_on_null ->
        lock_imprecise result
    end

let write_string_to_memory l state ~max_length formatting_result =
  let exact = Location_Bytes.cardinal_zero_or_one l in
  if not exact
  then begin
    Value_parameters.warning ~current:true
      "Destination is not precise%t"
      Value_util.pp_callstack;
  end;
  let l = ref (loc_bytes_to_loc_bits l) in
  let state = ref state in
  let sizeofchar = Bit_utils.sizeofchar() in
  let sizeofchar_size = Int_Base.inject sizeofchar in
  let sizeofchar_ival = Ival.inject_singleton sizeofchar in
  let s = formatting_result.string in
  let length = String.length s in
  let problem = ref false in
  let set_problem =
    {CilE.a_ignore with CilE.a_call = fun _ -> problem := true}
  in
  let with_alarms =
    { CilE.defined_logic  = set_problem;
      unspecified         = set_problem;
      others              = set_problem;
      imprecision_tracing = CilE.a_ignore }
  in
  for i = 0 to pred length do
    let v = Cvalue.V.inject_int (Int.of_int (int_of_char s.[i])) in
    let loc = Locations.make_loc !l sizeofchar_size in
    state := Eval_op.add_binding ~exact ~with_alarms !state loc v;
    l := Location_Bits.shift sizeofchar_ival !l
  done;
  let loc = Locations.make_loc !l sizeofchar_size in
  let state =
    if formatting_result.partial
    then
      let max_length = match max_length with
        | None -> None
        | Some n ->
          Some (Int.mul sizeofchar (Int.sub n (Int.of_int (succ length))))
      in
      let loc =
        Locations.make_loc
          (Location_Bits.shift
             (Ival.inject_top (Some Int.zero) max_length Int.zero sizeofchar)
             !l)
          sizeofchar_size
      in
      Eval_op.add_binding ~exact:false ~with_alarms
        !state loc Cvalue.V.top_int
    else
      Eval_op.add_binding ~exact ~with_alarms !state loc
        Cvalue.V.singleton_zero
  in
  if !problem
  then Value_parameters.warning ~current:true
      "Destination possibly invalid. assert(match format and arguments)%t"
      Value_util.pp_callstack;
  state

let interpret_format ~character_width state l args =
  if not (Location_Bytes.cardinal_zero_or_one l)
  then begin
    Value_parameters.error ~current:true
      "Format string could not be resolved%t"
      Value_util.pp_callstack;
    raise Db.Value.Aborted
  end;
  let alarm_format_and_arguments () =
    Value_parameters.warning ~current:true
      "assert(match format and arguments)%t"
      Value_util.pp_callstack;
  in
  let alarm_format () =
    Value_parameters.warning ~current:true
      "assert(valid format)%t"
      Value_util.pp_callstack;
  in
  let abort_format () =
    alarm_format ();
    raise Db.Value.Aborted
  in
  let do_bottom_format () =
    alarm_format ();
    raise Return_bottom
  in
  let do_bottom_format_and_arguments () =
    alarm_format_and_arguments ();
    raise Return_bottom
  in
  let l = ref (loc_bytes_to_loc_bits l) in
  let sizeofchar_size = Int_Base.inject character_width in
  let sizeofchar_ival = Ival.inject_singleton character_width in
  let result = ref (Unlocked (Buffer.create 17)) in
  let seen_percent = ref Not_seen in
  let args = ref args in
  try
    while true do
      let loc = Locations.make_loc !l sizeofchar_size in
      let c = Eval_op.find ~with_alarms state loc in
      let c = Cvalue.V.project_ival c in
      let c = Ival.project_int c in
      let c = Int.to_int c in
      assert (-128 <= c && c <= 255);
      let format_ends = c = 0 in
      if format_ends
      then raise Interpret_format_finished;
      let code =
        if c >= 0
        then c
        else c + 256
      in
      let c = char_of_int code in
      let eat_arg_and_reset_seen_percent expected_typ allowable_typ =
        match !args with
          (arg_exp, arg_v, _) :: remaining_args ->
          let arg_typ = Cil.typeOf arg_exp in
          let compare_typ = Cabs2cil.compatibleTypesp arg_typ in
          let v =
            if not (List.exists compare_typ expected_typ)
            then begin
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
                Value_parameters.warning
                  "Continuing analysis because this seems innocuous";
                let typ = List.hd expected_typ in
                let signed = Bit_utils.is_signed_int_enum_pointer typ in
                let size = Integer.of_int (Cil.bitsSizeOf typ) in
                let v, _ = Cvalue.V.cast ~size ~signed arg_v in
                v
              end
              else
                do_bottom_format_and_arguments()
            end
            else arg_v
          in
          args := remaining_args;
          seen_percent := Not_seen;
          v
        | [] ->
          Value_parameters.warning ~current:true "Too few arguments for format";
          do_bottom_format_and_arguments()
      in
      let current_seen_percent = !seen_percent in
      ( match current_seen_percent with
        | Seen(flags, width, precision, modifier, star) ->
          let catc s = s ^ String.make 1 c in
          let abort_modifier () =
            Value_parameters.warning ~current:true
              "modifier %s not supported (yet) for %%%c" modifier c;
            abort_format ()
          in
          ( match c with
            | '+' | '-' | '0' | '#' | ' '
              when width = None && precision = NotYet && modifier = "" ->
              if String.contains flags c
              then Value_parameters.warning ~current:true ~once:true
                  "repeated flag '%c' in format" c
              else
                seen_percent :=
                  Seen(catc flags, None, NotYet, "", false)
            | '.' when modifier = "" && precision = NotYet ->
              seen_percent :=
                Seen(flags, width, Ready, "", false)
            | _digit when c >= '0' && c <= '9' ->
              if modifier <> "" then begin
                Value_parameters.warning
                  ~current:true
                  "digit after modifier in format";
                do_bottom_format();
              end;
              if star then begin
                Value_parameters.warning
                  ~current:true
                  "digit after '*' in format";
                do_bottom_format();
              end;
              let catc i =
                Integer.add
                  (Integer.mul (Integer.of_int 10) i)
                  (Integer.of_int (code - (Char.code '0')))
              in
              let new_seen =
                ( match precision with
                  | NotYet | Ignored ->
                    let width =
                      match width with
                        None -> Integer.zero
                      | Some l -> l
                    in
                    let width = catc width in
                    Seen(flags, Some width, NotYet, "", false)
                  | Ready | Started _ ->
                    let p =
                      ( match precision with
                        | Started p -> p
                        | _ -> Integer.zero)
                    in
                    Seen(flags, width, Started (catc p), "", false))
              in
              seen_percent := new_seen
            | '*'  when modifier = "" && (not star) ->
              begin
                match precision with
                | Ready ->
                  let arg = eat_arg_and_reset_seen_percent [Cil.intType] [] in
                  if not Cvalue.V.(is_included arg top_int)
                  then begin
                    Value_parameters.warning ~current:true
                      "addresses appear to be passed for %%*";
                    raise_problem ()
                  end;
                  let arg_int = Ival.project_int (Cvalue.V.project_ival arg) in
                  (* If precision is negative, it should be ignored *)
                  if Int.lt arg_int Int.zero then
                    seen_percent := Seen (flags, width, Ignored, "", true)
                  else begin
                    let arg_v = Started arg_int in
                    seen_percent := Seen (flags, width, arg_v, "", true)
                  end
                | Started _ ->
                  Value_parameters.warning ~current:true
                    "'*' in format string after precision specifier";
                  do_bottom_format ();
                | NotYet ->
                  begin
                    match width with
                    | Some _ ->
                      Value_parameters.warning ~current:true
                        "'*' in format string after width and before \
                         precision specifier";
                      do_bottom_format ();
                    | None ->
                      begin
                        let arg =
                          eat_arg_and_reset_seen_percent [Cil.intType] []
                        in
                        if not Cvalue.V.(is_included arg top_int)
                        then
                          begin
                            Value_parameters.warning ~current:true
                              "addresses appear to be passed for %%*";
                            raise_problem ()
                          end;
                        let arg_int =
                          Ival.project_int (Cvalue.V.project_ival arg)
                        in
                        (* Negative width should be interpreted as a '-' flag
                           followed by a positive width *)
                        if Int.lt arg_int Int.zero then
                          seen_percent :=
                            Seen (flags ^ "-", Some (Int.neg arg_int),
                                  precision, "", true)
                        else
                          seen_percent :=
                            Seen (flags, Some arg_int, precision, "", true)
                      end
                  end
                | Ignored -> ()
              end

            | 's' ->
              let types, source_char_size =
                if modifier = "l"
                then
                  let wchar = Cil.theMachine.Cil.wcharType in
                  [Cil_types.TPtr(wchar, [])], Int.of_int (Cil.bitsSizeOf wchar)
                else if modifier = ""
                then
                  [Cil.charPtrType; Cil.ucharPtrType; Cil.scharPtrType;
                   Cil.charConstPtrType],
                  (Bit_utils.sizeofchar())
                  (* modifier list is exhaustive, all the other cases
                     are undefined *)
                else do_bottom_format ()
              in
              let precision = get_precision_value precision in
              let width = match width with
                | None -> None
                | Some a -> Some (Int.to_int a)
              in
              let arg = eat_arg_and_reset_seen_percent types [] in begin
                match flags with
                | "-" | "" ->
                  copy_string ~source_char_size ~precision ~width
                    result state arg flags
                | f ->
                  (* All the flags except "-" and "" result in
                     undefined behavior *)
                  Value_parameters.warning ~current:true
                    "invalid flag '%s' with %%s format specifier" f;
                  do_bottom_format();
              end
            | 'p' -> begin
                match precision with
                |NotYet ->
                  let typ =
                    if modifier = "" then Cil.voidPtrType
                    (* modifier list is exhaustive, all the other
                       cases are undefined *)
                    else do_bottom_format()
                  in
                  let _ = eat_arg_and_reset_seen_percent [typ] [] in
                  lock_imprecise result
                | _ ->
                  Value_parameters.warning ~current:true
                    "invalid precision with %%p format specifier";
                  do_bottom_format();
              end

            | 'd' | 'i' ->
              begin
                let typ, allowable =
                  if modifier = "h" || modifier = "hh"
                  then [Cil.intType; ],
                       [Cil.uintType]
                  else if modifier = ""
                  then begin
                    [Cil.intType], [Cil.uintType]
                  end
                  else if modifier = "l" then [Cil.longType], []
                  else if modifier = "ll" || modifier = "j"
                  then [Cil.longLongType], [Cil.ulongLongType]
                  else if modifier = "t"
                  then [Cil.theMachine.Cil.ptrdiffType], []
                  else abort_modifier()
                in
                let arg = eat_arg_and_reset_seen_percent typ allowable in
                begin
                  if String.contains flags '#' then begin
                    Value_parameters.warning ~current:true
                      "Invalid flag '#' with %%%c format specifier" c;
                    do_bottom_format();
                  end
                  else
                    copy_int current_seen_percent c result arg
                end
              end
            | 'u' | 'x' | 'X' | 'o' ->
              begin
                let typ, allowable =
                  if modifier = "h" || modifier = "hh"
                  then begin
                    if  Cil.theMachine.Cil.theMachine.sizeof_short <
                        Cil.theMachine.Cil.theMachine.sizeof_int
                    then [Cil.intType; ], [] (* This assumes short < int *)
                    else begin
                      Value_parameters.error ~current:true
                        "size_of_short >= size_of_int is not supported yet \
                         in printf functions. Aborting.";
                      abort_format ()
                    end
                  end
                  else if modifier = "" then [Cil.uintType], [Cil.intType]
                  else if modifier = "l" then [Cil.ulongType], [Cil.longType]
                  else if modifier = "ll" || modifier = "j"
                  then [Cil.ulongLongType], [Cil.longLongType]
                  else if modifier = "z" then
                    [Cil.theMachine.Cil.typeOfSizeOf], []
                  else abort_modifier()
                in
                let arg = eat_arg_and_reset_seen_percent typ allowable in
                copy_int current_seen_percent c result arg
              end
            | 'f' | 'F' | 'g' | 'G' | 'e' | 'E' | 'a' | 'A' ->
              begin
                let typ =
                  if modifier = "" || modifier = "l" then Cil.doubleType
                  else if modifier = "L" then Cil.longDoubleType
                  (* modifier list is exhaustive, all the other cases
                     are undefined *)
                  else do_bottom_format()
                in
                let arg = eat_arg_and_reset_seen_percent [typ] [] in
                copy_float current_seen_percent c result arg
              end
            | 'c' -> begin
                match precision with
                |NotYet ->
                  let typ =
                    if modifier = "" then Cil.intType
                    else if modifier = "l" then Cil.theMachine.Cil.wcharType
                    (* modifier list is exhaustive, all the other
                       cases are undefined *)
                    else do_bottom_format()
                  in
                  let arg = eat_arg_and_reset_seen_percent [typ] [] in
                  copy_char result arg
                | _ ->
                  Value_parameters.warning ~current:true
                    "invalid precision with a %%c format specifier";
                  do_bottom_format ();
              end

            | '%' ->
              begin
                if modifier <> "" then abort_modifier();
                add_char result '%';
                seen_percent := Not_seen
              end
            | 'L' | 'l' | 'h' | 'j' | 'z' | 't' ->
              begin
                seen_percent :=
                  Seen(flags, width, precision, catc modifier, false);
              end
            | _ ->
              Value_parameters.warning ~current:true
                "format undefined or not supported (yet)";
              abort_format ()

          );
        | Not_seen ->
          if c = '%'
          then seen_percent := Seen("", None, NotYet, "", false)
          else add_char result c ) ;

      l := Location_Bits.shift sizeofchar_ival !l
    done;
    assert false (* ugly to have to write this *)
  with
  | Ival.Not_Singleton_Int | Cvalue.V.Not_based_on_null
  | Emit_alarm ->
    alarm_format_and_arguments();
    let string =
      ( match !result with
        | Unlocked buffer -> Buffer.contents buffer
        | LockedImprecise string -> string)
    in
    { string = string ; partial = true }
  | Interpret_format_finished ->
    if !args != [] && PrintfShowTooManyArguments.get ()
    then begin
      Value_parameters.feedback ~current:true
        "Too many arguments for format. This is technically allowed.%t"
        Value_util.pp_callstack;
    end;
    ( match !result with
      | Unlocked buffer -> { string = Buffer.contents buffer; partial = false }
      | LockedImprecise string -> { string = string ; partial = true })

let interpret_format_char x =
  interpret_format ~character_width:(Bit_utils.sizeofchar()) x

let interpret_format_wchar x =
  interpret_format
    ~character_width:
      (Integer.of_int(Cil.bitsSizeOf Cil.theMachine.Cil.wcharType))
    x

let abstract_length fmtres =
  let s = fmtres.string in
  let length = Int.of_int (String.length s) in
  if fmtres.partial
  then Cvalue.V.inject_ival
      (Ival.inject_range (Some length) (Some Int.billion_one))
  else Cvalue.V.inject_int length

let tis_printf state args =
  try
    match args with
      (_,format,_) :: rest ->
      let formating_result = interpret_format_char state format rest in
      let v = abstract_length formating_result in
      Format.fprintf !formatter_stdout "%s@?" formating_result.string;
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_int v, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | [] ->
      Value_parameters.error ~current:true
        "printf() needs at least one argument. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Return_bottom -> bottom_result

let tis_fprintf state args =
  try
    match args with
      (file_expr,file,_) :: (_,format,_) :: rest ->
      let sizeof_loc =
        match Cil.(unrollType (typeOf file_expr)) with
        | Cil_types.TPtr (typ, _attrs) ->
          Integer.of_int (Cil.bytesSizeOf typ)
        | _ ->
          Value_parameters.error ~current:true
            "fprintf() expects a pointer type as first parameter";
          raise Db.Value.Aborted
      in
      let location = Aux.location_of_cvalue file ~sizeof_loc in
      if not (Locations.is_valid ~for_writing:false location) then begin
        Value_parameters.warning ~current:true
          "assert(%a is a valid pointer for fprintf function.)@\n@."
          Cil_printer.pp_exp file_expr;
      end;
      let formating_result = interpret_format_char state format rest in
      let v = abstract_length formating_result in
      Format.printf "@\nfprintf(%a,...)@\n%s@."
        Cil_printer.pp_exp file_expr
        formating_result.string;
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_int v, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | _ ->
      Value_parameters.error ~current:true
        "fprintf() needs at least two arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Return_bottom -> bottom_result

let tis_wprintf state args =
  try
    match args with
      (_,format,_) :: rest ->
      let fmtres = interpret_format_wchar state format rest in
      let v = abstract_length fmtres in
      Format.printf "@\n%s@." fmtres.string;
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_int v, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.NoCache;
        c_from = None; (* TODO?*)
        c_sureouts = None;
      }
    | [] ->
      Value_parameters.error ~current:true
        "wprintf() needs at least one argument. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Return_bottom -> bottom_result

let tis_sprintf state args =
  try
    match args with
    | (_,dest,_) :: (_,format,_) :: rest ->
      let fmtres = interpret_format_char state format rest in
      let state = write_string_to_memory dest state ~max_length:None fmtres in
      let v = abstract_length fmtres in
      let c_from =
        if fmtres.partial then None
        else
          let bits_dest = loc_bytes_to_loc_bits dest in
          let character_bits = Bit_utils.sizeofchar () in
          let size =
            Int.mul character_bits (Int.of_int (String.length fmtres.string))
          in
          let loc_dest = make_loc bits_dest (Int_Base.inject size) in
          let zone_dest = enumerate_valid_bits ~for_writing:true loc_dest in
          Some
            (Value_types.Closure
               (fun fromstate args ->
                  let deps = List.fold_left
                      Function_Froms.Deps.join
                      Function_Froms.Deps.bottom
                      args
                  in
                  Function_Froms.Memory.add_binding
                    ~exact:true fromstate zone_dest deps))
      in
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_int v, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = c_from; (* TODO?*)
        c_sureouts = None; }
    | _ ->
      Value_parameters.error ~current:true
        "sprintf() needs at least two arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with Return_bottom -> bottom_result

let tis_snprintf state args =
  try
    match args with
    | (_,dest,_) :: (exp_nv,nv,_) :: (_,format,_) :: rest ->
      let term_nv = Logic_utils.expr_to_term ~cast:true exp_nv in
      begin
        try
          let n, n_addr = Cvalue.V.split Base.null nv in
          if Ival.is_bottom n then raise Cvalue.V.Not_based_on_null;
          if not (Cvalue.V.is_bottom n_addr)
          then begin
            Value_parameters.warning ~current:true
              "snprintf() seems to be passed a mix of addresses and \
               integers for size argument; assert(\\based_on_null(%a)). \
               Will continue analysis only with the integers.%t"
              Cil_datatype.Term.pretty term_nv
              Value_util.pp_callstack;

            (* TODO: add alarm (need to add a new alarm type?) *)
          end;

          let mi, ma = Ival.min_and_max n in
          let n = Extlib.the mi in
          let precise_length = Int.equal n (Extlib.the ma) in
          (* The order of the next two lines as per
             https://twitter.com/fanf/status/429213105927626752 *)
          let fmtres = interpret_format_char state format rest in
          let orig_length = Int.of_int (String.length fmtres.string) in
          let return_value =
            if fmtres.partial
            then
              Cvalue.V.inject_ival (Ival.inject_range (Some orig_length) ma)
            else
              Cvalue.V.inject_int orig_length
          in
          (* if nv is zero nothing is written and dest may be a null
             pointer *)
          if Int.is_zero n
          then begin
            if precise_length
            then
              { Value_types.c_values =
                  [ Value_types.StateOnly
                      (Eval_op.wrap_int return_value, state) ];
                c_clobbered = Base.SetLattice.bottom;
                c_cacheable = Value_types.Cacheable;
                c_from = None; (* TODO?*)
                c_sureouts = None; }
            else
              let state_with_some_writing =
                write_string_to_memory dest state ~max_length:ma
                  { string = "" ; partial = true }
              in
              (* Necessary for if the destination is totally invalid. When the
                 argument is zero, the call still terminates even if
                 the destination is invalid. *)
              let state = Cvalue.Model.join state state_with_some_writing in
              { Value_types.c_values =
                  [ Value_types.StateOnly
                      (Eval_op.wrap_int return_value, state) ];
                c_clobbered = Base.SetLattice.bottom;
                c_cacheable = Value_types.Cacheable;
                c_from = None; (* TODO?*)
                c_sureouts = None; }
          end
          else
            let pn = Int.pred n in
            let fmtres =
              if precise_length
              then fmtres
              else { fmtres with partial = true }
            in
            let s = fmtres.string in
            let s =
              if Int.le orig_length pn
              then s
              else String.sub s 0 (Int.to_int pn)
            in
            (* overapproximated: *)
            let state =
              write_string_to_memory dest state ~max_length:ma
                { fmtres with string = s }
            in
            { Value_types.c_values =
                [ Value_types.StateOnly
                    (Eval_op.wrap_int return_value, state) ];
              c_clobbered = Base.SetLattice.bottom;
              c_cacheable = Value_types.Cacheable;
              c_from = None; (* TODO?*)
              c_sureouts = None; }
        (* TODO: return a negative value if an encoding error occurred *)
        with Cvalue.V.Not_based_on_null ->
          Value_parameters.warning ~current:true
            "snprintf() seems to be passed addresses for size argument; \
             assert(\\based_on_null(%a)).%t"
            Cil_datatype.Term.pretty term_nv
            Value_util.pp_callstack;
          raise Return_bottom
          (* TODO: add alarm *)
      end

    | _ ->
      Value_parameters.error ~current:true
        "snprintf() needs at least three arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Return_bottom -> bottom_result



let tis_asprintf state args =
  try
    match args with
    | (dest_exp, dest,_) :: (_,format,_) :: rest ->

      (* verify arguments *)
      let sizeof_loc =
        match Cil.(unrollType (typeOf dest_exp)) with
        | Cil_types.TPtr (typ, _attr) when typ = Cil.charPtrType ->
          Integer.of_int (Cil.bytesSizeOf typ)
        | _ ->
          Value_parameters.error ~current:true
            "asprintf() expects a pointer type as first argument";
          raise Return_bottom
      in
      let location = Aux.location_of_cvalue dest ~sizeof_loc in
      if not (Locations.is_valid ~for_writing:true location) then begin
        Value_parameters.warning ~current:true
          "assert(%a is a valid pointer for the asprintf function.)@\n@."
          Cil_printer.pp_exp dest_exp;
        raise Return_bottom
      end;

      (* get parsing result *)
      let fmtres = interpret_format_char state format rest in

      (* allocate memory for string *)
      let size_cval = abstract_length fmtres in
      let size_ival = Ival.add_int Ival.one (Cvalue.V.project_ival size_cval) in
      let size_v = Cvalue.V.inject_ival size_ival in
      let new_address, state =
        Builtins_lib_tis_malloc.alloc_abstract_strong
          dest_exp.eloc "tis_asprintf" size_v state
      in
      let sizeof_ptr =
        (Int_Base.inject (Int.of_int (Bit_utils.sizeofpointer ())))
      in
      let dest_loc =
        Locations.make_loc (Locations.loc_bytes_to_loc_bits dest) sizeof_ptr
      in
      let state =
        write_string_to_memory new_address state ~max_length:None fmtres
      in

      (* write alloced pointer to dst *)
      let exact = Locations.cardinal_zero_or_one dest_loc in
      if not exact
      then begin
        (* This should never happen in interpreter mode *)
        Value_parameters.error ~current:true
          "Destination is not precise%t. Aborting." Value_util.pp_callstack;
        raise Db.Value.Aborted
      end;

      let state =
        Eval_op.add_binding ~exact ~with_alarms state dest_loc new_address
      in
      (* end write alloced ptr *)

      let v = abstract_length fmtres in
      { Value_types.c_values =
          [ Value_types.StateOnly(Eval_op.wrap_int v, state) ];
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None; (* TODO?*)
        c_sureouts = None; }
    | _ ->
      Value_parameters.error ~current:true
        "asprintf() expects at least two arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with
  | Return_bottom -> bottom_result


let va_params state va_list =
  Value_variadic.map_varinfos_from_va_list va_list state
    (fun v ->
       let e = Cil.(dummy_exp (Lval (Var v, NoOffset))) in
       let loc = Locations.loc_of_varinfo v in
       let value =
         Eval_op.find ~with_alarms:CilE.warn_none_mode state loc
       in
       (e, value, Cvalue.V_Offsetmap.empty))

let tis_vprintf state args =
  try
    match args with
    | [fmt; (_, va_list, _)] ->
      tis_printf state (fmt :: va_params state va_list)
    | _ ->
      Value_parameters.error ~current:true
        "vprintf() needs two arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with Return_bottom -> bottom_result

let tis_vfprintf state args =
  try
    match args with
    | [file; fmt; (_, va_list, _)] ->
      tis_fprintf state (file :: fmt :: va_params state va_list)
    | _ ->
      Value_parameters.error ~current:true
        "vfprintf() needs three arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with Return_bottom -> bottom_result

let tis_vsnprintf state args =
  try
    match args with
    | [dest; n; fmt; (_, va_list, _)] ->
      tis_snprintf state (dest :: n :: fmt :: va_params state va_list)
    | _ ->
      Value_parameters.error ~current:true
        "vsnprintf() needs four arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with Return_bottom -> bottom_result

let tis_vasprintf state args =
  try
    match args with
    | [strp; fmt; (_, va_list, _)] ->
      tis_asprintf state (strp :: fmt :: va_params state va_list)
    | _ ->
      Value_parameters.error ~current:true
        "vasprintf() needs three arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with Return_bottom -> bottom_result

let tis_vsprintf state args =
  try
    match args with
    | [dest; fmt; (_, va_list, _)] ->
      tis_sprintf state (dest :: fmt :: va_params state va_list)
    | _ ->
      Value_parameters.error ~current:true
        "vsprintf() needs three arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with Return_bottom -> bottom_result

let tis_vwprintf state args =
  try
    match args with
    | [fmt; (_, va_list, _)] ->
      tis_wprintf state (fmt :: va_params state va_list)
    | _ ->
      Value_parameters.error ~current:true
        "vwprintf() needs two arguments. assert(false)%t"
        Value_util.pp_callstack;
      raise Db.Value.Aborted
  with Return_bottom -> bottom_result

let () =
  Builtins.register_builtin "tis_printf" tis_printf;
  Builtins.register_builtin "tis_fprintf" tis_fprintf;
  Builtins.register_builtin "tis_sprintf" tis_sprintf;
  Builtins.register_builtin "tis_snprintf" tis_snprintf;
  Builtins.register_builtin "tis_asprintf_interpreter" tis_asprintf;
  Builtins.register_builtin "tis_vprintf" tis_vprintf;
  Builtins.register_builtin "tis_vfprintf" tis_vfprintf;
  Builtins.register_builtin "tis_vsprintf" tis_vsprintf;
  Builtins.register_builtin "tis_vsnprintf" tis_vsnprintf;
  Builtins.register_builtin "tis_vasprintf_interpreter" tis_vasprintf;
  Builtins.register_builtin "tis_wprintf" tis_wprintf;
  Builtins.register_builtin "tis_vwprintf" tis_vwprintf




(*
  Local Variables:
  compile-command: "make -C ../../../../.."
  End:
*)
