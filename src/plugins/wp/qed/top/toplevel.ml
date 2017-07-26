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

(* -------------------------------------------------------------------------- *)
(* --- A toplevel for Qed                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Logic

let debug = ref false

let keymap = Parser.extend [
    "quit" ; "include" ;
    "print" ; "type" ;
    "simplify" ;
  ]

let feed () =
  let rec scan buffer =
    let line = read_line () in
    Buffer.add_char buffer '\n' ;
    let k = String.length line in
    if k > 0 && line.[k-1] = '.' then
      Buffer.add_substring buffer line 0 (k-1)
    else
      begin
        Buffer.add_string buffer line ;
        Format.printf "> @?" ;
        scan buffer
      end
  in
  Format.printf "# @?" ;
  let buffer = Buffer.create 80 in
  scan buffer ;
  Lexer.open_shell keymap (Buffer.contents buffer)

let protect = function
  | Input.SyntaxError(pos,msg) ->
      Format.printf "%a: %s@." Input.pp_position pos msg
  | Failure msg ->
      Format.printf "Failure: %s.@." msg
  | err ->
      Format.printf "No comprendo (%s).@." (Printexc.to_string err)

(* -------------------------------------------------------------------------- *)
(* ---  Qed Input Files Processing                                        --- *)
(* -------------------------------------------------------------------------- *)

let global : Ground.env ref = ref Ground.stdlib
let name c fs =
  match c , fs with
  | Constructor , _ -> "Constructor"
  | _ , {params=[]} -> "Value"
  | _  -> "Function"

let rec execute input =
  try
    while true do
      match Lexer.token input with
      | Lexer.END -> Lexer.skip input
      | Lexer.EOF -> raise Exit
      | Lexer.KEYWORD "quit" -> exit 0

      | Lexer.KEYWORD "include" ->
          Lexer.skip input;
          begin
            match Lexer.token input with
            | Lexer.STRING s ->
                Lexer.skip input;
                ignore (Lexer.is_key input ".") ;
                let s_input = Lexer.open_file keymap s in
                execute s_input ;
                Lexer.close s_input
            | _ ->
                Lexer.error input "usage: include \"<filename.qed>\""
          end

      | Lexer.KEYWORD "print" ->
          Lexer.skip input ;
          let e = Parser.parse_expr input in
          ignore (Lexer.is_key input ".") ;
          let tau,value = Ground.cc_exp !global e in
          Format.printf "@[<hov 2>%a@ : %a@].@."
            Ground.pp_term value
            Ground.pp_tau  tau ;
          if !debug then
            Format.printf "Debug:@\n%a@." Ground.debug value ;

      | Lexer.KEYWORD "simplify" ->
          Lexer.skip input ;
          let e = Parser.parse_expr input in
          ignore (Lexer.is_key input ".") ;
          let (tau,v) = Ground.cc_exp !global e in
          let value = Ground.simplify (Ground.e_and (Ground.flatten v)) in
          Format.printf "@[<hov 2>%a@ : %a@]@."
            Ground.pp_term value
            Ground.pp_tau tau ;
          if !debug then
            Format.printf "Debug:@\n%a@." Ground.debug value ;

      | Lexer.KEYWORD "type" ->
          Lexer.skip input ;
          let (xs,a) = Parser.parse_typedef input in
          if Lexer.is_key input "=" then
            begin
              let t = Parser.parse_type input in
              ignore (Lexer.is_key input ".") ;
              let tau = Ground.cc_tau !global xs t in
              global := Ground.add_typedef a tau !global ;
            end
          else
            begin
              ignore (Lexer.is_key input ".") ;
              global := Ground.add_datatype a xs !global ;
            end ;
          Format.printf "Type %s defined.@." (snd a)

      | Lexer.KEYWORD "let" ->
          Lexer.skip input ;
          let c = Parser.parse_category input in
          let x = Lexer.skip_ident input in
          if Lexer.is_key input ":" then
            begin
              let xs,r = Parser.parse_signature input in
              ignore (Lexer.is_key input ".") ;
              let fs = Ground.cc_sig !global xs r in
              global := Ground.add_symbol x c fs !global ;
              Format.printf "%s %s defined.@." (name c fs) (snd x)
            end
          else
            begin
              let xs = Parser.parse_args input in
              let tr =
                if Lexer.is_key input ":"
                then Some (Parser.parse_type input) else None
              in
              Lexer.skip_key input "=" ;
              let fe = Parser.parse_expr input in
              let fs,_ = Ground.cc_def !global xs tr fe in
              global := Ground.add_symbol x c fs !global ;
              ignore (Lexer.is_key input ".") ;
              Format.printf "%s %s defined.@." (name c fs) (snd x) ;
            end

      | lex ->
          Lexer.error input "No comprendo ! (%a)" Lexer.pp_lexeme lex ;
    done
  with
  | Exit -> Format.print_flush ()
  | err -> protect err

(* -------------------------------------------------------------------------- *)
(* ---  Main Options                                                      --- *)
(* -------------------------------------------------------------------------- *)

let has_files = ref false
let force_top = ref false

let extension file =
  let base = Filename.chop_extension file in
  let n = String.length file in
  let b = String.length base in
  base , String.sub file b (n-b)

let process file =
  has_files := true ;
  let base,ext = extension file in
  match ext with
  | ".qed" ->
      Format.printf "Qed file \"%s\".@." file ;
      let input = Lexer.open_file keymap file in
      execute input ;
      Format.printf "Done.@." ;
      Lexer.close input
  | _ ->
      Format.printf "Don't know what to do with \"%s\".@." file

let version = "0.3"

let toplevel () =
  Format.printf "    Qed %s@\n@\n" version ;
  while true do
    let input = feed () in
    execute input ;
  done

let version () =
  Format.printf
    "Qed version %s@\nLoic Correnson@\n(c) CEA 2009-2011.@."
    version ;
  exit 0

let run () =
  Arg.parse [
    "-top" , Arg.Set force_top , ": run toplevel" ;
    "-debug" , Arg.Set debug , ": debug mode" ;
    "-version" , Arg.Unit version , ": display version and exit" ;
  ] process "qed [options...] [files...]" ;
  if !force_top || not !has_files then toplevel ()

let () = Printexc.catch run ()
