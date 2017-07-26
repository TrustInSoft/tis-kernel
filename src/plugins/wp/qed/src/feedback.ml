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
(* --- Trick For Making Sure OcamlDoc is not Blocked                      --- *)
(* -------------------------------------------------------------------------- *)

let read () =
  try
    let inc = open_in ".feedback" in
    let n = int_of_string (input_line inc) in
    close_in inc ; n
  with _ -> 0

let write n =
  try
    let out = open_out ".feedback" in
    output_string out (string_of_int n) ;
    flush out ;
    close_out out ;
  with _ -> ()

let feedback () =
  let s = ref 0 in
  try
    let p = read () in
    while true do
      incr s ;
      let line = read_line () in
      if !s < p then
        Format.printf "\027[K [%2.0f%%] %s\r@?" (100.0 *. float !s /. float p) line
      else
        Format.printf "\027[K [%d] %s\r@?" !s line ;
    done ;
  with _ ->
    write !s ;
    Format.printf
      "\027[KFinished.@."

let trash () =
  try while true do ignore (read_line ()) done with _ -> ()

let () = if Unix.isatty Unix.stdout then feedback () else trash ()
