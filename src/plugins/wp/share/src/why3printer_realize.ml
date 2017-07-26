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
(*  The Why3 Verification Platform   /   The Why3 Development Team        *)
(*  Copyright 2010-2013   --   INRIA - CNRS - Paris-Sud University        *)
(*                                                                        *)
(*  This software is distributed under the terms of the GNU Lesser        *)
(*  General Public License version 2.1, with the special exception        *)
(*  on linking described in file LICENSE.                                 *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Why3 printer *)
open Why3
open Format
open Pp
open Ident
open Ty
open Term
open Decl
open Printer
open Theory

exception NotInRealizedTheories of theory

type ident_printers = {
  iprinter: ident_printer;
  cprinter: ident_printer;
  aprinter: ident_printer;
  tprinter: ident_printer;
  pprinter: ident_printer;
}

(* info *)

type info = {
  (** used in an thread unsafe way but there is not thread in why3 *)
  mutable info_syn : syntax_map;
  (** printer for imported theories during realization.
      empty when not doing realization *)
  symbol_printers : (string * ident_printers) Mid.t;
  (** printer for local symbols *)
  local_printers  : ident_printers;
}

let fresh_printers () =
  let bl = ["theory"; "type"; "function"; "predicate"; "inductive";
            "axiom"; "lemma"; "goal"; "use"; "clone"; "prop"; "meta";
            "namespace"; "import"; "export"; "end";
            "forall"; "exists"; "not"; "true"; "false"; "if"; "then"; "else";
            "let"; "in"; "match"; "with"; "as"; "epsilon" ] in
  let isanitize = sanitizer char_to_alpha char_to_alnumus in
  let lsanitize = sanitizer char_to_lalpha char_to_alnumus in
  {
    iprinter = create_ident_printer bl ~sanitizer:isanitize;
    cprinter = create_ident_printer bl ~sanitizer:isanitize;
    aprinter = create_ident_printer bl ~sanitizer:lsanitize;
    tprinter = create_ident_printer bl ~sanitizer:lsanitize;
    pprinter = create_ident_printer bl ~sanitizer:isanitize;
  }

let info = ref { info_syn = Mid.empty; symbol_printers = Mid.empty;
                 local_printers = fresh_printers ()}

let forget_tvs () =
  forget_all !info.local_printers.aprinter

let print_gen
    ?sanitizer ?(prefix="")
    ~getid ~getprinter  fmt x =
  let id = getid x in
  try
    let path,ipr = Mid.find id (!info).symbol_printers in
    fprintf fmt "%s.%s%s"
      path prefix (id_unique ?sanitizer (getprinter ipr) id)
  with Not_found ->
    let ipr = (!info).local_printers in
    Format.pp_print_string fmt prefix;
    Format.pp_print_string fmt (id_unique ?sanitizer (getprinter ipr) id)

(* type variables always start with a quote *)
let print_tv fmt x = print_gen
    ~getid:(fun tv -> tv.tv_name)
    ~getprinter:(fun p -> p.aprinter)
    ~prefix:"'"
    fmt x

(* logic variables always start with a lower case letter *)
let print_vs fmt x = print_gen
    ~getid:(fun vs -> vs.vs_name)
    ~getprinter:(fun p -> p.iprinter)
    ~sanitizer:String.uncapitalize
    fmt x

let forget_var vs = forget_id (!info).local_printers.iprinter vs.vs_name

(* theory names always start with an upper case letter *)
let print_th fmt x = print_gen
    ~getid:(fun th -> th.th_name)
    ~getprinter:(fun p -> p.cprinter)
    ~sanitizer:String.capitalize
    fmt x

let print_ts fmt x = print_gen
    ~getid:(fun ts -> ts.ts_name)
    ~getprinter:(fun p -> p.tprinter)
    fmt x

let print_ls fmt x = print_gen
    ~getid:(fun ls -> ls.ls_name)
    ~getprinter:(fun p -> p.iprinter)
    fmt x

(* constructor names always start with an upper case letter *)
let print_cs fmt x = print_gen
    ~getid:(fun ls -> ls.ls_name)
    ~getprinter:(fun p -> p.cprinter)
    ~sanitizer:String.capitalize
    fmt x

let print_pr fmt x = print_gen
    ~getid:(fun pr -> pr.pr_name)
    ~getprinter:(fun p -> p.pprinter)
    fmt x

let query_syntax id = query_syntax !info.info_syn id
let query_remove id = Mid.mem id !info.info_syn

(** Types *)

let protect_on x s = if x then "(" ^^ s ^^ ")" else s

let rec print_ty_node inn fmt ty = match ty.ty_node with
  | Tyvar v -> print_tv fmt v
  | Tyapp (ts, tl) -> begin match query_syntax ts.ts_name with
      | Some s -> syntax_arguments s (print_ty_node false) fmt tl
      | None -> begin match tl with
          | [] -> print_ts fmt ts
          | tl -> fprintf fmt (protect_on inn "%a@ %a")
                    print_ts ts (print_list space (print_ty_node true)) tl
        end
    end

let print_ty = print_ty_node false

(* can the type of a value be derived from the type of the arguments? *)
let unambig_fs fs =
  let rec lookup v ty = match ty.ty_node with
    | Tyvar u when tv_equal u v -> true
    | _ -> ty_any (lookup v) ty
  in
  let lookup v = List.exists (lookup v) fs.ls_args in
  let rec inspect ty = match ty.ty_node with
    | Tyvar u when not (lookup u) -> false
    | _ -> ty_all inspect ty
  in
  Opt.fold (fun _ -> inspect) true fs.ls_value

(** Patterns, terms, and formulas *)

let rec print_pat_node pri fmt p = match p.pat_node with
  | Pwild ->
    fprintf fmt "_"
  | Pvar v ->
    print_vs fmt v
  | Pas (p, v) ->
    fprintf fmt (protect_on (pri > 1) "%a as %a")
      (print_pat_node 1) p print_vs v
  | Por (p, q) ->
    fprintf fmt (protect_on (pri > 0) "%a | %a")
      (print_pat_node 0) p (print_pat_node 0) q
  | Papp (cs, pl) -> begin match query_syntax cs.ls_name with
      | Some s -> syntax_arguments s (print_pat_node 0) fmt pl
      | None -> begin match pl with
          | [] -> print_cs fmt cs
          | pl -> fprintf fmt (protect_on (pri > 1) "%a@ %a")
                    print_cs cs (print_list space (print_pat_node 2)) pl
        end
    end

let print_pat = print_pat_node 0

let print_vsty fmt v =
  fprintf fmt "%a:@,%a" print_vs v print_ty v.vs_ty

let print_const = Pretty.print_const
let print_quant = Pretty.print_quant
let print_binop = Pretty.print_binop

let prio_binop = function
  | Tand -> 3
  | Tor -> 2
  | Timplies -> 1
  | Tiff -> 1

let print_label = Pretty.print_label
let print_labels = print_iter1 Slab.iter space print_label

let print_ident_labels fmt id =
  if not (Slab.is_empty id.id_label) then
    fprintf fmt "@ %a" print_labels id.id_label

let rec print_term fmt t = print_lterm 0 fmt t

and print_lterm pri fmt t =
  if Slab.is_empty t.t_label then print_tnode pri fmt t
  else fprintf fmt (protect_on (pri > 0) "%a %a")
      print_labels t.t_label (print_tnode 0) t

and print_app pri fs fmt tl =
  match query_syntax fs.ls_name with
  | Some s -> syntax_arguments s print_term fmt tl
  | None ->
    let print_symb = if fs.ls_constr > 0 then print_cs else print_ls in
    begin match tl with
      | [] -> print_symb fmt fs
      | tl -> fprintf fmt (protect_on (pri > 5) "%a@ %a")
                print_symb fs (print_list space (print_lterm 6)) tl
    end

and print_tnode pri fmt t = match t.t_node with
  | Tvar v ->
    print_vs fmt v
  | Tconst c ->
    print_const fmt c
  | Tapp (fs, tl) when unambig_fs fs ->
    print_app pri fs fmt tl
  | Tapp (fs, tl) ->
    fprintf fmt (protect_on (pri > 0) "%a:%a")
      (print_app 5 fs) tl print_ty (t_type t)
  | Tif (f,t1,t2) ->
    fprintf fmt (protect_on (pri > 0) "if @[%a@] then %a@ else %a")
      print_term f print_term t1 print_term t2
  | Tlet (t1,tb) ->
    let v,t2 = t_open_bound tb in
    fprintf fmt (protect_on (pri > 0) "let %a = @[%a@] in@ %a")
      print_vs v (print_lterm 4) t1 print_term t2;
    forget_var v
  | Tcase (t1,bl) ->
    fprintf fmt "match @[%a@] with@\n@[<hov>%a@]@\nend"
      print_term t1 (print_list newline print_tbranch) bl
  | Teps fb ->
    let v,f = t_open_bound fb in
    fprintf fmt (protect_on (pri > 0) "epsilon %a.@ %a")
      print_vsty v print_term f;
    forget_var v
  | Tquant (q,fq) ->
    let vl,tl,f = t_open_quant fq in
    fprintf fmt (protect_on (pri > 0) "%a %a%a.@ %a") print_quant q
      (print_list comma print_vsty) vl print_tl tl print_term f;
    List.iter forget_var vl
  | Ttrue ->
    fprintf fmt "true"
  | Tfalse ->
    fprintf fmt "false"
  | Tbinop (b,f1,f2) ->
    let asym = Slab.mem Term.asym_label f1.t_label in
    let p = prio_binop b in
    fprintf fmt (protect_on (pri > p) "%a %a@ %a")
      (print_lterm (p + 1)) f1 (print_binop ~asym) b (print_lterm p) f2
  | Tnot f ->
    fprintf fmt (protect_on (pri > 4) "not %a") (print_lterm 4) f

and print_tbranch fmt br =
  let p,t = t_open_branch br in
  fprintf fmt "@[<hov 4>| %a ->@ %a@]" print_pat p print_term t;
  Svs.iter forget_var p.pat_vars

and print_tl fmt tl =
  if tl = [] then () else fprintf fmt "@ [%a]"
      (print_list alt (print_list comma print_term)) tl

(** Declarations *)

let print_tv_arg fmt tv = fprintf fmt "@ %a" print_tv tv
let print_ty_arg fmt ty = fprintf fmt "@ %a" (print_ty_node true) ty
let print_vs_arg fmt vs = fprintf fmt "@ (%a)" print_vsty vs

let print_constr fmt (cs,pjl) =
  let add_pj pj ty pjl = (pj,ty)::pjl in
  let print_pj fmt (pj,ty) = match pj with
    | Some ls -> fprintf fmt "@ (%a:@,%a)" print_ls ls print_ty ty
    | None -> print_ty_arg fmt ty
  in
  fprintf fmt "@[<hov 4>| %a%a%a@]" print_cs cs
    print_ident_labels cs.ls_name
    (print_list nothing print_pj)
    (List.fold_right2 add_pj pjl cs.ls_args [])

let print_type_decl fmt ts = match ts.ts_def with
  | None ->
    fprintf fmt "@[<hov 2>type %a%a%a@]@\n@\n"
      print_ts ts print_ident_labels ts.ts_name
      (print_list nothing print_tv_arg) ts.ts_args
  | Some ty ->
    fprintf fmt "@[<hov 2>type %a%a%a =@ %a@]@\n@\n"
      print_ts ts print_ident_labels ts.ts_name
      (print_list nothing print_tv_arg) ts.ts_args print_ty ty

let print_type_decl fmt ts =
  if not (query_remove ts.ts_name) then
    (print_type_decl fmt ts; forget_tvs ())

let print_data_decl fst fmt (ts,csl) =
  fprintf fmt "@[<hov 2>%s %a%a%a =@\n@[<hov>%a@]@]@\n@\n"
    (if fst then "type" else "with") print_ts ts
    print_ident_labels ts.ts_name
    (print_list nothing print_tv_arg) ts.ts_args
    (print_list newline print_constr) csl

let print_data_decl first fmt d =
  if not (query_remove (fst d).ts_name) then
    (print_data_decl first fmt d; forget_tvs ())

let print_ls_type fmt = fprintf fmt " :@ %a" print_ty

let ls_kind ls = if ls.ls_value = None then "predicate" else "function"

let print_param_decl fmt ls =
  fprintf fmt "@[<hov 2>%s %a%a%a%a@]@\n@\n"
    (ls_kind ls) print_ls ls
    print_ident_labels ls.ls_name
    (print_list nothing print_ty_arg) ls.ls_args
    (print_option print_ls_type) ls.ls_value

let print_param_decl fmt ls =
  if not (query_remove ls.ls_name) then
    (print_param_decl fmt ls; forget_tvs ())

let print_logic_decl fst fmt (ls,ld) =
  let vl,e = open_ls_defn ld in
  fprintf fmt "@[<hov 2>%s %a%a%a%a =@ %a@]@\n@\n"
    (if fst then ls_kind ls else "with") print_ls ls
    print_ident_labels ls.ls_name
    (print_list nothing print_vs_arg) vl
    (print_option print_ls_type) ls.ls_value print_term e;
  List.iter forget_var vl

let print_logic_decl first fmt d =
  if not (query_remove (fst d).ls_name) then
    (print_logic_decl first fmt d; forget_tvs ())

let print_ind fmt (pr,f) =
  fprintf fmt "@[<hov 4>| %a%a :@ %a@]"
    print_pr pr print_ident_labels pr.pr_name print_term f

let ind_sign = function
  | Ind   -> "inductive"
  | Coind -> "coinductive"

let print_ind_decl s fst fmt (ps,bl) =
  fprintf fmt "@[<hov 2>%s %a%a%a =@ @[<hov>%a@]@]@\n@\n"
    (if fst then ind_sign s else "with") print_ls ps
    print_ident_labels ps.ls_name
    (print_list nothing print_ty_arg) ps.ls_args
    (print_list newline print_ind) bl

let print_ind_decl s first fmt d =
  if not (query_remove (fst d).ls_name) then
    (print_ind_decl s first fmt d; forget_tvs ())

let print_pkind = Pretty.print_pkind

let print_prop_decl fmt (k,pr,f) =
  fprintf fmt "@[<hov 2>%a %a%a : %a@]@\n@\n" print_pkind k
    print_pr pr print_ident_labels pr.pr_name print_term f

let print_prop_decl fmt (k,pr,f) = match k with
  | Paxiom when query_remove pr.pr_name -> ()
  | _ -> print_prop_decl fmt (k,pr,f); forget_tvs ()

let print_list_next sep print fmt = function
  | [] -> ()
  | [x] -> print true fmt x
  | x :: r -> print true fmt x; sep fmt ();
    print_list sep (print false) fmt r

let print_decl fmt d = match d.d_node with
  | Dtype ts  -> print_type_decl fmt ts
  | Ddata tl  -> print_list_next nothing print_data_decl fmt tl
  | Dparam ls -> print_param_decl fmt ls
  | Dlogic ll -> print_list_next nothing print_logic_decl fmt ll
  | Dind (s, il) -> print_list_next nothing (print_ind_decl s) fmt il
  | Dprop p   -> print_prop_decl fmt p

let print_inst_ts fmt (ts1,ts2) =
  fprintf fmt "type %a = %a" print_ts ts1 print_ts ts2

let print_inst_ls fmt (ls1,ls2) =
  fprintf fmt "%s %a = %a" (ls_kind ls1) print_ls ls1 print_ls ls2

let print_inst_pr fmt (pr1,pr2) =
  fprintf fmt "prop %a = %a" print_pr pr1 print_pr pr2

let print_meta_arg fmt = function
  | MAty ty -> fprintf fmt "type %a" print_ty ty; forget_tvs ()
  | MAts ts -> fprintf fmt "type %a" print_ts ts
  | MAls ls -> fprintf fmt "%s %a" (ls_kind ls) print_ls ls
  | MApr pr -> fprintf fmt "prop %a" print_pr pr
  | MAstr s -> fprintf fmt "\"%s\"" s
  | MAint i -> fprintf fmt "%d" i

let print_qt fmt th =
  if th.th_path = [] then print_th fmt th else
    fprintf fmt "%a.%a"
      (print_list (constant_string ".") string) th.th_path
      print_th th

let print_tdecl fmt td = match td.td_node with
  | Decl d ->
    print_decl fmt d
  | Use th ->
    fprintf fmt "@[<hov 2>(* use %a *)@]@\n@\n" print_qt th
  | Clone (th,sm) when is_empty_sm sm ->
    fprintf fmt "@[<hov 2>(* use %a *)@]@\n@\n" print_qt th
  | Clone (th,sm) ->
    let tm = Mts.fold (fun x y a -> (x,y)::a) sm.sm_ts [] in
    let lm = Mls.fold (fun x y a -> (x,y)::a) sm.sm_ls [] in
    let pm = Mpr.fold (fun x y a -> (x,y)::a) sm.sm_pr [] in
    fprintf fmt "@[<hov 2>(* clone %a with %a,@ %a,@ %a *)@]@\n@\n"
      print_qt th (print_list comma print_inst_ts) tm
      (print_list comma print_inst_ls) lm
      (print_list comma print_inst_pr) pm
  | Meta (m,al) ->
    fprintf fmt "@[<hov 2>(* meta %s %a *)@]@\n@\n"
      m.meta_name (print_list comma print_meta_arg) al

let print_tdecls =
  let print_tdecl sm fmt td =
    (!info).info_syn <- sm; print_tdecl fmt td; sm, [] in
  let print_tdecl = Printer.sprint_tdecl print_tdecl in
  let print_tdecl task acc = print_tdecl task.Task.task_decl acc in
  Discriminate.on_syntax_map (fun sm -> Trans.fold print_tdecl (sm,[]))

let print_task args ?old:_ fmt task =
  (* In trans-based p-printing [forget_all] IST STRENG VERBOTEN *)
  (* forget_all (); *)
  print_prelude fmt args.prelude;
  fprintf fmt "theory Task@\n";
  print_th_prelude task fmt args.th_prelude;
  let rec print = function
    | x :: r -> print r; Pp.string fmt x
    | [] -> () in
  print (snd (Trans.apply print_tdecls task));
  fprintf fmt "end@."

(** TODO: th_local doesn't indicate the kind of an id (vs, ts, ls, ...)
    so we can't reserve the symbols in the corresponding printer.
    So we reserve in all of them.
*)
let reserve_ident pr id =
  ignore (id_unique pr.iprinter id);
  ignore (id_unique ~sanitizer:String.capitalize pr.cprinter id);
  ignore (id_unique pr.aprinter id);
  ignore (id_unique pr.tprinter id);
  ignore (id_unique pr.pprinter id)

let print_task printer_args fmt task =
  (* eprintf "Task:%a@.@." Pretty.print_task task; *)
  print_prelude fmt printer_args.prelude;
  (* find theories that are both used and realized from metas *)
  let realized_theories =
    Task.on_meta meta_realized_theory (fun mid args ->
        match args with
        | [Theory.MAstr src; Theory.MAstr dst] ->
          (* TODO: do not split string; in fact, do not even use a
             string argument *)
          let f,id =
            let l = Strings.rev_split '.' src in
            List.rev (List.tl l), List.hd l in
          let th = Env.read_theory printer_args.env f id in
          let id =
            let l = Strings.rev_split '.' dst in
            List.hd l in
          Mid.add th.Theory.th_name (th, dst, id) mid
        | _ -> assert false
      ) Mid.empty task in
  (* 2 cases: goal is clone T with [] or goal is a real goal *)
  let rec upd_realized_theories = function
    (** not realized *)
    | Some { Task.task_decl = { Theory.td_node =
                                  Theory.Decl { Decl.d_node = Decl.Dprop (Decl.Pgoal, _, _) }}} ->
      fprintf fmt "theory Task@\n";
      realized_theories
    (** realized *)
    | Some { Task.task_decl = { Theory.td_node = Theory.Clone (th,_) }} ->
      (** reserve the name used in the local theory in a consistent order *)
      Sid.iter (reserve_ident (!info).local_printers) th.Theory.th_local;
      begin
        try
          let (_,_, id) = Mid.find th.Theory.th_name realized_theories in
          fprintf fmt "theory %s@\n" id;
        with Not_found ->
          raise (NotInRealizedTheories(th))
      end;
      Mid.remove th.Theory.th_name realized_theories
    | Some { Task.task_decl = { Theory.td_node = Theory.Meta _ };
             Task.task_prev = task } ->
      upd_realized_theories task
    | _ -> assert false in
  let realized_theories = upd_realized_theories task in
  (** use prelude *)
  print_th_prelude task fmt printer_args.th_prelude;
  let realized_theories' =
    Mid.map (fun (th,s1,s2) -> fprintf fmt "use %s as %s@\n" s1 s2; th)
      realized_theories in
  let realized_symbols = Task.used_symbols realized_theories' in
  let local_decls = Task.local_decls task realized_symbols in
  (* eprintf "local_decls:%i@." (List.length local_decls); *)
  (* associate a special printer to each symbol in a realized theory *)
  let symbol_printers =
    let printers =
      Mid.map (fun th ->
          let pr = fresh_printers () in
          (** reserve all symbols in a consistent order *)
          Sid.iter (reserve_ident pr) th.Theory.th_local;
          pr
        ) realized_theories' in
    Mid.map (fun th ->
        let _,_,s2 = Mid.find th.Theory.th_name realized_theories in
        (s2, Mid.find th.Theory.th_name printers)
      ) realized_symbols in
  info := { !info with
            info_syn = get_syntax_map task;
            symbol_printers = symbol_printers;
          };
  let print_decls fmt dl =
    fprintf fmt "@\n@[<hov>%a@]" (print_list nothing print_decl) dl in
  print_decls fmt local_decls;
  fprintf fmt "end@."

let print_realize args ?old:_ fmt task =
  let info_bak = !info in (** printer without realization *)
  try
    info := { info_syn = Mid.empty; symbol_printers = Mid.empty;
              local_printers = fresh_printers ()};
    print_task args fmt task;
    info := info_bak
  with exn -> (** finally *)
    info := info_bak;
    raise exn


let () = register_printer "why3-realize" print_realize
    ~desc:"Printer@ for@ the@ logical@ format@ of@ Why3.
         @ Used for printing theories."

let () = Exn_printer.register (fun fmt -> function
    | NotInRealizedTheories th -> fprintf fmt
                                    "The@ theory %s@ is@ asked@ to@ be@ realized@ but@ it@ is@ specified \
                                     in no \"realized_theory\" meta." th.th_name.id_string
    | exn -> raise exn
  )
