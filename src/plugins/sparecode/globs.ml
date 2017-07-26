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
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil
module FC_File = File
open Cil_datatype

let dkey = Sparecode_params.register_category "globs"

let debug format = Sparecode_params.debug ~dkey ~level:2 format
let debug' format = Sparecode_params.debug ~dkey ~level:3 format

let used_variables = Varinfo.Hashtbl.create 257
let var_init = Varinfo.Hashtbl.create 257
let used_typeinfo = Typeinfo.Hashtbl.create 257
let used_compinfo = Compinfo.Hashtbl.create 257
let used_enuminfo = Enuminfo.Hashtbl.create 257

let clear_tables () =
  Varinfo.Hashtbl.clear used_variables;
  Varinfo.Hashtbl.clear var_init;
  Typeinfo.Hashtbl.clear used_typeinfo;
  Compinfo.Hashtbl.clear used_compinfo;
  Enuminfo.Hashtbl.clear used_enuminfo

class collect_visitor = object (self)

  inherit Visitor.frama_c_inplace

  method! vtype t = match t with
    | TNamed(ti,_) ->
      if Typeinfo.Hashtbl.mem used_typeinfo ti then SkipChildren
      else begin
        debug "add used typedef %s@." ti.tname;
        Typeinfo.Hashtbl.add used_typeinfo ti ();
        ignore (visitCilType (self:>Cil.cilVisitor) ti.ttype);
        DoChildren
      end
    | TEnum(ei,_) ->
      if Enuminfo.Hashtbl.mem used_enuminfo ei then SkipChildren
      else begin
        debug "add used enum %s@." ei.ename;
        Enuminfo.Hashtbl.add used_enuminfo ei (); DoChildren
      end
    | TComp(ci,_,_) ->
      if Compinfo.Hashtbl.mem used_compinfo ci then SkipChildren
      else begin
        debug "add used comp %s@." ci.cname;
        Compinfo.Hashtbl.add used_compinfo ci ();
        List.iter
          (fun f -> ignore (visitCilType (self:>Cil.cilVisitor) f.ftype))
          ci.cfields;
        DoChildren
      end
    | _ -> DoChildren

  method! vvrbl v =
    if v.vglob && not (Varinfo.Hashtbl.mem used_variables v) then begin
      debug "add used var %s@." v.vname;
      Varinfo.Hashtbl.add used_variables v ();
      ignore (visitCilType (self:>Cil.cilVisitor) v.vtype);
      try
        let init = Varinfo.Hashtbl.find var_init v in
        ignore (visitCilInit (self:>Cil.cilVisitor) v NoOffset init)
      with Not_found -> ()
    end;
    DoChildren

  method! vglob_aux g = match g with
    | GFun (f, _) ->
      debug "add function %s@." f.svar.vname;
      Varinfo.Hashtbl.add used_variables f.svar ();
      Cil.DoChildren
    | GAnnot _ -> Cil.DoChildren
    | GVar (v, init, _) ->
      let _ = match init.init with | None -> ()
                                   | Some init ->
                                     begin
                                       Varinfo.Hashtbl.add var_init v init;
                                       if Varinfo.Hashtbl.mem used_variables v then
                                         (* already used before its initialization (see bug #758) *)
                                         ignore (visitCilInit (self:>Cil.cilVisitor) v NoOffset init)
                                     end
      in Cil.SkipChildren
    | GFunDecl _ -> DoChildren
    | _ -> Cil.SkipChildren

end

class filter_visitor prj = object

  inherit Visitor.generic_frama_c_visitor (Cil.copy_visit prj)

  method! vglob_aux g =
    match g with
    | GFun (_f, _loc) (* function definition *)
      -> Cil.DoChildren (* keep everything *)
    | GVar (v, _, _) (* variable definition *)
    | GVarDecl (v, _)
    | GFunDecl (_, v, _) -> (* variable/function declaration *)
      if Varinfo.Hashtbl.mem used_variables v then DoChildren
      else begin
        debug "remove var %s@." v.vname;
        ChangeTo []
      end
    | GType (ti, _loc) (* typedef *) ->
      if Typeinfo.Hashtbl.mem used_typeinfo ti then DoChildren
      else begin
        debug "remove typedef %s@." ti.tname;
        ChangeTo []
      end
    | GCompTag (ci, _loc) (* struct/union definition *)
    | GCompTagDecl (ci, _loc) (* struct/union declaration *) ->
      if Compinfo.Hashtbl.mem used_compinfo ci then DoChildren
      else begin
        debug "remove comp %s@." ci.cname;
        ChangeTo []
      end
    | GEnumTag (ei, _loc) (* enum definition *)
    | GEnumTagDecl (ei, _loc) (* enum declaration *) ->
      if Enuminfo.Hashtbl.mem used_enuminfo ei then DoChildren
      else begin
        debug "remove enum %s@." ei.ename;
        DoChildren (* ChangeTo [] *)
      end
    | _ -> Cil.DoChildren
end

module Result =
  State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (Project.Datatype)
    (struct
      let name = "Sparecode without unused globals"
      let size = 7
      let dependencies = [ Ast.self ] (* delayed, see below *)
    end)

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:Result.self
         [ !Db.Pdg.self; !Db.Outputs.self_external ])

let rm_unused_decl =
  Result.memo
    (fun new_proj_name ->
       clear_tables ();
       let visitor = new collect_visitor in
       Visitor.visitFramacFileSameGlobals visitor (Ast.get ());
       debug "filtering done@.";
       let visitor = new filter_visitor in
       let new_prj = FC_File.create_project_from_visitor new_proj_name visitor in
       let ctx = Parameter_state.get_selection_context () in
       Project.copy ~selection:ctx new_prj;
       new_prj)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
