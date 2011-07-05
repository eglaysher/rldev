(*
   RLdev: cast (character translation) parser type definitions
   Connyrice (CFR) 2010-2011 Richard 23

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program; if not, write to the Free Software Foundation, Inc., 59 Temple
   Place - Suite 330, Boston, MA  02111-1307, USA.
*)

open Optpp
open Printf

(*
let curr_line = ref 1
*)

type deftab = (string, string) Hashtbl.t

let cast : deftab = Hashtbl.create 0

let cast_file = ref ""

let find key = Hashtbl.find cast key

let set key =
  Hashtbl.replace cast key

let unset key =
  Hashtbl.remove cast key

let exists key =
  Hashtbl.mem cast key

let get key =
  try Some (find key) with Not_found -> None

let get_def key ~def =
  try find key with Not_found -> def

let add jname tname = 
(*
  ksprintf sysInfo "Add character '%s'" jname;
*)
  if exists jname then ksprintf sysWarning "Character '%s' redefined" jname;
  set jname tname

let cast_list () = 
  let arr = DynArray.create () in
  Hashtbl.iter (fun key value -> DynArray.add arr key) cast;
  List.sort String.compare (DynArray.to_list arr)

let load file verbose = 
  if file = "" then sysError "no cast file specified";
  if verbose then ksprintf sysInfo "load cast file '%s'" file;

  cast_file := file;
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  List.iter (fun p -> add (fst p) (snd p)) 
    (CastParser.parse CastLexer.lex lexbuf);
  close_in ic

let init parsefun lexfun filename verbosity =
  if filename <> "" then
    let ic = open_in filename in
    if verbosity > 0 then Printf.ksprintf Optpp.sysInfo "Reading CAST: %s" filename;
    try
      parsefun lexfun (Lexing.from_channel ic);
      close_in ic
    with
      e -> close_in_noerr ic;
           raise e

(*
let init parsefun lexfun srcdir =
  let filename =
    if !App.gameexe <> "" then
      if Sys.file_exists !App.gameexe
      && List.mem (Unix.stat !App.gameexe).Unix.st_kind [Unix.S_REG; Unix.S_LNK]
      then !App.gameexe
      else Printf.ksprintf Optpp.sysError "`%s' is not a valid INI file" !App.gameexe
    else  (* check command-line *)
      try Sys.getenv "GAMEEXE" with Not_found ->  (* check environment *)
        try
          Filename.concat srcdir
            (List.find
              (fun f -> Sys.file_exists (Filename.concat srcdir f))
              ["GAMEEXE.INI"; "gameexe.ini";
               Filename.concat Filename.parent_dir_name "GAMEEXE.INI";
               Filename.concat Filename.parent_dir_name "gameexe.ini"])
        with Not_found -> ""
  in
  if filename <> "" then
    let ic = open_in filename in
    App.gameexe := filename;
    if !App.verbose then Printf.ksprintf Optpp.sysInfo "Reading INI: %s" filename;
    try
      parsefun lexfun (Lexing.from_channel ic);
      close_in ic
    with
      e -> close_in_noerr ic;
           raise e
  else
    Optpp.sysWarning "unable to locate `gameexe.ini': using default values"
*)