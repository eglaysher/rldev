(*
   Rlc: RealLive-compatible assembler
   Copyright (C) 2006 Haeleth
   Revised 2009-2011 by Richard 23

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

open Printf
open ExtList
open Optpp

open Game

(*
let games = Game.init GameParser.parse GameLexer.lex 
    (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose
*)
  
let set_target s =
  KeTypes.global_target := KeTypes.target_t_of_string 
    (String.lowercase s) ~err:(usageError ~app:App.app);
  KeTypes.target_forced := true

let set_compiler_version s =
  KeTypes.compiler_version := int_of_string s

let enum_of_array a = (* stolen from DynArray.enum, remove if it makes it into ExtLib *)
    let rec make start =
        let idxref = ref 0 in
        let next () =
            if !idxref >= Array.length a then
                raise Enum.No_more_elements
            else
                let retval = Array.unsafe_get a !idxref in
                incr idxref;
                retval
        and count () =
            if !idxref >= Array.length a then 0
            else Array.length a - !idxref
        and clone () =
            make !idxref
        in
        Enum.make ~next:next ~count:count ~clone:clone
    in
    make 0

external seek_version : string -> int * int * int * int = "rldev_get_interpreter_version"
let target_interpreter = ref ""
and auto_target = ref true

let set_target_version s =
  auto_target := false;
  try
    let a = Array.make 4 0
    and l = List.map int_of_string (ExtString.String.nsplit s ".") in
    List.iteri (Array.set a) l;
    KeTypes.global_version := a.(0), a.(1), a.(2), a.(3)
  with
    | Failure "int_of_string"
    | Invalid_argument "index out of bounds"
       -> if Sys.file_exists s then
            target_interpreter := s
          else
            usageError ( "target version must be specified as either " ^ 
              "an interpreter filename or up to four decimal integers " ^ 
              "separated by points" )

let set_interpreter fname =
  KeTypes.global_version := seek_version fname;
  if !KeTypes.global_target = `Default 
  then match String.lowercase (Filename.basename fname) with
    | "reallive" | "reallive.exe" -> KeTypes.global_target := `RealLive
    | "kinetic"  | "kinetic.exe"  -> KeTypes.global_target := `Kinetic
    | "avg2000"  | "avg2000.exe"  -> KeTypes.global_target := `Avg2000
    | _ -> ()


let set_game id =
(*
  Game.load (Filename.concat (Config.prefix ()) !App.game_file) (!App.verbose > 0); 
  Game.set_game (fun () -> !App.verbose > 0) id;
*)

  Game.set_game !App.verbose id;

  let v = Game.get_version () in
  if v <> "" then set_target_version v;

  App.game_id := id


(*
  printf "game id,version: %s,%s\n" s v
*)
  
(* Short forms used: defgioOtuvx *)

let options =
  [
    Opt { short = ""; long = "help"; argname = "";
          descr = "display this usage information";
          withoutarg = Some (fun () -> failwith "help");
          witharg = None };
    Opt { short = ""; long = "version"; argname = "";
          descr = "display " ^ App.app.name ^ " version information";
          withoutarg = Some (fun () -> failwith "version");
          witharg = None };
    Opt { short = "-v"; long = "verbose"; argname = "LVL";
          descr = "describe what " ^ App.app.name ^ 
            " is doing;\nlevel 2 is very verbose";
          withoutarg = set_flag App.verbose 1;
          witharg = Some (fun s -> App.verbose := int_of_string s) };
    Break;

  Message "Files and Directories:";
    Break;

    Opt { short = "-o"; long = "output"; argname = "FILE";
          descr = "override output filename";
          withoutarg = None;
          witharg = set_string App.outfile "output filename" };
    Opt { short = "-d"; long = "outdir"; argname = "DIR";
          descr = "place output file in DIR";
          withoutarg = None;
          witharg = set_string App.outdir "output directory" };
    Opt { short = "-r"; long = "resdir"; argname = "DIR";
          descr = "a directory containing resource files";
          withoutarg = None;
          witharg = set_string App.resdir "resource directory" };
    Opt { short = "-i"; long = "ini"; argname = "FILE";
          descr = "specify GAMEEXE.INI to use at compile-time " ^ 
            "(default: try to find automatically)";
          withoutarg = None;
          witharg = set_string App.gameexe "GAMEEXE.INI" };
    Break;

  Message "Text Encoding and Transformation:";
    Break;

    Opt { short = "-e"; long = "encoding"; argname = "ENC";
          descr = sprintf "input text encoding (default: %s)" Config.default_encoding;
          withoutarg = None;
          witharg = Some (fun e -> App.enc := String.uppercase e) };
    Opt { short = "-x"; long = "transform-output"; argname = "ENC";
          descr = "output encoding transformation (default: none)";
          withoutarg = None;
          witharg = Some (fun e -> TextTransforms.set_encoding e) };
    Opt { short = "";   long = "force-transform"; argname = "";
          descr = "don't abort when input can't be represented in output encoding";
          withoutarg = set_flag TextTransforms.force_encode true;
          witharg = None; };
    Opt { short = "-t"; long = "target"; argname = "TGT";
          descr = "specify target as RealLive, AVG2000, or Kinetic " ^ 
            "(default: try to autodetect, fall back on RealLive)";
          withoutarg = None;
          witharg = Some set_target };
    Opt { short = "-f"; long = "target-version"; argname = "VER";
          descr = "specify interpreter version, as either a version number or " ^ 
            "the filename of the interpreter (default: try to auto-detect)";
          withoutarg = None;
          witharg = Some set_target_version };
    Break;

  Message "Low-level Compiler Options:";
    Break;

    Opt { short = "-u"; long = "uncompressed"; argname = "";
          descr = "don't compress and encrypt output";
          withoutarg = set_flag App.compress false;
          witharg = None };
    Opt { short = "-g"; long = "no-debug"; argname = "";
          descr = "strip debugging information";
          withoutarg = set_flag App.debug_info false;
          witharg = None };
    Opt { short = ""; long = "no-metadata"; argname = "";
          descr = "strip RLdev metadata (not recommended)";
          withoutarg = set_flag App.metadata false;
          witharg = None };
    Opt { short = ""; long = "no-assert"; argname = "";
          descr = "disable runtime assertions";
          withoutarg = set_flag App.assertions false;
          witharg = None };
    Opt { short = ""; long = "safe-arrays"; argname = "";
          descr = "enable runtime bounds-checking for arrays";
          withoutarg = set_flag App.array_bounds true;
          witharg = None };
    Opt { short = ""; long = "flag-labels"; argname = "";
          descr = "append labelled variable names to flag.ini";
          withoutarg = set_flag App.flag_labels true;
          witharg = None };
    Break;

  Message "Game-specific Encryption:";
    Break;

    Opt { short = "-G"; long = "game"; argname = "GID";
(*
          descr = "Game ID (LB,LBd,LBEX,LBME,CFV,FIVE,SNOW)";
          descr = "Game ID (" ^ (String.concat "," (Game.load 
      (Filename.concat (Config.prefix ()) !App.game_file) 
      (!App.verbose > 0); Game.game_list())) ^ ")" ^
      (if !App.game_id <> "" then " default: " ^ !App.game_id else "");
*)
          descr = "game ID (" ^ (String.concat "," 
        (Game.load (Config.lib_file !App.game_file) 
      !App.verbose; Game.list())) ^ ")" ^
      (if !App.game_id <> "" then " default: " ^ !App.game_id else "");
          withoutarg = None;
          witharg = Some (fun s -> set_game s) };
(*
          witharg = Some (Rlcmp.set_game (fun () -> !App.verbose)) };
          witharg = Some (fun s -> Game.set_game (fun () -> !App.verbose > 0) s) };
*)
(*
          witharg = Some (fun s -> Game.load 
        (Filename.concat (Config.prefix ()) !App.game_file) (!App.verbose > 0); 
(*
      let game = Game.set_game (fun () -> !App.verbose > 0) s) };
      let game = Game.set_game (fun () -> !App.verbose > 0) s in
*)
      Game.set_game (fun () -> !App.verbose > 0) s;
      
      App.game_id := s;
      
      let v = Game.get_version () in
      if v <> "" then set_target_version v;
      
      printf "game id,version: %s,%s\n" s v
(*
      match (Option.get (Game.get s)).target 
*)      
(*
      match (Game.get s) with Some g -> (match g.GameTypes.target with Some (_,`Version v,_) -> 
        set_target_version (string_of_version v)) | None -> ()
*)
      ) };
*)
      
    Opt { short = "-c"; long = "compiler"; argname = "LONG";
          descr = "compiler version (default: 10002, CLANNAD FV " ^ 
            "and LB: 110002, LB-EX: 1110002, etc.)";
          withoutarg = None;
          witharg = Some set_compiler_version };
    Opt { short = "-k"; long = "key"; argname = "KEY";
          descr = "decoder key for compiler version 110002 (default is for LB)";
          withoutarg = None;
(*
          witharg = Some (Rlcmp.set_key (fun () -> !App.verbose)) };
*)
          witharg = Some (Rlcmp.set_key (fun () -> !App.verbose > 0)) };
  (*Opt { short = "-O"; long = "optimisation"; argname = "LEV";
          descr = "set optimisation level (default 1, 0 to disable)";
          withoutarg = None;
          witharg = arg_to_flag
                      (fun s ->
                        try Int32.of_string s
                        with _ -> usageError ~app:App.app "argument to -O must be an integer")
                      App.opt_level "optimisation level"; };*)

    Opt { short = "-F"; long = "from"; argname = "LINE";
          descr = "line number of script file at which to begin compilation";
          withoutarg = None;
          witharg = Some (fun s -> App.start_line := int_of_string s) };
    Opt { short = "-T"; long = "to"; argname = "LINE";
          descr = "line number of script file at which to end compilation";
          withoutarg = None;
          witharg = Some (fun s -> App.end_line := int_of_string s) };
            
    Opt { short = ""; long = "kfn"; argname = "FILE";
          descr = "specify the RealLive Function Definition File " ^ 
            "to use (default: " ^ !App.kfn_file ^ ")";
          withoutarg = None;
          witharg = set_string App.kfn_file "function definition file" };
    Opt { short = ""; long = "ext"; argname = "EXT";
          descr = "script file extension (default: " ^ !App.src_ext ^ ")";
          withoutarg = None;
          witharg = set_string App.src_ext "script file extension" };
  ]

(*
let () =
  try
    let file = ref "" in
    getopt options ~app:App.app
      (function "" -> () | x -> if !file = "" 
    then file := x  else failwith "help")
      (fun () -> failwith "help");
*)

(*
    if !file = "" then failwith "help";
*)

(*
  if(String.sub !file, (String.length !file) - 
    ((String.length !App.src_ext) + 1), 
    (String.length !file)) <> ("." ^ !App.src_ext)
    then file := !file ^ "." ^ !App.src_ext;
*)

(*
    then file := (if (String.sub x, ((String.length x) - 
    ((String.length !App.src_ext) + 1)), 
    (String.length x)) <> ("." ^ !App.src_ext)
*)

(*
let () =
  try
    let file = ref "" in
    getopt options ~app:App.app
      (function "" -> () | s -> printf "file: %s\n" s; if !file = "" 
    then let l1 = String.length s in
      let l2 = String.length !App.src_ext in
(*
      file := (if s.[l1 - l2] = 0x2e && 
*)
      file := (if (s.[l1 - l2] = '.') && 
        ((String.sub s (l1 - l2) l2) = !App.src_ext)
      then s ^ "." ^ !App.src_ext else s)
    else failwith "help")
      (fun () -> failwith "help");
*)
(*
    printf "s: %d\n" ((String.length s) - 
    ((String.length !App.src_ext) + 1));
    printf "e: %d\n" (String.length s);
    
    printf "sub: %s\n\n" (String.sub s ((String.length s) - 
    ((String.length !App.src_ext) + 1)) (String.length s))
*)
(*
  ;
*)
  
(*
    
(*
    file := (if (String.sub x ((String.length x) - 
    ((String.length !App.src_ext) + 1))
    (String.length x)) <> ("." ^ !App.src_ext)
*)

    let l1 = String.length s in
    let l2 = String.length !App.src_ext in
    
    printf "l1: %d\n" l1;
    printf "l2: %d\n" l2;
    
    String.sub l1
  (*
    (String.length !App.src_ext)
  *)
    file := (if s.[l1 - l2] = '.' && 
    (String.sub s l1 - l2 l2) = !App.src_ext
    then s ^ "." ^ !App.src_ext else s)
    else failwith "help")
      (fun () -> failwith "help");
*)

let () =
  try
    let file = ref "" in
    getopt options ~app:App.app
(*
      (function "" -> () | s -> printf "file: %s\n" s; if !file = "" 
*)
      (function "" -> () | s -> if !file = "" 
    then let l1 = String.length s in
      let l2 = String.length !App.src_ext in
(*
      file := (if s.[l1 - l2] = 0x2e && 
*)
      file := (if (s.[l1 - l2] = '.') && 
        ((String.sub s (l1 - l2) l2) = !App.src_ext)
      then s ^ "." ^ !App.src_ext else s)
    else failwith "usage")
      (fun () -> failwith "usage");
(*
    else failwith "help")
      (fun () -> failwith "help");
*)

(*  printf "file1: %s\n" !file; *)
    
    if !file = "" then failwith "usage";
    if !file = "" then failwith "help";
    
(*  printf "file2: %s\n" !file; *)
    
    (* Detect the target version if possible. *)
    if !auto_target || !target_interpreter <> "" then
     (if !target_interpreter <> "" then
        set_interpreter !target_interpreter
      else
        let find d =
          Filename.concat d
            (Enum.find
              (fun s -> List.mem (String.lowercase s) 
        ["reallive.exe"; "kinetic.exe"; 
          "avg2000.exe"; "siglusengine.exe" ])
              (enum_of_array (Sys.readdir d)))
        in
        let dir = 
          let ge = !App.gameexe in
          if ge = ""
          then "."
          else if String.lowercase (Filename.basename ge) = "gameexe.ini"
          then Filename.dirname ge
          else (App.gameexe := Filename.concat ge "gameexe.ini"; ge)
        in
        try
          set_interpreter (find dir)
        with Not_found ->
          ());

(*
  if !App.game_file <> ""Game.load (Filename.concat (Config.prefix ()) !App.game_file) (!App.verbose > 0); 
  let game = Game.set_game (fun () -> !App.verbose > 0) s) };
*)  
  
    CompilerFrame.compile !file;
    
(*
    TextTransforms.enclog App "dummy.txt"
*)
    TextTransforms.enclog App.app.name App.app.version !App.enc !file
  with
    | Failure "help"    -> display_help App.app options
    | Failure "version" -> display_version App.app
    | Failure "usage"   -> display_usage App.app
    | Failure e         -> sysError e
    | Error s           -> cliErrorDisp s; exit 2
    | Trace (s,n)       -> printTrace s n; exit 2
    | Text.Bad_char c   -> ksprintf sysError ("unable to encode the " ^^ 
      "character U+%04x. Check input and output encoding settings") c;
      
(*
      | _ -> (try
          ksprintf (error lexbuf) "expected [$\\(] in get_expr_term, found 0x%02x" (lexeme_char lexbuf 0)
        with
          | Optpp.Error s -> ksprintf Optpp.startTrace "%s" s) in
    try
      f lexbuf
    with
      | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expr_term"
*)