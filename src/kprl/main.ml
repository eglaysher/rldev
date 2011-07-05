(*
   Kprl: RealLive archiver and disassembler
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

open Optpp
open Printf
open Disassembler
open ExtList

open Game

(*
open RlTypes
*)

(*
let games = Game.init GameParser.parse GameLexer.lex 
    (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose
*)

(*
let games = Game.load (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose
let games = Game.load (Config.lib_file !App.game_file) !App.verbose
*)


(*
let () = 
  ignore (Game.init GameParser.parse GameLexer.lex 
    (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose)
*)

(* Auxiliary functions *)

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

let action: Archiver.action ref =
  ref (false, fun _ -> usageError "you must specify an action to be performed")

(*  "-a, -k, -l, -i, -K, -d, -x, -b, -c, --decompress, --encrypt or --decrypt") *)

(*
let action: Archiver.action ref =
  ref (sysWarning "no action specified, performing disassembly by default...";
  Archiver.disassemble)
*)

let action_set = ref 0
  
let set_action =
(*
  let action_set = ref 0 in
*)
  fun x () ->
    incr action_set;
    match !action_set with
      | 1 -> action := x
      | 2 -> sysWarning "Only the first specified action will be used"
      | _ -> ()

(*
let set_target s =
  match String.lowercase s with
    | "reallive" | "2" -> Disassembler.options.forced_target <- `RealLive
    | "avg2000" | "avg2k" | "1" -> Disassembler.options.forced_target <- `Avg2000
    | "kinetic" | "3" -> Disassembler.options.forced_target <- `Kinetic
(*
    | "siglus" | "siglusengine" | "4" -> Disassembler.options.forced_target <- `Siglus
*)
    | _ -> ksprintf (usageError ~app:App.app) "unknown target `%s'" s
*)

let set_target s =
  Disassembler.options.forced_target <- match String.lowercase s with
    | "reallive" | "2" -> `RealLive
    | "avg2000" | "avg2k" | "1" -> `Avg2000
    | "kinetic" | "3" -> `Kinetic
(*
    | "siglus" | "siglusengine" | "4" -> `Siglus
*)
    | _ -> ksprintf (usageError ~app:App.app) "unknown target `%s'" s

let set_target_version s =
  App.auto_target := false;
  try
    let a = Array.make 4 0
    and l = List.map int_of_string (ExtString.String.nsplit s ".") in
    List.iteri (Array.set a) l;
    App.target_version := a.(0), a.(1), a.(2), a.(3)
  with
    | Failure "int_of_string"
    | Invalid_argument "index out of bounds"
       -> if Sys.file_exists s then
            App.target_interpreter := s
          else
            usageError ("target version must be specified as either an " ^ 
                "interpreter filename or up to four decimal integers " ^ 
                "separated by points")

(*
let set_game s =
    sysWarning (sprintf "game id: %s" s);
    Game.set_game (!App.verbose > 0) s
*)

let set_game id =
  Game.set_game !App.verbose id;
  let v = Game.get_version () in
  if v <> "" then set_target_version v;
  App.game_id := id

(*
let list_env s =
  let home = try Sys.getenv "HOME" with Not_found -> Filename.dirname Sys.argv.(0)
  and rldir = try Sys.getenv "RLDEV" with Not_found -> "." in



let test_env s =
*)

(* Option definitions *)
(* Short options used: abcdfFgGikKlnNoprsStTuvxyZ *)

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
  (*
    Opt { short = "-E"; long = "ENV"; argname = "KEY[=VALUE]";
          descr = "get/set environment variables.";
          withoutarg = list_env;
          witharg = Some (fun s -> test_env s) };
  *)
  
    Break;

    Message "Archive actions (pick one):";
    Break;

    Opt { short = "-a"; long = "add"; argname = "";
          descr = "add to or update files in archive";
          withoutarg = Some (set_action Archiver.add);
          witharg = None };
    Opt { short = "-k"; long = "delete"; argname = "";
          descr = "remove files from archive";
          withoutarg = Some (set_action Archiver.remove);
          witharg = None };
    Opt { short = "-l"; long = "list"; argname = "";
          descr = "list archive contents";
          withoutarg = Some (set_action Archiver.list);
          witharg = None };
    Opt { short = "-i"; long = "info"; argname = "";
          descr = "display file info found in the header " ^ 
            "including entrypoint and kidoku tables";
          withoutarg = Some (set_action Archiver.info);
          witharg = None };
    Opt { short = "-K"; long = "check"; argname = "";
          descr = "check for unhandled bytecode (no output)";
          withoutarg = Some (set_action Archiver.check);
          witharg = None };
    Break;

    Opt { short = "-d"; long = "disassemble"; argname = "";
          descr = "disassemble RealLive or AVG2000 bytecode";
          withoutarg = Some (set_action Archiver.disassemble);
          witharg = None };

    Opt { short = "-m"; long = "map"; argname = "";
          descr = "generate inter-seen call/goto cross-reference";
          withoutarg = Some (set_action Archiver.map);
          witharg = None };

    Opt { short = "-x"; long = "extract"; argname = "";
          descr = "extract and decompress files";
          withoutarg = Some (set_action Archiver.extract);
          witharg = None };
(*
    Opt { short = "-X"; long = "extractclean"; argname = "";
          descr = "extract, decompress and decrypt files";
          withoutarg = Some (set_action Archiver.decrypt);
          witharg = None };
*)
    Opt { short = "-b"; long = "break"; argname = "";
          descr = "extract files without decompressing them";
          withoutarg = Some (set_action Archiver.break);
          witharg = None };
    Opt { short = "-c"; long = "compress"; argname = "";
          descr = "compress files without archiving them";
          withoutarg = Some (set_action Archiver.pack);
          witharg = None };
    Opt { short = ""; long = "decompress"; argname = "";
          descr = "decompress a scenario file";
          withoutarg = Some (set_action Archiver.decompress);
          witharg = None };
    Opt { short = ""; long = "encrypt"; argname = "";
          descr = "encrypt an extracted scenario file";
          withoutarg = Some (set_action Archiver.encrypt);
          witharg = None };
    Opt { short = ""; long = "decrypt"; argname = "";
          descr = "decrypt an extracted scenario file";
          withoutarg = Some (set_action Archiver.decrypt);
          witharg = None };
    Break;

    Message "Options when listing:";
    Break;

    Opt { short = "-N"; long = "names"; argname = "";
(*
          descr = "list dramatis personae for " ^
            "each scenario instead of sizes";
*)
          descr = "list dramatis personae instead of sizes";
          withoutarg = set_flag App.names_opt true;
          witharg = None };
    Break;

    Message "Options when extracting or disassembling:";
    Break;
    
    Opt { short = "-o"; long = "outdir"; argname = "DIR";
          descr = "place files in DIR";
          withoutarg = None;
          witharg = set_string App.outdir "output directory" };
    Opt { short = "-G"; long = "game"; argname = "GID";
(*
          descr = "Game ID (LB,LBd,LBEX,LBME,CFV,FIVE,SNOW)";
          descr = "Game ID (" ^ (String.concat "," Game.game_list) ^ ")";
          descr = "Game ID (" ^ (Game.load 
          (Config.lib_file !App.game_file) !App.verbose;
          String.concat "," games) ^ ")";
*)
          descr = "game ID (" ^ (String.concat "," 
            (Game.load (Config.lib_file !App.game_file) 
            !App.verbose; Game.list())) ^ ")" ^
            (if !App.game_id <> "" then " default: " ^ !App.game_id else "");
          withoutarg = None;
(*
          witharg = Some (Game.set_game (fun () -> !App.verbose > 0)) };
          witharg = Some (fun s -> Game.set_game (fun () -> !App.verbose > 0) s) };
          witharg = Some (fun s -> Game.set_game !App.verbose s) };
*)
          witharg = Some (fun s -> set_game s) };
(*
          witharg = Some (set_game) };
          witharg = Some (fun s -> Game.set_game (!App.verbose > 0) s) };
*)
(*
          witharg = Some (Rlcmp.set_game !App.verbose > 0) };
*)
(*
          witharg = Some (fun s -> Rlcmp.set_game !App.verbose > 0 s) };
          witharg = Some (fun () -> Rlcmp.set_game !App.verbose > 0 s) };
*)
    Break;

    Message "Options when disassembling only:";
    Break;
    
    Opt { short = "-e"; long = "encoding"; argname = "ENC";
          descr = sprintf "output text encoding (default: %s)" 
            Config.default_encoding;
          withoutarg = None;
          witharg = Some (fun e -> App.enc := String.uppercase e) };
    Opt { short = ""; long = "bom"; argname = "";
(*
          descr = "when output encoding is UTF-8, include BOM (byte-order mark)";
*)
          descr = "include BOM (byte-order mark) for UTF-8";
          withoutarg = set_flag App.bom true;
          witharg = None; };
    Opt { short = "-s"; long = "single-file"; argname = "";
          descr = "don't put text into a separate resource file";
          withoutarg = Some (fun () -> 
            if Disassembler.options.separate_all then 
            failwith "you cannot use -s and -S together";
            Disassembler.options.separate_strings <- false);
          witharg = None; };
    Opt { short = "-S"; long = "separate-all"; argname = "";
(*
          descr = "put absolutely all Japanese text in the resource file";
*)
          descr = "put all Japanese text in the resource file";
          withoutarg = Some (fun () -> 
          if not Disassembler.options.separate_strings then 
              failwith "you cannot use -s and -S together";
              Disassembler.options.separate_all <- true);
          witharg = None; };
    Opt { short = "-Z"; long = "separate-all-strings"; argname = "";
          descr = "put absolutely all Japanese text in the " ^ 
            "resource file, numbering strings as Snnn";
          withoutarg = Some (fun () -> 
            if not Disassembler.options.separate_strings then 
              failwith "you cannot use -s and -Z together";
              Disassembler.options.separate_all <- true; 
              Disassembler.options.id_strings <- true);
          witharg = None; };
    Opt { short = "-u"; long = "unreferenced"; argname = "";
          descr = "suppress unreferenced code";
          withoutarg = Some (fun () -> 
            Disassembler.options.suppress_uncalled <- true);
          witharg = None; };
    Opt { short = "-n"; long = "annotate"; argname = "";
(*
          descr = "annotate output with offsets and parameter names";
*)
          descr = "annotate output with offsets and param names";
          withoutarg = Some (fun () -> 
            Disassembler.options.annotate <- true);
          witharg = None; };
    Opt { short = "-r"; long = "no-codes"; argname = "";
          descr = "don't generate control codes in strings";
          withoutarg = Some (fun () -> 
            Disassembler.options.control_codes <- false);
          witharg = None; };
    Opt { short = "-g"; long = "debug-info"; argname = "";
          descr = "read debugging information";
          withoutarg = Some (fun () -> 
            Disassembler.options.read_debug_symbols <- true);
          witharg = None; };
    Opt { short = "-t"; long = "target"; argname = "TGT";
(*
          descr = "specify source as RealLive, AVG2000, or Kinetic";
*)
          descr = "compile for RealLive, AVG2000, or Kinetic";
          withoutarg = None;
          witharg = Some set_target };
    Opt { short = "-f"; long = "target-version"; argname = "VER";
(*
          descr = "specify interpreter version, as either a version " ^ 
            "number or the filename of the interpreter " ^ 
            "(default: try to auto-detect)";
*)
          descr = "interpreter version number (n.n.n.n) " ^ 
            "or filename (default: try to auto-detect)";
          withoutarg = None;
          witharg = Some set_target_version };
    Opt { short = "-y"; long = "key"; argname = "KEY";
          descr = "decoder key for compiler version 110002 (default is LB)";
          withoutarg = None;
          witharg = Some (Rlcmp.set_key (fun () -> !App.verbose > 0)) };
    Opt { short = ""; long = "force-transform"; argname = "ENC";
          descr = "";
          withoutarg = None;
          witharg = Some (fun s -> App.force_meta := 
            Some (TextTransforms.enc_of_string s)) };
          
    Opt { short = "-F"; long = "from"; argname = "ADDR";
(*
          descr = "starting address for disassembly (hex,dec,oct) " ^ 
            "or prefix with '#' to specify a source line #";
*)
          descr = "starting address for disassembly (hex,dec,oct); " ^ 
            "prefix with '#' for a line #";
          withoutarg = None;
(*
          witharg = set_int Disassembler.options.start_address "start address" };
*)
          witharg = Some (fun s -> Disassembler.options.start_address <- int_of_string s) };
    Opt { short = "-T"; long = "to"; argname = "ADDR";
(*
          descr = "address at which to stop disassembly (hex,dec,oct) " ^
            "or prefix with '#' to specify a source line #";
*)
          descr = "address at which to stop disassembly (hex,dec,oct); " ^
            "prefix with '#' for a line #";
          withoutarg = None;
(*
          witharg = set_int Disassembler.options.end_address "end address" };
*)
          witharg = Some (fun s -> Disassembler.options.end_address <- int_of_string s) };
    Opt { short = ""; long = "kfn"; argname = "FILE";
(*
          descr = "specify the RealLive function definition file to use " ^ 
            "(default: " ^ !App.kfn_file ^ ")";
*)
          descr = "specify a RealLive function definition file " ^ 
            "(default: " ^ !App.kfn_file ^ ")";
          withoutarg = None;
          witharg = set_string App.kfn_file "function definition file" };
    Opt { short = ""; long = "cast"; argname = "FILE";
(*
          descr = "Specify a translation file for the cast of characters to use " ^ 
            "(default: " ^ !App.cast_file ^ ")";
          descr = "specify a translation file for the cast of characters to use";
*)
          descr = "specify a cast of characters translation file";
          withoutarg = Some (fun s -> App.cast_file := "cast.ini" );
          witharg = set_string App.cast_file "cast translation file" };
    Opt { short = "-p"; long = "translation-prep"; argname = "";
(*
          descr = "output each resource twice, commenting out the first";
*)
          descr = "output resources twice (first commented)";
(*
          withoutarg = set_flag Disassembler.options.prep_res_file true;
*)
          withoutarg = Some (fun () -> Disassembler.options.prep_res_file <- true );
          witharg = None };
          
    Opt { short = ""; long = "ext"; argname = "EXT";
          descr = "script file extension (default: " ^ !App.src_ext ^ ")";
          withoutarg = None;
          witharg = set_string App.src_ext "script file extension" };
          
    Opt { short = ""; long = "opcodes"; argname = "";
          descr = "disassemble to raw opcodes only";
          withoutarg = Some (fun () -> Disassembler.options.opcodes <- true );
          witharg = None };
          
    Opt { short = ""; long = "hexdump"; argname = "";
          descr = "generate annotated hexdump file";
          withoutarg = Some (fun () -> Disassembler.options.hexdump <- true );
          witharg = None };
          
    Opt { short = ""; long = "raw-strings"; argname = "";
(*
          descr = "don't include special markup and escapes in strings";
*)
          descr = "don't include special markup / escapes";
          withoutarg = Some (fun () -> Disassembler.options.raw_strings <- true);
          witharg = None; };
  ]

(* Main program *)

let main =
(*
  ignore (Game.init GameParser.parse GameLexer.lex 
    (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose);
*)

(*
  ignore (Game.load (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose);
*)

  let files = ref [] in
  try
    (* Parse command-line options. *)
    getopt options (function "" -> () 
      | x -> files := !files @ [x]) 
(*
        (fun () -> failwith "help") ~app:App.app;
*)
        (fun () -> failwith "usage") ~app:App.app;
    if !files = [] then usageError "no input files";

    if !action_set = 0 then (
      sysWarning "no action specified, performing disassembly by default...";
      action := Archiver.disassemble
    );

(*
    ignore (Game.init CastParser.parse CastLexer.lex 
      !App.cast_file !App.verbose);
*)

(*
    ignore (Game.init !App.verbose);
*)

(*
    ignore (Game.read_file !App.verbose);
*)

(*
    ignore (Game.init 
      GameParser.parse GameLexer.lex 
        (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose);
*)

    if !App.cast_file <> "" then ignore (Cast.init CastParser.parse 
        CastLexer.lex !App.cast_file !App.verbose);
    
    (* Detect the target version if possible. *)
    if !App.auto_target || !App.target_interpreter <> "" then
     (let fname =
        if !App.target_interpreter <> "" then 
          !App.target_interpreter 
        else
          let find d =
            Filename.concat d
              (Enum.find
                (fun s -> List.mem (String.lowercase s) 
                  ["reallive.exe"; "kinetic.exe"; 
                   "avg2000.exe"; "siglus.exe"; 
                   "siglusengine.exe"])
                (enum_of_array (Sys.readdir d)))
          in
          let fname =
            let dir = Filename.dirname (List.hd !files) in
            try
              find dir
            with Not_found ->
              try
                find (Filename.concat dir Filename.parent_dir_name)
              with Not_found ->
                ""
          in
          App.target_interpreter := fname;
          fname
      in
      if fname <> "" then App.target_version := seek_version fname);

    (* Create an output directory if necessary. *)
    let create_outdir, action = !action in
    if create_outdir && !App.outdir <> "" then (
      let rec mkpath dir =
        if try let r = Unix.stat dir in r.Unix.st_kind <> Unix.S_DIR with _ -> true
        then begin
          mkpath (Filename.dirname dir);
          Unix.mkdir dir 0o755
        end
      in try mkpath !App.outdir
      with Unix.Unix_error (e, _, _) ->
        ksprintf sysError "cannot create directory `%s': %s" !App.outdir
          (match e with
            | Unix.EACCES -> "permission denied"
            | Unix.EROFS  -> "device is read-only"
            | Unix.ENOSPC -> "no space left on device"
            | _ -> String.uncapitalize (Unix.error_message e))
    );

    (* Run the selected command. *)
    
(*
    sysInfo "Run the selected command.";
    
    sysInfo ("games: " ^ (String.concat ", " (Game.game_list ())) ^ "\n\n");
*)
    
    action !files

  with
    | Failure "help"    -> display_help App.app options
    | Failure "version" -> display_version App.app
    | Failure "usage"   -> display_usage App.app
    | Failure e         -> sysError e
    | Error s           -> cliErrorDisp s; exit 2
    | Trace (s,n)       -> printTrace s n; exit 2
