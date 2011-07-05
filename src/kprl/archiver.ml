(*
   Kprl: SEEN.TXT archiving, encryption, and compression handling
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

(*pp *)

open Optpp
open Printf
open Binarray
open Bytecode
(*
open Cast
*)

(* open Ini; *)

let comp_ext = "rlc"
let uncomp_ext = "rl"

let empty_arc = "\000Empty RealLive archive"

(*
(* Retrieve offset and length of a file within an archive, or the file itself *)
let get_subfile_info archive idx =
  if Binarray.dim archive == 23
  then 0, 0
  else get_int archive (idx * 8), get_int archive (idx * 8 + 4)
*)

let get_subfile_info archive idx =
  if Binarray.dim archive == 23 then 0, 0
  else get_int archive (idx * 8), 
    get_int archive (idx * 8 + 4)

let get_subfile archive idx =
  match get_subfile_info archive idx with
    | _, 0 -> None
    | pos, len -> 
  Some (sub archive pos len)


(* RealLive's SEEN.TXT doesn't have a convenient PACL identifier in its header
   like in AVG32, so we have to test files by checking that their index table
   is sane. *)
(*
let is_archive fname =
  let arr = read_input fname in
  if Binarray.read arr 0 23 = "\000Empty RealLive archive" then `Yes (arr, `Empty) else
  let rec test_idx = function 10000 -> true | i ->
    match get_subfile_info arr i with
      | _, 0 -> test_idx (i + 1)
      | offset, len -> if offset + len > 80000 && offset + len <= dim arr && is_bytecode arr offset
                       then test_idx (i + 1)
                       else false in
  if dim arr >= 80000 && test_idx 0 then `Yes (arr, `Populated) else `No
*)

(*
(* RealLive's SEEN.TXT doesn't have a convenient PACL identifier in its header
   like in AVG32, so we have to test files by checking that their index table
   is sane. *)
(*
let seen_count fname =
*)
let is_archive fname = 
  let arr = read_input fname
(*
  and ctr = ref ~0 in
*)
  in
(*
  let rescount = ref ~-1
  if Binarray.read arr 0 23 <> "\000Empty RealLive archive" then
*)
(*
  if Binarray.read arr 0 23 = "\000Empty RealLive archive" then `Yes (arr, 0) else
  let rec test_idx = function 10000 -> true | i ->
    match get_subfile_info arr i with
      | _, 0 -> test_idx (i + 1)
      | offset, len -> if offset + len > 80000 && offset + len <= dim arr && is_bytecode arr offset
                     then (ignore (incr ctr); test_idx (i + 1))
                     else false in
  if dim arr >= 80000 && test_idx 0 then `Yes (arr, ctr) else `No
*)
  if Binarray.read arr 0 23 = "\000Empty RealLive archive" then `Yes (arr, 0) else
  let rec test_idx = function 10000, ctr -> `Yes (arr, ctr) | i, c ->
    match get_subfile_info arr i with
      | _, 0 -> test_idx (i + 1, c)
      | offset, len -> if offset + len > 80000 && offset + len <= dim arr && is_bytecode arr offset
                     then test_idx (i + 1, c + 1)
                     else `No in  
  if dim arr >= 80000 then test_idx (0, 0) else `No
*)

(*
let is_empty data = 
  try
    let arc = 
      match data with 
      | `File fname -> 
          match (read_input fname) with
              | `Archive a -> a
      | `Archive a -> a
    in
    Binarray.read arc 0 23 = empty_arc
  with 
    _ -> false
*)
    
(*   
          | _ -> printf 'NOT AN ARCHIVE...\n'; false;
    | `Archive arc -> test_arc arc 
    | `Seen seen -> false

          
  let test_arc arc = 
    Binarray.read arc 0 23 = EmptyArc    

          
          Binarray.read arc 0 23 = EmptyArc
    
  arc
  
  let arr = read_input fname in
  Binarray.read arr 0 23 = "\000Empty RealLive archive"
*)


(* RealLive's SEEN.TXT doesn't have a convenient PACL identifier in its header
   like in AVG32, so we have to test files by checking that their index table
   is sane. *)

(*
let is_archive fname = 
*)

(*
let seen_count fname = 
  let arr = read_input fname in
*)

let seen_count arr = 
(*
  let arr = read_input fname in
*)
(*
  let cnt = 
    if Binarray.read arr 0 23 = "\000Empty RealLive archive" then `Yes (arr, 0) else
    else if dim arr >= 80000 then test_idx (0, 0) 
    else `No
*)

  let rec test_idx = function 10000, count -> count
    | i, c ->
      match get_subfile_info arr i with
        | _, 0 -> test_idx (i + 1, c)
        | pos, len -> (* Optpp.sysInfo (Printf.sprintf "pos,len: %d,%d [%d]" pos len (pos + len)); *)
          if pos + len > 80000 && 
          pos + len <= dim arr && 
          is_bytecode arr pos then 
            test_idx (i + 1, c + 1) 
          (* else -c *)
          else if c > 0 then -c else -1
  in

(*
    if empty_arc arr then 0
*)

  let cnt = 
(*    if Binarray.read arr 0 23 = empty_arc then 0 *)
    if Binarray.read arr 0 23 = "\000Empty RealLive archive" then 0
    else if dim arr >= 80000 then test_idx (0, 0) 
    else -1 (* else None *)

  in 
  
(*
  ignore (if cnt < 0 then (printf "\nNot an archive...\n")
    else (printf "\n[Archive] Seen count: %d\n\n" cnt));
*)
  
(*
  arc, cnt
*)
  
(*
  if cnt > 0 then arr, cnt else cnt
*)

  archive := cnt <> -1;

(*
  ignore(printf "cnt: %d; archive: %s\n" cnt (if !archive then "true" else "false"));
*)

(*
  Optpp.sysInfo (Printf.sprintf "seen_cnt(): %d; archive: %s\n" cnt (if !archive then "true" else "false"));
*)
  
  cnt
    
  (*
    Binarray.read arr 0 23 = "\000Empty RealLive archive" then Some 0
    else if dim arr >= 80000 then test_idx (0, 0) 
    else None
*)
(*
    if dim arr >= 80000 then 
      test_idx (0, 0) else `No
*)

(*
  let cnt = 
    if Binarray.read arr 0 23 = "\000Empty RealLive archive" then Some 0
    else if dim arr >= 80000 then test_idx (0, 0) 
    else None
*)



      
(*
    let rec test_idx = function 10000, ctr -> 
      ignore (printf "\n[Archive] Seen count: %d\n\n" ctr); 
      Yes (arr, ctr) 
    | i, c ->
       match get_subfile_info arr i with
        | _, 0 -> test_idx (i + 1, c)
        | pos, len -> if pos + len > 80000 && 
          pos + len <= dim arr && 
          is_bytecode arr pos then 
            test_idx (i + 1, c + 1) 
          else `No in
    if dim arr >= 80000 then 
      test_idx (0, 0) else `No
*)

(*
    | i, c ->
*)
(*
    (dblog "Archive] Seen count: %d" ctr)
*)    
(*
    `Yes (arr, (dblog "Archive] Seen count: %d" ctr))
  let rec test_idx = function 10000, ctr -> `Yes (arr, ctr) | i, c ->
    match get_subfile_info arr i with
      | _, 0 -> test_idx (i + 1, c)
      | offset, len -> if offset + len > 80000 && 
                        offset + len <= dim arr && 
                        is_bytecode arr offset then 
                        test_idx (i + 1, c + 1) 
                        else `No in  
  if dim arr >= 80000 then 
    test_idx (0, 0) else `No
 *)

(* Wrapper for actions on an existing archive *)

let process_archive require_list (process_fun : string -> Binarray.t -> ISet.t -> unit) =
  function [] -> assert false | fname :: ranges ->
    let to_process =
      let full_range = ISet.add_range 0 9999 ISet.empty in
      if ranges = [] then
        if require_list then ISet.empty else full_range
      else
        let rec lex set =
          lexer
            | eof -> set
            | ['0'-'9']+ ["-~."]
             -> let s1 = Ulexing.latin1_sub_lexeme lexbuf 0 (Ulexing.lexeme_length lexbuf - 1) in
                let s2 =
                  (lexer
                    | ['0'-'9']+ -> Ulexing.latin1_lexeme lexbuf
                    | eof | _ -> failwith "malformed range parameter") lexbuf
                in
                lex (ISet.add_range (int_of_string s1) (int_of_string s2) set) lexbuf
            | ['0'-'9']+ -> lex (ISet.add (int_of_string (Ulexing.latin1_lexeme lexbuf)) set) lexbuf
            | "," -> lex set lexbuf

      
      (* R23: BEGIN negated file or range *)

            | ["!"] ['0'-'9']+ ["-~."]
             -> let s1 = Ulexing.latin1_sub_lexeme lexbuf 1 (Ulexing.lexeme_length lexbuf - 1) in
                let s2 =
                  (lexer
                    | ['0'-'9']+ -> Ulexing.latin1_lexeme lexbuf
                    | eof | _ -> failwith "malformed range parameter") lexbuf
                in
                lex (ISet.remove_range (int_of_string s1) (int_of_string s2) set) lexbuf
(*
            | ["!"] ['0'-'9']+ -> lex (ISet.remove (int_of_string (Ulexing.latin1_lexeme lexbuf)) set) lexbuf
*)

(*
            | ["!"] ['0'-'9']+ -> if ISet.is_empty then full_range; lex (ISet.remove (int_of_string (Ulexing.latin1_sub_lexeme lexbuf 1 (Ulexing.lexeme_length lexbuf - 1))) set) lexbuf
*)
  
      (* R23: END negated file or range *)

            | _ -> failwith "malformed range parameter"
        in
        ISet.inter full_range (lex ISet.empty (Ulexing.from_latin1_string (String.concat "," ranges)))
    in
    if ISet.is_empty to_process
    then
      failwith "no files to process"
(*
    else
      match is_archive fname with
  | `Yes (arc, c) -> ignore (printf "SEENS: %d\n" c); process_fun fname arc to_process
  | `No -> ksprintf failwith "%s is not a valid RealLive archive" (Filename.basename fname)
*)
    else
      let arc = read_input fname in
      match seen_count arc with
  | -1 -> ksprintf failwith "%s is not a valid RealLive archive" (Filename.basename fname)
  | c -> (* ignore (printf "SEENS: %d\n" c); *) process_fun fname arc to_process


(*
  | `Yes (arc, _) -> process_fun fname arc to_process
  | `No -> ksprintf failwith "%s is not a valid RealLive archive" (Filename.basename fname)
*)


(* Wrapper for actions reading an existing archive *)

let process_read (action : int -> Binarray.t -> unit) =
  process_archive false
    (fun _ arc to_process ->
       for i = 0 to 9999 do
         if ISet.mem i to_process then Option.may (action i) (get_subfile arc i)
       done)


(*
let archive = ref false
*)

(* Wrapper for actions on potentially unarchived files *)

let maybe_archive (action : string -> Binarray.t -> unit) =
  function [] -> assert false | (first :: _) as files ->
    if not (Sys.file_exists first) then ksprintf sysError "file `%s' not found" first;
    (*
    if is_archive first <> `No
    *)

(*
    if seen_count (read_input first) <> -1
    then process_read (fun i -> action (sprintf "SEEN%04d.TXT" i)) files
    else List.iter (fun s -> action s (read_input s)) files
*)

    let archive = seen_count (read_input first) <> -1 in
    if not archive then List.iter (fun s -> action s (read_input s)) files
    else process_read (fun i -> action (sprintf "SEEN%04d.TXT" i)) files
    


(* Actual processing functions *)

type action = bool * (string list -> unit)

(*
let try_extract notify arr =
  ignore (printf "\ntry_extract() -- %scompressed\n" 
    (if Bytecode.uncompressed_header (read arr 0 4) 
    then 'un' else ''));
*)
  

let try_extract notify arr =
  if Bytecode.uncompressed_header (read arr 0 4)
  then false, arr
  else (notify (); true, Rlcmp.decompress arr)

(*
let try_extract notify arr =
  if Bytecode.uncompressed_header (read arr 0 4) 
  then ((printf "\n  [uncompressed header]\n"); false, arr)
  else (notify (); ignore (printf "\n  [compressed header]\n"); true, Rlcmp.decompress arr)
*)

(* let ic = open_in (Filename.concat (Config.prefix ()) "reallive.kfn") in *)


let check =
  true,
  fun files ->
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;

    (* "Checking bytecode in %s" *)
    
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Checking %s" fname)
        arr 
        in ignore (Disassembler.check fname oarr))
      files

(*      
    my($file, @files) = @_;

     maybe_archive(sub {
        anon_fn_call("Archiver::check::process", @_) if $DEBUG;

        my($seen, $data) = @_;
        
        print '$seen: ', $seen, "\n",
            '$data: ',$data, "\n"
            if $DEBUG;
        
        try_extract($data);
        
        ksprintf(\&sysInfo, "Checking bytecode in %s", $seen)
            if App->app->setting('verbose') > 0 || 1;
        
        require "Disassembler.pm";
        
        Disassembler::disassemble($seen, $data);
    }, [$file, @files]);
}
*)

(*
let disassemble =
  let seen_map = DynArray.create () in
  
  true,
  fun files ->
(*
    if !App.cast_file <> "" then 
      (* 
      printf "cast_file: %s\n" !App.cast_file;
      *)
      ignore (Cast.init CastParser.parse CastLexer.lex 
        !App.cast_file !App.verbose);
*)    
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;
    
(*
    let srcdir = if fname = "-" then "." else Filename.dirname files.[0] in
*)    
(*
    if !App.cast_file <> "" then Cast.init CastParser.parse CastLexer.lex !App.cast_file;
*)
(*
    if !App.cast_file <> "" then 
      ignore (Cast.init CastParser.parse CastLexer.lex 
        !App.cast_file !App.verbose);
*)
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
        arr 
        in Disassembler.disassemble_source fname oarr)
      files

(*
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> ()) arr in
        if !App.verbose > 0 then ksprintf sysInfo "Disassembling %s" fname;
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> ()) arr in
        if !App.verbose > 0 then ksprintf sysInfo "Disassembling %s" fname;
        Disassembler.disassemble fname oarr)
      files
*)
*)



(*
let write_map (seen_map : (int, Disassembler.map_t) Hashtbl.t) seens = 
*)
let write_map seens =
      let format_jump jump = (* : Disassembler.jump_t = *)
        sprintf "Seen%04d(%05d) --> Seen%04d%-5s %s"
          jump.Disassembler.origin.Disassembler.seen 
          jump.Disassembler.origin.Disassembler.line 
          jump.Disassembler.target.Disassembler.scene (if jump.Disassembler.target.Disassembler.entry > -1 
            then (sprintf "(Z%02d)" jump.Disassembler.target.Disassembler.entry) else "")
          jump.Disassembler.kind 
      in
      
      let game = Game.get_label () in

      let oc = open_out (Filename.concat !App.outdir "seen-map.txt") in 

      fprintf oc "Seen map%s generated by %s %1.2f\n\n" 
        (if game <> "" then " for " ^ game else "")
        App.app.name App.app.version;
        
      List.iter 
        (fun idx -> 
        
(*
        let idx = try
          int_of_string (String.sub seen 4 4)
        with _ -> seen
        in
*)
        
        let map = Hashtbl.find Disassembler.seen_map idx in
        
        fprintf oc "== Seen%04d Summary ==\n\n" idx;
        
        if DynArray.length map.Disassembler.entrypoints > 0 then (
          fprintf oc "  ** entrypoints for Seen%04d **\n\n" idx;
        
          DynArray.iteri
            (fun i l -> 
              fprintf oc "    Z%02d: line %05d\n" i l
          ) map.Disassembler.entrypoints;
          
          output_char oc '\n'        
        );
        
        if DynArray.length map.Disassembler.calls > 0 then (
          fprintf oc "  ** outgoing calls from Seen%04d **\n\n" idx;
        
          DynArray.iter
            (fun call -> 
              output_string oc ("    " ^ (format_jump call) ^ "\n")
          ) map.Disassembler.calls;
          
          output_char oc '\n'
        );
        
        if DynArray.length map.Disassembler.gotos > 0 then (
          fprintf oc "  ** outgoing gotos from Seen%04d **\n\n" idx;
        
          DynArray.iter
            (fun goto -> 
              output_string oc ("    " ^ (format_jump goto) ^ "\n")
          ) map.Disassembler.gotos;
          
          output_char oc '\n'
        );
        
        if Hashtbl.length map.Disassembler.entries > 0 then (
          fprintf oc "  ** incoming calls/gotos to Seen%04d **\n\n" idx;
        
          (*
          Hashtbl.iter
            (fun idx jump -> 
              output_string oc (format_jump jump)
          ) map.Disassembler.entries
          *)
          
          Hashtbl.iter
            (fun eidx jump -> 
(*
              let addr = { Disassembler.empty_addr with scene = idx; entry = eidx } in
*)
              
              (*
              let jump = { Disassembler.empty_jump with 
                Disassembler.origin = { 
                  Disassembler.empty_loc with
                  Disassembler.seen = loc.Disassembler.seen;
                  Disassembler.line = loc.Disassembler.line
                }; Disassembler.target = {
                  Disassembler.empty_addr with
                  Disassembler.scene = idx;
                  Disassembler.entry = eidx
                } 
              } in
              *)
              
              output_string oc ("    " ^ (format_jump jump) ^ "\n")
          ) map.Disassembler.entries;
          
          output_char oc '\n'
        );
        
        output_char oc '\n'
      ) seens;
      
      close_out oc

let write_html_map seens =
      let format_jump seen jump = (* : Disassembler.jump_t = *)
        let origin_seen = jump.Disassembler.origin.Disassembler.seen in
		let origin_line = jump.Disassembler.origin.Disassembler.line in
		let target_seen = jump.Disassembler.target.Disassembler.scene in
		let target_entry = jump.Disassembler.target.Disassembler.entry in
		
		let caller = if origin_seen = seen then (
		  sprintf "<a name=\"Seen%04d.%05d\"></a>\nSeen%04d(%05d)" 
		    origin_seen origin_line origin_seen origin_line
		) else (
		  sprintf "<a href=\"#Seen%04d.%05d\">Seen%04d(%05d)</a>"
		    origin_seen origin_line origin_seen origin_line) in
			
		sprintf ("%s --> " ^^ 
          "<a href=\"#Seen%04d.Z%02d\">" ^^ 
          "Seen%04d%-5s</a> %s<br />") 
		  caller 
		  target_seen (if target_entry > -1 then target_entry else 0)
          target_seen 
		  (if target_entry > -1 then (sprintf "(Z%02d)" target_entry) else "")
          jump.Disassembler.kind
      in
	
(*		
		else (sprintf "<a name=\"Seen%04d.%05d\"></a>\n" ^^ 
          "<a href=\"#Seen%04d.%05d\">Seen%04d(%05d)</a>"
		  origin_seen origin_line origin_seen origin_line
		
		let caller = "<a name=\"Seen%04d.%05d\"></a>\n" ^^ 
          jump.Disassembler.origin.Disassembler.seen
		  
		  "<a href=\"#Seen%04d.%05d\">" ^^ 
		  "Seen%04d(%05d)</a>
		  
		  sprintf (
          (*
		  "<p>" ^^ 
		  *)
          "<a name=\"Seen%04d.%05d\"></a>\n" ^^ 
          "<a href=\"#Seen%04d.%05d\">" ^^ 
		  "Seen%04d(%05d)</a> --> " ^^ 
          "<a href=\"#Seen%04d.Z%02d\">" ^^ 
          "Seen%04d%-5s</a> %s" ^^
		  "<br />"
		  (*
		  "</p>"
		  *)
		  )
          jump.Disassembler.origin.Disassembler.seen 
          jump.Disassembler.origin.Disassembler.line 
          jump.Disassembler.origin.Disassembler.seen 
          jump.Disassembler.origin.Disassembler.line 
          jump.Disassembler.target.Disassembler.scene 
          (if jump.Disassembler.target.Disassembler.entry > -1 
            then jump.Disassembler.target.Disassembler.entry else 0)
          jump.Disassembler.target.Disassembler.scene 
          (if jump.Disassembler.target.Disassembler.entry > -1 
            then (sprintf "(Z%02d)" jump.Disassembler.target.Disassembler.entry) else "")
          jump.Disassembler.kind 
      in
*)
      
      let game = Game.get_label () in

      let oc = open_out (Filename.concat !App.outdir "seen-map.html") in 
      
      fprintf oc ("<html>\n<head>\n<title>Seen map%s</title>\n" ^^
	    "<style type=\"text/css\">\n" ^^ 
		"body { font-family: monospace }\n" ^^ 
		"h2 { margin-left: auto; margin-right: auto; }\n" ^^ 
		"</style>\n" ^^ 
		"</head>\n<body>\n" ^^
        "<h2>Seen map%s generated by %s %1.2f</h2>\n\n")
        (if game <> "" then " for " ^ game else "")
        (if game <> "" then " for " ^ game else "")
        App.app.name App.app.version;

(*
      fprintf oc "Seen map%s generated by %s %1.2f\n\n" 
        (if game <> "" then " for " ^ game else "")
        App.app.name App.app.version;
*)
        
      List.iter 
        (fun idx -> 
        
(*
        let idx = try
          int_of_string (String.sub seen 4 4)
        with _ -> seen
        in
*)
        
        let map = Hashtbl.find Disassembler.seen_map idx in
        
        fprintf oc (
          "<a name=\"Seen%04d\"></a>\n" ^^
          "<h2>== Seen%04d Summary ==</h2>\n\n") idx idx;
        
        if DynArray.length map.Disassembler.entrypoints > 0 then (
          fprintf oc (
            "  <a name=\"Seen%04d-entrypoints.\"></a>\n" ^^
            "  <h3>** entrypoints for Seen%04d **</h3>\n\n" ^^ 
            "<div style=\"margin-left:4em\">\n") idx idx;
        
          DynArray.iteri
            (fun i l -> 
(*
              fprintf oc (
                "    <a name=\"Seen%04d.Z%02d.%05d\"></a>\n" ^^ 
                "    <a name=\"Seen%04d.Z%02d\"></a>\n" ^^ 
*)
            (*
				"    Z%02d: line %05d<br />\n"
				) idx i l i l
			*)
              fprintf oc (
                "    <a name=\"Seen%04d.Z%02d\"></a>\n" ^^ 
				"    Seen%04d(Z%02d) <small>line %05d</small><br />\n"
				) idx i idx i l
          ) map.Disassembler.entrypoints;
          
          fprintf oc "</div>\n"
(*
          output_char oc '\n'
*)
        );
        
        if DynArray.length map.Disassembler.calls > 0 then (
          fprintf oc (
            "  <a name=\"Seen%04d-calls.\"></a>\n" ^^
            "  <h3>** outgoing calls from Seen%04d **</h3>\n\n" ^^
            "<div style=\"margin-left:4em\">\n") idx idx;
        
          DynArray.iter
            (fun call -> 
              output_string oc ("    " ^ (format_jump idx call) ^ "\n")
          ) map.Disassembler.calls;
          
          fprintf oc "</div>\n"
(*
          output_char oc '\n'
*)
        );
        
        if DynArray.length map.Disassembler.gotos > 0 then (
          fprintf oc (
            "  <a name=\"Seen%04d-gotos.\"></a>\n" ^^
            "  <h3>** outgoing gotos from Seen%04d **</h3>\n\n" ^^
            "<div style=\"margin-left:4em\">\n") idx idx;
        
          DynArray.iter
            (fun goto -> 
              output_string oc ("    " ^ (format_jump idx goto) ^ "\n")
          ) map.Disassembler.gotos;
          
          fprintf oc "</div>\n"
(*
          output_char oc '\n'
*)
        );
        
        if Hashtbl.length map.Disassembler.entries > 0 then (
          fprintf oc (
            "  <a name=\"Seen%04d-incoming.\"></a>\n" ^^
            "  <h3>** incoming calls/gotos to Seen%04d **</h3>\n\n" ^^
           "<div style=\"margin-left:4em\">\n") idx idx;
        
          (*
          Hashtbl.iter
            (fun idx jump -> 
              output_string oc (format_jump jump)
          ) map.Disassembler.entries
          *)
          
          Hashtbl.iter
            (fun eidx jump -> 
(*
              let addr = { Disassembler.empty_addr with scene = idx; entry = eidx } in
*)
              
              (*
              let jump = { Disassembler.empty_jump with 
                Disassembler.origin = { 
                  Disassembler.empty_loc with
                  Disassembler.seen = loc.Disassembler.seen;
                  Disassembler.line = loc.Disassembler.line
                }; Disassembler.target = {
                  Disassembler.empty_addr with
                  Disassembler.scene = idx;
                  Disassembler.entry = eidx
                } 
              } in
              *)
              
              output_string oc ("    " ^ (format_jump idx jump) ^ "\n")
          ) map.Disassembler.entries;
          
          fprintf oc "</div>\n"
(*
          output_char oc '\n'
*)
        );
        
        output_char oc '\n'
      ) seens;
      
      fprintf oc "</body>\n</html>";
      
      close_out oc

(*
let write_map seens =
      let format_jump jump = (* : Disassembler.jump_t = *)
        sprintf "Seen%04d(%05d) --> Seen%04d%-5s %s"
          jump.RlTypes.origin.RlTypes.seen 
          jump.RlTypes.origin.RlTypes.line 
          jump.RlTypes.target.RlTypes.scene (if jump.RlTypes.target.RlTypes.entry > -1 
            then (sprintf "(Z%02d)" jump.RlTypes.target.RlTypes.entry) else "")
          jump.RlTypes.kind 
      in
      
      let game = Game.get_label () in

      let oc = open_out (Filename.concat !App.outdir "seen-map.txt") in 

      fprintf oc "Seen map%s generated by %s %1.2f\n\n" 
        (if game <> "" then " for " ^ game else "")
        App.app.name App.app.version;
        
      List.iter 
        (fun idx -> 
        
(*
        let idx = try
          int_of_string (String.sub seen 4 4)
        with _ -> seen
        in
*)
        
        let map = Hashtbl.find RlTypes.seen_map idx in
        
        fprintf oc "== Seen%04d Summary ==\n\n" idx;
        
        if DynArray.length map.RlTypes.entrypoints > 0 then (
          fprintf oc "  ** entrypoints for Seen%04d **\n\n" idx;
        
          DynArray.iteri
            (fun i l -> 
              fprintf oc "    Z%02d: line %05d\n" i l
          ) map.RlTypes.entrypoints;
          
          output_char oc '\n'        
        );
        
        if DynArray.length map.RlTypes.calls > 0 then (
          fprintf oc "  ** outgoing calls from Seen%04d **\n\n" idx;
        
          DynArray.iter
            (fun call -> 
              output_string oc ("    " ^ (format_jump call) ^ "\n")
          ) map.RlTypes.calls;
          
          output_char oc '\n'
        );
        
        if DynArray.length map.RlTypes.gotos > 0 then (
          fprintf oc "  ** outgoing gotos from Seen%04d **\n\n" idx;
        
          DynArray.iter
            (fun goto -> 
              output_string oc ("    " ^ (format_jump goto) ^ "\n")
          ) map.RlTypes.gotos;
          
          output_char oc '\n'
        );
        
        if Hashtbl.length map.RlTypes.entries > 0 then (
          fprintf oc "  ** incoming calls/gotos to Seen%04d **\n\n" idx;
        
          (*
          Hashtbl.iter
            (fun idx jump -> 
              output_string oc (format_jump jump)
          ) map.RlTypes.entries
          *)
          
          Hashtbl.iter
            (fun eidx jump -> 
(*
              let addr = { RlTypes.empty_addr with scene = idx; entry = eidx } in
*)
              
              (*
              let jump = { RlTypes.empty_jump with 
                RlTypes.origin = { 
                  RlTypes.empty_loc with
                  RlTypes.seen = loc.RlTypes.seen;
                  RlTypes.line = loc.RlTypes.line
                }; RlTypes.target = {
                  RlTypes.empty_addr with
                  RlTypes.scene = idx;
                  RlTypes.entry = eidx
                } 
              } in
              *)
              
              output_string oc ("    " ^ (format_jump jump) ^ "\n")
          ) map.RlTypes.entries;
          
          output_char oc '\n'
        );
        
        output_char oc '\n'
      ) seens;
      
      close_out oc
*)


let disassemble =
  true,
  fun files ->
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;

(*    
    let seen_map : (int, Disassembler.map_t) Hashtbl.t = Hashtbl.create 0 in
*)

    let seens = DynArray.create () in

(*
    let mb_archive (action : bool -> string -> Binarray.t -> unit) =
      function [] -> assert false | (first :: _) as files ->
        if not (Sys.file_exists first) then ksprintf sysError "file `%s' not found" first;
        let archive = seen_count (read_input first) <> -1 in
        if not archive then List.iter (fun s -> action s (read_input s)) files
        else process_read (fun i -> action (sprintf "SEEN%04d.TXT" i)) files
*)
    
(*
    mb_archive
*)
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
        arr
        in
        
        let idx = int_of_string (String.sub fname 4 4) in
        DynArray.add seens idx;
        
        Disassembler.disassemble_source fname oarr
      ) files;

    if (seen_count (read_input (List.hd files)) <> -1) || 
      Disassembler.options.Disassembler.make_map 
      then (write_map (DynArray.to_list seens); 
        write_html_map (DynArray.to_list seens))

(*
let disassemble =
  true,
  fun files ->
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;

(*    
    let seen_map : (int, Disassembler.map_t) Hashtbl.t = Hashtbl.create 0 in
*)

    let seens = DynArray.create () in

(*
    let mb_archive (action : bool -> string -> Binarray.t -> unit) =
      function [] -> assert false | (first :: _) as files ->
        if not (Sys.file_exists first) then ksprintf sysError "file `%s' not found" first;
        let archive = seen_count (read_input first) <> -1 in
        if not archive then List.iter (fun s -> action s (read_input s)) files
        else process_read (fun i -> action (sprintf "SEEN%04d.TXT" i)) files
*)
    
(*
    mb_archive
*)
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
        arr
        in
        
        let idx = int_of_string (String.sub fname 4 4) in
        DynArray.add seens idx;
        
        Disassembler.disassemble_source fname oarr
      ) files;

    if (seen_count (read_input (List.hd files)) <> -1) || 
      RlTypes.options.RlTypes.make_map 
      then write_map (DynArray.to_list seens)
*)
    
(*    
let map =
  true,
  fun files ->
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;

(*    
    let seen_map : (int, Disassembler.map_t) Hashtbl.t = Hashtbl.create 0 in
*)

    let seens = DynArray.create () in
    
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
        arr
        in
        
        let idx = int_of_string (String.sub fname 4 4) in
        DynArray.add seens idx;
        
        Disassembler.generate_map fname oarr
      ) files;

    write_map (DynArray.to_list seens)
*)

let map =
  true,
  fun files ->
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;

(*    
    let seen_map : (int, Disassembler.map_t) Hashtbl.t = Hashtbl.create 0 in
*)

    let seens = DynArray.create () in
    
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
        arr
        in
        
        let idx = int_of_string (String.sub fname 4 4) in
        DynArray.add seens idx;
        
        Disassembler.generate_map fname oarr
      ) files;

    write_map (DynArray.to_list seens);
    write_html_map (DynArray.to_list seens)


(*        
(*
        let map = Disassembler.disassemble_source fname oarr in
*)

        Disassembler.disassemble_source fname oarr;
        
        let idx = int_of_string (String.sub fname 4 4) in

(*
        sysInfo (sprintf "seen idx: %d" idx);
        
        ksprintf sysInfo "seen idx: %d" idx;
*)
        
(*
        Hashtbl.add seen_map idx map;
*)

(*
        Hashtbl.add seen_map idx { 
          Disassembler.empty_map with 
          Disassembler.calls = DynArray.copy map.Disassembler.calls; 
          Disassembler.gotos = DynArray.copy map.Disassembler.gotos };
        
        if !App.verbose > 1 then (
          ksprintf sysInfo "  enpts: %d" (DynArray.length map.Disassembler.entrypoints);
          ksprintf sysInfo "  calls: %d" (DynArray.length map.Disassembler.calls);
          ksprintf sysInfo "  gotos: %d" (DynArray.length map.Disassembler.gotos)
        );

        if !App.verbose > 1 then (
          ksprintf sysInfo "Analyzing Seen%04d..." idx;
        );
            
        DynArray.iter
          (fun (call : Disassembler.jump_t) ->
            try
              let seen = Hashtbl.find seen_map call.Disassembler.target.Disassembler.scene in
              
              Hashtbl.add seen.Disassembler.entries 
                call.Disassembler.target.Disassembler.entry call
            with
              Not_found -> ()
        ) map.Disassembler.calls;
        
        DynArray.iter
          (fun (goto : Disassembler.jump_t) ->
            try              
              let seen = Hashtbl.find seen_map goto.Disassembler.target.Disassembler.scene in
              
              Hashtbl.add seen.Disassembler.entries 
                goto.Disassembler.target.Disassembler.entry goto
            with
              Not_found -> ()
        ) map.Disassembler.gotos;
*)

        DynArray.add seens idx
        
      ) files;

(*
      if DynArray.length seens > 0 then write_map Disassembler.seen_map seens
*)

      if DynArray.length seens > 0 then write_map seens
*)

(*
let disassemble =
(*
  let seen_map = DynArray.create () in
*)

  true,
  fun files ->
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;
    
    let seen_map : (int, Disassembler.map_t) Hashtbl.t = Hashtbl.create 0 in
  
(*
    let seen_map : (int, [ int; int ]) Hashtbl.t = Hashtbl.create 0
    
    let seen_map : (int, [ int; int ]) Hashtbl.t
*)

    let seens = DynArray.create () in
    
    maybe_archive
      (fun fname arr ->
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
        arr 
        in
        
        let map = Disassembler.disassemble_source fname oarr in
        
        (*
        Optpp.sysInfo (Printf.sprintf "index: %s" (String.sub fname 4 4));
        *)
        
        (*
        Hashtbl.add seen_map (string_of_int (int_of_string (String.sub fname 4 4))) map)
        *)
        
        let idx = int_of_string (String.sub fname 4 4) in

(*
        sysInfo (sprintf "seen idx: %d" idx);
        
        ksprintf sysInfo "seen idx: %d" idx;
*)
        
(*
        Hashtbl.add seen_map idx map;
*)

        Hashtbl.add seen_map idx { 
          Disassembler.empty_map with 
          Disassembler.calls = DynArray.copy map.Disassembler.calls; 
          Disassembler.gotos = DynArray.copy map.Disassembler.gotos };
        
        if !App.verbose > 1 then (
          ksprintf sysInfo "  enpts: %d" (DynArray.length map.Disassembler.entrypoints);
          ksprintf sysInfo "  calls: %d" (DynArray.length map.Disassembler.calls);
          ksprintf sysInfo "  gotos: %d" (DynArray.length map.Disassembler.gotos)
        );

        if !App.verbose > 1 then (
          ksprintf sysInfo "Analyzing Seen%04d..." idx;
        );
            
        DynArray.iter
          (fun (call : Disassembler.jump_t) ->
            try
              let seen = Hashtbl.find seen_map call.Disassembler.target.Disassembler.scene in
              
              Hashtbl.add seen.Disassembler.entries 
                call.Disassembler.target.Disassembler.entry call
            with
              Not_found -> ()
        ) map.Disassembler.calls;
        
        DynArray.iter
          (fun (goto : Disassembler.jump_t) ->
            try              
              let seen = Hashtbl.find seen_map goto.Disassembler.target.Disassembler.scene in
              
              Hashtbl.add seen.Disassembler.entries 
                goto.Disassembler.target.Disassembler.entry goto
            with
              Not_found -> ()
        ) map.Disassembler.gotos;

        DynArray.add seens idx
        
(*
        Hashtbl.iter (fun s _ -> 
        
        Hashtbl.add map_in 
*)        
      ) files;

      if DynArray.length seens > 0 then write_map seen_map seens
*)
  
(*
      ksprintf sysInfo "seens: %d" (DynArray.length seens);
      
      sysInfo (sprintf "seens: %d" (DynArray.length seens));
*)
      
(*
      let map_in : (string, string) Hashtbl.t = Hashtbl.create 0 in
*)
      
(*
      let map_in : (int, [ int; int ]) Hashtbl.t = Hashtbl.create 0
*)

(*
      if DynArray.length seens > 0 then
      
(*
      let oc = open_out (Filename.concat !App.outdir "seen-map.txt") in 
      
(*
      let game = 
        match Game.get_game () with
          | Some game -> 
            if game.title <> "" then game.title 
            else if game.id <> "" then game.id 
            else ""
          | _ -> ""
      in
*)

      let game = Game.get_label () in
(*      
      let name = if game.title <> "" then game.title else if game.id <> "" then game.id else "";
*)  
(*
      fprintf oc "Seen map " ^ (if name <> "" then "for " ^ name ^ " " else "") ^
*)

(*
      fprintf oc "Seen map " ^ (if game <> "" then "for " ^ game ^ " " else "") ^
        "generated by %s %1.2f -}\n\n" 
        App.app.name App.app.version bname;
*)

      fprintf oc "Seen map%s generated by %s %1.2f\n\n" 
        (if game <> "" then " for " ^ game else "")
        App.app.name App.app.version;
*)
    
(*
      let keys = DynArray.create () in
      Hashtbl.iter (fun s _ -> 
        DynArray.add keys s;
        Hashtbl.iter (fun s _ -> 
        
        Hashtbl.add map_in 
        
      ) seen_maps;
*)
      
(*
      DynArray.iter 
        (fun idx -> 
        
(*
        fprintf oc "== Seen%04d Summary ==\n\n" idx;
*)
        
(*
        output_string oc
*)

        let map = Hashtbl.find seen_map idx in
        
        if !App.verbose > 0 then (
          ksprintf sysInfo "Analyzing Seen%04d..." idx;
          ksprintf sysInfo "  calls: %d\n" (DynArray.length map.Disassembler.calls);
          ksprintf sysInfo "  gotos: %d\n" (DynArray.length map.Disassembler.gotos)
        );
            
        DynArray.iter
          (fun (call : Disassembler.jump_t) ->
            try
              let seen = Hashtbl.find seen_map call.Disassembler.target.Disassembler.scene in
              
(*
              Hashtbl.add seen.Disassembler.entries 
                call.Disassembler.target.Disassembler.entry 
                 call.Disassembler.origin
*)

              Hashtbl.add seen.Disassembler.entries 
                call.Disassembler.target.Disassembler.entry call
            with
              Not_found -> ()
        ) map.Disassembler.calls;
        
        DynArray.iter
          (fun (goto : Disassembler.jump_t) ->
            try
(*
              let seen = Hashtbl.find seen_map goto.target.seen in
              Hashtbl.add seen.entries goto.target.entry goto.origin
*)
              
              let target = goto.Disassembler.target in
              let seen = Hashtbl.find seen_map target.Disassembler.scene in
              
(*
              Hashtbl.add seen.Disassembler.entries 
                target.Disassembler.entry 
                goto.Disassembler.origin
*)
              Hashtbl.add seen.Disassembler.entries 
                target.Disassembler.entry goto
            with
              Not_found -> ()
        ) map.Disassembler.gotos;
      ) seens;
*)
      
(*
      let format_jump jump = (* : Disassembler.jump_t = *)
        sprintf "Seen%04d(%05d) %12s --> Seen%04d%s"
          jump.Disassembler.origin.Disassembler.seen 
          jump.Disassembler.origin.Disassembler.line 
          jump.Disassembler.kind 
          jump.Disassembler.target.Disassembler.scene (if jump.Disassembler.target.Disassembler.entry > -1 
          then (sprintf "(Z%02d)" jump.Disassembler.target.Disassembler.entry) else "")
      in
*)

      let format_jump jump = (* : Disassembler.jump_t = *)
        sprintf "Seen%04d(%05d) --> Seen%04d%-5s %s"
          jump.Disassembler.origin.Disassembler.seen 
          jump.Disassembler.origin.Disassembler.line 
          jump.Disassembler.target.Disassembler.scene (if jump.Disassembler.target.Disassembler.entry > -1 
            then (sprintf "(Z%02d)" jump.Disassembler.target.Disassembler.entry) else "")
          jump.Disassembler.kind 
      in
      
      let game = Game.get_label () in

      let oc = open_out (Filename.concat !App.outdir "seen-map.txt") in 

      fprintf oc "Seen map%s generated by %s %1.2f\n\n" 
        (if game <> "" then " for " ^ game else "")
        App.app.name App.app.version;
        
      DynArray.iter 
        (fun idx -> 
        let map = Hashtbl.find seen_map idx in
        
        fprintf oc "== Seen%04d Summary ==\n\n" idx;
        
        if DynArray.length map.Disassembler.calls > 0 then (
          fprintf oc "  ** outgoing calls from Seen%04d **\n\n" idx;
        
          DynArray.iter
            (fun call -> 
              output_string oc ("    " ^ (format_jump call) ^ "\n")
          ) map.Disassembler.calls;
          
          output_char oc '\n'
        );
        
        if DynArray.length map.Disassembler.gotos > 0 then (
          fprintf oc "  ** outgoing gotos from Seen%04d **\n\n" idx;
        
          DynArray.iter
            (fun goto -> 
              output_string oc ("    " ^ (format_jump goto) ^ "\n")
          ) map.Disassembler.gotos;
          
          output_char oc '\n'
        );
        
        if Hashtbl.length map.Disassembler.entries > 0 then (
          fprintf oc "  ** incoming calls/gotos to Seen%04d **\n\n" idx;
        
          (*
          Hashtbl.iter
            (fun idx jump -> 
              output_string oc (format_jump jump)
          ) map.Disassembler.entries
          *)
          
          Hashtbl.iter
            (fun eidx jump -> 
(*
              let addr = { Disassembler.empty_addr with scene = idx; entry = eidx } in
*)
              
              (*
              let jump = { Disassembler.empty_jump with 
                Disassembler.origin = { 
                  Disassembler.empty_loc with
                  Disassembler.seen = loc.Disassembler.seen;
                  Disassembler.line = loc.Disassembler.line
                }; Disassembler.target = {
                  Disassembler.empty_addr with
                  Disassembler.scene = idx;
                  Disassembler.entry = eidx
                } 
              } in
              *)
              
              output_string oc ("    " ^ (format_jump jump) ^ "\n")
          ) map.Disassembler.entries;
          
          output_char oc '\n'
        );
        
        output_char oc '\n'
      ) seens;
      
      close_out oc
*)
      
(*
        if Hashtbl.length map.calls > 0 then
          output_string (sprintf "  ** incoming gotos to Seen%04d **\n\n" idx);

          Hashtbl.iter
            (fun idx call -> 
              if output_string format_jump call
          ) map.entries
*)

(*
        DynArray.iter
          (fun call ->
            try
              let seen = Hashtbl.find seen_map call.target.seen in
              Hashtbl.add seen.entries call.target.entry call.origin
            with
              Not_found -> ()
        ) map.calls;
        
        DynArray.iter
          (fun goto ->
            try
              let seen = Hashtbl.find seen_map goto.target.seen in
              Hashtbl.add seen.subs goto.target.entry goto.origin
            with
              Not_found -> ()
        ) map.calls;
      ) seens
*)

(*
let extract =
  true,
  maybe_archive
    (fun fname arr ->
      let oname =
  Filename.concat !App.outdir (Filename.basename fname ^ ".uncompressed") 
      in
      let processed, oarr =
        try_extract
    (fun () ->
       if !App.verbose > 0 then ksprintf sysInfo "Decompressing %s to %s" fname oname) 
    arr
      in
      if processed
      then
        let ucheader =
    let hdr = read_file_header arr in 
    let itype =
      if hdr.header_version = 1 then "2K"
      else if hdr.compiler_version = 110002 then "RM"
      else "RL"
    in
    match !App.target_version with
      | (0, 0, 0, 0) -> sprintf "KP%s" itype
      | (a, b, c, d) -> sprintf "RD%s%c%c%c%c" itype (char_of_int d)
                          (char_of_int c) (char_of_int b)
                          (char_of_int a)
  in
        write oarr 0 ucheader;
        write_file oarr oname
      else 
        ksprintf sysInfo "Ignoring %s (not compressed)"
                   (Filename.basename fname))
*)

let extract =
  true,
  maybe_archive
    (fun fname arr -> let oname = Filename.concat !App.outdir 
  (*  (Filename.basename fname ^ ".uncompressed") *)
      (Filename.basename fname ^ "." ^ uncomp_ext) 
      in 
    let processed, oarr = 
      try_extract (fun () -> if !App.verbose > 0 then 
        ksprintf sysInfo "Decompressing %s to %s" fname oname) 
    arr
      in
      if processed then
        let ucheader =
    let hdr = read_file_header arr in 
    let itype =
      if hdr.header_version = 1 then "2K"
      else if hdr.compiler_version = 110002 then "RM"
      else "RL"
    in
    match !App.target_version with
      | (0, 0, 0, 0) -> sprintf "KP%s" itype
      | (a, b, c, d) -> sprintf "RD%s%c%c%c%c" itype 
                          (char_of_int d) (char_of_int c) 
                          (char_of_int b) (char_of_int a)
  in
        write oarr 0 ucheader;
        write_file oarr oname
      else 
        ksprintf sysInfo "Ignoring %s (not compressed)"
                   (Filename.basename fname))

(*
let break =
  true,
  process_read
    (fun idx arr -> let fname = sprintf "SEEN%04d.TXT" idx in
      if !App.verbose > 0 then ksprintf sysInfo "Extracting %s" fname;
      write_file arr (Filename.concat !App.outdir (Filename.basename fname)))
*)

let break =
  true,
  process_read
    (fun idx arr -> let fname = sprintf "SEEN%04d.TXT" idx in
      let oname = Filename.basename fname ^ "." ^ comp_ext in
      if !App.verbose > 0 then ksprintf sysInfo 
        "Extracting %s to %s" fname oname;
      write_file arr (Filename.concat !App.outdir oname))

let list =
  let pad c x = Format.fprintf c "%10s k" 
    (sprintf "%.2f" (x /. 1024.)) in
  false,
  process_read
    (fun idx arr ->
      let hdr = read_full_header arr in
      if !App.names_opt then
        if hdr.dramatis_personae <> [] then
          let rec loop =
            function
              | [] -> assert false
              | x::[] -> Format.printf "%s@." (Text.sjs_to_err x)
              | x::xs -> Format.printf "%s,@ " (Text.sjs_to_err x); loop xs
          in
          Format.printf "@[<4>SEEN%04d.TXT: " idx;
          loop (List.sort String.compare hdr.dramatis_personae)
        else Format.printf "SEEN%04d.TXT\n" idx
      else
        let unc = float_of_int (hdr.uncompressed_size + hdr.data_offset) in
        match hdr.compressed_size with
          | Some i -> let cmp = float_of_int (i + hdr.data_offset) in
                      Format.printf "SEEN%04d.TXT: %a -> %a   (%.2f%%)\n" 
                        idx pad unc pad cmp (cmp /. unc *. 100.)
          | None -> Format.printf "SEEN%04d.TXT: %a\n" idx pad unc)

(*
let info =
  let pad c x = Format.fprintf c "%10s k" 
    (sprintf "%.2f" (x /. 1024.)) in
  let l,b = ref 0,ref 0 in
  
  false,
  process_read
    (fun idx arr ->
      let hdr = read_full_header arr in
      if !App.names_opt then
        if hdr.dramatis_personae <> [] then
          let rec loop =
            function
              | [] -> assert false
              | x::[] -> Format.printf "%s@." (Text.sjs_to_err x)
              | x::xs -> Format.printf "%s,@ " (Text.sjs_to_err x); loop xs
          in
          Format.printf "@[<4>SEEN%04d.TXT: " idx;
          loop (List.sort String.compare hdr.dramatis_personae)
        else Format.printf "SEEN%04d.TXT\n" idx
      else
        let unc = float_of_int (hdr.uncompressed_size + hdr.data_offset) in
        let fname = sprintf "SEEN%04d.TXT" idx in
        
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
        arr in 
        
(*
        let oarr = arr in
*)        
        let lines,bytes = Disassembler.source_info fname oarr in
        
        l := !l + lines;
        b := !b + bytes;
        
        match hdr.compressed_size with
          | Some i -> let cmp = float_of_int (i + hdr.data_offset) in
                      Format.printf "SEEN%04d.TXT: %a -> %a   (%.2f%%)   strings: %d\n" 
                        idx pad unc pad cmp (cmp /. unc *. 100.) lines
          | None -> Format.printf "SEEN%04d.TXT: %a   strings: %d\n" idx pad unc lines;
          
        Format.printf "\nlines: %d; bytes: %d; kbytes: %.2f\n"
          !l !b ((float_of_int !b) /. 1024.))
*)

let info =
  true,
  fun files ->
    let ic = open_in (Filename.concat (Config.prefix ()) !App.kfn_file) in 
    KfnParser.parse KfnLexer.lex (Lexing.from_channel ic);
    close_in ic;

    let pad c x = Format.fprintf c "%10s k" 
      (sprintf "%.2f" (x /. 1024.)) in
      
    let l,b = ref 0,ref 0 in
  
(*  let archive = ref false;  *)
    
    (* "Checking bytecode in %s" *)

    let maybe_arc (action : string -> Binarray.t -> unit) =
      function [] -> assert false | (first :: _) as files ->
        if not (Sys.file_exists first) then ksprintf sysError "file `%s' not found" first;
        (*
        if is_archive first <> `No
        *)

(*
        if seen_count (read_input first) <> -1
        then process_read (fun i -> action (sprintf "SEEN%04d.TXT" i)) files
        else List.iter (fun s -> action s (read_input s)) files
*)


        let arr = read_input first in
        let archive = seen_count arr <> -1 in
        
(*        let archive = seen_count (read_input first) <> -1 in *)
(*        archive := seen_count (read_input first) <> -1;*)
        

        if archive then (
          if Binarray.read arr 0 23 = "\000Empty RealLive archive" then
            Format.printf "Empty RealLive archive\n%!"
          else
          Format.printf "Seen table\n\n%!";
          let rec loop = function 10000, count -> ()
            | i, c ->
              let p, l = get_subfile_info arr i in
              let s = if l = 0 then "" else sprintf "-%7d" (p + l - 1) in
              Format.printf "  SEEN%04d.TXT: %6d %7d%8s %7d\n%!" i (i * 8) p s l;
              loop(i + 1, c + 1)
          in
          loop(0, 0);
          Format.printf "\n\nEnd of Seen table\n\n%!";
        );
        
        if not archive then List.iter (fun s -> action s (read_input s)) files
        else process_read (fun i -> action (sprintf "SEEN%04d.TXT" i)) files
    in
    
(*    maybe_archive    *)
    maybe_arc
      (fun fname arr ->
      (*
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Checking %s" fname)
        arr 
      *)
      
        let hdr = read_full_header arr in
        if !App.names_opt then
          if hdr.dramatis_personae <> [] then
            let rec loop =
              function
                | [] -> assert false
                | x::[] -> Format.printf "%s@." (Text.sjs_to_err x)
                | x::xs -> Format.printf "%s,@ " (Text.sjs_to_err x); loop xs
            in
            Format.printf "@[<4>%s: " fname;
            loop (List.sort String.compare hdr.dramatis_personae)
          else Format.printf "%s\n" fname
        else
          let unc = float_of_int (hdr.uncompressed_size + hdr.data_offset) in
        
          let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Disassembling %s" fname)
          arr in 
        
(*
        let oarr = arr in
*)        
          let lines,bytes = Disassembler.source_info fname oarr in
        
          (*
          if bytes > 0 then 
          *)
          
          l := !l + lines;
          b := !b + bytes;
        
          match hdr.compressed_size with
            | Some i -> let cmp = float_of_int (i + hdr.data_offset) in
(*
                      Format.printf "%s: %a -> %a   (%.2f%%)   strings: %d; bytes: %d\n%!" 
                        fname pad unc pad cmp (cmp /. unc *. 100.) lines bytes
*)
                      Format.printf "%s: %a -> %a   (%.2f%%)   %7d %7d\n%!" 
                        fname pad unc pad cmp (cmp /. unc *. 100.) lines bytes
            | None -> Format.printf "%s: %a   strings: %d\n" fname pad unc lines)
      files;
      
      Format.printf "\nlines: %d; bytes: %d; kbytes: %.2f; mbytes: %.2f\n"
        !l !b ((float_of_int !b) /. 1024.) ((float_of_int !b) /. 1024. /. 1024.)



let pack =
  true,
  List.iter
    (fun fname ->
      let arr = read_input fname in
      if not (Bytecode.uncompressed_header (read arr 0 4)) then
        ksprintf sysInfo "Skipping %s: not an uncompressed bytecode file" (Filename.basename fname)
      else
        let oname =
          Filename.concat
            !App.outdir
          (if Filename.check_suffix fname ".uncompressed"
            then Filename.basename (Filename.chop_suffix fname ".uncompressed")
            else Filename.basename fname)
        in
        try
          if !App.verbose > 0 then ksprintf sysInfo 
            "Compressing %s to %s" fname oname;
          write_file (Rlcmp.compress arr) oname
        with
          Failure e ->
            ksprintf sysInfo "Skipping %s: %s" 
              (Filename.basename fname) e)

(*
let try_extract notify arr =
  if Bytecode.uncompressed_header (read arr 0 4)
  then false, arr
  else (notify (); true, Rlcmp.decompress arr)
*)

let decompress = 
  true,
  fun files ->
    maybe_archive
      (fun fname arr ->
(*
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Decompressing %s" fname)
        arr
        in 
*)
        ignore (sysError "not implemented");
    (*  Disassembler.disassemble fname oarr true) *)
        )
      files
      
let decrypt = 
  true,
  fun files ->
    maybe_archive
      (fun fname arr ->
(*
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Decrypting %s" fname)
        arr
        in
*)
        ignore (sysError "not implemented");
    (*  Disassembler.disassemble fname oarr true) *)
      )
      files

let encrypt = 
  true,
  fun files ->
    maybe_archive
      (fun fname arr ->
(*
        let _, oarr = try_extract (fun () -> if !App.verbose > 0 
            then ksprintf sysInfo "Encrypting %s" fname)
        arr
        in 
*)
        ignore (sysError "not implemented");
    (*  Disassembler.disassemble fname oarr true) *)
      )
      files
  
let output_int oc v =
  output_byte oc (v land 0xff);
  output_byte oc ((v lsr 8) land 0xff);
  output_byte oc ((v lsr 16) land 0xff);
  output_byte oc ((v lsr 24) land 0xff)

let read_and_compress fname =
  let arr = read_input fname in
  if not (is_bytecode arr 0) then (
    ksprintf sysWarning "unable to add '%s' to archive: not a bytecode file" fname;
    None
  )
  else if Bytecode.uncompressed_header (read arr 0 4) then
    Some (Rlcmp.compress arr)
  else
    Some arr

let do_arg f x = try f x with Sys_error _ -> Gc.full_major (); f x (* work around mmap() issues on Win32 *)

let rebuild_arc arc fname to_process =
  let rec tfn cv = let rv = sprintf "~temp%d.seen.tmp" cv in if Sys.file_exists rv then tfn (cv + 1) else rv in
  let tfn = tfn 0 in
  do_arg (Sys.rename fname) tfn;
  let oc = do_arg open_out_bin fname in
  try
    seek_out oc 80000;
    let processed, _ =
      IMap.fold
        (fun i source (rv, idx) ->
          let rlen =
            match source with
              | `Keep (offset, len)
               -> Binarray.output (Binarray.sub arc offset len) oc; len
              | `File fname
               -> (match read_and_compress fname with
                    | Some arr -> Binarray.output arr oc; Binarray.dim arr
                    | None -> 0)
          in
          IMap.add i (idx, rlen) rv, idx + rlen)
        to_process
        (IMap.empty, 80000) in
    seek_out oc 0;
    for i = 0 to 9999 do
      let offset, len = try IMap.find i processed with Not_found -> 0, 0 in
      output_int oc offset;
      output_int oc len
    done;
    close_out oc;
    Sys.remove tfn
  with
    e -> (try close_out oc with _ -> ());
         (try Sys.remove fname with _ -> ());
         (try Sys.rename tfn fname with _ -> sysWarning "handling exception in Archiver.rebuild_arc: cleanup failed, data may be corrupt");
         raise e

let add =
  false,
  function
    | [] -> assert false
    | _ :: [] -> failwith "no files to process"
    | fname :: files
     -> let arc, existing =
          if Sys.file_exists fname then
(*
            match is_archive fname with
              | `No -> ksprintf failwith "%s is not a valid RealLive archive" (Filename.basename fname);
(*              
              | `Yes (_, `Empty) -> Binarray.create 0, IMap.empty
              | `Yes (a, `Populated) ->
*)
              | `Yes (_, 0) -> Binarray.create 0, IMap.empty
              | `Yes (a, c) ->
*)
            let a = read_input fname in
            match seen_count a with
              | -1 -> ksprintf failwith "%s is not a valid RealLive archive" (Filename.basename fname);
              | 0 -> Binarray.create 0, IMap.empty
              | c ->
      ignore (printf "SEEN COUNT: %d\n" c);
      let rec loop cv acc =
        if cv = 10000 then
                      acc
                    else
                      loop (cv + 1)
                        (match get_subfile_info a cv with
                           | 0, 0 -> acc
                           | data -> IMap.add cv (`Keep data) acc)
                  in
                    a, loop 0 IMap.empty
          else
            let oc = open_out_bin fname in
              output_string oc "\000Empty RealLive archive";
              close_out oc;
              Binarray.create 0, IMap.empty
        in
        let contents, do_process =
          List.fold_left
            (fun (acc, found) fname ->
              if not (Sys.file_exists fname) then
                (ksprintf sysWarning "file not found: %s" fname; acc, found)
              else
                try
                  Scanf.sscanf
                    (Filename.basename fname)
                    "%_[Ss]%_[Ee]%_[Ee]%_[Nn]%u"
                    (fun idx -> IMap.add idx (`File fname) acc),
                  true
                with
                  Scanf.Scan_failure _ ->
                    ksprintf sysWarning
                      "unable to add '%s' to archive: file name must begin 'SEENxxxx', where 0 <= xxxx <= 9999" fname;
                    acc, found)
            (existing, false)
            files
        in
        if do_process then
          rebuild_arc arc fname contents
        else
          failwith "no files to process"


let remove =
  false,
  process_archive true
    (fun fname arc to_process ->
      let any_removed = ref false
      and any_remain = ref false in
      let to_keep =
        let rec loop cv acc =
          if cv = 10000 then
            acc
          else
            loop (cv + 1)
                 (match get_subfile_info arc cv with
                    | 0, 0 -> acc
                    | data when ISet.mem cv to_process -> any_removed := true; acc
                    | data -> any_remain := true; IMap.add cv (`Keep data) acc)
        in
        loop 0 IMap.empty
      in
      if not !any_removed then
        sysInfo "No files to remove."
      else if not !any_remain then (
        sysWarning "all archive contents removed";
        let oc = do_arg open_out_bin fname in
        for i = 0 to 9999 do output_string oc "\000\000\000\000\000\000\000\000" done;
        close_out oc
      )
      else
        rebuild_arc arc fname to_keep)
