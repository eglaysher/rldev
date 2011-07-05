(*
   RLdev: game definition file
   RikiTikki (K) 2010-2011 Richard 23

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
open Config
open GameTypes

(* open GameParser *)

(*
exception Eof

type engine = [ `RealLive | `Kinetic | `Avg2000 | `Siglus ]

type version = int * int * int * int

type target = engine * version * int

type subkey = int * int * int list

type key = subkey list

type t = string * string * string * int * string list * target * key
*)

(*
type ids = string list
*)

(*
type target_t = [ `RealLive | `Kinetic | `Avg2000 | `Siglus ]

type version = int * int * int * int

type target = target_t * [ `Any | `Version of version ] * [ `Any | `Integer of int ]

(*
type subkey = int * int * char list

type subkeys = subkey list

type key = [ `None | `Key of subkeys ]
*)

type subkey_t = { 
    offset: int;
    length: int;
    data: int list; }
(*
    data: char list; }
    data: int array; }
*)
*)

(*
let subkey offset length data = { 
    offset = offset; 
    length = length;
    data = data }
*)
    
(*
type subkeys = subkey_t list
*)

(*
type key = [ None | Some subkeys ]
*)

(*
type key = [ `None | `Key of (subkey list) ]
*)

(*
type t = { 
    id:         string;
    title:         string;
    by:         string;
    seens:         int;
    inherits:     string list option;
    target:     GameTypes.target option;
    key:         GameTypes.subkey_t list option;
    dir:         string; 
}
*)

type t = game_t

let empty_game = {
    id =        "";
    title =     "";
    by =        "";
    seens =     -1;
    inherits =  None;
    target =    None;
    key =       None;
    dir =       ""; 
}

(*
    target = ( `RealLive, `Any, `Any );
*)

let create (ident, title, pub, seens, inherits, target, key) = 
  let game = { empty_game with 
    id =        ident;
    title =     title;
    by =        pub;
    seens =     seens;
    inherits =  inherits;
    target =    target;
    key =       key;
  } in
  game

(*
game FIVE "5 -Faibu-" by Ram
    for RealLive 1.5.9.1

    using key from 256 for 128:
        0xe5 0xe8 0x20 0xe8 0x6e 0x91 0xb4 0xb1
        0x4b 0xc5 0x34 0x9e 0xad 0x2c 0x71 0x32

    and key from 384 for 128:
*)


let join list delim =
  let f str item = str ^ delim ^ item in
  let str = List.fold_left f list in
  str

let join list delim = String.concat delim list

(*
let join lst delim =
  let str = List.fold_left (fun str item -> str ^ delim ^ item) lst in
  str
*)

(*
let string_of_list ?(sep = ",") f p = String.concat (sep ^ " ") (List.map f p)
*)

let string_of_list ?(sep = ",") f p = String.concat (sep ^ " ") (List.map f p)


let string_of_version =
  function
    | a, b, 0, 0 -> sprintf "%d.%d" a b
    | a, b, c, 0 -> sprintf "%d.%d.%d" a b c
    | a, b, c, d -> sprintf "%d.%d.%d.%d" a b c d

(*
let string_of_target t = (* (targ,vers,comp) = *)
  match t with
    | Some targ,vers,comp ->
      (match targ with 
        | `RealLive -> "RealLive"
        | `Kinetic -> "Kinetic"
        | `Avg2000 -> "Avg2000"
        | `Siglus -> "Siglus") ^ 
      (match vers with 
        | `Any -> ""
        | `Version v -> " " ^ string_of_version v) ^ 
      (match comp with 
        | `Any -> ""
        | `Integer i -> " " ^ string_of_int i)
    | None -> ""
*)

let string_of_target (targ,vers,comp) = 
  (match targ with 
    | `RealLive -> "RealLive"
    | `Kinetic -> "Kinetic"
    | `Avg2000 -> "Avg2000"
    | `Siglus -> "Siglus") ^ 
  (match vers with 
    | `Any -> ""
    | `Version v -> " " ^ string_of_version v) ^ 
  (match comp with 
    | `Any -> ""
    | `Integer i -> " " ^ string_of_int i)


(*
let string_of_subkey key =
    sprintf "key from %d for %d:\n        %s" key.offset key.length
      (join (List.map (fun v -> sprintf "%02x" v) key.data) " ")
*)

(*
let string_of_subkey key =
    sprintf "key from %d for %d:\n        %s" key.offset key.length
      (string_of_list (fun v -> sprintf "%02x" v) key.data ~sep:" ")
*)

(*
let string_of_subkey key =
    sprintf "key from %d for %d:\n        %s" key.offset key.length
      (String.concat " " (List.map (fun v -> sprintf "%02x" v) key.data))
(*
      (join (List.map (fun v -> sprintf "%02x" v) key.data) " ")
*)
*)
(*
let string_of_subkey key =
    let i = ref 0;
    (sprintf "key from %d for %d:\n        %s" key.offset key.length)
      (String.concat " " (List.map (fun v -> sprintf "%02x%s" v 
        (incr i; if not !i mod 8 then "\n        " else "")) key.data))
*)

let string_of_subkey key =
    sprintf "key from %d for %d:%s" key.offset 
      key.length (String.concat " " (Array.to_list 
        (Array.mapi (fun i v -> sprintf "%s%02x" 
          (if i mod 8 = 0 then "\n        " else "") v) 
          (Array.of_list key.data))))
        
(*
let string_of_key key =
  join (List.iter (fun sk ->
    sprintf "key from %d for %d:\n        %s" sk.offset sk.length
      join (List.iter (fun v -> sprintf "%02x" v) sk.data) " "
  ) key) "\n\n    and "
*)

(*
let string_of_key key =
  join (List.map string_of_subkey key) "\n\n    and "
*)

(*
let string_of_key key =
  string_of_list string_of_subkey key ~sep:"\n    and "
*)

let string_of_key key =
  String.concat "\n    and " (List.map string_of_subkey key)

let string_of_game game = 
  "Game" ^ (if game.id <> "" then " " ^ game.id else "") 
    ^ (if game.title <> "" then " \"" ^ game.title ^ "\"" else "")
    ^ (if game.by <> "" then " by " ^ game.by else "")
    ^ (if game.seens <> -1 then sprintf " with %d seens" game.seens else "")

(*
    ^ (match game.inherits with | Some (l) -> " inherits " ^ (join l ", ") | None -> "")
*)
    ^ (match game.inherits with | Some (l) -> " inherits " ^ (String.concat ", " l) | None -> "")

(*
    ^ (match game.inherits with | Some l -> " inherits " ^ 
      (string_of_list (fun s -> s) l ~sep:", ") | None -> "")
*)
    ^ (match game.target with | Some t -> " for " ^ (string_of_target t) | None -> "") 
    ^ (match game.key with | Some k -> "\n    using " ^ (string_of_key k) | None -> "")


(*
let string_of_target targ =
    
                  -> let cm = 
                       match c with
                         | "reallive" -> `RealLive
                         | "avg2000" -> `Avg2000
                         | "kinetic" -> `Kinetic
                         | s -> kprintf failwith "unknown version `%s'" s
                     in v_acc, cm :: c_acc

let string_of_version =
  function
    | a, b, 0, 0 -> sprintf "%d.%d" a b
    | a, b, c, 0 -> sprintf "%d.%d.%d" a b c
    | a, b, c, d -> sprintf "%d.%d.%d.%d" a b c d
*)

(*
let string_of_list ?(sep = ",") f p = String.concat (sep ^ " ") (List.map f p)
*)

(*
let file = ref ""
let line = ref 1
*)

(*
let file = ref GameParser.file
*)

type lex_fun_t = Lexing.lexbuf -> int
type parse_fun_t = lex_fun_t * Lexing.lexbuf -> int

type deftab = (string, t) Hashtbl.t

let games : deftab = Hashtbl.create 0

let game = ref ""

let game_file = ref ""

(*
  ignore (Game.init GameParser.parse GameLexer.lex 
    (Filename.concat (Config.prefix ()) !App.game_file) !App.verbose);
*)        

(* Read and parse the game definition file *)

exception Parse_error of string

(*
let load file verbose = 
  game_file := file;
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  GameParser.parse GameLexer.lex lexbuf;
  close_in ic
*)

(*
let init parsefun lexfun file verbose = 
(*
  let ic = open_in (Filename.concat (Config.prefix ()) file) in
*)
  game_file := file;
  
  if verbose > 0 then ksprintf sysInfo "read game definition file '%s'" file;
  
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  parsefun lexfun lexbuf;
  
(*
  (try
    Array.iter
      (fun (id, title, pub, seens, ids, targ, key) -> 
        let game = { empty_game with 
          id = ident;
          title = title;
          by = pub;
          seens = seens;
          inherits = inherits;
          target = target;
          key = key;
        } in
  
        if not (Game.exists ident) then
          set ident game
        else
          printf "Warning: Game '%s' redefined.\n"
      ) (parsefun lexfun lexbuf)
  with 
    | Eof -> ignore (exit 0)

    | Parse_error e -> ignore (sysError (sprintf "%s %s at line %d char %d"
                        "Parse_error" e (!GameLexer.line) 
                        ((Lexing.lexeme_start lexbuf) - !GameLexer.lpos))) 

    | ext ->    ignore (sysError (sprintf "%s at line %d char %d" "Error" !GameLexer.line
                        ((Lexing.lexeme_start lexbuf) - !GameLexer.lpos)))
  );
*)
  
  close_in ic  

  sysInfo ("games: " ^ (join game_list ", ") ^ "\n\n");
*)

(*
let read_file ?(parsefun = GameParser.parse) ?(lexfun = GameLexer.lex) ?(file = "games.info") verbose =
*)

(*
let read_file ?(parsefun = GameParser.parse) ?(lexfun = GameLexer.lex) ?(file = "games.info") verbose =
  let ic = open_in (Filename.concat (Config.prefix ()) file) in
  let lexbuf = Lexing.from_channel ic in
  
(*
  Parsing.set_trace true;
*)
  
  (try
    Array.iter
      (fun (id, title, pub, seens, ids, targ, key) -> 
        let game = { empty_game with 
          id = ident;
          title = title;
          by = pub;
          seens = seens;
          inherits = inherits;
          target = target;
          key = key;
        } in
  
        if not (Game.exists ident) then
          set ident game
        else
          printf "Warning: Game '%s' redefined.\n"
      ) (parsefun lexfun lexbuf)
  with 
    | Eof -> ignore (exit 0)

    | Parse_error e -> ignore (sysError (sprintf "%s %s at line %d char %d"
                        "Parse_error" e (!GameLexer.line) 
                        ((Lexing.lexeme_start lexbuf) - !GameLexer.lpos))) 

    | ext ->    ignore (sysError (sprintf "%s at line %d char %d" "Error" !GameLexer.line
                        ((Lexing.lexeme_start lexbuf) - !GameLexer.lpos)))
  );

  close_in ic  
*)
    
(*
    let parse_error err_message =
  kprintf Optpp.sysError "parsing %s: %s at line %d" !file err !line
  close_in ic  
*)

(*
  try
    while true do
      parsefun lexfun lexbuf;
      (*
      GameLexer.read_file(parsefun lexfun filename verbose)
      *)
    done
  with Eof ->
    exit 0
  close_in ic
*)

(*
let semvalue = parsefun(lexfun, lexbuf)
*)
  
let find key = Hashtbl.find games key

let set key =
  Hashtbl.replace games key

let unset key =
  Hashtbl.remove games key

let exists key =
  Hashtbl.mem games key

let get key =
  try Some (find key) with Not_found -> None

let get_def key ~def =
  try find key with Not_found -> def

(*
let add game = 
(*
  printf "Adding game '%s'\n" game.id;

  (match game.key with 
    | Some k -> printf "  game.key length: %d\n" (List.length k)
    | None -> ());
    
  printf "%s\n\n" (string_of_game game);
*)  

  if exists game.id then printf "Warning: Game '%s' redefined.\n" game.id;
  set game.id game
*)

let add game = 
(*
  ksprintf sysInfo "Add game '%s'" game.id;
*)
  if exists game.id then ksprintf sysWarning "Game '%s' redefined" game.id;
  set game.id game

(*
let game_list () = 
*)
let list () =
  let arr = DynArray.create () in
(*  
  List.sort (DynArray.to_list (Hashtbl.iter (fun key value -> DynArray.add arr key) games))
*)
(*
  Hashtbl.iter 
    (fun key value -> 
      printf "[GAME] %s\n" key; DynArray.add arr key) 
    games;
*)

  Hashtbl.iter (fun key value -> DynArray.add arr key) games;
    
  List.sort String.compare (DynArray.to_list arr)

(*
let set_game verbose id =
  if exists id then (
    game := id;
    printf "Game id '%s'\n" id;
    id
  ) else if id <> "" then (
    printf "Warning: unknown game '%s'\n" id;
    ""
  ) else ( (* let idx = match id with *)
    printf "Warning: no game id specified\n";
    ""
  );
  
  !game
*)

(*
let set_game verbose id =
  printf "set_game '%s'\n" id;
  sysWarning id;
  
  if exists id then (
    game := id;
    printf "Game id '%s'\n" id
  ) else if id <> "" then (
    printf "Warning: unknown game '%s'\n" id
  ) else ( (* let idx = match id with *)
    printf "Warning: no game id specified\n"
  )
*)

let set_game verbose id =
  if verbose > 2 then (
    ksprintf sysInfo "set_game(%s,%s)\n\n" (if verbose > 1 then "true" else "false") id;
    ksprintf sysInfo "games: %s" (join (list ())", ")
  );


(*
  let g = Hashtbl.find games id in
*)

(*
  let g = get id in
  if verbose() then sysInfo ("GAME: " ^ (string_of_game g) ^ "\n");
*)

(*
  if verbose() then sysInfo ("GAME: " ^ (string_of_game (get id)) ^ "\n");
*)

(*
  try 
    find id; 
    if verbose() then sysInfo (sprintf "Game id '%s'\n" id);
    game := id
*)
(*
  (try
    (* find id; *)
    Hashtbl.find games id;
    if verbose() then sysInfo (sprintf "Game id '%s'\n" id);
    game := id
  with
    Not_found -> 
      sysError (if id = "" then "invalid game id" 
        else sprintf "unknown game '%s'" id)
  )
*)
  
(*
  if get id <> None then (
*)

  if exists id then (
     if verbose > 1 then ksprintf sysInfo "Game id '%s'" id;
    game := id
  ) else (
    sysError (if id = "" then "invalid game id" 
      else sprintf "unknown game '%s'" id)
  );

  if verbose > 2 then ksprintf sysInfo "GAME ID: %s" !game
  
(*  
  if exists id then (
    if verbose() then sysInfo (sprintf "Game id '%s'\n" id);
    game := id
  ) else sysError (if id = "" then "invalid game id" 
    else sprintf "unknown game '%s'" id)
*)

(*
let get_game = get !game
*)


let get_game () = try Some (Hashtbl.find games !game) with Not_found -> None

(*
let get_game () = 
(*
  sysInfo "get_game";
*)
  Some (Hashtbl.find games !game)
*)

let get_label () =
 (*
 let game = get_game () in
*)

  try
    let game = Option.get (get_game ()) in
  
    if game.title <> "" then game.title
    else if game.id <> "" then game.id
    else ""
  with
    _ -> ""
    
(*
let get_key = get game
*)

(*
let get_key =
  match get !game with 
    | Some g -> 
      (match g.key with
        | `Key k -> k 
        | `None -> [])
    | None -> []
*)

let get_key () =
(*
  sysInfo "get_key()\n";
  sysInfo (sprintf "  game: %s\n\n" !game);
  ksprintf sysInfo "GAME: %s\n\n" (match get !game with 
    | Some g -> string_of_game g
    | None -> "None");
*)
  
  match get !game with 
    | Some g -> 
      (match g.key with
        | Some k -> k 
        | None -> [])
    | None -> []

let get_version () =
(*
  printf "get_version()\n";
  printf "id: %s\n" !game;
*)
  match get !game with 
    | Some g -> 
      (*(printf "target: %s\n" (match g.target with | Some t -> string_of_target t | None -> "None");*)
      (match g.target with
        | Some (_, v, _) -> 
            (match v with | `Any -> "" | `Version v -> string_of_version v)
        | None -> "")
    | None -> ""



let load file verbosity = 
  if file = "" then sysError "no game definition file specified";
(*
  if verbose > 0 then ksprintf sysInfo "load game definition file '%s'" file;
*)

  if verbosity > 0 then ksprintf sysInfo "load game definition file '%s'" file;

(*
  let (//) = Filename.concat in
  let path = Config.prefix () // file in
*)

(*
  let path = Filename.concat (Config.prefix ()) file in
*)
  
  game_file := file;
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  List.iter add (GameParser.parse GameLexer.lex lexbuf);
(*
  GameParser.parse GameLexer.lex lexbuf;
*)
  close_in ic

let init parsefun lexfun file verbosity = 
(*
  let ic = open_in (Filename.concat (Config.prefix ()) file) in
*)
  game_file := file;
  
  if verbosity > 0 then ksprintf sysInfo "read game definition file '%s'" file;
  
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
(*
  let arr = parsefun lexfun lexbuf in
*)
  List.iter add (parsefun lexfun lexbuf);
  
  close_in ic;
  
(*
  printf "game count: %d\n" (List.length arr);
  
  List.iter (fun g -> add g; printf "[game] %s\n" g.id) arr;

  sysInfo ("games: " ^ (join (game_list()) ", ") ^ "\n\n");

  sysInfo ("games: " ^ (String.concat ", " (List.map (fun g -> g.id) arr)) ^ "\n\n");
*)
  
(*  game_list () *)

  list ()
