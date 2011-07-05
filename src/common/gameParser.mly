/*
   RLdev: game definition file parser
   Corythirp (K) 2010-2011 Richard 23

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
*/

%{

(*
open Parsing
open Game
*)

open Printf
open GameTypes

(*
type target_t = [ `RealLive | `Kinetic | `Avg2000 | `Siglus ]

type version = int * int * int * int

type target = target_t * [ `Any | `Version of version ] * [ `Any | `Integer of int ]

type subkey_t = { 
	offset: int;
    length: int;
    data: int list; }

type game = `Game of string * string * string * int * string list option
	* target option * subkey_t list option * string
*)

(*
type subkey_t = { 
	offset: int;
    length: int;
    data: int list; }

type game = Game of string * string * string * int * string list option
	* ([ `RealLive | `Kinetic | `Avg2000 | `Siglus ] 
	  * [ `Any | `Version of int * int * int * int ] 
	  * [ `Any | `Integer of int ]) option 
    * ( int * int * int) list option 
	* string
*)

(*
exception Eof

type ids = string list

type target_t = [ `RealLive | `Kinetic | `Avg2000 | `Siglus ]

type version = int * int * int * int

type target = target_t * [ `Any | `Version of version ] * [ `Any | `Integer of int ]

type subkey = int * int * int list

type key = [ `None | `Key of subkey list ]
*)

(*
type game = string * string * string * int * string list * target * key
*)

(*
type games = [ `Game of game ] list
*)

(*
type game = { 
	id: string;
    title: string;
    by: string;
    seens: int;
	inherits: ids option;
	target: target option;
	key: key option;
	dir: string; }
*)

(*
let empty_game = {
	id = "";
    title = "";
    by = "";
    seens = -1;
	inherits = None;
	target = None;
	key = None;
	dir = ""; 
}
*)

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

let string_of_list ?(sep = ",") f p = String.concat (sep ^ " ") (List.map f p)

(*
let process_game (ident, title, pub, seens, inherits, target, key) =
*)

(*
let process_game (ident, title, pub, seens, inherits, target, key) = 
  Game.add (Game.create (ident, title, pub, seens, inherits, target, key))
*)

(*
let process_game (ident, title, pub, seens, inherits, target, key) = 
  let game = Game.create (ident, title, pub, seens, inherits, target, key) in
  Game.add game;
  game
*)
  
(*
let process_game (ident, title, pub, seens, inherits, target, key) = 
(*
  printf "game '%s'\n" ident;
  printf "title: '%s'\n" title;
  printf "pub: '%s'\n" pub;
  printf "seens: %d\n" seens;
  printf "inherits '%s'\n" (match inherits with 
	Some ids -> string_of_list (fun s -> s) ids 
	| None -> "None");
*)
(*
  printf "target '%s'\n" (match target with 
	Some t -> () | None -> "None");
  printf "keys '%s'\n" key;
*)

(*  
  (ident, title, pub, seens, inherits, target, key)
*)

(*
  let game = Game.empty_game with 
*)

(*
  let game = { Game.empty_game with 
	id = ident;
    title = title;
    by = pub;
    seens = seens;
	inherits = inherits;
	target = target;
	key = key;
  } in
*)

  let game = Game.create (ident, title, pub, seens, inherits, target, key) in
  if not (Game.exists ident) then (
    printf "Adding game '%s'\n" ident;
	match key with 
	  | Some k -> printf "  key length: %d\n" (List.length k)
	  | None -> printf "  no key\n";
	match game.Game.key with 
	  | Some k -> printf "  game.key length: %d\n" (List.length k)
	  | None -> ();
	Game.set ident game
  ) else
    printf "Warning: Game '%s' redefined.\n" ident

(*
  ident, title, pub, seens, inherits, target, key
*)
*)


(*
let games a = a
*)

(*
  let parse_error err_message =
    kprintf Optpp.sysError "parsing GAMEEXE.INI: %s at line %d" err_message !curr_line
*)

(* Read and parse the game definition file *)

%}

%token EOF
%token Co Cm Pt
%token AND BY END FOR FROM GAME INHERITS KEY NO NONE SEENS USING WITH
%token<string> IDENT STRING
%token<int> INTEGER

/*
%start parse
%type<unit> parse
*/

/*
%start parse
%type<[ `Game of (string, string, string, int, string list option, Game.target option, Game.subkey_t list option) ] list> parse
*/

/*
%start parse
%type<Game.t list> parse
%type<GameTypes.game list> parse
*/

%start parse
%type<GameTypes.game_t list> parse

%%

/*
parse:
  | EOF {}
  | games parse {}
*/

/*
parse:
  | EOF { [] }
  | games EOF { $1 }
*/

parse: games { $1 }

/*
parse:
  | games EOF 	{}
  | games		{}
*/

/*
games: / * empty * /		{ [] }
  | game				{ $1 :: [] }
  | game AND games		{ $1 :: $3 }
  | game Cm games		{ $1 :: $3 }
  | game games			{ $1 :: $2 }
*/

games: 
  | game				{ $1 :: [] }
  | game AND games		{ $1 :: $3 }
  | game Cm games		{ $1 :: $3 }
  | game games			{ $1 :: $2 }

/*
game: 
  GAME IDENT title pub seens inherits target key
    { process_game ($2, $3, $4, $5, $6, $7, $8); $2, $3, $4, $5, $6, $7, $8 }
*/

/*
game: 
  GAME IDENT title pub seens inherits target key
    { (* process_game ($2, $3, $4, $5, $6, $7, $8); *) `Game ($2, $3, $4, $5, $6, $7, $8) }
*/

game: 
  GAME IDENT title pub seens inherits target key
    { create_game ($2, $3, $4, $5, $6, $7, $8); }

/*
game: 
  GAME IDENT title pub seens inherits target key
    { `Game ($2, $3, $4, $5, $6, $7, $8) }
*/
  
/*
    { $2, $3, $4, $5, $6, $7, $8 }
*/

title:  /*empty*/ 		{ "" }
  | IDENT				{ $1 }
  | STRING				{ $1 }

pub:  /*empty*/ 		{ "" }
  | BY IDENT			{ $2 }
  | BY STRING			{ $2 }

seens:   /*empty*/ 		{ -1 }
  | WITH INTEGER SEENS	{ $2 }
  | WITH INTEGER		{ $2 }

inherits: /* empty */	{ None }
  | INHERITS NONE		{ None }
  | INHERITS ids		{ Some $2 }

ids:
  | IDENT				{ $1 :: [] }
  | IDENT Cm ids		{ $1 :: $3 }
  | IDENT ids			{ $1 :: $2 }

target: /* empty */				{ None }
  | FOR engine version compiler { Some ($2, $3, $4) }

engine: IDENT
  {
    match String.lowercase $1 with
      | "siglusengine" 
	  | "siglus" -> `Siglus
      | "reallive" -> `RealLive
      | "avg2000" -> `Avg2000
      | "kinetic" -> `Kinetic
      | s -> kprintf failwith "unknown game engine `%s'" s
  }

/*
version: / * empty * / 			{ None }
  | vstamp						{ Some $1 }
*/

version: /* empty */ 			{ `Any }
  | vstamp						{ `Version $1 }

/*
compiler: / * empty * / 		{ None }
  | INTEGER						{ Some $1 }
*/

compiler: /* empty */ 			{ `Any }
  | INTEGER						{ `Integer $1 }

vstamp:
  | INTEGER                                  { $1,  0,  0,  0 }
  | INTEGER Pt INTEGER                       { $1, $3,  0,  0 }
  | INTEGER Pt INTEGER Pt INTEGER            { $1, $3, $5,  0 }
  | INTEGER Pt INTEGER Pt INTEGER Pt INTEGER { $1, $3, $5, $7 }

/*
key: / * empty * /		{ `None }
  | USING subkeys		{ `Key $2 }
  | USING NONE			{ `None }
  | USING NO KEY		{ `None }
*/

key: /* empty */		{ None }
  | USING subkeys		{ Some $2 }
  | USING NONE			{ None }
  | USING NO KEY		{ None }

subkeys:
  | subkey				{ $1 :: [] }
  | subkey AND subkeys	{ $1 :: $3 }
  | subkey Cm subkeys	{ $1 :: $3 }

subkey: 
  | KEY FROM INTEGER FOR INTEGER Co bytes	{ GameTypes.subkey $3 $5 $7 }

/*
subkey: 
  | KEY FROM INTEGER FOR INTEGER Co bytes	{ { offset = $3; length = $5; data = $7 } }
*/

/*
bytes: 
  | INTEGER				{ (char_of_int $1) :: [] }
  | INTEGER bytes		{ (char_of_int $1) :: $2 }
*/

bytes: 
  | INTEGER				{ $1 :: [] }
  | INTEGER bytes		{ $1 :: $2 }
    