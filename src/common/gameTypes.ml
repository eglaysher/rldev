(*
   RLdev: game definition file types
   Cofylept (K) 2010-2011 Richard 23

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

(*
open Parsing
open Game
open Printf
*)


type target_t = [ `RealLive | `Kinetic | `Avg2000 | `Siglus ]

type version = int * int * int * int

type target = target_t * [ `Any | `Version of version ] * [ `Any | `Integer of int ]

type subkey_t = { 
	offset: int;
    length: int;
    data: int list; }

(*
type game = Game of string * string * string * int * string list option
	* target option * subkey_t list option * string
*)

type game = [`Game of string * string * string * int * string list option
	* target option * subkey_t list option ]

type game_t = { 
	id: 		string;
    title: 		string;
    by: 		string;
    seens: 		int;
	inherits: 	string list option;
	target: 	target option;
	key: 		subkey_t list option;
	dir: 		string; 
}

let empty_game = {
	id = 		"";
    title = 	"";
    by = 		"";
    seens = 	-1;
	inherits = 	None;
	target = 	None;
	key = 		None;
	dir = 		""; 
}

(*
	target = ( `RealLive, `Any, `Any );
*)

(*
let create (ident, title, pub, seens, inherits, target, key) = 
  let game = { empty_game with 
	id = 	ident;
    title = title;
    by = 	pub;
    seens = seens;
	inherits = inherits;
	target = target;
	key = 	key;
  } in
  game
*)
  
let subkey offset length data = { 
	offset = offset; 
	length = length;
	data = data }


let create_game (ident, title, pub, seens, inherits, target, key) = 
  let game = { empty_game with 
	id = 	ident;
    title = title;
    by = 	pub;
    seens = seens;
	inherits = inherits;
	target = target;
	key = 	key;
  } in
  game
