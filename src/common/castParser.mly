/*
   RLdev: cast (character translation) file parser
   Copyfight (K) 2010-2011 Richard 23

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
  open Cast
*)

(*
let curr_line = ref 1
*)

%}

%token EOF
%token Eq
%token<string> STRING

/*
%start parse
%type<unit> parse
*/

%start parse
%type<(string * string) list> parse

%%

/*
parse:
  | error
    { Printf.eprintf "Error: parsing character file: syntax error at line %d\n" (symbol_start_pos ()).Lexing.pos_lnum;
      exit 2 }
*/

/*
parse:
  | EOF {}
  | char_def parse {}
*/

parse:
  | EOF { [ ] }
  | char_def parse { $1 :: $2 }

/*
module_def:
  | STRING Eq STRING
     { Hashtbl.add cast $1 $2 }
  | STRING Eq
  | STRING
     {}
*/

/*
char_def:
  STRING Eq STRING
    { set $1 $3 }
  | STRING Eq
  | STRING
    {}
*/

char_def:
  STRING Eq STRING
    { ($1,$3) }
  | STRING Eq
    { ($1,"") }
  | STRING
    { ($1,"") }

/*
char_def:
  STRING Eq STRING
    { ($1,$3) }

no_def:
  | STRING Eq
    { [] }
  | STRING
    { [] }
*/