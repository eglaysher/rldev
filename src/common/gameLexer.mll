(*
   RLdev: game definition file lexer
   KerbCrawled (K) 2010-2010

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


{
open Lexing
open Printf
open GameParser        (* The type token is defined in parser.mli *)

exception Eof 

let debug = ref 0

let line = ref 1
and lpos = ref 0 
  
let cpos lexbuf = 
  (Lexing.lexeme_start lexbuf) - !lpos
}
  

(*
let utf8 = 
    ['\x21'-'\x7f'] 
    | ['\x80'-'\xdf'] _ 
    | ['\xe0'-'\xef'] _ _ 
    | ['\xf0'-'\xf7'] _ _ _ 
    | ['\xf8'-'\xfb'] _ _ _ _ 
    | ['\xfc'-'\xfd'] _ _ _ _ _
*)

let utf8a = 
      ['\x80'-'\xdf'] _ 
    | ['\xe0'-'\xef'] _ _ 
    | ['\xf0'-'\xf7'] _ _ _ 
    | ['\xf8'-'\xfb'] _ _ _ _ 
    | ['\xfc'-'\xfd'] _ _ _ _ _
let utf8 = 
    ['\x21'-'\x7f'] | utf8a
let comment = ';' [^ '\n']* | "//" [^ '\n']* | '#' [^ '\n']*
let space   = [' ' '\t']+
      
rule lex =
  parse
    | ","       { Cm }
    | ":"       { Co }
    | "."       { Pt }
    | "game"      { GAME }
    | "by"      { BY }
    | "with"    { WITH }
    | "seens"   { SEENS }
    | "inherits" { INHERITS }
    | "for"     { FOR }
    | "using"   { USING }
    | "no"      { NO }
    | "key"     { KEY }
    | "from"    { FROM }
    | "and"     { AND }

(*
    | eof       { printf "eof\n"; EOF }
*)
    | eof       { (* if Debug then printf "eof\n"; *) EOF (* raise Eof *) }
    | comment    { (* if Debug then printf "line comment\n"; *) lex lexbuf }
    | space      { (* if Debug then printf "whitespace\n"; *) lex lexbuf }

    | '\r'? '\n' { lpos := Lexing.lexeme_start lexbuf; incr line; lex lexbuf }
                (* Lexing.new_line; lexbuf.new_line; *) 
            

    | '\'' ((['\x20'-'\x26' '\x28'-'\x7f'] | utf8a)* as s) '\''
        { if !debug > 0 then printf "single quoted STRING: '%s'\n" s; STRING s }

    | '"' ((['\x20' '\x21' '\x23'-'\x7f'] | utf8a)* as s) '"'
        { if !debug > 0 then printf "double quoted STRING: '%s'\n" s; STRING s }

    | ("0x" (['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f']) as i)
        { if !debug > 0 then printf "hex INTEGER: %s\n" i; INTEGER (int_of_string i) }

    | ['0'-'9']+ as i
        { if !debug > 0 then printf "INTEGER: %s\n" i; INTEGER (int_of_string i) }

    | ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '$' '?']*
        { if !debug > 0 then printf "IDENT: %s\n" (Lexing.lexeme lexbuf); IDENT (Lexing.lexeme lexbuf) }

(*
    | utf8+
        { if Debug then printf "STRING: '%s'\n" (Lexing.lexeme lexbuf); STRING (lexeme lexbuf) }
*)

    | _ as c
        { printf "illegal character `%c' in reallive.kfn" c;
            Printf.kprintf failwith "illegal character `%c' in reallive.kfn" c }
(*
{ let read_file ?(parsefun) ?(lexfun = lex) filename verbose = 
  GameParser.read_file parsefun lexfun filename verbose }
*)