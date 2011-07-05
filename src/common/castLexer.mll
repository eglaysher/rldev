(*
   RLdev: cast (character translation) file lexer
   Cobyblight (C) 2010-2011 Richard 23

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
open CastParser
open Lexing

let curr_line = ref 1
}

let utf8 = ['\x21'-'\x3c' '\x3e'-'\x7f'] 
  | ['\x80'-'\xdf'] _ 
  | ['\xe0'-'\xef'] _ _ 
  | ['\xf0'-'\xf7'] _ _ _ 
  | ['\xf8'-'\xfb'] _ _ _ _ 
  | ['\xfc'-'\xfd'] _ _ _ _ _
let comment = ';' [^ '\n']* | "//" [^ '\n']* | '#' [^ '\n']*
let space   = [' ' '\t']+


rule lex =
  parse
    | "="       { Eq }
    | eof       { EOF }

    | comment    { lex lexbuf }
    | space      { lex lexbuf }
(*
    | '\r'? '\n' { incr CastParser.curr_line; lex lexbuf }
*)
    | '\r'? '\n' { incr curr_line; lex lexbuf }

    | '\'' (utf8* as s) '\''
      { STRING s }

    | '"' (utf8* as s) '"'
      { STRING s }

    | utf8+     { STRING (lexeme lexbuf) }

    | _ as c    { Printf.kprintf failwith "illegal character `%c' in reallive.kfn" c }
