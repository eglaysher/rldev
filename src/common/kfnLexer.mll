(*
   RLdev: function definition file lexer
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

{ open KfnParser
  open Lexing }

rule lex =
  parse
    | "="       { Eq }
    | "<"       { Lt }
    | ">"       { Gt }
    | "("       { Lp }
    | ")"       { Rp }
    | "{"       { Lbr }
    | "}"       { Rbr }
    | "?"       { Qu }
    | ","       { Cm }
    | "*"       { St }
    | "+"       { Pl }
    | ":"       { Co }
    | "."       { Pt }
    | "#"       { Ha }
    | "-"       { Hy }
    | "module"  { MODULE }
    | "fun"     { FUN }
    | "ver"     { VER }
    | "end"     { END }
    | "int"     { INT }
    | "intC"    { INTC }
    | "intV"    { INTV }
    | "str"     { STR }
    | "strC"    { STRC }
    | "strV"    { STRV }
    | "res"     { RES }
    | "special" { SPECIAL }
    | eof       { EOF }

    | [' ' '\t' '\r']+
      { lex lexbuf }

    | "//" [^ '\n']* '\n' | '\n'
      { lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1 };
        lex lexbuf }

    | ['0'-'9']+ as i
        { INTEGER (int_of_string i) }

    | '$' (['0'-'9' 'A'-'F' 'a'-'f']+ as i)
        { INTEGER (int_of_string ("0x" ^ i)) }

    | ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '$' '?']*
        { IDENT (lexeme lexbuf) }

    | '\'' ([^ '\'']* as s) '\''
        { STRING s }

    | _ as c
        { Printf.kprintf failwith "illegal character `%c' in reallive.kfn" c }
