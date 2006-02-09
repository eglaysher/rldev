(*
   CvtKpacRes: Kpac-format lexer
   Copyright (C) 2006 Haeleth

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
  open Printf
  open Lexing
}

let sjs1 = ['\x81'-'\x9f' '\xe0'-'\xef' '\xf0'-'\xfc']
let sjs2 = ['\x40'-'\x7e' '\x80'-'\xfc']
let sp = [' ' '\t']*

rule line xt b =
  parse
   (* End of line *)
    | eof 
        { if Buffer.length b = 0 then None else Some (xt, Buffer.contents b) }
    | sp '\r'? '\n' 
    | sp "//" [^ '\n']* '\n' 
        { if Buffer.length b = 0 then line xt b lexbuf else Some (xt, Buffer.contents b) }

    | "\\0" { Some (xt, "") }

   (* Escaped line ends, comments, and whitespace *)
    | '\\' sp '\r'? '\n' { line xt b lexbuf }
    | "{-" { comment lexbuf; line xt b lexbuf }
    | sp+ { if Buffer.length b > 0 then Buffer.add_string b (String.make (String.length (lexeme lexbuf)) ' '); 
            line xt b lexbuf }
    
   (* Features needing subtle alteration *)
    | "\\N" 
        { bprintf b "\\r"; 
          line xt b lexbuf }
    | "\\e" sp ('{' [^ '}']* '}' as arg) 
        { bprintf b "\\em%s" arg; 
          line xt b lexbuf }
    | "\\w" sp ('{' [^ '}']* '}' as arg) 
        { bprintf b "\\wait%s" arg; 
          line xt b lexbuf }
    | '\\' (['s' 'i' 'x' 'I' 'X'] as t) sp (['0'-'9']+ as len) sp '{'
        { let t =
            match t with
              | 's' | 'i' -> t
              | _ -> kprintf Optpp.sysWarning "unsupported code \\%c{} - replacing with \\i{}" t; 'i'
          in
          bprintf b "\\%c%s%s{" t (if len <> "" then ":" else "") len;
          simparg true b lexbuf;
          line xt b lexbuf }

   (* Unsupported features (including incompatible ones) *)
    | "\\b" | "\\r" { line xt b lexbuf }
    | '\\' ['g' 'G'] sp '{'
        { simparg false b lexbuf;
          skip_gloss_part_2 lexbuf;
          line xt b lexbuf }

   (* Other features needing special treatment *)
    | "\\a" (sp '{' sp (['0'-'9']+ as len) sp '}')?
        { let c = try int_of_string (Option.get len) with _ -> 1 in
          for i = 1 to c do Buffer.add_string b "\\a" done;
          line (xt + c) b lexbuf }
    | "\\f" (* Hardwired for `Kanon' *)
        { bprintf b "\\f";
          line (xt + 1) b lexbuf }
    | "\\f" sp '{'
        { bprintf b "\\f{";
          let nb = Buffer.create 0 in
          simparg true nb lexbuf;
          Buffer.add_buffer b nb;
          let strs = Str.split (Str.regexp "[]:();,.[ \t\r\n+=*/%&|^!~{}-]+") (Buffer.contents nb) in
          let c = List.fold_left (fun acc s -> if s = "$s" then acc + 1 else acc) 0 strs in
          let c = if c = 0 then 0 else c - 1 in
          line (xt + c) b lexbuf }
   
   (* Stuff needing escaping *)
    | '<' { Buffer.add_string b "\\<"; line xt b lexbuf }
   
   (* Anything else *)    
    | sjs1 sjs2 | _
        { Buffer.add_string b (lexeme lexbuf); line xt b lexbuf }
      
and comment =
  parse
    | "-}"           { }
    | "-" | [^ '-']* { comment lexbuf }

and simparg withclose b =
  parse
    | '}' { if withclose then Buffer.add_char b '}' }
    | "{-" { comment lexbuf; simparg withclose b lexbuf }
    | "\\}" 
    | "\\\\"
    | sjs1 sjs2 
    | _ { Buffer.add_string b (lexeme lexbuf); simparg withclose b lexbuf }

and skip_gloss_part_2 =
  parse
    | sp "=" sp '{'
    | sp "->" sp '{'
        { simparg false (Buffer.create 0) lexbuf }
