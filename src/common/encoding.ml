(*
    RLdev: character encoding utility functions
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

(* Type definitions *)

type unicode_mapping =
  { mutable db_to_uni : int array array;
    mutable uni_to_db : int IMap.t; }

type generic_mapping =
  { mutable encode : int IMap.t;
    mutable decode : int IMap.t; }

(*
let badc = (Hashtbl.create 0)
let badc : (char, int) Hashtbl.t = Hashtbl.create 0
*)

(*
let badc : (int, int) Hashtbl.t = Hashtbl.create 0
*)

let enc_type =
  function
    | "SHIFTJIS" | "SHIFT_JIS" | "SHIFT-JIS" 
    | "SJS" | "SJIS" | "CP932" -> `Sjs
    | "EUC-JP" | "EUC" | "EUC_JP" -> `Euc
    | "UTF8" | "UTF-8" -> `Utf8
    | _ -> `Other

(*
let enclog obj log =    
  if Hashtbl.length badc > 0 then 
    if log = "" then let log = "mapenc.kh" in
    let oc = open_out "mapenc.kh.txt" in
    let sz = out_channel_length oc in
    
    Printf.printf "obj: %s\n" obj;
    Printf.printf "log: %s\n" log;
    
    if sz = 0 then (
      output_string oc "# Specific Encoding Non-Conformity Report\n";
      output_string oc "# Generic Remap Table for Compliant Output\n";
      Printf.fprintf oc "# Presented in glorious %s by %s %1.2f\n"
        (String.lowercase !App.enc) App.app.name App.app.version;
        (*
        (String.lowercase obj.app.enc) app.name app.version;

        *)
      output_string oc ("[ ] Customize the table as desired, rename to \n" ^
        "\"mapenc.kh\" and move to the script or project directory.\n\n")
  ) else (
    seek_out oc sz
  );
  
(*
  let bada = DynArray () in
  Hashtbl.iter (fun ch -> DynArray.add bada ch)
*)
  
  let bada = Array.create () in
  Hashtbl.iter (fun ch -> 
    Array.append bada ch) 
    bata;
    
  Array.sort bada;
  
  Array.iter (fun ch -> fprintf oc 
    "U+%04x '%c' => ""    // instances: %d\n"
    ch ch (Hashtbl.find badc ch)) bada;
      
  close_out oc;
  
  Hashtbl.length badc
else
  Unix.unlink "./enclog.kn.txt";
  0
*)  
(*
  try
    Unix.unlink "./enclog.kn.txt"
  with _ -> ()
*)

(*
type label = location * string * Text.t
*)

(*
type encoding =
  [ `Sjs | `Euc | `Utf8 ]
*)


(*
val string_of_encoding : bool -> string
Return the string representation of a boolean.
*)

let string_of_encoding =
  function
    | `Sjs -> "Shift-JIS"
    | `Euc -> "EUC-JP"
    | `Utf8 -> "Utf-8"
    | _ -> "Other"

(*
val bool_of_string : string -> bool
Convert the given string to a boolean. 
Raise Invalid_argument "bool_of_string" 
if the string is not "true" or "false".
*)

let encoding_of_string s =
  match String.uppercase s with
    | "SHIFTJIS" | "SHIFT_JIS" | "SHIFT-JIS" 
    | "SJS" | "SJIS" | "CP932" -> `Sjs
    | "EUC-JP" | "EUC" | "EUC_JP" -> `Euc
    | "UTF8" | "UTF-8" -> `Utf8
    | _ -> raise (Invalid_argument "encoding_of_string")
(*
    | _ -> `Other
*)



let sjs_to_euc s =
  let b = Buffer.create 0 in
  let rec getc idx =
    if idx = String.length s then
      Buffer.contents b
    else
      let c = s.[idx] in
      let c1 =
        match c with
          | '\x00'..'\x7f' -> Buffer.add_char b c; -1
          | '\xa0'..'\xdf' -> Printf.bprintf b "\x8e%c" c; -1
          | '\x81'..'\x9f' -> int_of_char c - 0x71
          | '\xe0'..'\xef'
          | '\xf0'..'\xfc' -> int_of_char c - 0xb1
          | _ -> Optpp.sysError "invalid character in input"
      in
      if c1 = -1 then
        getc (idx + 1)
      else
        let c1 = (c1 lsl 1) + 1 in
        if idx + 1 = String.length s then Optpp.sysError "invalid character in input";
        let c2 = int_of_char s.[idx + 1] in
        let c2 = if c2 > 0x7f then c2 - 1 else c2 in
        let c1', c2' =
          if c2 >= 0x9e
          then (c1 + 1) lor 0x80, (c2 - 0x7d) lor 0x80
          else c1 lor 0x80, (c2 - 0x1f) lor 0x80
        in
        Buffer.add_char b (char_of_int c1');
        Buffer.add_char b (char_of_int c2');
        getc (idx + 2)
  in
  getc 0
  