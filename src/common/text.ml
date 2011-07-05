(*
   RLdev: unicode handling
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

open Optpp
open Printf
open Encoding
open Cast

(*
type text_enc =
  [ `Sjs | `Euc | `Utf8 ]
*)
 
let badc : (int, int) Hashtbl.t = Hashtbl.create 0

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

(*
App.bchars = Hashtbl.create () in
*)

(*
Hashtbl.add Add.badc (try 
  Hashtbl.find App.badc c
  with Not_found -> 0) + 1;
*)

let sq1 = ref "'"
and sq2 = ref "'"
and dq1 = ref "\""
and dq2 = ref "\""
and ch1 = ref "<<"
and ch2 = ref ">>"
and hel = ref "..."

(*
let fancy ?(state = true) = 
*)

let fancy state = 
  if state = true then (
    sq1 := "'";
    sq2 := "'";
    dq1 := "\"";
    dq2 := "\"";
    ch1 := "<<";
    ch2 := ">>";
    hel := "..."
  ) else (
    sq1 := "\xE2\x80\x98";
    sq2 := "\xE2\x80\x99";
    dq1 := "\xE2\x80\x9C";
    dq2 := "\xE2\x80\x9D";
    ch1 := "\xC2«";
    ch2 := "\xC2»";
    hel := "…"
  )

exception Bad_char of int

type t = int array

let append = Array.append
and length = Array.length
and empty = [||]
and iter = Array.iter
and map = Array.map

module Buf =
  struct
    type buf = int DynArray.t
    let create = DynArray.make
    and add_int = DynArray.add
    and add_char b c = DynArray.add b (int_of_char c)
    and add_string b = Array.iter (DynArray.add b)
    let add_array = add_string
    and length = DynArray.length
    and contents = DynArray.to_array
    and clear = DynArray.clear
  end

let to_arr x = x
and of_arr x = x

let of_char c = [| c |]

let to_sjs a =
  let b = Buffer.create 0 in
  Array.iter
    (fun c ->
      try
        Buffer.add_string b (IMap.find c Cp932.uni_to_db)
      with
        Not_found -> bprintf b "\\u{$%04x}" c)
    a;
  Buffer.contents b

let of_sjs s =
  let b = Buf.create 0 in
  let rec getc idx =
    if idx = String.length s then
      Buf.contents b
    else
      let c = s.[idx] in
      match c with
        | '\x81'..'\x9f' | '\xe0'..'\xef' | '\xf0'..'\xfc' when idx + 1 < String.length s
         -> Buf.add_int b (Cp932.db_to_uni.(int_of_char c - 0x81).(int_of_char s.[idx + 1] - 0x40));
            getc (idx + 2)
        | _
         -> Buf.add_int b (Cp932.sb_to_uni.(int_of_char c));
            getc (idx + 1)
  in
  getc 0

let to_utf8 a =
  let b = Buffer.create 0 in
  let mask = 0b111111 in
  Array.iter
    (fun i ->
      if i < 0 || i >= 0x4000000 then (
        Buffer.add_char b (Char.chr (0xfc + (i lsr 30)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 24) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 18) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (i land mask)))
      ) else if i <= 0x7f then
        Buffer.add_char b (Char.unsafe_chr i)
      else if i <= 0x7ff then (
        Buffer.add_char b (Char.unsafe_chr (0xc0 lor (i lsr 6)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (i land mask)))
      ) else if i <= 0xffff then (
        Buffer.add_char b (Char.unsafe_chr (0xe0 lor (i lsr 12)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (i land mask)))
      ) else if i <= 0x1fffff then (
        Buffer.add_char b (Char.unsafe_chr (0xf0 + (i lsr 18)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (i land mask)))
      ) else (
        Buffer.add_char b (Char.unsafe_chr (0xf8 + (i lsr 24)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 18) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (i land mask)))
      ))
    a;
  Buffer.contents b

let sjs_to_utf8_prep s =
  let a = (of_sjs s) in
  let b = Buffer.create 0 in
  let mask = 0b111111 in
  Array.iteri
    (fun i c ->
      (* printf "c: %02x\n" i; *)
      if c < 0 || c >= 0x4000000 then (
        Buffer.add_char b (Char.chr (0xfc + (c lsr 30)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 24) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 18) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))

      ) else if c <= 0x7f then (
        if c = 0x7d then (
        let p =
          let rec findch s i = 
            (* printf "findch('%s', %d)\n" s i; *)
            if i > 0 then
              if String.sub s i 2 = "\\{" then i + 2
              else findch s (i - 1)
            else -1
          in
          findch (Buffer.contents b) (Buffer.length b - 2)
        in
        (* printf "p: %d\n" p; *)
        if p > -1 then (
          (* printf "s: %s\n" (Buffer.sub b p (Buffer.length b - p)); *)
          match Cast.get (Buffer.sub b p (Buffer.length b - p)) with
            | Some c2 -> let s = (Buffer.sub b 0 p) ^ c2 in
                Buffer.reset b;
                Buffer.add_string b s
            | None -> ()
        );
        Buffer.add_char b (Char.unsafe_chr c);
        ignore(if i + 1 < Array.length a then 
            if a.(i + 1) <> 0x3000 && a.(i + 1) <> 0x20 then
                Buffer.add_string b " "
        )
        
        ) else (
        Buffer.add_char b (Char.unsafe_chr c)
        );
(*
      ) else if c <= 0x7f then (
        Buffer.add_char b (Char.unsafe_chr i)
*)
      ) else if c <= 0x7ff then (
        Buffer.add_char b (Char.unsafe_chr (0xc0 lor (c lsr 6)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
      ) else if c <= 0xffff then (
        if c = 0x8163 then Buffer.add_string b !hel
        else if c = 0x3001 then Buffer.add_string b ", "    (* 0x8141 *)
        else if c = 0x3002 then Buffer.add_string b "."        (* 0x8142 *)
        else if c > 0xFF00 && c < 0xFF5F then 
            Buffer.add_char b (Char.unsafe_chr((c land 0xFF) + 0x20))
        else if c = 0x3008 then Buffer.add_string b "<"        (* 0x8171 *)
        else if c = 0x3009 then Buffer.add_string b ">"        (* 0x8172 *)
        else if c = 0x300A then Buffer.add_string b !ch1    (* 0x8173*)
        else if c = 0x300B then Buffer.add_string b !ch2    (* 0x8174 *)
        else if c = 0x300C then Buffer.add_string b !dq1    (* 0x8175 *)
        else if c = 0x300D then Buffer.add_string b !dq2    (* 0x8176 *)
        else if c = 0x300E then Buffer.add_string b !sq1    (* 0x8177 *)
        else if c = 0x300F then Buffer.add_string b !sq2    (* 0x8178 *)
        else (
          Buffer.add_char b (Char.unsafe_chr (0xe0 lor (c lsr 12)));
          Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
          Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
        )
      ) else if c <= 0x1fffff then (
        Buffer.add_char b (Char.unsafe_chr (0xf0 + (c lsr 18)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
      ) else (
        Buffer.add_char b (Char.unsafe_chr (0xf8 + (c lsr 24)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 18) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
      ))
    a;
(*
    (of_sjs s);
*)
(*
  let a = Buffer.create 0 in
  String.iter (fun c -> bprintf a "%02X" (int_of_char c)) (Buffer.contents b);
  printf "hex: %s\n" (Buffer.contents a);
  printf "buf: %s\n\n" (Buffer.contents b);
*)
  Buffer.contents b

(*
let sjs_to_utf8_prep s =
  let a = (of_sjs s) in
  let b = Buffer.create 0 in
  let mask = 0b111111 in
  Array.iteri
    (fun i c ->
      (* printf "c: %02x\n" i; *)
      if c < 0 || c >= 0x4000000 then (
        Buffer.add_char b (Char.chr (0xfc + (c lsr 30)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 24) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 18) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))

      ) else if c <= 0x7f then (
        if c = 0x7d then (
(*
      ) else if i = 0x7d then (
*)
        let p =
          let rec findch s i = 
            (* printf "findch('%s', %d)\n" s i; *)
            if i > 0 then
              if String.sub s i 2 = "\\{" then i + 2
              else findch s (i - 1)
            else -1
          in
          findch (Buffer.contents b) (Buffer.length b - 2)
        in
        (* printf "p: %d\n" p; *)
        if p > -1 then (
          (* printf "s: %s\n" (Buffer.sub b p (Buffer.length b - p)); *)
            
(*
          let c2 = Cast.find (Buffer.sub b p (Buffer.length b - p)) in
          if c2 <> "" then
            let s = (Buffer.sub b 0 (p - 1)) ^ c2 in
            Buffer.reset b;
            Buffer.add_string b s
*)

          match Cast.get (Buffer.sub b p (Buffer.length b - p)) with
            | Some c2 -> let s = (Buffer.sub b 0 p) ^ c2 in
                Buffer.reset b;
                Buffer.add_string b s
            | None -> ()
        );
        Buffer.add_char b (Char.unsafe_chr c);
    (*
        ignore (if i < Array.length a && not(a.[i + 1] = 0x3000 || a.[i + 1] = 0x20) then
            Buffer.add_char b ' ')
    *)
        ignore(if i + 1 < Array.length a then 
            if a.(i + 1) <> 0x3000 && a.(i + 1) <> 0x20 then
                Buffer.add_string b " "
        )
        
        ) else (
        Buffer.add_char b (Char.unsafe_chr c)
        );
(*
      ) else if c <= 0x7f then (
        Buffer.add_char b (Char.unsafe_chr i)
*)
      ) else if c <= 0x7ff then (
        Buffer.add_char b (Char.unsafe_chr (0xc0 lor (c lsr 6)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
      ) else if c <= 0xffff then (
(*
        if c = 0x8163 then Buffer.add_string b "..."
*)
        if c = 0x8163 then Buffer.add_string b "…"
(*
        else if c = 0x8142 then Buffer.add_string b "."
*)
        else if c = 0x3001 then Buffer.add_string b ", "    (* 0x8141 *)
        else if c = 0x3002 then Buffer.add_string b "."        (* 0x8142 *)
        else if c > 0xFF00 && c < 0xFF5F then 
          Buffer.add_char b (Char.unsafe_chr((c land 0xFF) + 0x20))
(*
        else if c = 0x8148 then Buffer.add_string b "?"        (* 0x8148 *)
        else if c = 0x8149 then Buffer.add_string b "!"        (* 0x8149 *)
*)
        else if c = 0x3008 then Buffer.add_string b "<"        (* 0x8171 *)
        else if c = 0x3009 then Buffer.add_string b ">"        (* 0x8172 *)
(*
        else if c = 0x300A then Buffer.add_string b "<<"    (* 0x8175 *)
        else if c = 0x300B then Buffer.add_string b ">>"    (* 0x8175 *)
*)
(*
        else if c = 0x300A then Buffer.add_string b "«"        (* 0x8173*)
        else if c = 0x300B then Buffer.add_string b "»"        (* 0x8174 *)
*)

(*
        else if c = 0x300A || c = 0x300B then (                (* 0x8173 0x8174 *)
          Buffer.add_char b (Char.unsafe_chr (0xc2));
(*
          Buffer.add_char b (Char.unsafe_chr (if c = 0x300A then 0xAB else 0xBB))
*)
          Buffer.add_string b (if c = 0x300A then "«" else "»")
        )
*)
        else if c = 0x300A then Buffer.add_string b "\xC2«"        (* 0x8173*)
        else if c = 0x300B then Buffer.add_string b "\xC2»"        (* 0x8174 *)
(*
        else if c = 0x300C then Buffer.add_string b "\""    (* 0x8175 *)
        else if c = 0x300D then Buffer.add_string b "\""    (* 0x8176 *)
        else if c = 0x300E then Buffer.add_string b "'"        (* 0x8177 *)
        else if c = 0x300F then Buffer.add_string b "'"        (* 0x8178 *)
*)
        else if c = 0x300C then Buffer.add_string b "\xE2\x80\x9C"    (* 0x8175 *)
        else if c = 0x300D then Buffer.add_string b "\xE2\x80\x9D"    (* 0x8176 *)
        else if c = 0x300E then Buffer.add_string b "\xE2\x80\x98"    (* 0x8177 *)
        else if c = 0x300F then Buffer.add_string b "\xE2\x80\x99"    (* 0x8178 *)

(*
        else if c = 0x300C || c = 0x300D then (                (* 0x8175 “ 0x8176 ” *)
          Buffer.add_char b (Char.unsafe_chr (0xe2));
          Buffer.add_char b (Char.unsafe_chr (0x80));
          Buffer.add_char b (Char.unsafe_chr (0x9C + (c - 0x300C)));
        )
        else if c = 0x300E || c == 0x300F then (            (* 0x8176 ‘ 0x8177 ’ *)
          Buffer.add_char b (Char.unsafe_chr (0xe2));
          Buffer.add_char b (Char.unsafe_chr (0x80));
          Buffer.add_char b (Char.unsafe_chr (0x98 + (c - 0x300E)));
        )
*)
(*
        else if c = 0x300D then Buffer.add_string b "“"        (* 0x8176 *)
        else if c = 0x300D then Buffer.add_string b "”"        (* 0x8176 *)
        else if c = 0x300E then Buffer.add_string b "‘"        (* 0x8177 *)
        else if c = 0x300F then Buffer.add_string b "’"        (* 0x8178 *)
*)
(*
        else if c = 0xFF5E then Buffer.add_string b "~"        (* 0x8160 *)
*)
        else (
          Buffer.add_char b (Char.unsafe_chr (0xe0 lor (c lsr 12)));
          Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
          Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
        )
      ) else if c <= 0x1fffff then (
        Buffer.add_char b (Char.unsafe_chr (0xf0 + (c lsr 18)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
      ) else (
        Buffer.add_char b (Char.unsafe_chr (0xf8 + (c lsr 24)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 18) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 12) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor ((c lsr 6) land mask)));
        Buffer.add_char b (Char.unsafe_chr (0x80 lor (c land mask)))
      ))
    a;
(*
    (of_sjs s);
*)
(*
  let a = Buffer.create 0 in
  String.iter (fun c -> bprintf a "%02X" (int_of_char c)) (Buffer.contents b);
  printf "hex: %s\n" (Buffer.contents a);
  printf "buf: %s\n\n" (Buffer.contents b);
*)
  Buffer.contents b
*)

(*
let sjs_to_tr_prep enc s =
  match enc_type enc with
    | `Sjs -> s
    | `Euc -> sjs_to_euc s
    | `Utf8 -> to_tr_prep (of_sjs s)
    | `Other -> ksprintf sysError "unknown encoding `%s' (valid encodings are Shift_JIS, EUC-JP, UTF-8)" enc
*)

let sjs_to_enc enc s =
  match enc_type enc with
    | `Sjs -> s
    | `Euc -> sjs_to_euc s
    | `Utf8 -> to_utf8 (of_sjs s)
    | `Other -> ksprintf sysError "unknown encoding `%s' (valid encodings are Shift_JIS, EUC-JP, UTF-8)" enc

let norm =
  Array.map
    (fun ch ->
      if ch >= int_of_char 'A' && ch <= int_of_char 'Z'
      then ch + 32
      else ch)

let getc_sjs chan =
  try
    let c1 = IO.read_byte chan in
    if (c1 >= 0x81 && c1 <= 0x9f) || (c1 >= 0xe0 && c1 <= 0xef) || (c1 >= 0xf0 && c1 <= 0xfc) then
      Cp932.db_to_uni.(c1 - 0x81).(IO.read_byte chan - 0x40)
    else
      Cp932.sb_to_uni.(c1)
  with
    Invalid_argument "index out of bounds" -> sysError "invalid character in input: check encoding settings"

let getc_euc chan =
  try
    let c1 = IO.read_byte chan in
    if c1 < 0x7f then
      c1
    else
      let c1 = c1 - 0x80
      and c2 = IO.read_byte chan - 0x80 in
      Cp932.db_to_uni
           .((c1 + 1) lsr 1 + if c1 < 95 then -17 else 47)
           .(c2 + if c1 mod 2 = 0 then 62 else if c2 > 95 then -32 else -33)
  with
    Invalid_argument "index out of bounds" -> sysError "invalid character in input: check encoding settings"

let getc_utf8 chan =
  let c1 = IO.read_byte chan in
  if c1 <= 0x7f then
    c1
  else let c2 = IO.read_byte chan in
  if c1 <= 0xdf then
    (c1 - 0xc0) lsl 6 lor (c2 land 0x7f)
  else let c3 = IO.read_byte chan in
  if c1 <= 0xef then
    ((c1 - 0xe0) lsl 6 lor (c2 land 0x7f)) lsl 6 lor (c3 land 0x7f)
  else let c4 = IO.read_byte chan in
  if c1 <= 0xf7 then
    (((c1 - 0xf0) lsl 6 lor (c2 land 0x7f)) lsl 6 lor (c3 land 0x7f)) lsl 6 lor (c4 land 0x7f)
  else let c5 = IO.read_byte chan in
  if c1 <= 0xfb then
    ((((c1 - 0xf8) lsl 6 lor (c2 land 0x7f)) lsl 6 lor (c3 land 0x7f)) lsl 6 lor (c4 land 0x7f)) lsl 6 lor (c5 land 0x7f)
  else let c6 = IO.read_byte chan in
  if c1 <= 0xfd then
    (((((c1 - 0xfc) lsl 6 lor (c2 land 0x7f)) lsl 6 lor (c3 land 0x7f)) lsl 6 lor (c4 land 0x7f)) lsl 6 lor (c5 land 0x7f)) lsl 6 lor (c6 land 0x7f)
  else
    sysError "invalid character in input: check encoding settings"

let get_getc enc =
  match enc_type enc with
    | `Euc -> getc_euc
    | `Sjs -> getc_sjs
    | `Utf8 -> getc_utf8
    | `Other -> ksprintf sysError "unknown encoding `%s' (valid encodings are Shift_JIS, EUC-JP, UTF-8)" enc

let ustream enc ic =
  let chan = IO.input_channel ic
  and f = get_getc enc in
  let strm = Stream.from (fun _ -> try Some (f chan) with IO.No_more_input -> None) in
  begin match Stream.peek strm with
    | Some (0xfffe | 0xfeff) -> Stream.junk strm (* Ignore BOM *)
    | _ -> ()
  end;
  strm

let to_string enc t = 
  match enc_type enc with
    | `Sjs -> to_sjs t
    | `Euc -> sjs_to_euc (to_sjs t)
    | `Utf8 -> to_utf8 t
    | `Other -> ksprintf sysError "unknown encoding `%s' (valid encodings are Shift_JIS, EUC-JP, UTF-8)" enc

let of_string enc s =
  let chan = IO.input_string s
  and f = get_getc enc
  and b = Buf.create 0 in
  try
    while true do Buf.add_int b (f chan) done;
    assert false
  with _ ->
    Buf.contents b

let to_err = to_string Config.default_encoding
and of_err = of_string Config.default_encoding

let sjs_to_err = sjs_to_enc Config.default_encoding

let ident =
  let memo = Hashtbl.create 0 in
  fun s ->
    try
      Hashtbl.find memo s
    with Not_found ->
      let rv = of_sjs (String.lowercase s) in
      Hashtbl.add memo s rv;
      rv
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