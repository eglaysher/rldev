(*
    RLdev: transformations of Unicode to the CP932 codespace
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

open ExtString
open Printf
open Encoding
open Text

let force_encode = ref false

(* Encode Simplified Chinese text.
   Fail on characters that cannot be represented in the GBK encoding. *)
let encode_kfc fail text =
  let b = Buffer.create 0 in
  Text.iter
    (fun ch ->
      let gbch = 
        if ch <= 0x7f 
        then ch 
        else try
          IMap.find ch Cp936.map.uni_to_db 
        with Not_found ->
          if !force_encode
          then 0x20
          else (fail ch; assert false)
      in
      assert (gbch >= 0 && gbch <= 0xf7fe);
      if gbch < 0x80 then
        Buffer.add_char b (char_of_int gbch)
      else 
        match gbch with
        (* Special cases: 
            1. Characters we want to encode directly as their CP932 equivalents;
            2. Characters this would collide with, which we encode in the place of other, non-existent characters *)
          | 0xa1b8 -> Buffer.add_string b "\x81\x75" | 0xbba2 -> Buffer.add_string b "\x81\x53"
          | 0xa1ba -> Buffer.add_string b "\x81\x77" | 0xdda2 -> Buffer.add_string b "\x82\x52"
          | 0xa3a8 -> Buffer.add_string b "\x81\x69" | 0xb5a2 -> Buffer.add_string b "\x82\x53"
      
        (* General case *)
          | _ -> let c1 = (gbch lsr 8) land 0xff - 0xa1
                 and c2 = gbch land 0xff - 0xa1 in
                 assert (c1 >= 0 && c2 >= 0);
                 let c1 = c1 * 2 + (c2 mod 2) + 0x40
                 and c2 = c2 / 2 + 0x81 in
                 let c1 = if c1 >= 0x7f then c1 + 1 else c1
                 and c2 = if c2 > 0x9f then c2 + 0x40 else c2 in
                 Buffer.add_char b (char_of_int c2);
                 Buffer.add_char b (char_of_int c1))
    text;
  Buffer.contents b

(* The inverse. *)
let decode_kfc text =
  let b = Buf.create 0 in
  let rec getc idx =
    if idx = String.length text then
      Buf.contents b
    else
      let c = text.[idx] in
      match c with
        | '\x81'..'\x9f' | '\xe0'..'\xef' | '\xf0'..'\xfc' when idx + 1 < String.length text
            -> let a1 = int_of_char c and a2 = int_of_char text.[idx + 1] in
               let c1, c2 =
                 match (a1 lsl 8) lor a2 with
                   | 0x8175 -> 0xa1 - 0xa1, 0xb8 - 0xa1
    	           | 0x8177 -> 0xa1 - 0xa1, 0xba - 0xa1
    	           | 0x8169 -> 0xa3 - 0xa1, 0xa8 - 0xa1
    	           | 0x8153 -> 0xbb - 0xa1, 0xa2 - 0xa1
    	           | 0x8252 -> 0xdd - 0xa1, 0xa2 - 0xa1
    	           | 0x8253 -> 0xb5 - 0xa1, 0xa2 - 0xa1
                   | _ -> let c2 = ((if a1 > 0xdf then a1 - 0x40 else a1) - 0x81) * 2
                          and c1 = (if a2 >= 0x80 then a2 - 1 else a2) - 0x40 in
                          c1 / 2, c2 + (c1 mod 2)
               in
               begin try
                 Buf.add_int b Cp936.map.db_to_uni.(c1).(c2);
                 getc (idx + 2)
               with Invalid_argument "index out of bounds" ->
                 ksprintf failwith "decode_kfc: could not decode %02x%02x (-> %02x%02x)" a1 a2 c1 c2
               end
        | '\x00'..'\x7f' -> Buf.add_char b c; getc (idx + 1)
        | _ -> failwith "decode_kfc: malformed string"
  in
  getc 0

(* As above, but for Western text *)
let encode_cp1252 fail text =
  let b = Buffer.create 0 in
  Text.iter
    (fun ch ->
      if ch >= 0x1ff00 && ch < 0x20000 then
        (* Special case for encoding name variables and exfont moji. *)
        Buffer.add_string b (IMap.find (ch - 0x10000) Cp932.uni_to_db)
      else
        let wc = 
          match ch with
            | 0x20ac -> 0x80 | 0x201a -> 0x82 | 0x0192 -> 0x83 | 0x201e -> 0x84
            | 0x2026 -> 0x85 | 0x2020 -> 0x86 | 0x2021 -> 0x87 | 0x02c6 -> 0x88 
            | 0x2030 -> 0x89 | 0x0160 -> 0x8a | 0x2039 -> 0x8b | 0x0152 -> 0x8c 
            | 0x017d -> 0x8e | 0x2018 -> 0x91 | 0x2019 -> 0x92 | 0x201c -> 0x93 
            | 0x201d -> 0x94 | 0x2022 -> 0x95 | 0x2013 -> 0x96 | 0x2014 -> 0x97 
            | 0x02dc -> 0x98 | 0x2122 -> 0x99 | 0x0161 -> 0x9a | 0x203a -> 0x9b 
            | 0x0153 -> 0x9c | 0x017e -> 0x9e | 0x0178 -> 0x9f | _      -> ch
        in
        let c =
          if wc < 0 || wc > 0xff 
          then if !force_encode then '\x20' else (fail ch; assert false)
          else char_of_int wc
        in
        match c with
          | '\x00' .. '\x7f' -> Buffer.add_char b c
          | '\x80' .. '\xbf' -> Buffer.add_char b '\x89'; Buffer.add_char b c
          | '\xc0' .. '\xfe' -> Buffer.add_char b (char_of_int (wc - 0x1f))
          | '\xff'           -> Buffer.add_string b "\x89\xc0")
    text;
  Buffer.contents b

let decode_cp1252 text =
  let b = Buf.create 0 in
  if String.fold_left
       (function 
          | `Encoded -> 
            (function
              | '\x80' .. '\xbf' as ch
                 -> Buf.add_int b 
                      (match int_of_char ch with 
                         | 0x80 -> 0x20ac | 0x82 -> 0x201a | 0x83 -> 0x0192 | 0x84 -> 0x201e 
                         | 0x85 -> 0x2026 | 0x86 -> 0x2020 | 0x87 -> 0x2021 | 0x88 -> 0x02c6 
                         | 0x89 -> 0x2030 | 0x8a -> 0x0160 | 0x8b -> 0x2039 | 0x8c -> 0x0152 
                         | 0x8e -> 0x017d | 0x91 -> 0x2018 | 0x92 -> 0x2019 | 0x93 -> 0x201c 
                         | 0x94 -> 0x201d | 0x95 -> 0x2022 | 0x96 -> 0x2013 | 0x97 -> 0x2014 
                         | 0x98 -> 0x02dc | 0x99 -> 0x2122 | 0x9a -> 0x0161 | 0x9b -> 0x203a 
                         | 0x9c -> 0x0153 | 0x9e -> 0x017e | 0x9f -> 0x0178 | c    -> c);
                    `Normal
              | '\xc0' -> Buf.add_char b '\xff'; `Normal
              | _ -> failwith "decode_cp1252: malformed string")
          | `Normal ->
            (function
              | '\x00' .. '\x7f' as ch -> Buf.add_char b ch; `Normal
              | '\x81' .. '\x82' as ch -> `Sjis ch
              | '\x89' -> `Encoded
              | '\xa1' .. '\xdf' as ch -> Buf.add_int b (int_of_char ch + 0x1f); `Normal
              | ch -> ksprintf failwith "decode_cp1252: malformed string (found 0x%02x)" (int_of_char ch))
          | `Sjis c1 -> 
            (fun c2 -> 
              try
                Buf.add_int b Cp932.db_to_uni.(int_of_char c1).(int_of_char c2); `Normal
              with _ ->
                (* no warnings, so just skip for now *)
                Buf.add_char b '_'; `Normal))
       `Normal text
  <> `Normal
  then failwith "decode_cp1252: malformed string"
  else Buf.contents b


(* For Korean text, we take advantage of the fact that CP949 is based on EUC-KR.  We can
   almost simply apply the standard EUC-JP <-> Shift_JIS transformation! 
   (In practice, we must modify the transformation to take into account the characters
   that need to be inserted as their actual CP932 equivalents: these are the same as
   for the Chinese encoding above.) *)
let encode_hangul fail text =
  let b = Buffer.create 0 in
  Text.iter
    (fun ch ->
      let hach = 
        if ch <= 0x7f 
        then ch 
        else try
          IMap.find ch Cp949.map.uni_to_db
        with Not_found ->
          if !force_encode
          then 0x20
          else (fail ch; assert false)
      in
      assert (hach >= 0 && hach <= 0xfdfe);
      if hach < 0x80 then
        Buffer.add_char b (char_of_int hach)
      else 
        match hach with
        (* Special cases: 
            1. Characters we want to encode directly as their CP932 equivalents;
            2. Characters this would collide with, which we encode in the place of other, non-existent characters *)
          | 0xa1b8 -> Buffer.add_string b "\x81\x75" | 0xa1d6 -> Buffer.add_string b "\xef\xfa"
          | 0xa1ba -> Buffer.add_string b "\x81\x77" | 0xa1d8 -> Buffer.add_string b "\xef\xfb"
          | 0xa3a8 -> Buffer.add_string b "\x81\x69" | 0xa1ca -> Buffer.add_string b "\xef\xfc"

        (* General case *)
          | _ -> let c1 = hach lsr 8 land 0xff - 0x80
                 and c2 = hach land 0xff - 0x80 in
                 let c1 = (c1 + 1) lsr 1 + if c1 < 0x5f then 0x70 else 0xb0
                 and c2 = c2 + if c1 mod 2 = 0 then 0x7e else if c2 > 0x5f then 0x20 else 0x1f in
                 Buffer.add_char b (char_of_int c1);
                 Buffer.add_char b (char_of_int c2))
    text;
  Buffer.contents b

let decode_hangul text =
  let b = Buf.create 0 in
  let rec getc idx =
    if idx = String.length text then
      Buf.contents b
    else
      let c = text.[idx] in
      match c with
        | '\x81'..'\x9f' | '\xe0'..'\xef' | '\xf0'..'\xfc' when idx + 1 < String.length text
            -> let a1 = int_of_char c and a2 = int_of_char text.[idx + 1] in
               let c1, c2 =
                 match (a1 lsl 8) lor a2 with
                   | 0x8175 -> 0x21, 0x38
    	           | 0x8177 -> 0x21, 0x3a
    	           | 0x8169 -> 0x23, 0x28
    	           | 0xeffa -> 0x21, 0x56
    	           | 0xeffb -> 0x21, 0x58
    	           | 0xeffc -> 0x21, 0x4a
                   | _ -> let c1 = ((if a1 < 0xe0 then a1 - 0x71 else a1 - 0xb1) lsl 1) + 1 
                          and c2 = if a2 > 0x7f then a2 - 1 else a2 in
                          if c2 >= 0x9e
                          then c1 + 1, c2 - 0x7d
                          else c1, c2 - 0x1f
               in
               begin try
                 Buf.add_int b Cp949.map.db_to_uni.(c1 - 0x21).(c2 - 0x21);
                 getc (idx + 2)
               with Invalid_argument "index out of bounds" ->
                 ksprintf failwith "decode_hangul: could not decode %02x%02x (-> %02x%02x)" a1 a2 c1 c2
               end
        | '\x00'..'\x7f' -> Buf.add_char b c; getc (idx + 1)
        | _ -> failwith "decode_hangul: malformed string"
  in
  getc 0


(* Output transformations *)
let outenc = ref `None

let describe () =
  match !outenc with
    | `None -> "no output transformation"
    | `Chinese -> "the `Chinese' transformation of GB2312"
    | `Western -> "the `Western' transformation of CP1252"
    | `Korean  -> "the `Korean' transformation of KS X 1001"

(* Encode text according to a given Unicode -> CP932-codespace transformation *)
let make_cp932_compatible fail text =
  match !outenc with
    | `Chinese -> encode_kfc fail text
    | `Western -> encode_cp1252 fail text
    | `Korean  -> encode_hangul fail text
    | `None -> assert false

(* Decode text again *)
let read_cp932_compatible fail text =
  try match !outenc with
    | `None -> Text.of_sjs text
    | `Chinese -> decode_kfc text
    | `Western -> decode_cp1252 text
    | `Korean  -> decode_hangul text
  with Failure s ->
    fail s;
    Text.empty

(* Try to load a supported transformation corresponding to an input string *)
let init =
  let loaded_936 = ref false 
  and loaded_949 = ref false in
  function
    | `None -> outenc := `None
    | `Western -> outenc := `Western
    | `Chinese -> outenc := `Chinese; if not !loaded_936 then (loaded_936 := true; Cp936.init ())
    | `Korean  -> outenc := `Korean; if not !loaded_949 then (loaded_949 := true; Cp949.init ())

let enc_of_string enc =
  match String.uppercase enc with
    | "" | "NONE" | "JAPANESE" | "JP" | "CP932" | "SHIFT_JIS" | "SJIS" | "SHIFT-JIS" | "SHIFTJIS" -> `None
    | "CHINESE" | "ZH" | "CN" | "CP936" | "GB2312" | "GBK" -> `Chinese
    | "WESTERN" | "ENGLISH" | "EN" | "CP1252" -> `Western
    | "KOREAN"  | "KO" | "KR" | "CP949" | "KSC" | "KSC5601" | "KSX1001" | "HANGUL" -> `Korean
    | _ -> ksprintf failwith "unknown output transformation `%s'" enc
  
let set_encoding enc = init (enc_of_string enc)

(* Convert a string to the format required for RealLive bytecode *)
let to_sjs_bytecode a =
  let b = Buffer.create 0 in
  Text.iter
    (fun c ->
      try
        Buffer.add_string b (IMap.find c Cp932.uni_to_db)
      with Not_found ->
        if !force_encode
        then Buffer.add_string b " "
        else raise (Text.Bad_char c))
    a;
  Buffer.contents b

let to_bytecode a =
  match !outenc with
    | `None -> to_sjs_bytecode a
    | enc -> make_cp932_compatible (fun c -> raise (Text.Bad_char c)) a
