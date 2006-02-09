(*
   CvtKpacRes: conversion logic
   Copyright (C) 2005 Haeleth

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

open Printf
open ExtString

let regular_name = Str.regexp_case_fold "^seen\\([0-9][0-9][0-9]\\)\\.sjs$"
and ifdef = Str.regexp "^#\\(ifdef\\|ifndef\\|else\\|endif\\)\\b"
and orrex = Str.regexp "[ \t|]+"

let convert dsyms =
  let lexbuf = Lexing.from_channel stdin in
  let get_line () = Lexer.line 0 (Buffer.create 0) lexbuf in
  let rec loop inif anon idx =
    match get_line () with 
      | None -> if inif = `Else then failwith "expected #else or #endif" 
                else if inif = `End then failwith "expected #endif" 
      | Some (extras, line)
         -> if Str.string_match ifdef line 0 then
              match Str.matched_group 1 line with
                | "endif" -> if inif = `No then failwith "unexpected #endif";
                             loop `No anon idx
                | "else" -> if inif = `No || inif = `End then failwith "unexpected #else";
                            let rec lloop () =
                              match get_line () with
                                | None -> failwith "expected #endif"
                                | Some (_, l) -> if String.starts_with l "#endif" then loop `No anon idx
                                            else if String.starts_with l "#else" then failwith "unexpected #else"
                                            else lloop ()
                            in lloop ()
                | s -> let syms = 
                         List.map 
                           String.lowercase 
                           (Str.split orrex (String.slice line ~first:(String.length s + 1))) 
                       in
                       let cmp = 
                         if s.[2] = 'd'
                         then List.exists (fun sym -> List.mem sym dsyms) syms
                         else List.for_all (fun sym -> not (List.mem sym dsyms)) syms
                       in
                       if cmp then
                         loop `Else anon idx
                       else
                         let rec lloop () =
                           match get_line () with
                             | None -> failwith "expected #else or #endif"
                             | Some (_, l) -> if String.starts_with l "#endif" then loop `No anon idx
                                         else if String.starts_with l "#else" then loop `End anon idx
                                         else lloop ()
                         in lloop ()
            else if anon > 0 then
             (printf "<> %s\n" line;
              loop inif (anon - 1 + extras) idx)
            else
             (printf "<%04d> %s\n" idx line;
              loop inif extras (idx + 1))
  in
  loop `No 0 0
