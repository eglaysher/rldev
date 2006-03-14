(*
    Rlc: text output compilation (rlBabel edition)
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
(*syntax camlp4o *)
(*ppopt pa_macro.cmo ./pa_matches.cmo *)

open Printf
open KeTypes
open KeAst
open Codegen

let token_name_left   = Text.of_arr [| 0x01 |]
and token_name_right  = Text.of_arr [| 0x02 |]
and token_break       = Text.of_arr [| 0x03 |]
and token_setindent   = Text.of_arr [| 0x04 |]
and token_clearindent = Text.of_arr [| 0x05 |]
(* 6, 7 are exfont markers *)
and token_quote       = Text.of_arr [| 0x08 |]
and token_emphasis    = Text.of_arr [| 0x09 |]
and token_regular     = Text.of_arr [| 0x0a |]

let compile addstrs loc text =
  let b = DynArray.create () in
  let ignore_one_space = ref false in
  let appending = ref false in
  let flush display =
    if display then 
      if !appending then (
        appending := false;
        Meta.call "__vwf_TextoutAppend" [`Str (nowhere, b)];
        Meta.call "__vwf_TextoutDisplay" []
      )
      else Meta.call "__vwf_TextoutDisplay" [`Str (nowhere, b)]
    else 
      if !appending then Meta.call "__vwf_TextoutAppend" [`Str (nowhere, b)]
      else (
        appending := true;
        Meta.call "__vwf_TextoutStart" [`Str (nowhere, b)]
      );
    DynArray.clear b
  in
  Output.add_kidoku loc;
  let rec parse elt =
    if !ignore_one_space && not (elt matches `Space _) then ignore_one_space := false;
    match elt with
      | `EOS | `Delete _ | `Rewrite _ -> assert false
      | `Add elt -> Queue.add elt addstrs
      | `Text _
      | `Hyphen _  (* we may want special handling for hyphens? *)
      | `LLentic _ 
      | `RLentic _ -> DynArray.add b elt
      | `DQuote l -> DynArray.add b (`Text (l, `Sbcs, token_quote))
      | `Space (l, i) -> let j = if i > 0 && !ignore_one_space then (ignore_one_space := false; i - 1) else i in
                         DynArray.add b (`Space (l, j))
      | `RCur l -> DynArray.add b (`Text (l, `Sbcs, token_name_right));
                   ignore_one_space := true (* ...leave this alone for now... *)
      | `Asterisk _ 
      | `Percent _ -> DynArray.add b elt; flush true (* to make sure the next character doesn't turn them into a name *)
      | `Speaker l -> DynArray.add b (`Text (l, `Sbcs, token_name_left));
      | `Code (loc, id, _, params) when id = Text.ident "e" || id = Text.ident "em"
         -> let idx, size =
              match params with
                | [`Simple (_, idx)] -> idx, None
                | [`Simple (_, idx); `Simple (_, size)] -> idx, Some size
                | _ -> ksprintf (error loc) "incorrect parameters to code \\%s{}" (Text.to_sjs id)
            in
            Option.may (fun sz -> flush true; Meta.call "FontSize" [sz]) size;
            DynArray.add b (`Text (loc, `Dbcs, Text.of_arr [| 5 + Text.length id |]));
            begin try
              let i = match !Global.expr__normalise_and_get_const idx ~expect:`Int ~abort_on_fail:false
                      with `Integer i -> i | _ -> assert false in
              DynArray.add b (`Text (loc, `Sbcs, ksprintf Text.of_sjs "%02ld" i))
            with Exit ->
              flush false;
              Memory.open_scope ();
                let svar = Memory.get_temp_str () in
                Meta.call ~rv:svar "itoa" [idx; Meta.int 2];
                Meta.call "__vwf_TextoutAppend" [svar];
              Memory.close_scope ();
            end;            
            (**)
            Option.may (fun _ -> flush true; Meta.call "FontSize" []) size
      | `Code (loc, id, e, p) when id = Text.of_arr [| 0x73 |] (* \s{} *)
         -> let parm = match p with [`Simple (_, s)] -> s | _ -> error loc "the control code \\s{} must have one and only one parameter" in
            if not (parm matches `SVar _) then ksprintf (error loc) "Oops, expected string variable but found `%s'" (string_of_expr parm); (* it should, by this stage *)
            if e <> None then error loc "the control code \\s{} cannot have a length specifier";
            flush false;
            Meta.call "__vwf_TextoutAppend" [parm]
      | `Code (loc, id, e, p) when id = Text.of_arr [| 0x69 |](* \i{} *)
         -> let parm = match p with [`Simple (_, i)] -> i | _ -> error loc "the control code \\i{} must have one and only one parameter" in
            if normalised_expr_is_const parm then
              let s = 
                Global.int32_to_string_padded
                  (match e with None -> 0 | Some e -> Int32.to_int (int_of_normalised_expr e))
                  (int_of_normalised_expr parm)
              in
              DynArray.add b (`Text (loc, `Sbcs, Text.of_sjs s));
            else 
              let length = match e with None -> [] | Some e -> [`Int (nowhere, int_of_normalised_expr e)] in
              flush false;
              Memory.open_scope ();
                let svar = Memory.get_temp_str () in
                Meta.call ~rv:svar "itoa" (parm :: length);
                Meta.call "__vwf_TextoutAppend" [svar];
              Memory.close_scope ()
      | `Code (loc, id, e, p) when id = Text.of_arr [| 0x6e |] || id = Text.of_arr [| 0x72 |] (* \n, \r *)
         -> let s = Text.to_err id in
            if e <> None then ksprintf (error loc) "the control code \\%s cannot have a length specifier" s;
            if p <> [] then ksprintf (error loc) "the control code \\%s does not take any parameters" s;
            if s = "r" then DynArray.add b (`Text (loc, `Sbcs, token_clearindent));
            DynArray.add b (`Text (loc, `Sbcs, token_break));

      | `Code (loc, id, e, p) when id = Text.of_arr [| 0x62 |] (* \b *)
         -> DynArray.add b (`Text (loc, `Sbcs, token_emphasis));
      | `Code (loc, id, e, p) when id = Text.of_arr [| 0x75 |] (* \u *)
         -> DynArray.add b (`Text (loc, `Sbcs, token_regular));
         
      (* TODO: other special case codes? *)        
            
      | `Code (loc, id, e, params)
         -> flush true;
            if e <> None then ksprintf (error loc) "the control code \\%s cannot have a length specifier" (Text.to_err id);
            Function.compile (loc, None, Text.to_sjs id, id, params, None) ~is_code:true
      | `Name (l, lg, i, w)
         -> let lg = if lg = `Local then "\x81\x93" else "\x81\x96" in
            let i =
              try
                match !Global.expr__normalise_and_get_const i ~expect:`Int ~abort_on_fail:false with `Integer i -> Int32.to_int i | _ -> assert false
              with Exit ->
                error l "name index must be constant in rlBabel-formatted text"
                        (* at least till I work out a way to generate a variable A-Z at runtime *)
            in
            DynArray.add b (`Text (l, `Dbcs, Text.map ((+) 0x10000) (Text.of_sjs (StrTokens.make_name lg i))));
            Option.may
              (fun w ->
                try
                  match !Global.expr__normalise_and_get_const w ~expect:`Int ~abort_on_fail:false with 
                    | `Integer i -> DynArray.add b (`Text (l, `Dbcs, Text.of_arr [| 0x1ff10 + Int32.to_int i |]))
                    | _ -> assert false                
                with Exit ->
                  flush false;
                  Memory.open_scope ();
                    let svar = Memory.get_temp_str () in
                    Meta.call ~rv:svar "itoa_w" [w; Meta.int 2];
                    Meta.call "__vwf_TextoutAppend" [svar];
                  Memory.close_scope ())
              w
      | `Gloss (loc, _, _) -> error loc "not implemented: \\g{}"
      | `Ruby (loc, _, _)  -> error loc "not implemented: \\ruby{} in rlBabel-formatted text"
  in
  DynArray.iter parse text;
  flush true
