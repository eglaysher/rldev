(*
   Rlc: compiler framework
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
(*pp pa_macro.cmo ./pa_matches.cmo *)

DEFINE DEBUG_STATEMENTS

open Printf
open ExtList
open ExtString
open Optpp
open KeTypes
open KeAst
open Codegen

open Ulexing

(*
open ULexing
open UParser
*)

(*
let get_token = function 
  let (token,value,lstate) = KeULexer.get_token in
*)

(*
let get_token loc lexbuf = 
  let tok = KeULexer.get_token (loc) in (* lexbuf in *)

  tok
*)

let token_of_id id =
match id with
  |   0 -> "EOF"
  | 257 -> "LPAR"
  | 258 -> "RPAR"
  | 259 -> "LSQU"
  | 260 -> "RSQU"
  | 261 -> "LCUR"
  | 262 -> "RCUR"
  | 263 -> "SADD"
  | 264 -> "SSUB"
  | 265 -> "SMUL"
  | 266 -> "SDIV"
  | 267 -> "SMOD"
  | 268 -> "SAND"
  | 269 -> "SOR"
  | 270 -> "SXOR"
  | 271 -> "SSHL"
  | 272 -> "SSHR"
  | 273 -> "SET"
  | 274 -> "ADD"
  | 275 -> "SUB"
  | 276 -> "MUL"
  | 277 -> "DIV"
  | 278 -> "MOD"
  | 279 -> "AND"
  | 280 -> "OR"
  | 281 -> "XOR"
  | 282 -> "SHL"
  | 283 -> "SHR"
  | 284 -> "EQU"
  | 285 -> "NEQ"
  | 286 -> "LTE"
  | 287 -> "LTN"
  | 288 -> "GTE"
  | 289 -> "GTN"
  | 290 -> "LAND"
  | 291 -> "LOR"
  | 292 -> "NOT"
  | 293 -> "TILDE"
  | 294 -> "COLON"
  | 295 -> "COMMA"
  | 296 -> "SEMI"
  | 297 -> "POINT"
  | 298 -> "ARROW"
  | 309 -> "STR"
  | 311 -> "DEOF"
  | 312 -> "DHALT"
  | 313 -> "DTARGET"
  | 314 -> "DVERSION"
  | 315 -> "DLOAD"
  | 316 -> "DIF"
  | 317 -> "DELSE"
  | 318 -> "DELSEIF"
  | 319 -> "DENDIF"
  | 320 -> "DFOR"
  | 324 -> "DSET"
  | 325 -> "DUNDEF"
  | 326 -> "DHIDING"
  | 327 -> "OP"
  | 328 -> "USCORE"
  | 329 -> "RETURN"
  | 330 -> "IF"
  | 331 -> "ELSE"
  | 332 -> "WHILE"
  | 333 -> "REPEAT"
  | 334 -> "TILL"
  | 335 -> "FOR"
  | 336 -> "CASE"
  | 337 -> "OF"
  | 338 -> "OTHER"
  | 339 -> "ECASE"
  | 340 -> "BREAK"
  | 341 -> "CONTINUE"
  | 342 -> "RAW"
  | 343 -> "ENDRAW"
  |  id -> sprintf "%d" id
  
  
(*
handle_textout next (`Return (l, _, _) as elt) =
*)  

let rt_msg_event loc msg = 
  prettily_blk_ln !Optpp.base_indent (Printf.sprintf 
    "[RT] %s @ [Line %d, file: %s]" 
    msg loc.line loc.file)

let rt_msg_reply loc msg = 
  prettily_blk_ln !Optpp.base_indent (Printf.sprintf 
    "  --[RV]--> %s @ [Line %d, file: %s]" 
    msg loc.line loc.file)

(*
let trace_eval f rv : statement = f
let compile_stub text = do_compile compile_stub text
*)

(*    
let trace_eval (loc, rv, _, t, parms, label as f) rv = (* : statement = *)  
  let result = eval_as_code f in
  rt_msg_event loc (string_of_statement f)
  rt_msg_reply loc (string_of_statement result)
  rv = result
*)

(* rv = : statement = *)  

(*
let trace_eval (loc, rv, _, t, parms, label as fn) : statement = 
  rt_msg_event loc (string_of_statement fn);
  rt_msg_reply loc rv 
  eval_as_code loc fn rv in
  
  
  rt_msg_reply loc msg = loc msg = string_of_expr rv
*)


(*
let trace_eval (loc, rv, _, _, _, _ as fn) : statement = 
let trace_eval fn : statement = 
*)
(*
let trace_eval fn : statement = 
  let loc = loc_of_statement fn in
  rt_msg_event loc (string_of_statement fn);
  Intrinsic.eval_as_code fn
*)
 
(*
let trace_eval ((loc, rv, _, t, parms, label) as fn) : statement = 
  rt_msg_event loc (string_of_statement (loc, rv, _, t, parms, label))
(*let (loc, rv, _, _, _, _) = f in*)
(*let loc = loc_of_statement(fn) in*)
  let loc = loc_of_statement fn in
  rt_msg_event loc (string_of_statement fn);
  Intrinsic.eval_as_code fn
  (*
  loc fn rv [] 
  *)
  (*
  rt_msg_reply loc (string_of_expr rv)
  *)
  *)
  

(*  
  (loc, rv, fn loc [] Some result)    
  rt_msg_reply loc (string_of_expr (handle_eval_result rv))
*)
  
(*  
  let result = eval_as_code fn in
  let eval_as_code (loc, rv, _, t, parms, label) : statement = 
*)
(*
  Intrinsic.eval_as_expr (l, s, t, [], None)
*)
(*  
  rv = result
*)

(*
let handle_eval_result =
  fun loc rv parms ->
    match rv with
      | None -> ksprintf (error loc) ("the return value " ^ 
         "of the `%s' intrinsic cannot be ignored") fn
      | Some rv -> `Assign (loc, rv, `Set, eval_as_expr 
         (loc, fn, Text.ident fn, parms, None))
*)
(*
  ;
  let result = eval_as_code f in
  result
*)

  
(*  
if App.runtime_trace > 0 then 
  trace_textout ( handle_textout ) 
else handle_textout
*)


(*
let get_token lstate lexbuf =
(*
  let aux = { term = `ResStr; file = fname; line = 1; res = res } in
  let aux = { term = `ResStr; file = lstate.file; line = lstate.line; res = res } in
*)

(*
  let return t = t, lstate in
*)

(*
  let loc = lstate in
*)
  
  let tkn, loc = KeULexer.get_token lstate lexbuf in

(*
  let t = ULexing.lexeme lexbuf in
  let t = latin1_lexeme lexbuf in
*)
  
(*
  let (ti, v) = Ulexing.lexeme lexbuf in
*)

(*
  let ti = fst (Ulexing.latin1_lexeme lexbuf) in
*)

  let ti = 666 in
  
(*
  let t = token_of_id ti in
*)
  
(*
  Buffer.add_string b (latin1_lexeme lexbuf); quot lexbuf
  sysInfo (sprintf "[%04d] token: %s [%s]" loc.line t v); (* t v); *) (* tkn); *)
  sysInfo (sprintf "[%04d] token (%d): %s [%s]" loc.line ti t v);
*)
  
(*
  sysInfo (sprintf "[%04d] token (%d): %s" (loc.line) ti t);
*)
  sysInfo (sprintf "[%04d] token (%d)" (loc.line) ti);

  
  tkn, loc
*)
  
(*
  let t,v = tkn.KeAstParser.token in 
*)

(*
match tkn with
  | EOF
  | LPAR
  | RPAR
  | LSQU
  | RSQU
  | LCUR
  | RCUR
  | SADD
  | SSUB
  | SMUL
  | SDIV
  | SMOD
  | SAND
  | SOR
  | SXOR
  | SSHL
  | SSHR
  | SET
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | AND
  | OR
  | XOR
  | SHL
  | SHR
  | EQU
  | NEQ
  | LTE
  | LTN
  | GTE
  | GTN
  | LAND
  | LOR
  | NOT
  | TILDE
  | COLON
  | COMMA
  | SEMI
  | POINT
  | ARROW
  | INTEGER i
  | DRES t
  | STRING tkns
  | LABEL (str, txt)
  | IDENT (str, txt)
  | GOTO (str, txt)
  | VAR i
  | SVAR i
  | REG i
  | INT i
  | STR
  | DWITHEXPR (str, t)
  | DEOF
  | DHALT
  | DTARGET
  | DVERSION
  | DLOAD
  | DIF
  | DELSE
  | DELSEIF
  | DENDIF
  | DFOR
  | DIFDEF b
  | DINLINE b
  | DDEFINE t
  | DSET
  | DUNDEF
  | DHIDING
  | OP
  | USCORE
  | RETURN
  | IF
  | ELSE
  | WHILE
  | REPEAT
  | TILL
  | FOR
  | CASE
  | OF
  | OTHER
  | ECASE
  | BREAK
  | CONTINUE
  | RAW
  | ENDRAW
  | GOTOCASE (str, txt)
  | GOTOON (str, txt)
  | SELECT (str, num)
  | SPECIAL s (*of ([ `S | `Pause ]) *)
  
  in
  *)
  (*
  let (t,v,s) = KeULexer.get_token lexbuf in
  *)
  

  (*
This expression has type
  (int -> string -> 'a -> 'b, unit, string, 'a -> 'b) format4
but is here used with type
  (int -> string -> 'a -> 'b, unit, string) format =
    (int -> string -> 'a -> 'b, unit, string, string) format4
    *)
(*  
  lexer
    | _ -> rollback lexbuf; KeULexer.get_token loc lexbuf
    
    eof -> return EOF

    KeULexer.get_token loc lexbuf

    
  lexer
  let tkn, loc = KeULexer.get_token (loc aux) lexbuf in  
  let tkn, nloc = KeULexer.get_token loc lexbuf in
*)

(*
  let tok,loc = KeULexer.get_token () in (* lexbuf in *)
  
  let tok,loc = fn () in
*)
  
(*
  tvn
*)

(*
let get_token2 lexbuf =
(*
  (token,value,lstate) = KeULexer.get_token
*)
  
(*
  let (t,v,s) = KeULexer.get_token lstate in
*)

(*
  let (t,v,s) = KeULexer.get_token lexbuf in
  sysInfo (sprintf "[%04d] token: %s %s" s.line t v);
  (t,v,s)
*)
  
  let tl = KeULexer.get_token lexbuf in 
  
  let tok,loc = tl in

  let (t,v) = tok in

  sysInfo (sprintf "[%04d] token: %s %s" loc.line t v);
  
  tvl  
(*
  let (tok, loc) = KeULexer.get_token lexbuf in
  
  let (t,v) = tok in
  
  sysInfo (sprintf "[%04d] token: %s %s" loc.line t v);
  
  (tok, loc)
*)
*)
  
(*
  (token,value,lstate) = KeULexer.get_token
  
  sysInfo (sprintf "[%04d] token: %s %s" lstate.line token value );

  (token, value, lstate)
*)
  
  

let get_ast_of_string ?(file = nowhere.file) ?(line = nowhere.line) s =
  KeULexer.call_parser_on_string KeAstParser.program { file = file; line = line } s

let get_ast_of_channel enc ?(file = nowhere.file) ?(line = nowhere.line) ic =
  KeULexer.call_parser KeAstParser.program KeULexer.get_token (KeULexer.lex_channel ic ~enc) ~file ~line

let get_ast_of_file file =
  let s = String.create 128
  and ic = open_in file in
  begin try
    ignore (input ic s 0 128);
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
  end;
  let enc =
    let s = String.strip s in
    if not (String.starts_with s "{-#")
    then !App.enc
    else
      (* Parse file directives: "{-# key value [key2 value2...] #-" at start of file *)
      let dirs =
        (let rec f acc = function
          | [] -> acc
          | [a] -> (a, "") :: acc
          | a :: b :: rest -> f ((a, b) :: acc) rest
         in f [])
          (List.filter ((<>) "")
            (String.nsplit 
              (String.map 
                (function '\r' | '\n' | '\t' -> ' ' | c -> c)
                 (String.strip (String.slice s ~first:3 ~last:(String.find s "#-"))))
              " "))
      in
      try
        String.uppercase
          (snd 
            (List.find 
              (fun (a, _) -> String.starts_with (String.lowercase a) "enc") 
               dirs))
      with Not_found ->
        !App.enc
  and ic = open_in file in
  try    
    let rv = get_ast_of_channel enc ic ~file:(Filename.basename file) ~line:1 in
    close_in ic;
    rv
  with
    e -> close_in_noerr ic;
         raise e

let disambiguate =

(*
    sysInfo "parse";
    
    ignore(printf "disambiguate() with %s\n"
        string_of_param);
*)
        
  let mf =
    function
      | `Simple (_, e) -> e
      | `Complex (l, _)
      | `Special (l, _, _) -> error l "expected expression as parameter to inline expansion"
  in
  function
    | `VarOrFn (loc, s, t)
       -> (* printf "VarOrFn %s\n" s; *) (* printf "VarOrFn %s\n" (Text.to_err t); *)
(*       
          if s = "pause" && not (Memory.defined (Text.ident "__DynamicLineationUsed__")) then
            Memory.define ~scoped:false (Text.ident "__DynamicLineationUsed__") (`Integer 1l);
*)

(*
    ignore(printf "disambiguate(`VarOrFn) with %s\n" s);
*)

          if List.mem s [ "pause"; "spause"; "page" ] && 
            not (Memory.defined (Text.ident "__DynamicLineationUsed__")) then
            Memory.define ~scoped:false (Text.ident "__DynamicLineationUsed__") (`Integer 1l);
            
          if Memory.defined t then
            `Hiding (loc, t, Memory.get_as_code ~loc ~s t)
          else
            `FuncCall (loc, None, s, t, [], None)
    | `FuncCall (loc, rv, s, t, params, None) when Memory.defined t
       -> if rv = None
          then `Hiding (loc, t, Memory.get_as_code ~loc ~s t ~args:(List.map mf params))
          else `Assign (loc, Option.get rv, `Set, Memory.get_as_expression ~loc ~s t ~args:(List.map mf params))
    | elt
       -> elt


let break_stack = Stack.create ()
and continue_stack = Stack.create ()


(*
let trace_textout next (`Return (loc, _, _) as elt) rv = 
  rt_msg_event loc (string_of_statement next);
  let result = handle_textout next elt in
  rt_msg_reply result;
  rv = result  
*)

let rec parse ast =

(*
    sysInfo "parse";
*)
    
(*
    ignore(printf "parse(): %s\n" (string_of_statement ast));
*)

(*
    ignore(printf "parse() with %s\n", 
        string_of_statement);
*)

(*
    ignore(printf "ast length: %d\n" (DynArray.length ast -1) );
*)

  for i = 0 to DynArray.length ast - 1 do
(* 
ignore(printf "[>%d<]\n" i);
*)
    match disambiguate (DynArray.unsafe_get ast i) with
      | `Return (_, false, _) as elt
         -> (* Glomp pause commands here. *)
            let next =
              if i == DynArray.length ast - 1 then `No else
              match DynArray.unsafe_get ast (i + 1) with
                | `FuncCall (l, None, _, t, [], None)
                | `VarOrFn (l, _, t) -> if t = Text.ident "pause" then `Pause l
                                        else if t = Text.ident "page" then `Page l (* (printf "PAGE\n"; `Page l ) *)
                                        else `No
                | _ -> `No
            in


            if next <> `No then (
              DynArray.unsafe_set ast (i + 1) `Null;
              if not (Memory.defined (Text.ident "__DynamicLineationUsed__")) then
                Memory.define ~scoped:false (Text.ident "__DynamicLineationUsed__") (`Integer 1l);
            );

            if next <> `No then DynArray.unsafe_set ast (i + 1) `Null;
            
            if !App.runtime_trace > 0 
            then trace_textout next elt
            else handle_textout next elt
      | `Null -> ()
      | elt -> (* ignore(printf "** %d ** \n" i); *) 
        parse_elt elt
  done

and trace_textout next (`Return (loc, _, _) as elt) = (* rv = *) 
  rt_msg_event loc (string_of_statement elt);
  let result = handle_textout next elt in
  (*
  rt_msg_reply (lhandle_textout next elt);
  rt_msg_reply loc (string_of_statement result);
  *)
  rt_msg_reply loc (string_of_statement elt);
  result
  
(*
  let result = match handle_textout next elt with
    | Some rv -> string_of_expr(rv) 
    | None | _ -> "undef"
  in rt_msg_reply loc result  (* rv = result *)

  rt_msg_reply loc (handle_textout next elt)  (* rv = result *)
  let result = handle_textout next elt in
  rt_msg_reply (elhandle_textout next elt)
*)

and handle_textout next (`Return (l, _, _) as elt) =
  let ret =
    match Expr.normalise elt with
      | `Single (`Return (_, _, e)) -> e
      | `Nothing 
      | `Single _ -> assert false
      | `Multiple elts
         -> assert (DynArray.length elts > 0);
            let last = DynArray.last elts in
            DynArray.delete_last elts;
            parse elts;
            Memory.close_scope ();
            match last with `Return (_, _, e) -> e | _ -> assert false
  in
  if type_of_normalised_expr ret = `Int then
    error l ("textout expressions must be strings. " ^ 
       "If you did not intend this expression to be displayed, " ^ 
       "you should precede it with the `return' keyword");
  let dynalin = Text.ident "__DynamicLineation__" in
  if not (Memory.defined dynalin) || (Memory.get_as_expression dynalin matches `Int (_, 0l))
  then Textout.compile_stub (l, ret, next)
  else (
    if Memory.defined (Text.ident "__TEXTOUT_KH__")
    then Textout.compile (l, ret, next)
    else if Memory.defined (Text.ident "__RLBABEL_KH__")
    then Textout.compile_vwf (l, ret, next)
    else error l "__DynamicLineation__ defined, but no recognised dynamic lineation library loaded"
  )

  
and parse_elt elt =

(*
    ignore(printf "parse_elt(): %s\n" (string_of_statement elt));
*)
    
  match disambiguate elt with
    | #structure as s -> parse_struct s
    | `Return (_, false, _) as elt' -> handle_textout `No elt'
    | elt ->
      match Expr.normalise elt with
        | `Nothing 
           -> IFDEF DEBUG_STATEMENTS THEN if Memory.defined (Text.ident "__Trace__") then 
              eprintf "No-op:  %s\n%!" (string_of_statement elt) ELSE () END
        | `Single elt'
           -> IFDEF DEBUG_STATEMENTS THEN if Memory.defined (Text.ident "__Trace__") then 
              eprintf "Single: %s\n%!" (string_of_statement elt) ELSE () END;
              parse_norm_elt elt'
        | `Multiple elts
           -> IFDEF DEBUG_STATEMENTS THEN if Memory.defined (Text.ident "__Trace__") then 
              eprintf "Multi:  %s\n%!" (string_of_statement elt) ELSE () END;
              let last = DynArray.last elts in
              DynArray.delete_last elts;
              DynArray.iter parse_norm_elt elts;
              if last matches `LoadFile _ | `DConst _ | `Define _
              then (Memory.close_scope (); parse_norm_elt last) (* for declarations, so the declared items aren't hidden. *)
              else (parse_norm_elt last; Memory.close_scope ()) (* general case: so tempvars in funccalls ARE. *)

and parse_norm_elt : statement -> unit =

(*
    ignore(printf "parse_norm_elt(): %s\n" (string_of_statement));
*)
    
  function `Null -> assert false
    | `Return _  -> ((* do nothing - "return _" == nop *))
    | `VarOrFn _ -> assert false (* converted to `FuncCall or `Return in parse_elt *)
    | #structure -> assert false (* fully handled in parse and parse_elt *)
    | `Decl d -> Variables.allocate d
    | #directive as d -> Directive.compile d
    | `Halt loc -> Output.add_code loc "\000"
    | `Break loc -> (try parse_elt (`FuncCall (nowhere, None, "goto", Text.ident "goto", [], Some (Stack.top break_stack))) with Stack.Empty -> error loc "break outside breakable structure")
    | `Continue loc -> (try parse_elt (`FuncCall (nowhere, None, "goto", Text.ident "goto", [], Some (Stack.top continue_stack))) with Stack.Empty -> error loc "continue outside loop")
    | `Label l -> Output.add_label l
(*
    | `GotoList g -> Goto.goto_on g
*)
    | `GotoOn g   -> Goto.goto_on g
    | `GotoCase g -> Goto.goto_case g
    | `Assign ((loc, _, _, _) as a) -> Output.add_code loc (code_of_assignment a)
(*
    | `FuncCall (_, _, _, t, _, _ as f)
        -> if Intrinsic.is_builtin t
           then parse_elt (Intrinsic.eval_as_code f)
           else Function.compile f
*)

(*
    | `FuncCall (_, _, _, t, _, _ as f)
        -> if Intrinsic.is_builtin t then 
              parse_elt (
                if !App.runtime_trace > 0 
                then trace_eval f else 
                Intrinsic.eval_as_code f
            ) else Function.compile f
*)

    | `FuncCall (l, _, _, t, _, _ as f) as elt
        -> if Intrinsic.is_builtin t then (
            (*    then trace_eval elt else
 *)              parse_elt (
                if !App.runtime_trace > 0 then (
                
                  rt_msg_event l (string_of_statement elt);
                  let result = Intrinsic.eval_as_code f in
                  rt_msg_reply l (string_of_statement result);
                  result
                
                (* then trace_eval elt else *)
                ) else Intrinsic.eval_as_code f)
            ) else Function.compile f
            
    | `Select (l, _, _, _, _, _ as f)
       -> let dynalin = Text.ident "__DynamicLineation__" in
          if not (Memory.defined dynalin) 
          || (Memory.get_as_expression dynalin matches `Int (_, 0l))
          || Memory.defined (Text.ident "__TEXTOUT_KH__")
          then Select.compile f
          else if Memory.defined (Text.ident "__RLBABEL_KH__")
          then Select.compile_vwf f
          else error l "__DynamicLineation__ defined, but no recognised dynamic lineation library loaded"
    | `UnknownOp o -> Function.compile_unknown o
    | `LoadFile (l, f)
       -> parse_elt
           (let file = StrTokens.to_string (str_of_normalised_expr f) ~enc:Config.default_encoding in
            let trypath p = let ic = open_in p in close_in ic; p in
            get_ast_of_file
              (try trypath file with _ ->
                 try trypath (file ^ ".kh") with _ ->
                   try trypath (Config.prefix () // file) with _ ->
                     try trypath (Config.prefix () // file ^ ".kh")
              with Sys_error s ->
                ksprintf (error l) "Cannot load `%s': %s" file (try snd (String.split s ": ") with _ -> s)))
    | `RawCode (l, c)
       -> List.iter
            (function
              | `Bytes s -> Output.add_code l s
              | `Int i -> Output.add_code l (code_of_int32 i)
              | `Ident (s, _)
                 -> assert (String.length s > 0);
                    match s.[0] with
                      | '#' 
                         -> let s = if String.length s mod 2 == 0 then (s.[0] <- '0'; s) else String.lchop s in
                            let out = String.create (String.length s / 2) and hex = "0x__" in
                            begin try
                              for i = 0 to String.length out - 1 do
                                hex.[2] <- s.[i * 2];
                                hex.[3] <- s.[i * 2 + 1];
                                out.[i] <- char_of_int (int_of_string hex)
                              done;
                              Output.add_code l out
                            with Failure "int_of_string" ->
                              error l "syntax error in raw block: #... not hex"
                            end
                      | '?' -> Output.add_code l (String.lchop (TextTransforms.to_bytecode (Text.of_string !App.enc s)))
                      (*
                      | _ -> failwith "not implemented: anything to do with identifiers in raw blocks")
                      *)
                      | _ -> error l "not implemented: anything to do with identifiers in raw blocks")
            c

and parse_struct =

(*
    sysInfo "parse struct";
*)    

    (*
    ignore(printf "parse_struct(): %s\n" (string_of_expr));
    *)

  function
(*    | `Seq smts -> parse smts *)
    | `Seq smts -> (* ignore(printf "got Seq...parse(smts)\n"); *) 
        parse smts
    | `Hiding (l, t, e)
       -> let sym =
            try
              Memory.pull_sym t
            with Not_found ->
              ksprintf (error l) "cannot hide `%s': symbol not defined" (Text.to_sjs t)
          in
          Memory.define (Text.ident "__INLINE_CALL__") (`Macro (`Int (nowhere, 0l))) ~scoped:false;
          Memory.define (Text.ident "__CALLER_FILE__") (`Macro (`Str (nowhere, Global.dynArray (`Text (nowhere, `Sbcs, Text.of_err l.file))))) ~scoped:false;
          Memory.define (Text.ident "__CALLER_LINE__") (`Macro (`Int (nowhere, Int32.of_int l.line))) ~scoped:false;
          parse_elt e;
          Memory.undefine nowhere "__INLINE_CALL__" (Text.ident "__INLINE_CALL__");
          Memory.undefine nowhere "__CALLER_FILE__" (Text.ident "__CALLER_FILE__");
          Memory.undefine nowhere "__CALLER_LINE__" (Text.ident "__CALLER_LINE__");
          Memory.replace_sym t sym
    | `Block (_, smts)
       -> Memory.open_scope ();
          parse smts;
          Memory.close_scope ()
   (* Special case for "if ... go(to|sub)" in all cases except "if variable goto" in Kinetic *)
    | `If (l, e, `FuncCall (_, None, s, t, [], (Some _ as label)), None)
       when t = Text.ident "gosub"
         || (t = Text.ident "goto"
             && (has_goto_if ()
                 || (try ignore (Expr.normalise_and_get_const e ~abort_on_fail:false); true with _ -> false)))
       -> parse_elt (`FuncCall (l, None, s ^ "_if", Text.append t (Text.ident "_if"), [`Simple (l, e)], label))
   (* Special case for "if ... (break|continue)" in all cases except "if variable" in Kinetic *)
    | `If (l, e, (`Break _ | `Continue _ as bc), None)
       when has_goto_if ()
         || (try ignore (Expr.normalise_and_get_const e ~abort_on_fail:false); true with _ -> false)
       -> let lbl =
            try match bc with
                  | `Break _ -> Stack.top break_stack
                  | `Continue _ -> Stack.top continue_stack
            with
              Stack.Empty -> parse_norm_elt bc (* to save duplicating error reporting *); assert false
          in
          parse_elt (`FuncCall (l, None, "goto_if", Text.ident "goto_if", [`Simple (l, e)], Some lbl))
          
    | `If (l, e, smt, elsesmt) 
       -> let lendif = unique_label nowhere in
          let lelse = if elsesmt = None then lendif else unique_label nowhere in
          begin try
            if Expr.normalise_and_get_int e ~abort_on_fail:false <> 0l
            then parse_elt smt
            else Option.may parse_elt elsesmt
          with Exit ->
            parse_elt (`FuncCall (nowhere, None, "goto_unless", Text.ident "goto_unless", [`Simple (l, e)], Some lelse));
            parse_elt smt;
            Option.may
              (fun smt ->
                List.iter parse_elt
                  [`FuncCall (nowhere, None, "goto", Text.ident "goto", [], Some lendif);
                   `Label lelse;
                    smt])
              elsesmt;
            parse_elt (`Label lendif)
          end
    | `While (l, e, smt)
       -> if try Expr.normalise_and_get_int e ~abort_on_fail:false <> 0l with Exit -> true then
            let loop = unique_label nowhere and skip = unique_label nowhere in
            Stack.push skip break_stack;
            Stack.push loop continue_stack;
            List.iter parse_elt
              [`Label loop;
               `FuncCall (nowhere, None, "goto_unless", Text.ident "goto_unless", [`Simple (nowhere, e)], Some skip);
               smt;
               `FuncCall (nowhere, None, "goto", Text.ident "goto", [], Some loop);
               `Label skip];
            ignore (Stack.pop break_stack, Stack.pop continue_stack)
    | `Repeat (l, smts, e)
       -> let loop = unique_label nowhere and skip = unique_label nowhere and cont = unique_label nowhere in
          Stack.push skip break_stack;
          Stack.push cont continue_stack;
          Memory.open_scope ();
          List.iter parse_elt
            [`Label loop;
             `Seq smts;
             `Label cont;
             `FuncCall (nowhere, None, "goto_unless", Text.ident "goto_unless", [`Simple (l, e)], Some loop);
             `Label skip];
          Memory.close_scope ();
          ignore (Stack.pop break_stack, Stack.pop continue_stack)
    | `For (l, pre, e, inc, smt)
       -> let loop = unique_label nowhere and skip = unique_label nowhere in
          Stack.push skip break_stack;
          Stack.push loop continue_stack;
          Memory.open_scope ();
          List.iter parse_elt
            [`Seq pre;
             `Label loop;
             `FuncCall (nowhere, None, "goto_unless", Text.ident "goto_unless", [`Simple (l, e)], Some skip);
             (match smt with `Block (_, elts) -> `Seq elts | _ -> smt);
             `Seq inc;
             `FuncCall (nowhere, None, "goto", Text.ident "goto", [], Some loop);
             `Label skip];
          Memory.close_scope ();
          ignore (Stack.pop break_stack, Stack.pop continue_stack)
    | `Case (l, e, [], other)
       -> (* Degenerate case *)
          parse_elt (`Assign (l, `Store nowhere, `Set, e));
          if other <> None then
            let skip = unique_label nowhere in
            Stack.push skip break_stack;
            Memory.define (Text.ident "__ConstantCase__") (`Macro (`Int (nowhere, 1l))) ~warnings:false;
            parse (Option.get other);
            Memory.undefine nowhere "__ConstantCase__" (Text.ident "__ConstantCase__");
            parse_elt (`Label skip);
            ignore (Stack.pop break_stack)
    | `Case (l, e, ofs, other)
       -> let skip = unique_label nowhere in
          Stack.push skip break_stack;
          begin try
            (* Try to select a case at compile-time. *)
            let expr = Expr.normalise_and_get_int e ~abort_on_fail:false in
            let after = List.dropwhile (fun (e, smts) -> expr <> Expr.normalise_and_get_int e ~abort_on_fail:false) ofs in
            let smts = match after with
              | [] -> (match other with
                        | Some smts -> smts
                        | None
                         -> ksprintf (error l) 
                              "unable to find a case matching %ld, and no other clause was given" 
                              expr)
              | cases
               -> let smts = DynArray.create () in
                  let rec loop = function
                    | [] 
                     -> Option.may (fun a -> DynArray.append a smts) other;
                        smts
                    | (_, case) :: cases
                     -> let last = if DynArray.length case > 0 then Some (DynArray.last case) else None in
                        if last matches Some (`Break _) then (
                          DynArray.append (DynArray.sub case 0 (DynArray.length case - 1)) smts;
                          smts
                        ) else (
                          DynArray.append case smts;
                          loop cases
                        )
                  in loop cases
            in
            Memory.define (Text.ident "__ConstantCase__") (`Macro (`Int (nowhere, 1l))) ~warnings:false;
            parse smts;
            Memory.undefine nowhere "__ConstantCase__" (Text.ident "__ConstantCase__")
          with Exit -> 
            (* If variables are involved, we must compile to a jump table instead. *)
            let olbl = if other = None then skip else unique_label nowhere in
            (* First, check whether the cases defined are constant and consecutive.
               If all this holds true, use goto_on(). *)
            try
              (* Check all cases are constant (and record the constant values) *)
              let cases, bodies =
                List.fold_left
                  (fun (ca, oa) (ce, l, oe) -> ((ce, l) :: ca), (oe :: oa))
                  ([], 
                   match other with 
                     | None -> []
                     | Some body -> let da = DynArray.create () in
                                    DynArray.add da (`Label olbl);
                                    DynArray.append body da; [da])
                  (List.rev_map
                    (fun (case, body) -> 
                      let l = unique_label nowhere
                      and da = DynArray.create () in
                      DynArray.add da (`Label l);
                      DynArray.append body da;
                      Expr.normalise_and_get_int case ~abort_on_fail:false, l, da)
                    ofs)
              in
              
              (* Check all cases are consecutive (and get them sorted if they are). *)
              
              let cases = List.sort cases ~cmp:(fun (a, _) (b, _) -> compare a b) in
              let first = fst (List.hd cases) in
              let desired = List.init (List.length cases) (fun i -> Int32.add first (Int32.of_int i)) in
              if not (List.for_all2 (fun (a, _) b -> a = b) cases desired) then raise Exit;
              
              (* We pass!  Compile to a nice efficient goto_on() statement. *)
              
              Memory.define (Text.ident "__ConstantCase__") (`Macro (`Int (nowhere, 0l))) ~warnings:false;
(*
              parse_elt (`GotoOn (l, `Goto, `Op (nowhere, e, `Sub, `Int (nowhere, first)), List.map snd cases));
*)
(*
              parse_elt (`GotoOn (l, ("goto_on", "goto_on"), 
*)              

              let str = "goto_on" in
                             
(*
              str, Text.ident str
              parse_elt (`GotoOn (l, (Text.to_sjs tkn, Text.norm tkn)
              let tkn = (str, Text.ident str) in
*)

(*
              parse_elt (`GotoList (l, (str, Text.ident str),
*)
              parse_elt (`GotoOn (l, (str, Text.ident str),
                `Op (nowhere, e, `Sub, `Int (nowhere, first)), 
                List.map snd cases));
              
              Meta.goto olbl;
              List.iter parse bodies;
              Memory.undefine nowhere "__ConstantCase__" (Text.ident "__ConstantCase__")
            with Exit ->
              (* Use goto_case() to handle complex situations. *)
              
              let cases, ofs =
                List.fold_left
                  (fun (ca, oa) (ce, oe) -> (ce :: ca), (oe :: oa))
                  ([`Default olbl], 
                   match other with None -> [] | Some smts -> [`Label olbl :: DynArray.to_list smts])
                  (List.rev_map
                    (fun (e, smts) ->
                      let l = unique_label nowhere in
                      let c = `Match (e, l) in
                      let o = `Label l :: DynArray.to_list smts in
                      c, o)
                    ofs)
              in
              Memory.define (Text.ident "__ConstantCase__") (`Macro (`Int (nowhere, 0l))) ~warnings:false;
(*
              parse_elt (`GotoCase (l, `Goto, e, cases));
*)

              let str = "goto_case" in
              let tkn = (str, Text.ident str) in

(*
              parse_elt (`GotoCase (l, ("goto_case", "goto_case"), e, cases));
              parse_elt (`GotoCase (l, (str, Text.ident str), e, cases));
*)

              parse_elt (`GotoCase (l, tkn, e, cases));
              
              List.iter (List.iter parse_elt) ofs;
              Memory.undefine nowhere "__ConstantCase__" (Text.ident "__ConstantCase__")
          end;
          parse_elt (`Label skip);
          ignore (Stack.pop break_stack)
    | `DIf (l, e, iftrue, iffalse)
       -> let c = match Expr.normalise_and_get_const e ~expect:`Int with `Integer i -> i <> 0l | _ -> assert false in
          if not c then
            match iffalse with
              | `DEndif _ -> ()
              | `DElse (_, smts) -> parse smts
              | `DIf _ as dif -> parse_struct (dif :> structure)
          else
            parse iftrue
    | `DFor (l, s, t, start, finish, smt)
       -> let start = Int32.to_int (Expr.normalise_and_get_int start)
          and finish = Int32.to_int (Expr.normalise_and_get_int finish) in
          let f i =
            Memory.define t (`Macro (`Int (l, Int32.of_int i)));
            parse_elt smt;
            Memory.undefine l s t
          in
          if finish >= start 
          then for i = start   to   finish do f i done
          else for i = start downto finish do f i done

let initialise_modular_recursion : unit =

(*
    ignore(printf "initialise_modular_recursion()\n");
*)
    

  StrLexer.KeULexer.init KeULexer.call_parser_on_text KeULexer.get_token;

(*
  StrLexer.KeULexer.init KeULexer.call_parser_on_text get_token;
*)
  KeAst.memory__get_as_expression := Memory.get_as_expression;
  Global.expr__normalise_and_get_const := Expr.normalise_and_get_const;
  Global.compilerFrame__parse := parse;
  Global.intrinsic__is_builtin := Intrinsic.is_builtin;
  Global.expr__disambiguate := Expr.expr_disambiguate


let compile file =

(*
    ignore(printf "CompilerFrame::compile()\n");
*)

(*
  if !App.verbose > 0 then sysInfo file;
*)
  
(*
  let ast = DynArray.create () in
  let srcdir = if fname = "-" then "." else Filename.dirname fname in
*)
  
  let ast = DynArray.create () in
  let fdir = Filename.dirname file in
  let fname = Filename.basename file in
  let srcdir = if fname = "-" then "." else fdir in
  if !App.outdir = "" then App.outdir := srcdir;

  (* Initialise. *)
  
  KeTypes.init ();
  Ini.init IniParser.inifile IniLexer.lex srcdir;
  
  DynArray.add ast
   (ksprintf get_ast_of_string
     "#define __RLC__,
              __Optimisation__ = %ld,
              __Compiler__ = '%s',
              __RlcVersion__ = %d
              %s%s%s"
     !App.opt_level
     App.app.name
     (int_of_float (App.app.version *. 100.))
     (if !App.debug_info then "" else ", __NoDebug__")
     (if !App.assertions then "" else ", __NoAssert__")
     (if !App.array_bounds then ", __SafeArrays__" else ""));

  (* Read RTL *)

  if !App.verbose > 0 then sysInfo "Loading Kepago/RealLive RTL";
  DynArray.add ast (get_ast_of_file (Config.prefix () // "system.kh"));
  
  (* Switch to source directory. *)
  
  
  let oldcwd = Sys.getcwd () in
  
(*
  if !App.resdir != "" then App.resdir := 
    Filename.concat oldcwd !App.resdir;
*)
  
  if !App.resdir != "" && Filename.is_relative !App.resdir then
      App.resdir := Filename.concat oldcwd !App.resdir;
      
  
  
  (*
  DynArray.add App.appdirs (Sys.getcwd ());
  DynArray.add App.appdirs srcdir;
  *)
  
  
  Sys.chdir srcdir;
  
  
  (* Read project and/or seen header (if present). *)
  
(*
  if !App.verbose > 0 then ksprintf sysInfo 
    "working directory: \"%s\"\n", (Sys.getcwd ());
*)

(*
  if !App.verbose > 0 then ksprintf Optpp.sysInfo 
    "cwd: %s\n" (Sys.getcwd ());
*)
        
(*
  if Sys.file_exists "global.kh" then (
    if !App.verbose > 0 then sysInfo "Loading project header";
    DynArray.add ast (get_ast_of_file "global.kh");
  );
*)

  let bname = Filename.chop_extension fname in 
  
  (*
  ksprintf sysInfo "String.length bname: %d" 
    (String.length bname);

  ksprintf sysInfo "String.sub bname 0 4: %s" 
    (String.lowercase (String.sub bname 0 4));

  ksprintf sysInfo "String.sub bname 4 4: %d" 
    (int_of_string (String.sub bname 4 4));
  *)
  
  if String.length bname = 8 && 
    String.lowercase (String.sub bname 0 4) = "seen" then (
    let idx = int_of_string (String.sub bname 4 4) in 
    let nam = Printf.sprintf "global%04d.kh" idx in
    
    (*
    ksprintf sysInfo "idx: %d" idx; 
    ksprintf sysInfo "nam: \"%s\"" nam;
    
    ksprintf sysInfo "exists: %s" (string_of_bool (Sys.file_exists nam));
    *)
    
    if Sys.file_exists nam then (
      if !App.verbose > 0 then
        sysInfo "Loading seen header";
      DynArray.add ast (get_ast_of_file nam)
    ) else if Sys.file_exists "global.kh" then (
      if !App.verbose > 0 then sysInfo "Loading project header";
      DynArray.add ast (get_ast_of_file "global.kh");
    )
  );

      
  (* Read input file. *)
  
  if !App.verbose > 0 then sysInfo "Lexing and parsing";
  
(*
  if !App.verbose > 0 then ksprintf sysInfo 
    "working directory: \"%s\"\n", Sys.getcwd ();  
*)

  (*
  if !App.verbose > 0 then ksprintf Optpp.sysInfo 
    "cwd: %s\n" (Sys.getcwd ());
  *)
  
  DynArray.add ast
   (if fname = "-"
    then get_ast_of_channel !App.enc stdin ~file:"stdin" ~line:1
    else get_ast_of_file (Filename.basename fname));
    (*
    else get_ast_of_file (Filename.concat srcdir (Filename.basename fname)));
    *)

  (* Normalise and compile. *)

(*
  Sys.chdir (if !App.resdir != "" then !App.resdir else srcdir);
*)
  
  if !App.verbose > 0 then sysInfo "Compiling";
(*
  parse ast;
*)

  (* reset working directory if resource directory defined *)
  
  (*
  if !App.resdir != "" then Sys.chdir oldcwd;
  *)
  
(*
  if !App.verbose > 0 then ksprintf sysInfo 
    "working directory: \"%s\"\n", Sys.getcwd ();
*)

  (*
  if !App.verbose > 0 then ksprintf Optpp.sysInfo 
    "cwd: %s\n" (Sys.getcwd ());
  *)
  
    (try
(*
        loop_or (loop_and (get_expr_cond lexbuf) lexbuf) lexbuf
*)
        parse ast
    with
        | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expr_bool");
  
  (* Hardwired while we don't have proper functions. *)
  
  Textout.finalise ();

  (* Finish off file. *)
  
  if !App.debug_info then 
    Output.add_code nowhere
      "\x82\x72\x82\x85\x82\x85\x82\x8e\x82\x64\x82\x8e\x82\x84\xff\xff\xff\xff\xff\
       \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
       \xff\xff\xff\xff\xff\xff\xff\xff"
  else
    Output.add_code nowhere "\x00";

  (* Postconditions *)
  
  assert (Stack.length Memory.scope = 1);

  (* Output code. *)

  Sys.chdir oldcwd;
  
  if !App.verbose > 0 then sysInfo "Assembling";
(*  Sys.chdir oldcwd; *)
  BytecodeGen.generate ();
  
  if !App.verbose > 0 then sysInfo ""

