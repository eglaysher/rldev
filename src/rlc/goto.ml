(*
   Rlc: goto function compilation
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

open Printf
open KeTypes
open KeAst
open Codegen

(*
let goto (loc, to_sub, label) =
  let opmodule, opcode =
    match !global_target, to_sub with
      | `Kinetic, `Goto  -> 5, 1  | _, `Goto  -> 1, 0
      | `Kinetic, `Gosub -> 5, 5  | _, `Gosub -> 1, 5
  in
  Output.add_code loc (code_of_opcode 0 opmodule opcode 0 0);
  Output.add_ref label

let goto_if (loc, to_sub, if_unless, expr, label) =
  if normalised_expr_is_const expr then
    match int_of_normalised_expr expr, if_unless with
      | 0l, `If -> ()
      | 0l, `Unless | _, `If -> goto (loc, to_sub, label)
      | _, `Unless -> ()
  else
    let opmodule, opcode =
      let nd () = error loc "goto_if is not defined for the Kinetic target" in
      match !global_target, to_sub, if_unless with
        | `Kinetic, `Goto,  `If     -> nd () | _, `Goto,  `If     -> 1, 1
        | `Kinetic, `Goto,  `Unless -> 5, 2  | _, `Goto,  `Unless -> 1, 2
        | `Kinetic, `Gosub, `If     -> 5, 6  | _, `Gosub, `If     -> 1, 6
        | `Kinetic, `Gosub, `Unless -> 5, 7  | _, `Gosub, `Unless -> 1, 7
    in
    ksprintf (Output.add_code loc) "%s(%s)"
      (code_of_opcode 0 opmodule opcode 0 0)
      (code_of_expr expr);
    Output.add_ref label
*)

(*
let special_case (function__compile : ?is_code:bool -> funccall_type -> unit) loc s id neg call params label =
  let param = match params with [`Simple (_, e)] -> e | _ -> assert false in
  if not (normalised_expr_is_const param) then
    (* Not a special case *)
    false
  else (
    (* Is a special case *)
(*
    let drop, use =
      match int_of_normalised_expr param, id with
        | 0l, ("goto_if" | "gosub_if") -> true, ""
        | _, "goto_if" | 0l, "goto_unless" -> false, "goto"
        | _, "gosub_if" | 0l, "gosub_unless" -> false, "gosub"
        | _, ("goto_unless" | "gosub_unless") -> true, ""
        | _ -> ksprintf (error loc) "internal error: passed unknown (if) function `%s' to Goto.special_case" s
    in
*)

(*
    let drop, use =
      match int_of_normalised_expr param, neg, call with
        | 0l, false, _ -> true, ""
        | _, false, false | 0l, true, false -> false, "goto"
        | _, _, true | 0l, _, true -> false, "gosub"
        | _, true, _ -> true, ""
        | _ -> ksprintf (error loc) "internal error: passed unknown (if) function `%s' to Goto.special_case" s
    in
*)

(*
    if not (match int_of_normalised_expr param, neg with
        | 0l, false -> true
        | 0l, true -> false
        | _, true -> true
        | _, false -> false
(*
        | _ -> ksprintf (error loc) "internal error: passed unknown (if) function `%s' to Goto.special_case" s
*)
    ) then 
      let ident = if call then "gosub" else "goto" in      
      function__compile (loc, None, ident, Text.ident ident, [], label);
      true
    else false
*)

(*    
    if not (match int_of_normalised_expr param, neg with
        | 0l, false -> true
        | _, false | 0l, true -> false
        | _, true -> true

        | 0l, true -> false
        | _, true -> true
        | _, false -> false
(*
        | _ -> ksprintf (error loc) "internal error: passed unknown (if) function `%s' to Goto.special_case" s
*)
    ) then 
      let ident = if call then "gosub" else "goto" in      
      function__compile (loc, None, ident, Text.ident ident, [], label);
      true
    else false
*)

(*
    (match int_of_normalised_expr param, neg with
        | 0l, false -> true
        | _, false | 0l, true -> false
        | _, true -> true
    ) || (
      let ident = if call then "gosub" else "goto" in      
      function__compile (loc, None, ident, Text.ident ident, [], label);
      true
    )
*)

    (match int_of_normalised_expr param with
        | 0l -> !neg | _ -> neg) || (
      let ident = if call then "gosub" else "goto" in      
      function__compile (loc, None, ident, Text.ident ident, [], label);
      true
    )

  )
(*
    drop || (function__compile (loc, None, use, Text.ident use, [], label); true)
*)
*)

(*
let goto_on (loc, to_sub, expr, labels) =
  let opmodule, opcode =
    match !global_target, to_sub with
      | `Kinetic, `Goto  -> 5, 3  | _, `Goto  -> 1, 3
      | `Kinetic, `Gosub -> 5, 8  | _, `Gosub -> 1, 8
  in
*)

(*
let goto_on (loc, (str, txt), expr, labels) =
(*
  let (str, txt) = to_sub;
*)  
  
(*
  let opmodule, opcode =
    match !global_target, to_sub with
      | `Kinetic, `Goto  -> 5, 3  | _, `Goto  -> 1, 3
      | `Kinetic, `Gosub -> 5, 8  | _, `Gosub -> 1, 8
  in
*)
  
(*
  let fndef = KeTypes.rlfun str;
*)

  let func = KeTypes.rlfun str in 

(*
  func.op_type func.op_module func.op_code argc' overload'
*)
  
(*
  let opmodule, opcode = 
    func.op_module, func.op_code
  in
*)

(*
  let opmodule = func.op_module in 
  let opcode = func.op_code in
*)
  
(*
  ksprintf (Output.add_code loc) "%s(%s){"
    (code_of_opcode 0 opmodule opcode (List.length labels) 0)
    (code_of_expr expr);
*)

  ksprintf (Output.add_code loc) "%s(%s){"
    (code_of_opcode 0 func.op_module func.op_code 
    (List.length labels) 0) (code_of_expr expr);
  List.iter Output.add_ref labels;
  Output.add_code nowhere "}"

(*
let goto_case (loc, to_sub, expr, cases) =
  let opmodule, opcode =
    match !global_target, to_sub with
      | `Kinetic, `Goto  -> 5, 4  | _, `Goto  -> 1, 4
      | `Kinetic, `Gosub -> 5, 9  | _, `Gosub -> 1, 9
  in
  ksprintf (Output.add_code loc) "%s(%s){"
    (code_of_opcode 0 opmodule opcode (List.length cases) 0)
    (code_of_expr expr);
*)
*)

let special_case (function__compile : ?is_code:bool -> funccall_type -> unit) loc s id neg call params label =
  let param = match params with [`Simple (_, e)] -> e | _ -> assert false in
  if not (normalised_expr_is_const param) then
    (* Not a special case *)
    false
  else (
    (* Is a special case *)
    if (match int_of_normalised_expr param with
      | 0l -> neg | _ -> not neg) then (
      let ident = if call then "gosub" else "goto" in      
      function__compile (loc, None, ident, Text.ident ident, [], label)
    );
    
    true
  )


let goto_on (loc, (str, txt), expr, labels) =
  let func = KeTypes.rlfun str in 

  ksprintf (Output.add_code loc) "%s(%s){"
    (code_of_opcode 0 func.op_module func.op_code 
    (List.length labels) 0) (code_of_expr expr);
  List.iter Output.add_ref labels;
  Output.add_code nowhere "}"


let goto_case (loc, (str, txt), expr, cases) =
  let func = KeTypes.rlfun str in

  ksprintf (Output.add_code loc) "%s(%s){"
    (code_of_opcode 0 func.op_module func.op_code
    (List.length cases) 0) (code_of_expr expr);
    
  List.iter
    (function
      | `Default label
         -> Output.add_code nowhere "()";
            Output.add_ref label
      | `Match (expr, label)
         -> ksprintf (Output.add_code nowhere) 
            "(%s)" (code_of_expr expr);
            Output.add_ref label
    ) cases;
    
  Output.add_code nowhere "}"
