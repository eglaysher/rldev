(* 
   Kprl: RealLive disassembler
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

(*pp pa_macro.cmo *)

open Optpp
open Printf
open ExtList
open ExtString
open Ulexing
open KfnTypes

let debug =
    (fun () -> !App.verbose)

(* The actual ISet module appears to have issues. :/ *)

module ISet = Set.Make (struct type t = int;; let compare = compare end)


(* Internal disassembler options, modified by command-line flags. *)

type options = { 
    mutable separate_strings: bool;
    mutable separate_all: bool;
    mutable id_strings: bool;
    mutable read_debug_symbols: bool;
    mutable annotate: bool;
    mutable control_codes: bool;
    mutable suppress_uncalled: bool;
    mutable no_smart_resources: bool;
    mutable smart_resources_exclude: string list;
    mutable forced_target: [`None | `RealLive | `Avg2000 | `Kinetic];
    mutable uses_excl_kidoku: bool; 
    mutable start_address: int;
    mutable end_address: int; 
    mutable prep_res_file: bool; 
    mutable opcodes: bool; 
    mutable hexdump: bool;
    mutable raw_strings: bool;
    mutable make_map: bool;
    mutable mode: [`None | `RealLive | `Avg2000 | `Kinetic];
    mutable vers: int * int * int * int;
}

let string_of_version =
  function
    | a, b, 0, 0 -> sprintf "%d.%d" a b
    | a, b, c, 0 -> sprintf "%d.%d.%d" a b c
    | a, b, c, d -> sprintf "%d.%d.%d.%d" a b c d

let string_of_mode version =
  let v = string_of_version version in
  function
    | `None -> "[error]"
    | `RealLive -> "RealLive " ^ v
    | `Avg2000 -> "AVG2000 " ^ v
    | `Kinetic -> "Kinetic " ^ v

let options = { 
    separate_strings = true;
    separate_all = false;
    id_strings = false;
    read_debug_symbols = false;
    annotate = false;
    control_codes = true;
    suppress_uncalled = false;
    no_smart_resources = true;
    smart_resources_exclude = [];
    forced_target = `None;
    uses_excl_kidoku = false;
    start_address = -1;
    end_address = -1; 
    prep_res_file = false; 
    opcodes = false; 
    hexdump = false;
    raw_strings = false;
    make_map = false;
    mode = `None;
    vers = 0, 0, 0, 0;
}


(* "Modules" are categories of function, represented by the second byte of a function
   call in bytecode.  Names are assigned to some of these for convenience when identifying
   unknown functions' general areas of effect. *)

let modules = ref IMap.empty (* filled by Archiver.disassemble *)

let string_of_module m = try IMap.find m !modules with Not_found -> sprintf "%03d" m

let () =
  KfnTypes.handle_module := (fun num name -> modules := IMap.add num name !modules)


(* The opcode type models a function call in its bytecode representation. *)

type opcode = { 
    op_type: int;
    op_module: int;
    op_function: int;
    op_overload: int; 
}

let string_of_opcode op =
  sprintf "op<%d:%s:%05d, %d>" op.op_type 
   (string_of_module op.op_module) 
   op.op_function op.op_overload


(* Functions implemented as special cases: *)

(*
DEFINE GotoOn      = { op_module = 001; op_function = 00003 }
DEFINE GotoCase    = { op_module = 001; op_function = 00004 }
DEFINE GosubOn     = { op_module = 001; op_function = 00008 }
DEFINE GosubCase   = { op_module = 001; op_function = 00009 }
DEFINE PGotoOn     = { op_module = 005; op_function = 00003 }
DEFINE PGotoCase   = { op_module = 005; op_function = 00004 }
DEFINE PGosubOn    = { op_module = 005; op_function = 00008 }
DEFINE PGosubCase  = { op_module = 005; op_function = 00009 }
*)
DEFINE Select      = { op_module = 002 }
DEFINE Ruby        = { op_module = 003; op_function = 00120 }
DEFINE Strcpy      = { op_module = 010; op_function = 00000 }
DEFINE Strcat      = { op_module = 010; op_function = 00002 }


(* The fndef type models a function call in its Kepago representation.  To
   determine whether a bytecode function has a Kepago fndef associated with
   it, look for (string_of_opcode x) in the fndefs hash table. *)

type fndef_rec = { 
    fn_ident: string;
    fn_params: parameter list option;
    fn_flags: flag list;
    fn_ccode: string; 
}

type versioned_t =
  [`None | `RealLive | `Avg2000 | `Kinetic ]

type fndef_t =
  | Common of fndef_rec
  | Versioned of ((int * int * int * int -> versioned_t -> bool) * fndef_rec) list

let fndefs : (string, fndef_t) Hashtbl.t = Hashtbl.create 0 (* filled by Archiver.disassemble *)

let () = (* We need to pass some processing code to the generic .kfn file handler. *)
  KfnTypes.which_ident := snd;
  KfnTypes.handle_opcode :=
    (fun verlimits ident ccstr flags op_type op_module op_function prototypes ->
      let make_rec verlimit old_rec new_rec =
        match old_rec, verlimit with
          | Some (Versioned old), None -> Versioned (old @ [(fun _ _ -> true), new_rec])
          | _, None -> Common new_rec
          | None, Some limit -> Versioned [limit, new_rec]
          | Some (Common old), Some limit -> Versioned ((limit, new_rec) :: [(fun _ _ -> true), old])
          | Some (Versioned old), Some limit -> Versioned ((limit, new_rec) :: old) in
      let opcode =
        { op_type = op_type;
          op_module = op_module;
          op_function = op_function;
          op_overload = 0; } in
      let add_op verlimit i pt =
        let opstr = string_of_opcode { opcode with op_overload = i } in
        let ident' = if ident = "" then opstr else ident in
        let existing = try Some (Hashtbl.find fndefs opstr) with Not_found -> None in
        Hashtbl.replace fndefs opstr
          (make_rec verlimit existing
            { fn_ident = ident';
              fn_params = pt;
              fn_flags = flags;
              fn_ccode = ccstr; })
      in
      let verlimit =
        match 
          List.fold_left
            (fun (v_acc, c_acc) -> function
               | Class c 
                  -> let cm = 
                       match c with
                         | "reallive" -> `RealLive
                         | "avg2000" -> `Avg2000
                         | "kinetic" -> `Kinetic
                         | s -> kprintf failwith "unknown version `%s'" s
                     in v_acc, cm :: c_acc
               | Compare f 
                  -> f :: v_acc, c_acc)
            ([], []) verlimits
        with
          | [], [] -> None
          | vp, [] -> Some (fun v _ -> List.fold_left (fun rv p -> rv && p v) true vp)
          | [], cp -> Some (fun _ m -> List.mem m cp)
          | vp, cp -> Some (fun v m -> List.fold_left (fun rv p -> rv && p v) (List.mem m cp) vp)
      in
      List.iteri (add_op verlimit) prototypes)


(* The command_t type is used to store disassembled bytecode in memory; a
   complete file must be read before it can be written to disk, in order
   that labels can be inserted as appropriate. *)

type command_t = { 
    offset: int;
    mutable lineno: int;
    kepago: command_elt list;
    opcode: string;
    is_jmp: bool;
    hidden: bool;
    unhide: bool;
    ident:  string;
    ctype:  string;
    args:   string list;
}

and command_elt = S of string | P of int | STORE of string

let empty_cmd = { 
    offset = -1;
    lineno = -1;
    kepago = [];
    opcode = "";
    is_jmp = false;
    hidden = false;
    unhide = false; 
    ident  = "";
    ctype  = "";
    args   = [];
}

let base_cmd lexbuf = { empty_cmd with offset = lexeme_start lexbuf }

(*
let type map_type = (int, [ int; int ]) Hashtbl.t
*)

(* Global state may be evil, but it's damn convenient.  These variables store
   global data for the file currently being disassembled. *)

let commands = DynArray.create ()
let resstrs  = DynArray.create ()
let pointers = ref ISet.empty
(*
let seen_map = Hashtbl.create 0
*)

(*
let seen_map : (string, string) Hashtbl.t = Hashtbl.create 0
*)

(*
let seen_map : (int, [ int; int ]) Hashtbl.t = Hashtbl.create 0
*)

type loc_t = {
  seen: int;
  line: int;
}

let empty_loc = {
  seen = -1;
  line = -1
}

type addr_t = {
  scene: int;
  entry: int;
}

let empty_addr = {
  scene = -1;
  entry = -1
}

type jump_t = {
  origin: loc_t;
  target: addr_t;
  kind:   string;
}

let empty_jump = {
    origin = empty_loc;
    target = empty_addr;
    kind   = ""
}

type map_t = {
(*
  mutable subs: (int, loc) Hashtbl.t;
  mutable entries: (int, loc) Hashtbl.t;
  mutable subs: jump Hashtbl.t;
  mutable entries: jump Hashtbl.t;
*)
(*
  mutable entrypoints: DynArray.t;
  mutable subs: (int, loc) Hashtbl.t;
*)

  mutable entrypoints: int DynArray.t;
(*
  mutable entries: (int, loc_t) Hashtbl.t;
*)
  mutable entries: (int, jump_t) Hashtbl.t;
  mutable calls: jump_t DynArray.t;
  mutable gotos: jump_t DynArray.t
}

let empty_map = {
(*
  subs = Hashtbl.create 0;
*)
  entrypoints = DynArray.create ();
  entries = Hashtbl.create 0;
  calls = DynArray.create ();
  gotos = DynArray.create ()
}

let copy_map map =
  { 
    entrypoints = DynArray.copy map.entrypoints; 
(*
    entries = Hashtbl.copy map.entries; 
*)
    
    entries = (
      let e: (int, jump_t) Hashtbl.t = Hashtbl.create 0 in
    
      Hashtbl.iter
        (fun k v -> 
          Hashtbl.add e k v
      ) map.entries;
      
      e
    );
    
    calls = DynArray.copy map.calls; 
    gotos = DynArray.copy map.gotos;
  }


let map = ref empty_map

let seen_map : (int, map_t) Hashtbl.t = Hashtbl.create 0

    
let rescount = ref ~-1
let strcount = ref ~-1
let data_offset = ref 0

let reset_state () =
  DynArray.clear commands;
  DynArray.clear resstrs;
  pointers := ISet.empty;
  
(*
  seen_map := empty_map;
*)
  
  map := {
    entrypoints = DynArray.create ();
    entries = Hashtbl.create 0;
    calls = DynArray.create ();
    gotos = DynArray.create ()
  };

(*
  seen_map = { seen_map with seen_map.
*)

(*
  DynArray.clear seen_map.entrypoints;
  Hashtbl.clear seen_map.entries;
  DynArray.clear seen_map.calls;
  DynArray.clear seen_map.gotos;
*)

(*
  ignore(
  seen_map.entrypoints = DynArray.create ();
  seen_map.entries = Hashtbl.create 0;
  seen_map.calls = DynArray.create ();
  seen_map.gotos = DynArray.create ()
  );
*)
  
(*
  Hashtbl.add seen_map "" "";
*)
  
  
  
(*
  Hashtbl.clear seen_map; 
*)

(*
  seen_map = {
(*
    subs = Hashtbl.create 0;
*)
    entrypoints = DynArray.create ();
    entries = Hashtbl.create 0;
    calls = DynArray.create ();
    gotos = DynArray.create ()
  };
*)

(*
  seen_map = empty_map;
*)

  rescount := ~-1;
  strcount := ~-1


(* Output functions. *)

let safen s =
  if String.length s = 0 then ""
  else if s.[0] = '-' then " " ^ s
  else s
  
(*
let print_cmd labels oc cmd =
  if options.annotate then fprintf oc "{-%08x-} " (cmd.offset + !data_offset);
  if options.read_debug_symbols && cmd.lineno > -1 then fprintf oc "#line %d " cmd.lineno;
*)
  
(*
  List.iter
    (function
(*
      | S s | STORE s -> output_string oc (Text.to_string !App.enc (TextTransforms.read_cp932_compatible sysError s))
*)
(*
      | S s | STORE s -> try
                 Str.string_match "#line" ['0'-'9']+ s 0
               with Not_found ->
                 output_string oc (Text.to_string !App.enc 
                   (TextTransforms.read_cp932_compatible sysError s))
*)                   

      | S s | STORE s -> if (try
                 String.sub s 0 6 <> "#line "
               with Not_found -> false) 
               then output_string oc (Text.to_string !App.enc 
                 (TextTransforms.read_cp932_compatible sysError s))


      | P i -> try
                 let i' = IMap.find i labels in
                 fprintf oc " @%d" i'
               with Not_found -> 
                 fprintf oc " @unknown%d" i)
    cmd.kepago;
  fprintf oc "\n%!"
*)

(*
let print_cmd labels oc cmd =  
  let s = String.concat "" (List.map (function
(*
      | S s | STORE s -> if (try String.sub s 0 6 = "#line " with Not_found -> false) then 
*)
      | S s | STORE s -> 
        if (try String.sub s 0 6 = "#line " with _ -> false) 
          then "" else Text.to_string !App.enc 
          (TextTransforms.read_cp932_compatible sysError s)
      | FP i -> try sprintf " @%d" (IMap.find i labels)
        with Not_found -> sprintf " @unknown%d" i
(*
      | FP (s,e) -> "")
*)
(*
      | FP fp -> "")
*)
    )
    cmd.kepago)
  in
  
  if s <> "" then (
    if options.annotate then fprintf oc "{-%08x-} " (cmd.offset + !data_offset);
(*
    if options.read_debug_symbols && cmd.lineno > -1 then fprintf oc "#line %d " cmd.lineno;
*)
    if options.read_debug_symbols && cmd.ctype = "dbline" then fprintf oc "#line %d " cmd.lineno;
    let c = if cmd.opcode <> "" then " // " ^ cmd.opcode ^ " type: " ^ cmd.ctype else "" in 
    fprintf oc "%s%s\n%!" s c
  )
    
let print_cmd2 labels oc cmd =
  if options.annotate then fprintf oc "{-%08x-} " (cmd.offset + !data_offset);
(*
  if options.read_debug_symbols && cmd.lineno > -1 then fprintf oc "#line %d " cmd.lineno;
*)
  if options.read_debug_symbols && cmd.ctype = "dbline" then fprintf oc "#line %d " cmd.lineno;
  fprintf oc "\n%!"


let format_cmd labels cmd =  
  let s = String.concat "" (List.map (function
      | S s | STORE s -> 
        if (try String.sub s 0 6 = "#line " with _ -> false) 
          then "" else Text.to_string !App.enc 
          (TextTransforms.read_cp932_compatible sysError s)
(*
      | P i -> try sprintf " @%d" (IMap.find i labels)
        with Not_found -> sprintf " @unknown%d" i
*)
(*
      | FP (s,e) -> "")
*)
(*
      | FP s -> "")
*)
      | FP i -> try sprintf " @%d" (IMap.find i labels)
        with Not_found -> sprintf " @unknown%d" i
    )
    cmd.kepago)
  in
  
(*
  (sprintf "%05d " cmd.lineno) ^ (
*)
  if s <> "" then (
    (if options.annotate then sprintf "{-%08x-} " 
      (cmd.offset + !data_offset) else "") ^
(*
    (if options.read_debug_symbols && cmd.lineno > -1 
      then sprintf "#line %d " cmd.lineno else "") ^ s ^
*)
(*
    (if options.read_debug_symbols && cmd.lineno > -1 
      then sprintf "#line %d " cmd.lineno else "") ^ s ^
*)
    (if options.read_debug_symbols && cmd.ctype = "dbline" 
      then sprintf "#line %d " cmd.lineno else "") ^ s ^
(*
    (if cmd.opcode <> "" then " // " ^ cmd.opcode else "") ^ "\n"
*)
    (if options.opcodes then (
      (if cmd.opcode <> "" || cmd.ctype <> "" then " // " else "") ^ 
      (if cmd.opcode <> "" then cmd.opcode else "") ^
      (if cmd.ctype <> "" then " ctype: " ^ cmd.ctype else "")
    ) else "") ^ "\n"
  ) else s
(*
  )
*)

let format_cmd2 labels cmd =
  (if options.annotate then sprintf "{-%08x-} " 
    (cmd.offset + !data_offset) else "") ^
(*
  (if options.read_debug_symbols && cmd.lineno > -1 
*)
  (if options.read_debug_symbols && cmd.ctype = "dbline" 
    then sprintf "#line %d " cmd.lineno else "") ^ "\n"
*)

(*
let prerr_cmd cmd =
  print_cmd IMap.empty stderr { cmd with offset = cmd.offset + !data_offset }
*)

(*
let prerr_cmd cmd = 
  prerr_string (format_cmd cmd)
*)

(*
let debug_dump () =
  DynArray.iter prerr_cmd commands
*)

(*
let debug_dump () =
  DynArray.iter prerr_cmd commands
*)

(* Functions to handle data as it's disassembled. *)

let command cmd text =
  DynArray.add commands { cmd with kepago = [S text] }

(*
(*
let make_resource rescount text =
  let resfmt =
    if (String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40")
    then format_of_string "\\%s"
    else format_of_string "%s"
  in
  sprintf ("<%04d> " ^^ resfmt) rescount text,
  sprintf "#res<%04d>" rescount
*)

(*
let make_resource index text = 
  make_res false index text
*)
*)

let make_res inline index text =
(*
  let resid = sprintf (if inline = true 
    then "S%03d" else "%04d") index
  in
*)

  let resid = if inline = true
    then sprintf "S%03d" index 
    else sprintf "%04d" index 
  in

  let resfmt =
(*
    if (String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40")
*)
    if not options.raw_strings && ((String.length text > 0 && text.[0] = ' ')
        || (String.length text > 1 && String.sub text 0 2 = "\x81\x40"))
      then format_of_string "\\%s"
      else format_of_string "%s"
  in
  
  let txt = 
    if not options.raw_strings then text
    else (if(String.length text > 0 && text.[0] = ' ') 
      then String.sub text 1 (String.length text - 1)
      else (if(String.length text > 1 && String.sub text 0 2 = "\x81\x40")
      then String.sub text 2 (String.length text - 2)
      else text))
  in
(*
  if resid = 2 printf ("<%s> " ^^ resfmt) resid text
*)
  
  sprintf ("<%s> " ^^ resfmt) resid txt,
  sprintf "#res<%s>" resid

(*
(*
let make_resource index text = 
  make_res false index text
*)
*)


let make_resource ?(inline=false) count text =
(*
  ignore (if count = 29 then printf "<%04d> %s\n\n" count text);
*)
  
  let resfmt =
(*
    if (String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40")
*)
    if not options.raw_strings && ((String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40"))
    then format_of_string "\\%s"
    else format_of_string "%s"
  in

  let txt = 
    if not options.raw_strings then text
    else (if(String.length text > 0 && text.[0] = ' ') 
      then String.sub text 1 ((String.length text) - 1)
      else (if(String.length text > 1 && String.sub text 0 2 = "\x81\x40")
      then String.sub text 2 ((String.length text) - 2)
      else text))
  in
  
(*
  sprintf ("<%04d> " ^^ resfmt) rescount text,
  sprintf "#res<%04d>" rescount
*)

(*
  let key = sprintf (format_of_string 
    (if inline then "S%03d" else "%04d")) count
  in
*)

  let key = if inline
    then sprintf "S%03d" count
    else sprintf "%04d" count
  in
  
  sprintf ("<%s> " ^^ resfmt) key txt,
  sprintf "#res<%s>" key

(*
let make_resource ?(inline=false) count text =
(*
  ignore (if count = 29 then printf "<%04d> %s\n\n" count text);
*)
  
(*
  let resfmt =
(*
    if (String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40")
*)
    if not options.raw_strings && ((String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40"))
    then format_of_string "\\%s"
    else format_of_string "%s"
  in
*)

  let resfmt =
    if (String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40")
    then format_of_string "\\%s"
    else format_of_string "%s"
  in

(*
  let txt = 
    if not options.raw_strings then text
    else (if(String.length text > 0 && text.[0] = ' ') 
      then String.sub text 1 ((String.length text) - 1)
      else (if(String.length text > 1 && String.sub text 0 2 = "\x81\x40")
      then String.sub text 2 ((String.length text) - 2)
      else text))
  in
*)
  
(*
  sprintf ("<%04d> " ^^ resfmt) rescount text,
  sprintf "#res<%04d>" rescount
*)

(*
  let key = sprintf (format_of_string 
    (if inline then "S%03d" else "%04d")) count
  in
*)

  let key = if inline
    then sprintf "S%03d" count
    else sprintf "%04d" count
  in
  
  sprintf ("<%s> " ^^ resfmt) key text,
  sprintf "#res<%s>" key
*)

let reshash = Hashtbl.create 0

(*
let append_last_resstr s =
  let rl = DynArray.length resstrs - 1 in
  let prev = DynArray.get resstrs rl in
  let pos = if options.no_smart_resources then "" else
    let idx = String.index prev ' ' + 1 in
    Hashtbl.find reshash (String.sub prev idx (String.length prev - idx)) in
  let news =
    if String.length s = 0 then prev else
    if String.length prev > 0 && prev.[String.length prev - 1] = '\n' then
      if not options.raw_strings then
        if s.[0] = ' ' || (String.length s > 1 && String.sub s 0 2 = "\x81\x40")
        then sprintf "%s  \\%s" prev s
        else sprintf "%s  %s" prev s
      else prev ^ s
    else prev ^ s
  in
  DynArray.set resstrs rl news;
  if options.no_smart_resources then () else
    let idx = String.index news ' ' + 1 in
    Hashtbl.add reshash (String.sub news idx (String.length news - idx)) pos
*)

let append_last_resstr s =
  let rl = DynArray.length resstrs - 1 in
  let prev = DynArray.get resstrs rl in
  let pos = if options.no_smart_resources then "" else
    let idx = String.index prev ' ' + 1 in
    Hashtbl.find reshash (String.sub prev idx (String.length prev - idx)) in
  let news =
    if String.length s = 0 then prev else
    if String.length prev > 0 && prev.[String.length prev - 1] = '\n' then
      if s.[0] = ' ' || (String.length s > 1 && String.sub s 0 2 = "\x81\x40")
      then sprintf "%s  \\%s" prev s
      else sprintf "%s  %s" prev s
    else prev ^ s
  in
  DynArray.set resstrs rl news;
  if options.no_smart_resources then () else
    let idx = String.index news ' ' + 1 in
    Hashtbl.add reshash (String.sub news idx (String.length news - idx)) pos

(*
(*
let resource text =
  (* testing *)
  try
    if options.no_smart_resources then raise Not_found;
    if List.mem text options.smart_resources_exclude then raise Not_found;
    Hashtbl.find reshash text
  with Not_found ->
  (* end testing *)
  incr rescount;
  let rs, rv = make_resource !rescount text in
  (* testing *)
  if options.no_smart_resources then () else Hashtbl.add reshash text rv;
  (* end testing *)
  DynArray.add resstrs rs;
  rv
*)

(*
let resvalue = 
  (* testing *)
  try
    if options.no_smart_resources then raise Not_found;
    if List.mem text options.smart_resources_exclude then raise Not_found;
    Hashtbl.find reshash text
  with Not_found ->
  (* end testing *)
  incr strcount;
  let rs, rv = make_res true !strcount text in
  (* testing *)
  if options.no_smart_resources then () 
  else Hashtbl.add reshash text rv;
  (* end testing *)
  DynArray.add resstrs rs;
  rv
*)
*)


let resource text =
  (* testing *)
  try
    if options.no_smart_resources then raise Not_found;
    if List.mem text options.smart_resources_exclude then raise Not_found;
    Hashtbl.find reshash text
  with Not_found ->
  (* end testing *)
  incr rescount;
(*
  let rs, rv = make_resource !rescount text in
  let rs, rv = make_res false !rescount text in
*)
  let rs, rv = make_resource !rescount text in
  (* testing *)
  if options.no_smart_resources then () 
  else Hashtbl.add reshash text rv;
  (* end testing *)
  DynArray.add resstrs rs;
  rv

let resvalue text =
  (* testing *)
  try
    if options.no_smart_resources then raise Not_found;
    if List.mem text options.smart_resources_exclude then raise Not_found;
    Hashtbl.find reshash text
  with Not_found ->
  (* end testing *)
  incr strcount;
(*
  let rs, rv = make_resource !strcount text in
  let rs, rv = make_res true !strcount text in
*)
  let rs, rv = make_resource ~inline:true !strcount text in
  (* testing *)
  if options.no_smart_resources then () 
  else Hashtbl.add reshash text rv;
  (* end testing *)
  DynArray.add resstrs rs;
  rv

(*  
let res inline text =
  (* testing *)  
  try
    if options.no_smart_resources then raise Not_found;
    if List.mem text options.smart_resources_exclude then raise Not_found;
    Hashtbl.find reshash text
  with Not_found ->
  (* end testing *)
(*
  let count = 
    if inline then rescount 
    else strcount 
  in
  incr count;
*)
  let count = 
    if inline then (
        incr rescount; !rescount
    ) else (
        incr strcount; !strcount
    )
  in

(*
  let rs, rv = make_res inline !count text in
*)
  let rs, rv = make_resource count text in
  (* testing *)
  if options.no_smart_resources then () 
  else Hashtbl.add reshash text rv;
  (* end testing *)
  DynArray.add resstrs rs;
  rv


let resource text = 
  res false text

let resvalue text = 
  res true text
*)

(*
let make_resource rescount text =
  let resfmt =
    if (String.length text > 0 && text.[0] = ' ')
    || (String.length text > 1 && String.sub text 0 2 = "\x81\x40")
    then format_of_string "\\%s"
    else format_of_string "%s"
  in
  sprintf ("<%04d> " ^^ resfmt) rescount text,
  sprintf "#res<%04d>" rescount

let reshash = Hashtbl.create 0

let append_last_resstr s =
  let rl = DynArray.length resstrs - 1 in
  let prev = DynArray.get resstrs rl in
  let pos = if options.no_smart_resources then "" else
    let idx = String.index prev ' ' + 1 in
    Hashtbl.find reshash (String.sub prev idx (String.length prev - idx)) in
  let news =
    if String.length s = 0 then prev else
    if String.length prev > 0 && prev.[String.length prev - 1] = '\n' then
      if s.[0] = ' ' || (String.length s > 1 && String.sub s 0 2 = "\x81\x40")
      then sprintf "%s  \\%s" prev s
      else sprintf "%s  %s" prev s
    else prev ^ s
  in
  DynArray.set resstrs rl news;
  if options.no_smart_resources then () else
    let idx = String.index news ' ' + 1 in
    Hashtbl.add reshash (String.sub news idx (String.length news - idx)) pos

let resource text =
  (* testing *)
  try
    if options.no_smart_resources then raise Not_found;
    if List.mem text options.smart_resources_exclude then raise Not_found;
    Hashtbl.find reshash text
  with Not_found ->
  (* end testing *)
  incr rescount;
  let rs, rv = make_resource !rescount text in
  (* testing *)
  if options.no_smart_resources then () else Hashtbl.add reshash text rv;
  (* end testing *)
  DynArray.add resstrs rs;
  rv
*)

let pointer offset =
  pointers := ISet.add offset !pointers;
  P offset


(* The disassembler itself is implemented as a lexer; it seemed like a good
   idea at the time, and it does make some things (like get_string) simpler
   to write. *)

let regexp sjs1 = ['\x81'-'\x9f' '\xe0'-'\xef' '\xf0'-'\xfc']
let regexp sjs2 = ['\x40'-'\x7e' '\x80'-'\xfc']

(* Lexer utility functions *)
   
let printbytes lexbuf n1 =
    if n1 < 0 then ""
    else
        let rec readbytes_h n = (
            match n with
                | 0 -> ""
                | _ -> let f = (lexer
                            | eof -> " eof"
                            | _ -> (let c = (lexeme_char lexbuf 0) in
                                    let s = sprintf "0x%02x " c ^ readbytes_h (n - 1) in
                                    rollback lexbuf;
                                    s)) in
                            f lexbuf) in
        readbytes_h n1
        

let error lexbuf s =
    try
        ksprintf sysError "%s near 0x%06x" (Text.sjs_to_err s) 
          (lexeme_start lexbuf + !data_offset)
    with
        | Optpp.Error s ->
            if debug() > 0 then
                ksprintf Optpp.cliError "%s\nNext 100 bytes:\n%s" s (printbytes lexbuf 100)
            else
                ksprintf Optpp.cliError "%s" s

let warning lexbuf s =
  ksprintf sysWarning "%s near 0x%06x" (Text.sjs_to_err s) 
    (lexeme_start lexbuf + !data_offset)

let get_int32 = lexer _ _ _ _ ->
  let (%<) a b = Int32.shift_left (Int32.of_int a) b
  and (+) a b = Int32.add a b
  and l = lexeme lexbuf 
  in Int32.of_int l.(0) 
   + l.(1) %< 8
   + l.(2) %< 16
   + l.(3) %< 24

and get_int = lexer _ _ _ _ ->
  let l = lexeme lexbuf
  in l.(0)
   + l.(1) lsl 8
   + l.(2) lsl 16
   + l.(3) lsl 24

and get_int16 = lexer _ _ ->
  let l = lexeme lexbuf
  in l.(0)
   + l.(1) lsl 8

and get_byte = lexer _ -> (lexeme lexbuf).(0)

let expect lexbuf char fn =
  let n = get_byte lexbuf in
  if n != Char.code char then
    ksprintf (error lexbuf) "expected `%c', found 0x%02x in %s" char n fn

let expect2 lexbuf char fname cmd str = (* cmd str = *)
  let n = get_byte lexbuf in
  if n != Char.code char then (
    command cmd str; 
    (* fn (); *)
    ksprintf (error lexbuf) "expected `%c', found 0x%02x in %s" char n fname
  )

let expect3 lexbuf char fname tcmds = (*cmd fn = *)
  let n = get_byte lexbuf in
  if n != Char.code char then (
(*
    ignore (command cmd (fn ()));
    ignore (fn ());
*)
    DynArray.iter(fun c -> DynArray.add commands c) tcmds;
    ksprintf (error lexbuf) "expected `%c', found 0x%02x in %s" char n fname
  )
 
let peek_is char =
  lexer
    | eof -> false
    | _ -> let c = lexeme_char lexbuf 0 in rollback lexbuf; c = char

let peek_isn't char lb = not (peek_is char lb)

let peek =
  lexer
    | eof -> raise End_of_file
    | _ -> let c = lexeme_char lexbuf 0 in rollback lexbuf; char_of_int c

(* Expressions. *)

let dp s = 
    if debug() > 1 then printf "%s%!" s

let pns s = 
    dp (sprintf "%s\n" s);
    s
    
let variable_name lexbuf c =
    let decode =  function
    | 0x0a -> Config.svar_prefix ^ "K"   | 0x0b -> Config.ivar_prefix ^ "L"
    | 0x0c -> Config.svar_prefix ^ "M"   | 0x12 -> Config.svar_prefix ^ "S"
    | 0x00 -> Config.ivar_prefix ^ "A"   | 0x01 -> Config.ivar_prefix ^ "B"
    | 0x02 -> Config.ivar_prefix ^ "C"   | 0x03 -> Config.ivar_prefix ^ "D"
    | 0x04 -> Config.ivar_prefix ^ "E"   | 0x05 -> Config.ivar_prefix ^ "F"
    | 0x06 -> Config.ivar_prefix ^ "G"   | 0x19 -> Config.ivar_prefix ^ "Z"
    | 0x1a -> Config.ivar_prefix ^ "Ab"  | 0x1b -> Config.ivar_prefix ^ "Bb"
    | 0x1c -> Config.ivar_prefix ^ "Cb"  | 0x1d -> Config.ivar_prefix ^ "Db"
    | 0x1e -> Config.ivar_prefix ^ "Eb"  | 0x1f -> Config.ivar_prefix ^ "Fb"
    | 0x20 -> Config.ivar_prefix ^ "Gb"  | 0x33 -> Config.ivar_prefix ^ "Zb"
    | 0x34 -> Config.ivar_prefix ^ "A2b" | 0x35 -> Config.ivar_prefix ^ "B2b"
    | 0x36 -> Config.ivar_prefix ^ "C2b" | 0x37 -> Config.ivar_prefix ^ "D2b"
    | 0x38 -> Config.ivar_prefix ^ "E2b" | 0x39 -> Config.ivar_prefix ^ "F2b"
    | 0x3a -> Config.ivar_prefix ^ "G2b" | 0x4d -> Config.ivar_prefix ^ "Z2b"
    | 0x4e -> Config.ivar_prefix ^ "A4b" | 0x4f -> Config.ivar_prefix ^ "B4b"
    | 0x50 -> Config.ivar_prefix ^ "C4b" | 0x51 -> Config.ivar_prefix ^ "D4b"
    | 0x52 -> Config.ivar_prefix ^ "E4b" | 0x53 -> Config.ivar_prefix ^ "F4b"
    | 0x54 -> Config.ivar_prefix ^ "G4b" | 0x67 -> Config.ivar_prefix ^ "Z4b"
    | 0x68 -> Config.ivar_prefix ^ "A8b" | 0x69 -> Config.ivar_prefix ^ "B8b"
    | 0x6a -> Config.ivar_prefix ^ "C8b" | 0x6b -> Config.ivar_prefix ^ "D8b"
    | 0x6c -> Config.ivar_prefix ^ "E8b" | 0x6d -> Config.ivar_prefix ^ "F8b"
    | 0x6e -> Config.ivar_prefix ^ "G8b" | 0x81 -> Config.ivar_prefix ^ "Z8b"
    | i -> ksprintf (warning lexbuf) "unrecognised variable index 0x%02x in variable_name" i;
           sprintf "VAR%02x" i in
    pns (decode c)

                (*  1    2    3    4    5    6    7    8     9   10    11 *)
let op_string = [| "+"; "-"; "*"; "/"; "%"; "&"; "|"; "^"; "<<"; ">>"; "" |]



(* Kepago operator precedences differ from those used internally by RealLive, so 
   we use a recursive-descent parser to build expression trees (get_expr_*
   functions) and flatten that with appropriate parentheses in get_expression. *)

let rec get_expr_token lexbuf =
  let f = lexer
    | 0xff -> Int32.to_string (get_int32 lexbuf)
    | 0xc8 -> "store"
    | [^ 0xc8 0xff] '['
      -> let i = variable_name lexbuf (lexeme_char lexbuf 0) in
        let e = get_expression lexbuf in
            (try 
                expect lexbuf ']' "get_expr_token";
                sprintf "%s[%s]" i e
            with
                | Optpp.Error s -> ksprintf Optpp.startTrace "%s\nExpression so far:\n%s[%s]\n"    s i e)
    | eof -> error lexbuf "unexpected end of file in get_expr_token"
    | _ -> ksprintf (error lexbuf) "unknown token type 0x%02x in get_expr_token" (lexeme_char lexbuf 0) in
    try
        f lexbuf
    with
        | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expr_token"

and get_expr_term lexbuf =
    let f = lexer
        | "$" -> `Atom (get_expr_token lexbuf)
        | "\\\000" -> (* Unary plus?  We ignore it, anyway. *) get_expr_term lexbuf
        | "\\\001" -> `Minus (get_expr_term lexbuf)
        | "(" -> let c = get_expr_bool lexbuf in
            (try 
                expect lexbuf ')' "get_expr_term";
                c
            with
                | Optpp.Error s -> ksprintf Optpp.startTrace "%s" s)
        | eof -> error lexbuf "unexpected end of file in get_expr_term"
        | _ -> (try
                    ksprintf (error lexbuf) "expected [$\\(] in get_expr_term, found 0x%02x" (lexeme_char lexbuf 0)
                with
                    | Optpp.Error s -> ksprintf Optpp.startTrace "%s" s) in
        try
            f lexbuf
        with
            | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expr_term"

and get_expr_arith lexbuf =
  let rec loop_hi_prec tok =
    lexer
      | '\\' [0x02-0x09] 
          -> let op = lexeme_char lexbuf 1 in
             let rhs = get_expr_term lexbuf in
             loop_hi_prec (`Binary (tok, op, rhs)) lexbuf
      | eof 
      | _ -> rollback lexbuf;
             tok
  and loop tok =
    lexer
      | '\\' [0x00-0x01] 
          -> let op = lexeme_char lexbuf 1 in
             let rhs = loop_hi_prec (get_expr_term lexbuf) lexbuf in
             loop (`Binary (tok, op, rhs)) lexbuf
      | eof 
      | _ -> rollback lexbuf;
             tok
  in
    try
        loop (loop_hi_prec (get_expr_term lexbuf) lexbuf) lexbuf
    with
        | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expr_arith"

and get_expr_cond lexbuf =
  let rec loop tok =
    lexer 
      | '\\' [0x28-0x2d]
          -> let op = lexeme_char lexbuf 1 in
             let rhs = get_expr_arith lexbuf in
             loop (`Binary (tok, op, rhs)) lexbuf
      | eof 
      | _ -> rollback lexbuf;
             tok
  in
    try
        loop (get_expr_arith lexbuf) lexbuf
    with
        | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expr_cond"

and get_expr_bool lexbuf =
  let rec loop_and tok =
    lexer "\\<" -> loop_and (`Binary (tok, 0x3c, get_expr_cond lexbuf)) lexbuf
      | eof | _ -> rollback lexbuf; tok
  and loop_or tok =
    lexer "\\=" -> loop_or (`Binary (tok, 0x3d, loop_and (get_expr_cond lexbuf) lexbuf)) lexbuf
      | eof | _ -> rollback lexbuf; tok
  in
    try
        loop_or (loop_and (get_expr_cond lexbuf) lexbuf) lexbuf
    with
        | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expr_bool"
    

and get_expression =
  let op_string x =
    if x <= 0x09 then op_string.(x)
    else match x with
      | 0x28 -> "==" | 0x29 -> "!="
      | 0x2a -> "<=" | 0x2b -> "<"
      | 0x2c -> ">=" | 0x2d -> ">"
      | 0x3c -> "&&" | 0x3d -> "||"
      | _ -> "[unknown operator]"
  and prec x =
    if x <= 0x09 then [| 4; 4; 5; 5; 5; 5; 4; 4; 6; 6 |].(x)
    else if x <= 0x29 then 2
    else if x <= 0x2d then 3
    else if x == 0x3c then 1
    else if x == 0x3d then 0
    else assert false
  in
  let rec traverse =
    function
      | `Atom s -> dp "atom "; pns s
      | `Minus (`Atom s) -> ksprintf pns "-%s" s
      | `Minus (`Minus e) -> dp "--atom\n"; traverse e
      | `Minus (`Binary _ as e) -> ksprintf pns "-(%s)" (traverse e)
      (* TODO: special cases *) 
      | `Binary (a, op, b) 
         -> let a' = traverse a
            and b' =
              match b with
                | `Binary (_, bop, _) when prec bop <= prec op 
                    -> let t = traverse b in if t.[0] = '~' then t else ksprintf pns "(%s)" t
                | _ -> traverse b
            in
            if op = 0x07 && b' = "-1" then
              sprintf "~%s" (match a with `Binary _ -> ksprintf pns "(%s)" a' | _ -> a')
            else if op = 0x28 && b' = "0" then
              sprintf "!%s" (match a with `Binary _ -> ksprintf pns "(%s)" a' | _ -> a')
            else if op = 0x29 && b' = "0" then
              a'
            else
              let a'' =
                match a with
                  | `Binary (_, aop, _) when prec aop < prec op 
                      -> ksprintf pns "(%s)" a'
                  | _ -> a'
              in
              ksprintf pns "%s %s %s" a'' (op_string op) b'
  in
  fun lexbuf -> 
    try
        traverse (get_expr_bool lexbuf)
    with
        | Optpp.Trace (s, n)  -> Optpp.contTrace s n "get_expression"
        

and get_assignment cmd =
  let op =
    lexer
      | '\\' [0x14-0x1e] -> op_string.(lexeme_char lexbuf 1 - 0x14)
(*
      | _ -> ksprintf (error lexbuf) "expected 0x5c[14-1e], found 0x%02x in get_assignment" (lexeme_char lexbuf 0)
*)
      | _ -> ksprintf (error lexbuf) "expected 0x5c[14-1e], found 0x%02x%02x in get_assignment" 
            (lexeme_char lexbuf 0) (lexeme_char lexbuf 1)
  in fun lexbuf ->
    let itok = try get_expr_token lexbuf with | Optpp.Trace (s, n) -> Optpp.contTrace s n "get_assignment" in
    let op = op lexbuf in    
    let etok = try get_expression lexbuf with | Optpp.Trace (s, n) -> Optpp.contTrace s n (sprintf "get_assignment: %s %s= " itok op) in
    (* Check for assignments to/from STORE and fake return values as appropriate *)
    let unstored =
      if etok = "store"  then
        let ccmd = ref (DynArray.length commands - 1) in
        while !ccmd >= 0 && (DynArray.get commands !ccmd).hidden do decr ccmd done;
        match DynArray.get commands !ccmd with
          | { kepago = STORE _ :: tl } as lcmd
              -> DynArray.set commands !ccmd { lcmd with kepago = S (sprintf "%s %s= " itok op) :: tl };
                 false
          | _ -> true
      else true in
    if unstored then DynArray.add commands { cmd with kepago = [S (sprintf "%s %s= %s" itok op etok)] }

and get_string ?(in_ruby = false) sep_str lexbuf =
  let b = Buffer.create 0 in
  let q = ref false in
  let rec quot =
    lexer
      | eof -> raise End_of_file
      | "\\\"" -> Buffer.add_char b '\"'; quot lexbuf (* FIXME: I think this is not actually true. *)
      | '\\' -> Buffer.add_string b "\\\\"; quot lexbuf (* FIXME: Not sure about this one. *)
      | '\'' -> Buffer.add_string b (if sep_str then "\'" else "\\'"); quot lexbuf

(*
      | '\"' -> unquot lexbuf
      | '\"' -> if !q then Buffer.contents b else unquot lexbuf
*)

      | '\"' -> if !q then Buffer.contents b else unquot lexbuf

      | (sjs1 _)+ | _ -> Buffer.add_string b (latin1_lexeme lexbuf); quot lexbuf
  and unquot =
    lexer
      | eof -> Buffer.contents b

(*
      | '\"' -> quot lexbuf
      | '\"' -> q := Buffer.length b = 0; quot lexbuf
*)

(*
let regexp sjs1 = ['\x81'-'\x9f' '\xe0'-'\xef' '\xf0'-'\xfc']
let regexp sjs2 = ['\x40'-'\x7e' '\x80'-'\xfc']
*)
      | '\"' -> q := Buffer.length b = 0; quot lexbuf
      
      | (sjs1 _ | ['A'-'Z' '0'-'9' '?' '_'])+ -> Buffer.add_string b (latin1_lexeme lexbuf); unquot lexbuf
      | "###PRINT"
          -> expect lexbuf '(' "get_string.print";
             let e = get_expression lexbuf in
             expect lexbuf ')' "get_string.print";
             bprintf b "\\%c{%s}" (if e.[0] = 's' then 's' else 'i') e;
             unquot lexbuf
      | _ -> rollback lexbuf;
             Buffer.contents b
  in
  let s = unquot lexbuf in
  
  if in_ruby then
    s
  else
    if sep_str || (options.separate_strings && options.separate_all
                   && (lexer sjs1 -> true | eof | _ -> false) (from_latin1_string s)) then
      (* resource s *)
      if sep_str || not options.id_strings
        then resource s 
        else resvalue s
    else
      sprintf "'%s'" s

and get_data ?(sep_str = false) =
(* printf "get_data()\n"; *)
  lexer
    | ',' -> get_data lexbuf
    | '\n' _ _ -> get_data lexbuf
    | ['A'-'Z' '0'-'9' '?' '_' '\"'] | sjs1 
    | "###PRINT(" -> rollback lexbuf; get_string sep_str lexbuf
    | 'a' _
        -> let i = lexeme_char lexbuf 1 
           and b = Buffer.create 0 in
           ksprintf (warning lexbuf) "found 0x61%02x in general-case handler" i;
           expect lexbuf '(' "get_data.special";
           bprintf b "__special[%d](%s" i (get_data lexbuf);
           while peek_isn't 0x29 lexbuf do bprintf b ", %s" (get_data lexbuf) done;
           expect lexbuf ')' "get_data.special";
           Buffer.contents b ^ ")"
    | _ -> rollback lexbuf; get_expression lexbuf


let add_textout_fails cmd s =
  if DynArray.empty commands then
    true
  else
    let i = ref (DynArray.length commands - 1) in
    while !i >= 0 && (DynArray.unsafe_get commands !i).hidden do decr i done;
    if !i < 0 then
      true
    else
(*
      match (DynArray.unsafe_get commands !i).kepago with
*)
      let lcmd = DynArray.unsafe_get commands !i in
      match lcmd.kepago with 
        | [] | [P _] | [STORE _] | _::_::_ -> true

        (* Special cases for \size{} *)
(*
        | [S last] when String.length last > 10 && String.sub last 0 10 = "FontSize("
*)

(*
        | [S last] when lcmd.opcode = "<0:Msg:00101, 1>"
        | [S last] when String.length lcmd.opcode > 12 && String.sub lcmd.opcode 0 13 = "<0:Msg:00101,"
        | [S last] when String.length last > 10 && String.sub last 0 10 = "FontSize ("
        | [S last] when String.length last > 9 && String.sub last 0 9 = "FontSize("
*)

        | [S last] when String.length lcmd.opcode > 14 && 
          String.sub lcmd.opcode 0 15 = "op<0:Msg:00101,"
         -> let s = (if options.separate_strings then ksprintf resource 
              else ksprintf (sprintf "'%s'")) "\\size{%s}%s" 
              (if List.length lcmd.args > 0 then 
              List.nth lcmd.args 0 else "") s in
            DynArray.set commands !i { DynArray.unsafe_get 
              commands !i with kepago = [S s]; opcode = "" };
            false

(*
        | [S last] when String.length lcmd.opcode > 12 && 
          String.sub lcmd.opcode 0 13 = "<0:Msg:00101, 1>"
         -> let s = (if options.separate_strings then ksprintf resource 
              else ksprintf (sprintf "'%s'")) "\\size{%s}%s" (List.nth lcmd.args 0) s in
            DynArray.set commands !i { DynArray.unsafe_get commands !i with kepago = [S s]; opcode = ""};
            false
*)
(*
        | [S last] when (String.length last > 9 && String.sub last 0 9 = "FontSize(") || 
          (String.length lcmd.opcode > 14 && String.sub lcmd.opcode 0 15 = "op<0:Msg:00101,")
*)
        | [S last] when String.length last > 9 && 
          String.sub last 0 9 = "FontSize("
         -> (* ksprintf sysInfo "last: %s; offset: %05d; opcode: %s" last lcmd.offset lcmd.opcode; *)
            let a = String.index last '(' and b = String.index last ')' in
            let arg = String.sub last (a + 1) (b - a - 1) in
            let s = (if options.separate_strings then ksprintf resource else ksprintf (sprintf "'%s'")) "\\size{%s}%s" arg s in
            DynArray.set commands !i { DynArray.unsafe_get commands !i with kepago = [S s]; opcode = "" };
            false
        | [S "FontSize"]
         -> (* ksprintf sysInfo "last: %s; offset: %05d; opcode: %s" "FontSize" lcmd.offset lcmd.opcode; *)
            let s = (if options.separate_strings then ksprintf resource else ksprintf (sprintf "'%s'")) "\\size{}%s" s in
            DynArray.set commands !i { DynArray.unsafe_get commands !i with kepago = [S s]; opcode = "" };
            false
        (* General case *)
        | [S last]
         -> if options.separate_strings then
              (*
              let _, prev_res = make_resource !rescount "" in
              *)
              (*
              let prev_res = sprintf "#res<%04d>" !rescount in
              if last = prev_res
              *)
              if last = sprintf "#res<%04d>" !rescount
              then (append_last_resstr s; false)
              else true
            else
              if last.[0] = '\'' && last.[String.length last - 1] = '\'' then
                let ns =
                  if String.length s = 0 then last else
                    let prev = String.sub last 0 (String.length last - 1) in
                    let addfmt =
                      if String.length prev > 0 && prev.[String.length prev - 1] = '\n' then
                        if not options.raw_strings then
                          if s.[0] = ' ' || (String.length s > 1 && String.sub s 0 2 = "\x81\x40")
                          then format_of_string "%s \\%s'"
                          else format_of_string "%s %s'"
                        else format_of_string "%s %s'" (* "%s%s" *)
                      else format_of_string "%s%s'"
                    in
                    sprintf addfmt prev s
                in
                DynArray.set commands !i { DynArray.unsafe_get commands !i with kepago = [S ns] };
                false
              else
                true

let force_textout cmd s =
  if options.separate_strings then
    command cmd (resource s)
  else
    ksprintf (command cmd) "'%s'" s


let read_textout_alt cmd lexbuf =
  let b = Buffer.create 0 in
  let esc = options.separate_strings && not options.raw_strings in
  
  let rec quot add =
    lexer
      | eof -> raise End_of_file

(*
      | "\\" 
      | "\\ " -> if add && not options.raw_strings then Buffer.add_string b (latin1_lexeme lexbuf); unquot add lexbuf
*)

      | "\\\"" -> if add then Buffer.add_char b '\"'; quot add lexbuf
      | '\\' -> if add then Buffer.add_string b "\\\\"; quot add lexbuf
      | '\'' -> if add then Buffer.add_string b (if esc then "'" else "\\'"); quot add lexbuf
      | "<"  -> if add then Buffer.add_string b (if esc then "\\<" else "<"); quot add lexbuf
      | "//" -> if add then Buffer.add_string b (if esc then "\\//" else "//"); quot add lexbuf
      | "{-" -> if add then Buffer.add_string b (if esc then "{\\-" else "{-"); quot add lexbuf
      | '\"'  -> unquot add lexbuf
      | sjs1 _ | _ -> if add then Buffer.add_string b (latin1_lexeme lexbuf); quot add lexbuf

  and unquot add =
    lexer
      (* Special characters *)
      | ["\000#$\n@"] -> rollback lexbuf; Buffer.contents b
      | eof -> Buffer.contents b

(*
      | "\\" 
      | "\\ " -> if add && not options.raw_strings then Buffer.add_string b (latin1_lexeme lexbuf); unquot add lexbuf
*)

      | '\"' -> quot add lexbuf
      | ',' -> unquot add lexbuf
      | '\\' -> if add then Buffer.add_string b "\\\\"; unquot add lexbuf
      | '\'' -> if add then Buffer.add_string b (if esc then "'" else "\\'"); unquot add lexbuf
      | "<"  -> if add then Buffer.add_string b (if esc then "\\<" else "<"); unquot add lexbuf
      | "//" -> if add then Buffer.add_string b (if esc then "\\//" else "//"); unquot add lexbuf
      | "{-" -> if add then Buffer.add_string b (if esc then "{\\-" else "{-"); unquot add lexbuf
      
      (* Name variables *)
      | '\x81'['\x93' '\x96'] '\x82'['\x60'-'\x79'] ('\x82'['\x60'-'\x79'])?
       -> if add && not options.raw_strings then (
            let s = latin1_lexeme lexbuf in
            let lm = if s.[1] = '\x93' then 'l' else 'm'
            and c1 = char_of_int (int_of_char s.[3] - 0x1f)
            and c2 = if String.length s = 6 then String.make 1 
              (char_of_int (int_of_char s.[5] - 0x1f)) else "" in
            bprintf b "\\%c{%c%s}" lm c1 c2);
          unquot add lexbuf

      (* Indexed name variables *)
      | '\x81'['\x93' '\x96'] '\x82'['\x60'-'\x79'] ('\x82'['\x60'-'\x79'])? '\x82'['\x4f'-'\x58']
       -> if add && not options.raw_strings then (
            let s = latin1_lexeme lexbuf in
            let lm = if s.[1] = '\x93' then 'l' else 'm'
            and c1 = char_of_int (int_of_char s.[3] - 0x1f)
            and c2 = if String.length s >= 6 && s.[5] >= '\x60' then 
              String.make 1 (char_of_int (int_of_char s.[5] - 0x1f)) else ""
            and index = int_of_char s.[String.length s - 1] - 0x4f in
            bprintf b "\\%c{%c%s, %d}" lm c1 c2 index);
          unquot add lexbuf

      (* äGï∂éö *)
      | "\x81\x94" '\x82'['\x60'-'\x61'] '\x82'['\x4f'-'\x58'] '\x82'['\x4f'-'\x58']
       -> if add && not options.raw_strings then (
            let l = latin1_lexeme lexbuf in
            let code = if l.[3] = '\x60' then "e" else "em"
            and idx = (int_of_char l.[5] - 0x4f) * 10 + (int_of_char l.[7] - 0x4f) in
            bprintf b "\\%s{%d}" code idx);
          unquot add lexbuf

      (* Ultra-special case - sized äGï∂éö, though for simplicity's sake we only detect a subset of sizes *)
      | "#\000\003\101\000\001\000\000(" [^')']+ ")" ('@' _ _)?
        "\x81\x94" '\x82'['\x60'-'\x61'] '\x82'['\x4f'-'\x58'] '\x82'['\x4f'-'\x58']
        "#\000\003\101\000\000\000\001"
       -> if add && not options.raw_strings then (
            let c, l = String.split (String.slice (latin1_lexeme lexbuf) ~first:9) "\x81\x94" in
            let sizearg = get_expression (from_latin1_string (fst (String.split c ")") ^ ","))
            and code = if l.[1] = '\x60' then "e" else "em"
            and idx = (int_of_char l.[3] - 0x4f) * 10 + (int_of_char l.[5] - 0x4f) in
            bprintf b "\\%s{%d, %s}" code idx sizearg);
          unquot add lexbuf

      (* Lenticulars *)
      | "\x81\x79" -> if add && not options.raw_strings then Buffer.add_string b "\\{"; 
          unquot (if add then not options.raw_strings else add) lexbuf
      | "\x81\x7a" -> if add then Buffer.add_string b "}"; unquot (if not add then not add else add) lexbuf
      | "\x81\x7a" (" \"" | "\" ") ("\\\"" | '\'')
       -> if add then Buffer.add_string b "} ";
          if add then Buffer.add_char b (char_of_int (lexeme_char lexbuf (lexeme_length lexbuf - 1)));
          quot (if not add then not add else add) lexbuf

      (* Other characters *)
      | '\x81' sjs2
      | (['\x82'-'\x9f' '\xe0'-'\xef' '\xf0'-'\xfc'] sjs2)+
      | _ -> if add then Buffer.add_string b (latin1_lexeme lexbuf); unquot add lexbuf
  in
  match unquot true lexbuf with
    | "\x82\x72\x82\x85\x82\x85\x82\x8e\x82\x64\x82\x8e\x82\x84\xff\xff\xff\xff\xff\
       \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
       \xff\xff\xff\xff\xff\xff\xff\xff"
        -> ksprintf (command { cmd with is_jmp = true }) "eof"
    | "" -> command cmd "'' "
    | s -> if add_textout_fails cmd s then
             force_textout cmd s


(*
and get_string ?(in_ruby = false) sep_str lexbuf =
  let b = Buffer.create 0 in
  let rec quot =
    lexer
      | eof -> raise End_of_file
      | "\\\"" -> Buffer.add_char b '\"'; quot lexbuf (* FIXME: I think this is not actually true. *)
      | '\\' -> Buffer.add_string b "\\\\"; quot lexbuf (* FIXME: Not sure about this one. *)
      | '\'' -> Buffer.add_string b (if sep_str then "\'" else "\\'"); quot lexbuf
      | '\"' -> unquot lexbuf
      | (sjs1 _)+ | _ -> Buffer.add_string b (latin1_lexeme lexbuf); quot lexbuf
  and unquot =
    lexer
      | eof -> Buffer.contents b
      | '\"' -> quot lexbuf
      | (sjs1 _ | ['A'-'Z' '0'-'9' '?' '_'])+ -> Buffer.add_string b (latin1_lexeme lexbuf); unquot lexbuf
      | "###PRINT"
          -> expect lexbuf '(' "get_string.print";
             let e = get_expression lexbuf in
             expect lexbuf ')' "get_string.print";
             bprintf b "\\%c{%s}" (if e.[0] = 's' then 's' else 'i') e;
             unquot lexbuf
      | _ -> rollback lexbuf;
             Buffer.contents b
  in
  let s = unquot lexbuf in
  if in_ruby then
    s
  else
    if sep_str || (options.separate_strings && options.separate_all
                   && (lexer sjs1 -> true | eof | _ -> false) (from_latin1_string s)) then
      resource s
    else
      sprintf "'%s'" s

and get_data ?(sep_str = false) =
  lexer
    | ',' -> get_data lexbuf
    | '\n' _ _ -> get_data lexbuf
    | ['A'-'Z' '0'-'9' '?' '_' '\"'] | sjs1 | "###PRINT(" -> rollback lexbuf; get_string sep_str lexbuf
    | 'a' _
        -> let i = lexeme_char lexbuf 1 
           and b = Buffer.create 0 in
           ksprintf (warning lexbuf) "found 0x61%02x in general-case handler" i;
           expect lexbuf '(' "get_data.special";
           bprintf b "__special[%d](%s" i (get_data lexbuf);
           while peek_isn't 0x29 lexbuf do bprintf b ", %s" (get_data lexbuf) done;
           expect lexbuf ')' "get_data.special";
           Buffer.contents b ^ ")"
    | _ -> rollback lexbuf; get_expression lexbuf


let add_textout_fails cmd s =
  if DynArray.empty commands then
    true
  else
    let i = ref (DynArray.length commands - 1) in
    while !i >= 0 && (DynArray.unsafe_get commands !i).hidden do decr i done;
    if !i < 0 then
      true
    else
      match (DynArray.unsafe_get commands !i).kepago with
        | [] | [P _] | [STORE _] | _::_::_ -> true
        (* Special cases for \size{} *)
        | [S last] when String.length last > 10 && String.sub last 0 10 = "FontSize ("
         -> let a = String.index last '(' and b = String.index last ')' in
            let arg = String.sub last (a + 1) (b - a - 1) in
            let s = (if options.separate_strings then ksprintf resource else ksprintf (sprintf "'%s'")) "\\size{%s}%s" arg s in
            DynArray.set commands !i { DynArray.unsafe_get commands !i with kepago = [S s] };
            false
        | [S "FontSize"]
         -> let s = (if options.separate_strings then ksprintf resource else ksprintf (sprintf "'%s'")) "\\size{}%s" s in
            DynArray.set commands !i { DynArray.unsafe_get commands !i with kepago = [S s] };
            false
        (* General case *)
        | [S last]
         -> if options.separate_strings then
              let _, prev_res = make_resource !rescount "" in
              if last = prev_res
              then (append_last_resstr s; false)
              else true
            else
              if last.[0] = '\'' && last.[String.length last - 1] = '\'' then
                let ns =
                  if String.length s = 0 then last else
                    let prev = String.sub last 0 (String.length last - 1) in
                    let addfmt =
                      if String.length prev > 0 && prev.[String.length prev - 1] = '\n' then
                        if s.[0] = ' ' || (String.length s > 1 && String.sub s 0 2 = "\x81\x40")
                        then format_of_string "%s \\%s'"
                        else format_of_string "%s %s'"
                      else format_of_string "%s%s'"
                    in
                    sprintf addfmt prev s
                in
                DynArray.set commands !i { DynArray.unsafe_get commands !i with kepago = [S ns] };
                false
              else
                true

let force_textout cmd s =
  if options.separate_strings then
    command cmd (resource s)
  else
    ksprintf (command cmd) "'%s'" s
*)

let read_textout cmd lexbuf =
  let b = Buffer.create 0 in
  let rec quot =
    lexer
      | eof -> raise End_of_file
      | "\\\"" -> Buffer.add_char b '\"'; quot lexbuf
      | '\\' -> Buffer.add_string b "\\\\"; quot lexbuf
      | '\'' -> Buffer.add_string b (if options.separate_strings then "'" else "\\'"); quot lexbuf
      | "<"  -> Buffer.add_string b (if options.separate_strings then "\\<" else "<"); quot lexbuf
      | "//" -> Buffer.add_string b (if options.separate_strings then "\\//" else "//"); quot lexbuf
      | "{-" -> Buffer.add_string b (if options.separate_strings then "{\\-" else "{-"); quot lexbuf
      | '\"'  -> unquot lexbuf
      | sjs1 _ | _ -> Buffer.add_string b (latin1_lexeme lexbuf); quot lexbuf
  and unquot =
    lexer
      (* Special characters *)
      | ["\000#$\n@"] -> rollback lexbuf; Buffer.contents b
      | eof -> Buffer.contents b
      | '\"' -> quot lexbuf
      | ',' -> unquot lexbuf
      | '\\' -> Buffer.add_string b "\\\\"; unquot lexbuf
      | '\'' -> Buffer.add_string b (if options.separate_strings then "'" else "\\'"); unquot lexbuf
      | "<"  -> Buffer.add_string b (if options.separate_strings then "\\<" else "<"); unquot lexbuf
      | "//" -> Buffer.add_string b (if options.separate_strings then "\\//" else "//"); unquot lexbuf
      | "{-" -> Buffer.add_string b (if options.separate_strings then "{\\-" else "{-"); unquot lexbuf

      (* Name variables *)
      | '\x81'['\x93' '\x96'] '\x82'['\x60'-'\x79'] ('\x82'['\x60'-'\x79'])?
       -> let s = latin1_lexeme lexbuf in
          let lm = if s.[1] = '\x93' then 'l' else 'm'
          and c1 = char_of_int (int_of_char s.[3] - 0x1f)
          and c2 = if String.length s = 6 then String.make 1 (char_of_int (int_of_char s.[5] - 0x1f)) else "" in
          bprintf b "\\%c{%c%s}" lm c1 c2;
          unquot lexbuf

      (* Indexed name variables *)
      | '\x81'['\x93' '\x96'] '\x82'['\x60'-'\x79'] ('\x82'['\x60'-'\x79'])? '\x82'['\x4f'-'\x58']
       -> let s = latin1_lexeme lexbuf in
          let lm = if s.[1] = '\x93' then 'l' else 'm'
          and c1 = char_of_int (int_of_char s.[3] - 0x1f)
          and c2 = if String.length s >= 6 && s.[5] >= '\x60' then String.make 1 (char_of_int (int_of_char s.[5] - 0x1f)) else ""
          and index = int_of_char s.[String.length s - 1] - 0x4f in
          bprintf b "\\%c{%c%s, %d}" lm c1 c2 index;
          unquot lexbuf

      (* äGï∂éö *)
      | "\x81\x94" '\x82'['\x60'-'\x61'] '\x82'['\x4f'-'\x58'] '\x82'['\x4f'-'\x58']
       -> let l = latin1_lexeme lexbuf in
          let code = if l.[3] = '\x60' then "e" else "em"
          and idx = (int_of_char l.[5] - 0x4f) * 10 + (int_of_char l.[7] - 0x4f) in
          bprintf b "\\%s{%d}" code idx;
          unquot lexbuf

      (* Ultra-special case - sized äGï∂éö, though for simplicity's sake we only detect a subset of sizes *)
      | "#\000\003\101\000\001\000\000(" [^')']+ ")" ('@' _ _)?
        "\x81\x94" '\x82'['\x60'-'\x61'] '\x82'['\x4f'-'\x58'] '\x82'['\x4f'-'\x58']
        "#\000\003\101\000\000\000\001"
       -> let c, l = String.split (String.slice (latin1_lexeme lexbuf) ~first:9) "\x81\x94" in
          let sizearg = get_expression (from_latin1_string (fst (String.split c ")") ^ ","))
          and code = if l.[1] = '\x60' then "e" else "em"
          and idx = (int_of_char l.[3] - 0x4f) * 10 + (int_of_char l.[5] - 0x4f) in
          bprintf b "\\%s{%d, %s}" code idx sizearg;
          unquot lexbuf

      (* Lenticulars *)
      | "\x81\x79" -> Buffer.add_string b "\\{"; unquot lexbuf
      | "\x81\x7a" -> Buffer.add_string b "}"; unquot lexbuf
      | "\x81\x7a" (" \"" | "\" ") ("\\\"" | '\'')
       -> Buffer.add_string b "} ";
          Buffer.add_char b (char_of_int (lexeme_char lexbuf (lexeme_length lexbuf - 1)));
          quot lexbuf

      (* Other characters *)
      | '\x81' sjs2
      | (['\x82'-'\x9f' '\xe0'-'\xef' '\xf0'-'\xfc'] sjs2)+
      | _ -> Buffer.add_string b (latin1_lexeme lexbuf); unquot lexbuf
  in
  match unquot lexbuf with
    | "\x82\x72\x82\x85\x82\x85\x82\x8e\x82\x64\x82\x8e\x82\x84\xff\xff\xff\xff\xff\
       \xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
       \xff\xff\xff\xff\xff\xff\xff\xff"
        -> ksprintf (command { cmd with is_jmp = true }) "eof"
    | "" -> command cmd "'' "
    | s -> if add_textout_fails cmd s then
             force_textout cmd s

(*
let read_goto_case to_or_sub cmd argc lexbuf =
  let expr = get_expression lexbuf in
  expect lexbuf '{' "read_goto_case";
  let cases = List.init argc
    (fun i ->
      (lexer
        | "()" -> [if i == 0 then S "_:" else S "; _:";
                   pointer (get_int lexbuf)]
        | '(' -> let e = get_expression lexbuf in
                 if next lexbuf != Char.code ')' then
                   error lexbuf "expected `)' in read_goto_case";
                 [S (sprintf "%s%s:" (if i == 0 then "" else "; ") e);
                  pointer (get_int lexbuf)]
        | _ -> error lexbuf "expected `(' in read_goto_case") lexbuf) in
  expect lexbuf '}' "read_goto_case";
  DynArray.add commands { cmd with kepago = S (sprintf "go%s_case %s { " to_or_sub expr) :: List.flatten cases @ [S " }"] }
*)

(*
let read_goto_on to_or_sub cmd argc lexbuf =
  let expr = get_expression lexbuf in
  expect lexbuf '{' "read_goto_on";
  let ptrs =
    List.init argc
      (fun i ->
        let p = pointer (get_int lexbuf) in
        if i == 0 then
          [p]
        else
          [S ","; p])
  in
  expect lexbuf '}' "read_goto_on";
  DynArray.add commands { cmd with kepago = S (sprintf "go%s_on %s { " to_or_sub expr) :: List.flatten ptrs @ [S " }"] }
*)

let read_cases argc lexbuf =
  expect lexbuf '{' "read_cases";
  let cases = List.init argc
    (fun i ->
      (lexer
        | "()" -> [if i == 0 then S "_:" else S "; _:";
                   pointer (get_int lexbuf)]
        | '(' -> let e = get_expression lexbuf in
                 if next lexbuf != Char.code ')' then
                   error lexbuf "expected `)' in read_cases";
                 [S (sprintf "%s%s:" (if i == 0 then "" else "; ") e);
                  pointer (get_int lexbuf)]
        | _ -> error lexbuf "expected `(' in read_cases") lexbuf) in
  expect lexbuf '}' "read_cases";
  cases
  
(*
  DynArray.add commands { cmd with kepago = S (sprintf "go%s_case %s { " to_or_sub expr) :: List.flatten cases @ [S " }"] }
*)

  
let read_gotos argc lexbuf =
  expect lexbuf '{' "read_gotos";
  let ptrs =
    List.init argc
      (fun i ->
        let p = pointer (get_int lexbuf) in
        if i == 0 then
          [p]
        else
          [S ","; p])
  in
  expect lexbuf '}' "read_gotos";
  ptrs
  
(*
  List.flatten ptrs @ [S " }"]
*)

let read_select =
  let rec skip_debug_info mode =
    lexer
      | '\n' -> ignore ((if mode = `Avg2000 then get_int else get_int16) lexbuf); skip_debug_info mode lexbuf
      | ',' -> skip_debug_info mode lexbuf
      | _ -> rollback lexbuf; ()
  in
  fun mode cmd opcode argc lexbuf ->
    let fn = match opcode.op_function with 
      | 0 -> "select_w" 
      | 1 -> "select" 
      | 2 -> "select_s2" 
      | 3 -> "select_s" 
(*
      | 4 -> "select_btnobj"
      | 10 -> "select_w2"
*)
      | 10 -> "select_cancel"
      | 11 -> "select_msgcancel"
      | 12 -> "select_btncancel"
      | 13 -> "select_btnwkcancel"
(*
      | 14 -> "select_btnobjcancel"
      | 20 -> "select_btnobjinit"
      | 21 -> "select_btnobjinitall"
      | 22 -> "select_btnobjstart"
      | 23 -> "select_btnobjend"
      | 30 -> "select_btnobjnow_hit"
      | 31 -> "select_btnobjnow_push"
      | 32 -> "select_btnobjnow_decide"
      | 33 -> "select_btnobjnow_push_left"
      | 34 -> "select_btnobjnow_decide_left"
      | 35 -> "select_btnobjnow_push_right"
      | 26 -> "select_btnobjnow_decide_right"
      | 122 -> "select_btnobjstart_with_right_click"
*)

(*
  /// select_w                <0:Sel:00000, 0> ('windowNo'+)   // (special case)  // SEL
  /// select                  <0:Sel:00001, 0> ('windowNo'+)   // (special case)  // SELMSG
  /// select_s2               <0:Sel:00002, 0> ()              // (special case)  // SELBTN
  ///                                          ('selBtnNo')
  ///                                          ('x', 'y')
  ///                                          ('selBtnNo', 'x', 'y')   
  /// select_s                <0:Sel:00003, 0> ()              // (special case)  // SELBTNWK
  ///                                          ('selBtnNo')
  ///                                          ('x', 'y')
  ///                                          ('selBtnNo', 'x', 'y')
  /// select_cancel           <0:Sel:00010, 0> ('windowNo'+)   // (special case)  // SELCANCEL
  /// select_msgcancel        <0:Sel:00011, 0> ('windowNo'+)   // (special case)  // SELMSGCANCEL
  /// select_btncancel        <0:Sel:00012, 0> ()              // (special case)  // SELBTNCANCEL
  ///                                          ('selBtnNo')
  ///                                          ('x', 'y')
  ///                                          ('selBtnNo', 'x', 'y')
  /// select_btnwkcancel      <0:Sel:00013, 0> ()              // (special case)  // SELBTNWKCANCEL
  ///                                          ('selBtnNo')
  ///                                          ('x', 'y')
  ///                                          ('selBtnNo', 'x', 'y')
  /// select_btnobjcancel     <0:Sel:00014, 0> ('grpNo')       // (special case)  // SELBTNOBJCANCEL
  ///                                          ('grpNo', 'seNo')
  ///                                          ()
  ///                                          ('grpNo', 'seNo', strC 'saveMessageStr')
  /// select_btnobjinit       <0:Sel:00020, 0> ('grpNo')       // (special case)  // SELBTNOBJINIT
  /// select_btnobjinitall    <0:Sel:00021, 0> ()              // (special case)  // SELBTNOBJINITALL
  /// select_btnobjstart      <0:Sel:00022, 0> ('grpNo')       // (special case)  // SELBTNOBJSTART
  ///                                          ('grpNo', 'seNo')
  ///                                          ()
  /// select_btnobjend        <0:Sel:00023, 0> ()              // (special case)  // SELBTNOBJEND
  /// SELBTNOBJNOW_HIT (store) <0:Sel:00030, 1> () () // (special case)
  /// SELBTNOBJNOW_PUSH (store) <0:Sel:00031, 1> () () // (special case)
  /// SELBTNOBJNOW_DECIDE (store) <0:Sel:00032, 1> () () // (special case)
  /// SELBTNOBJNOW_PUSH_LEFT (store) <0:Sel:00033, 1> () () // (special case)
  /// SELBTNOBJNOW_DECIDE_LEFT (store) <0:Sel:00034, 1> () () // (special case)
  /// SELBTNOBJNOW_PUSH_RIGHT (store) <0:Sel:00035, 1> () () // (special case)
  /// SELBTNOBJNOW_DECIDE_RIGHT (store) <0:Sel:00036, 1> () () // (special case)
  /// SELBTNOBJSTART_WITH_RIGHT_CLICK <0:Sel:00122, 2> ('grpNo')
  ///                                          ('grpNo', 'seNo')
  ///                                          ()
*)
      | n -> ksprintf (warning lexbuf) "found unimplemented select function 0:Sel:%05d" n; sprintf "select_%05d" n
    in
    let fn' =
      if peek_is (Char.code '(') lexbuf then (
        expect lexbuf '(' "read_select";
        let e = get_expression lexbuf in
        expect lexbuf ')' "read_select";
        sprintf "%s[%s]" fn e
      )
      else fn
    in
(*
    expect lexbuf '{' "read_select";
*)
    let rv = 
    if peek_is (Char.code '{') lexbuf then (
    expect lexbuf '{' "read_select";
    let has_conds = ref false in
    let cases = List.init argc
      (fun i ->
        skip_debug_info mode lexbuf;
        let get_cond lexbuf =
          has_conds := true;
          expect lexbuf '(' "read_select.get_cond";
          let b = Buffer.create 0 in
          let rec loop () =
            if peek_isn't (Char.code ')') lexbuf then
              let cond =
                if peek_is (Char.code '(') lexbuf then (
                  expect lexbuf '(' "read_select";
                  let e = get_expression lexbuf in
                  expect lexbuf ')' "read_select";
                  sprintf " if %s" e
                ) else ""
              in
              let func, seekarg =
                match get_byte lexbuf with
                  | 0x30 -> "colour", true
                  | 0x31 -> "title", true
                  | 0x32 -> "hide", false
                  | 0x33 -> "blank", false
                  | 0x34 -> "cursor", true
                  | c -> ksprintf (error lexbuf) "unknown function %c in read_select" (char_of_int c)
              in
              let arg =
                let p = peek lexbuf in
                if seekarg && p <> ')' && (p < '0' || p > '9') then
                  sprintf "(%s)" (get_expression lexbuf)
                else ""
              in
              if Buffer.length b > 0 then Buffer.add_string b "; ";
              Buffer.add_string b func;
              Buffer.add_string b arg;
              Buffer.add_string b cond;
              loop ()
          in
          loop ();
          expect lexbuf ')' "read_select.get_cond";
          sprintf "%s: " (Buffer.contents b)
        in
        (* Allow for blank options *)
        let cond =            
          if peek_is (Char.code '(') lexbuf
          then get_cond lexbuf
          else "" 
        in
        cond ^ (if peek_is 0x0a lexbuf then "''" else 
          get_data ~sep_str:options.separate_strings lexbuf))
    in
    skip_debug_info mode lexbuf;
    expect lexbuf '}' "read_select";
    [S (sprintf "%s%s%s%s" fn'
         (if !has_conds then "(\n    " else "(")
         (String.concat (if !has_conds then ",\n    " else ", ") cases)
         (if !has_conds then "\n)" else ")"))]
    ) else
      [S (sprintf "%s" fn')]
        
(*
    let rv =
      [S (sprintf "%s %s%s%s" fn'
*)
(*
    let rv =
      [S (sprintf "%s%s%s%s" fn'
           (if !has_conds then "(\n    " else "(")
           (String.concat (if !has_conds then ",\n    " else ", ") cases)
           (if !has_conds then "\n)" else ")"))]
*)
    in
    DynArray.add commands { cmd with kepago = STORE 
      (if options.annotate then "{-STORE =-}" else "") :: rv }

let read_ruby cmd argc lexbuf =
  (* Special case to read \ruby{} code *)
  let add_text s =
    if add_textout_fails cmd s then
      force_textout cmd s
  in
  match argc with
    | 0 -> add_text "\\ruby{"
    | 1 -> expect lexbuf '(' "read_ruby";
           ksprintf add_text "}={%s}"
            (if peek_is 0x24 lexbuf
             then sprintf "\\s{%s}" (get_expression lexbuf)
             else get_string options.separate_strings lexbuf ~in_ruby:true);
           expect lexbuf ')' "read_ruby"
    | n -> ksprintf (error lexbuf) "%d arguments in \\ruby{} code: expected 0 or 1" n

let read_unknown_function cmd opstr argc lexbuf =
  if argc == 0 then
    command cmd opstr
  else
    let rec loop b not_first n lexbuf =
      if n == -1 then (
        warning lexbuf "argument overflow";
        Buffer.add_string b "{-extra arguments:-}"
      );
      (lexer
        | ')' -> if n > 0 then ksprintf (warning lexbuf) 
               "too few arguments in call to %s" opstr else ()
        | _ -> if not_first then Buffer.add_string b ", ";
               rollback lexbuf;
               Buffer.add_string b (get_data lexbuf);
               loop b true (n - 1) lexbuf)
      lexbuf
    in
    (try
        expect lexbuf '(' "read_unknown_function"
    with
        | Optpp.Error s -> ksprintf Optpp.startTrace 
          "%s\nExpression so far:\n%s\n" s opstr);
    let buffer = Buffer.create 0 in
(*
    bprintf buffer "%s (" opstr;
*)
    bprintf buffer "%s(" opstr;
    loop buffer false argc lexbuf;
    Buffer.add_char buffer ')';
    DynArray.add commands { cmd with kepago = [S (Buffer.contents buffer)] }


let read_complex_param b withparens opens params lexbuf =
  let rec read_param not_first param =
    lexer
      | 'a' -> if withparens then warning lexbuf "expected `)'"; rollback lexbuf
      | ')' -> if not withparens then rollback lexbuf
      | _ -> let pf, ps = match param with [] -> [], [] | (_, pf) :: ps -> pf, ps in
             rollback lexbuf;
             if not_first then Buffer.add_string b ", ";
             if options.annotate then
               (try
                  bprintf b "{-%s-} "
                    (match List.find (function Tagged _ -> true | _ -> false) pf
                     with Tagged s -> s | _ -> assert false)
                with Not_found -> ());
             let e = get_data lexbuf in
             if not not_first && opens = "{" && String.length e > 0 
               && e.[0] = '-' then Buffer.add_char b ' ';             
             Buffer.add_string b e;
             read_param true ps lexbuf
  in
  read_param false params lexbuf


let skip_debug lexbuf = 
  if peek_is 0x0a lexbuf then (ignore (get_byte lexbuf); ignore (get_int16 lexbuf))


(* Spaghetti alert! *)

let read_soft_function mode version cmd opcode argc fndef_gen lexbuf =
  let fndef =
    match fndef_gen with
      | Common def -> def
      | Versioned defs -> snd (List.find (fun pred, _ -> pred version mode) defs)
  in
  let control_code = options.control_codes && fndef.fn_ccode <> "" in
  match fndef.fn_params with None -> read_unknown_function 
    cmd fndef.fn_ident argc lexbuf | Some params ->
    let tcmds = DynArray.create () in
    let add_tcmd str = 
      let c = base_cmd lexbuf in
      DynArray.add tcmds { c with kepago = [S str] }
    in
(*
    let cmd = { cmd with is_jmp = List.mem IsJump fndef.fn_flags } in
*)

    let ctype = if List.mem IsJump fndef.fn_flags then "jump" 
      else if List.mem IsRet fndef.fn_flags then "return"
      else if List.mem IsGoto fndef.fn_flags then "goto"
      else if List.mem IsCall fndef.fn_flags then "call"
      else ""

    in
    
(*
    let cmd = { cmd with is_jmp = 
      List.mem IsJump fndef.fn_flags 
      || List.mem IsRet fndef.fn_flags; ctype = ctype } in
*)
      
    let cmd = { cmd with is_jmp = 
      List.mem IsSkip fndef.fn_flags 
      || List.mem IsRet fndef.fn_flags; 
      ident = fndef.fn_ident; ctype = ctype } in
      
    DynArray.add tcmds { cmd with kepago = [S (sprintf "%s %s argc: %d" 
                        fndef.fn_ident (string_of_opcode opcode) argc) ];
                        ctype = ctype };

    let args = DynArray.create () in
        
    let pre, params, ptrs =
      (* Case 1: no parameters given. *)
      if argc == 0 && (params = [] || peek_isn't (Char.code '(') lexbuf) 
      then (
        (* Warn if some parameters were required. *)
        if List.exists 
          (fun (_, flags) -> 
            not (List.exists (function Uncount | Fake | 
              Argc | Optional -> true | _ -> false) flags)) 
          params 
        then ksprintf (warning lexbuf) 
          "expected %d args to %s, but argc = 0" 
          (List.length params) fndef.fn_ident;
        "",
        String.concat ", " 
          (List.filter_map 
            (fun (_, flags) ->
              if List.mem Fake flags 
              then Some (match List.find (function Tagged s -> 
                true | _ -> false) flags with Tagged s -> s | _ -> assert false)
              else None)
            params),
        []
      )
      (* Case 2: parameters given. *)
      else
        let pre = ref "" in
        let ptrs = ref [] in
        
        if argc = 0 && List.exists (fun (_, flags) -> not (List.exists (function Optional | Uncount | Fake | Argc -> true | _ -> false) flags)) params then
          ksprintf (warning lexbuf) "expected %d args to %s, but argc = 0" (List.length params) fndef.fn_ident;
        let rec loop b not_first argc lexbuf =
          function
            | [] -> if peek_is 0x0a lexbuf then (ignore (get_byte lexbuf); ignore (get_int16 lexbuf));
        (*
        expect2 lexbuf ')' "read_soft_function" (fun() -> (command cmd fndef.fn_ident));
        *)
                ignore (expect2 lexbuf ')' "read_soft_function.1" cmd fndef.fn_ident);
                    if argc != 0 then
                      ksprintf (warning lexbuf) "argc = %d at end of %s call" argc fndef.fn_ident
  
            | (ptype, pflags) :: ps when List.mem Fake pflags
             -> if not_first then Buffer.add_string b ", ";
                let s = (try match List.find (function Tagged _ -> true 
                  | _ -> false) pflags with Tagged s -> s | _ -> assert false
                   with Not_found -> ksprintf failwith "untagged fake parameter in %s" fndef.fn_ident)
                in
                
                add_tcmd s;
                Buffer.add_string b s;
                DynArray.add args s;
                
                loop b true argc lexbuf ps
  
            | (ptype, pflags) :: ps when argc = 0 && List.exists 
                (function Optional | Argc -> true | _ -> false) pflags
             -> if peek_is 0x0a lexbuf then (
                  ignore (get_byte lexbuf); 
                  ignore (get_int16 lexbuf)
                );
                expect3 lexbuf ')' "read_soft_function.2" tcmds
                
            | (ptype, pflags) :: ps
             -> (* warn if out of args *)
                if argc == 0 && not (List.mem Uncount pflags) then
                  ksprintf (warning lexbuf) "argc = 0 before end of %s call" fndef.fn_ident;
                if peek_is 0x29 lexbuf then (
                  ignore (get_byte lexbuf);
                  ksprintf (warning lexbuf) "out of parameters before end of %s call" fndef.fn_ident
                )
                else let () = () in
  
                (* Add comma if not first parameter. *)
                if not_first && not (List.mem Return pflags) then Buffer.add_string b ", ";
  
                (* Add label if annotations are enabled and it's a tagged parameter. *)
                if options.annotate then
                  (try
                    let tag = match List.find (function Tagged _ -> true | _ -> false) pflags
                              with Tagged s -> s | _ -> assert false
                    in bprintf b "{-%s-} " tag
                   with Not_found -> ());
  
                (* ll is used when looping to permit one parameter to be read multiple times in `+' params *)
                let ll = if List.mem Argc pflags && argc > 1 then (ptype, pflags) :: ps else ps in
  
                match ptype with
                 (* If it's a complex parameter... *)
                  | Complex defs
                   -> if peek_is 0x0a lexbuf then (ignore (get_byte lexbuf); ignore (get_int16 lexbuf));

              expect3 lexbuf '(' "read_soft_function.3" tcmds;
              
                    (*  let cc = empty_cmd with offset = lexeme_start lexbuf in *)
                      let cc = base_cmd lexbuf in
                      let bb = Buffer.create 0 in
                      Buffer.add_char bb '{';
                      read_complex_param bb true "{" defs lexbuf;
                      Buffer.add_char bb '}';
                      Buffer.add_buffer b bb;
                      DynArray.add tcmds { cc with kepago = [S (Buffer.contents bb)] };
                      
                      loop b true (argc - 1) lexbuf ll
  
                 (* If it's a special parameter... *)
                  | Special sdefs
                   -> 
                      if peek_is 0x0a lexbuf then (
                        ignore (get_byte lexbuf); 
                        ignore (get_int16 lexbuf)
                      );
                      expect3 lexbuf 'a' "read_soft_function.4" tcmds;
                      let _, sdef, sflags = 
                        let id = 
                          let i1 = get_byte lexbuf in
                          if peek_is 0x61 lexbuf then (
                            ignore (get_byte lexbuf);
                            (((get_byte lexbuf) + 1) lsl 8) lor i1
                          ) else i1
                        in
                        try List.find (fun (a, _, _) -> a == id) sdefs
            with _ -> 
                        
                        ignore (List.iter (fun (a, _, _) -> ignore (printf ": %d\n" a)) sdefs); 
                        
                        DynArray.iter(fun c -> DynArray.add commands c) tcmds; 
                        ksprintf (error lexbuf) "invalid special parameter in %s call" fndef.fn_ident 
                      in
                      let opens, closes, defs =
                        match sdef with
                          | Named (name, params) -> sprintf "%s(" name, ")", params
                          | AsComplex params when List.mem NoParens sflags -> "", "", params
                          | AsComplex params -> "{", "}", params
                        in
                      let withparens = not (List.mem NoParens sflags) in
                      if withparens then expect3 lexbuf '(' "read_soft_function.5" tcmds;
                      let cc = base_cmd lexbuf in
                      let bb = Buffer.create 0 in
                      Buffer.add_string bb opens;
                      read_complex_param bb withparens opens defs lexbuf;
                      Buffer.add_string bb closes;
                      Buffer.add_buffer b bb;
                      DynArray.add args (Buffer.contents bb);
                      
                      DynArray.add tcmds { cc with kepago = [S (Buffer.contents bb)] };

                      loop b true (argc - 1) lexbuf ll
  
                 (* ...otherwise it's a normal parameter. *)
                  | _
                   -> let cc = base_cmd lexbuf in
                      let d = get_data lexbuf ~sep_str:(options.separate_strings && ptype = ResStr) in
                      if List.mem Return pflags then (
                        pre := sprintf "%s = " d;
                        loop b not_first (argc - 1) lexbuf ps
                      ) else (
                        Buffer.add_string b d;
                        DynArray.add args d;
                        DynArray.add tcmds { cc with kepago = [S d] };
                        loop b true (if List.mem Uncount pflags then argc else argc - 1) lexbuf ll
                      )
        in
        expect3 lexbuf '(' "read_soft_function.6" tcmds;
        let buffer = Buffer.create 0 in

        let gc = List.mem HasCases fndef.fn_flags in
        if gc || List.mem HasGotos fndef.fn_flags then (
          let e = get_expression lexbuf in
          Buffer.add_string buffer e;
          DynArray.add args e;
          expect lexbuf ')' ("read_soft_function." ^ 
            (if gc then "cases" else "gotos") ^ " expr");
          ptrs := (if gc then read_cases else read_gotos) argc lexbuf
        ) else loop buffer false argc lexbuf params;

        !pre, Buffer.contents buffer, !ptrs
    in
    let ccode_form = lazy 
      (if params = "" then
         sprintf "\\%s%s" fndef.fn_ccode
           (if List.mem NoBraces fndef.fn_flags then
              if List.mem IsLbr fndef.fn_flags then
                if options.separate_strings then "\n" else "\\\n"
              else ""
            else "{}")
       else
         sprintf "\\%s{%s}" fndef.fn_ccode params)
    in
    if not control_code || add_textout_fails cmd (Lazy.force ccode_form) then
      if options.control_codes && List.mem IsTextout fndef.fn_flags then
        force_textout cmd (Lazy.force ccode_form)
      else
        let label =
          if List.mem IsGoto fndef.fn_flags 
            then [pointer (get_int lexbuf)]
        
(*
          else if ctype = "call" || ctype = "jump" then (
            let s,e = if DynArray.length args > 0 then int_of_string (DynArray.get args 0),
              if DynArray.length args > 1 then int_of_string (DynArray.get args 1) else -1
              else -1,-1
            in
            
            if debug () > 0 then 
              sysInfo (sprintf "Seen%04d %12s Seen%04d%s" 
                scene fndef.fn_ident scene (if entry > -1
                then (sprintf "(Z%02d)" entry) else ""));
                
(*
            [ (FP s,e) ]
*)

(*
            let fp = sprintf "%d %d" s e in
*)
            
            let fp = s in 
            
            [ FP fp ]
          )
*)
          
          else if ptrs <> [] then S ("{ ") 
            :: List.flatten ptrs @ [S " }"]      
          else []
        in
        
        let rv,op =            
(*
            cmd.opcode <- pre ^ (string_of_opcode opcode) ^ pargs;
            
            pre ^ fndef.fn_ident ^ pargs
          in
*)

          let pargs = if params <> "" then "(" ^ params ^ ")" else "" in
          let pfunc = pre ^ fndef.fn_ident ^ pargs in
          let pcode = pre ^ (string_of_opcode opcode) ^ pargs in

          let rv = S pfunc :: label in
          if List.mem PushStore fndef.fn_flags then
            STORE (if options.annotate then "{-STORE =-}" else "") :: rv,pcode
          else
            rv,pcode
        in
        
        (*
        if not (params = "" && DynArray.length args = 0) 
        then Optpp.sysInfo (Printf.sprintf "args: %d; params: %s" 
          (DynArray.length args) params);
        *)
        
(*
        if List.mem IsJump fndef.fn_flags 
        || List.mem IsCall fndef.fn_flags then (
          let scene,entry = 
            if DynArray.length args > 0 
              then DynArray.get args 0,
              if DynArray.length args > 1  
              then DynArray.get args 1 else ""
            else "",""
          in
*)
(*
        let iscall = List.mem IsCall fndef.fn_flags in
        if iscall || (List.mem IsJump fndef.fn_flags) then (
*)

(*
        if ctype = "call" || ctype = "jump" then (
          let scene,entry = 
            if DynArray.length args > 0 then int_of_string (DynArray.get args 0),
              if DynArray.length args > 1 then int_of_string (DynArray.get args 1) else -1
            else -1,-1
          in
          
(*
          sysInfo (Printf.sprintf "scene: %s; entry: %s" scene entry);
*)
          sysInfo (Printf.sprintf "scene: %d; entry: %d; type: %s" scene entry ctype);

(*
          let jscene = if scene = "" then "" else 
            (sprintf "Seen%04d" (int_of_string scene)) ^ 
            (if entry = "" then "" else sprintf 
            "(Z%02d)" (int_of_string entry)) in
*)

(*
          let jscene = if scene = "" then -1 else int_of_string scene in
          let jentry = if entry = "" then -1 else int_of_string entry in
*)

(*
          Hashtbl.add seen_map scene jscene :: jentry;
*)          
      

(*
          DynArray.add
(*
            (if iscall then seen_map.calls else seen_map.gotos) 
*)
            (if ctype = "call" then seen_map.calls else seen_map.gotos) 
            { empty_jump with target = { scene = scene; 
            entry = entry }; kind = fndef.fn_ident };
*)

(*
          DynArray.add
            (if iscall then seen_map.calls else seen_map.gotos) 
            { empty_jump with target = { empty_addr with 
            scene = scene; entry = entry }; 
            kind = fndef.fn_ident };
*)

(*
          if debug () > 0 then sysInfo (sprintf "%s %s" fndef.fn_ident jscene)
*)
          
          if debug () > 0 then 
            sysInfo (sprintf "Seen%04d %12s Seen%04d%s" 
              scene fndef.fn_ident scene (if entry > -1
              then (sprintf "(Z%02d)" entry) else ""))
        );
*)

(*
          if ctype = "call" || ctype = "jump" then (
            let s,e = if DynArray.length args > 0 then int_of_string (DynArray.get args 0),
              if DynArray.length args > 1 then int_of_string (DynArray.get args 1) else -1
              else -1,-1
            in
            
            if debug () > 0 then 
              sysInfo (sprintf "Seen%04d %12s Seen%04d%s" 
                scene fndef.fn_ident scene (if entry > -1
                then (sprintf "(Z%02d)" entry) else ""));
          );
*)
        
        DynArray.add commands { cmd with kepago = rv; 
          opcode = op; args = DynArray.to_list args }

let read_function mode version offset opcode argc lexbuf =
  let cmd = { empty_cmd with offset = offset } in
  match opcode with
(*  
    | GosubCase | PGosubCase -> read_goto_case "sub" cmd argc lexbuf
    | GotoCase  | PGotoCase -> read_goto_case "to" cmd argc lexbuf
    | GosubOn | PGosubOn -> read_goto_on "sub" cmd argc lexbuf
    | GotoOn  | PGotoOn -> read_goto_on "to" cmd argc lexbuf
*)

(*
    | Select -> read_select mode cmd opcode argc lexbuf
*)
(*
    | Select when not Hashtbl.mem fndefs (string_of_opcode opcode)-> read_select mode cmd opcode argc lexbuf
*)
(*
    | Select -> let opstr = string_of_opcode opcode in
           if Hashtbl.mem fndefs opstr then
             try
               read_soft_function mode version cmd opcode argc (Hashtbl.find fndefs opstr) lexbuf
             with
               Not_found ->
                 ksprintf sysWarning
                   "found %s, which is defined for some interpreters, but not %s"
                   (string_of_opcode opcode) (string_of_mode version mode);
                 read_select mode cmd opcode argc lexbuf
           else
             read_select mode cmd opcode argc lexbuf
*)
    | Strcpy
    | Strcat -> expect lexbuf '(' "read_function";
                let a = get_data lexbuf in
                let b = get_data lexbuf in
                if opcode.op_overload = 1 then
(*
                  DynArray.add commands { cmd with kepago = [S (sprintf "strcpy (%s, %s, %s)" a b (get_data lexbuf))] }
*)
                  DynArray.add commands { cmd with kepago = [S (sprintf "strcpy(%s, %s, %s)" a b (get_data lexbuf))] }
                else
                  DynArray.add commands { cmd with kepago = [S (sprintf "%s %s= %s" a (if opcode.op_function = 0 then "" else "+") b)] };
                expect lexbuf ')' "read_function"
    | Ruby -> read_ruby cmd argc lexbuf
    | _ -> let opstr = string_of_opcode opcode in
           if Hashtbl.mem fndefs opstr then
             try
               read_soft_function mode version cmd opcode argc (Hashtbl.find fndefs opstr) lexbuf
             with
               Not_found ->
                 ksprintf sysWarning
                   "found %s, which is defined for some interpreters, but not %s"
                   (string_of_opcode opcode) (string_of_mode version mode);
(*
                 read_unknown_function cmd opstr argc lexbuf
*)
(*
                 if opcode.op_module == 002 then 
                   read_select mode cmd opcode argc lexbuf
                 else 
                   read_unknown_function cmd opstr argc lexbuf
*)
                 match opcode with
                   | Select -> read_select mode cmd opcode argc lexbuf
                   | _ -> read_unknown_function cmd opstr argc lexbuf
           else
(*           
             read_unknown_function cmd opstr argc lexbuf
*)
(*
             if opcode.op_module == 002 then 
               read_select mode cmd opcode argc lexbuf
             else 
               read_unknown_function cmd opstr argc lexbuf
*)
             match opcode with
               | Select -> read_select mode cmd opcode argc lexbuf
               | _ -> read_unknown_function cmd opstr argc lexbuf
             

(*
let dbl = ref ~-1
*)

let read_command hdr mode version lexbuf2 =
(*
  let debug_line =
    let c = { base_cmd lexbuf with hidden = not options.read_debug_symbols } in
    let i = (if mode = `Avg2000 then get_int else get_int16) lexbuf in
    if DynArray.length commands > 0 then
      if (DynArray.last commands).kepago = [S (sprintf "#line %d" (i - 1))] then
        DynArray.set commands (DynArray.length commands - 1) { DynArray.last commands with hidden = true };
          ksprintf (command c) "#line %d" i
*)

(*
  let dl = !dbl in
*)
  
  let f = (lexer
    (* ends in themselves *)
    | eof -> raise End_of_file
    | '\000' -> command { base_cmd lexbuf with is_jmp = true } "halt"

    (* function *)
    | '#' -> let offset = lexeme_start lexbuf in
             let otype = next lexbuf in
             let omodule = next lexbuf in
             let func = get_int16 lexbuf in
             let argc = get_int16 lexbuf in
             let opcode =
               { op_type = otype;
                 op_module = omodule;
                 op_function = func;
                 op_overload = next lexbuf; } in
             read_function mode version offset opcode argc lexbuf

    (* assignment *)
    | '$' -> get_assignment (base_cmd lexbuf) lexbuf

    (* debug symbols *)
    | '\n' -> let c = { base_cmd lexbuf with hidden = not options.read_debug_symbols; ctype = "dbline" } in
              let i = (if mode = `Avg2000 then get_int else get_int16) lexbuf in

              if DynArray.length commands > 0 then
                if (DynArray.last commands).kepago = [S (sprintf "#line %d" (i - 1))] then
                  DynArray.set commands (DynArray.length commands - 1) 
                    { DynArray.last commands with hidden = true };

(*
              if DynArray.length commands > 0 then
                if (DynArray.last commands).lineno = i - 1 then
                  DynArray.set commands (DynArray.length commands - 1) { DynArray.last commands with hidden = true };
*)
              (* dbl := i; *)
              ksprintf (command c) "#line %d" i
    | ',' -> let c = { base_cmd lexbuf with hidden = not options.read_debug_symbols; ctype = "debug" } in
             command c ","

    (* kidoku flag marker *)
    | '@'
    | '!' -> if lexeme_char lexbuf 0 = Char.code '!' then options.uses_excl_kidoku <- true;
             let c = base_cmd lexbuf in
             let i = (if mode = `Avg2000 then get_int else get_int16) lexbuf in
             if i < 0 || i > Array.length hdr.Bytecode.kidoku_lnums 
               then error lexbuf "kidoku flag index out of bounds";
             let l = Int32.to_int hdr.Bytecode.kidoku_lnums.(i) - 1_000_000 in
             let c, s =
               if l >= 0 then (
                 if hdr.Bytecode.entry_points.(l) <> c.offset then
                   ksprintf sysWarning ("address of entrypoint %03d " ^^ 
                     "is 0x%06x, but the header claims it's 0x%06x")
                     l c.offset hdr.Bytecode.entry_points.(l);
(*                     
                 { c with unhide = true }, sprintf "#entrypoint %03d" l
                 c, sprintf "#entrypoint %03d" l
                 c, sprintf "\n#entrypoint %03d // Z%02d\n" l l
*)
(*
                 { c with unhide = true; ctype = "entrypoint" },
                   sprintf "\n#entrypoint %03d // Z%02d\n" l l
*)
                 { c with unhide = true; ctype = "entrypoint" },
                   sprintf "#entrypoint %03d // Z%02d" l l

               )
               else { c with hidden = not options.read_debug_symbols;
                 ctype = "kidoku" }, sprintf "{- kidoku %03d -}" i
             in
             command c s

    (* textout *)
    | _ -> let c = base_cmd lexbuf in
           rollback lexbuf;
           read_textout c lexbuf) in
(*           
    try
        f lexbuf2
    with
        | Optpp.Trace (s, n) -> Optpp.contTrace s n "read_command"
*)

    try
        f lexbuf2;
        
    (*
    if dl > -1 then (
          dbl := -1; 
          DynArray.set commands (DynArray.length commands - 1) 
            { DynArray.last commands with lineno = dl; hidden = false }
        )
    *)
    
    with
        | Optpp.Trace (s, n) -> Optpp.contTrace s n "read_command"


(*
let to_tr_prep a =
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
        if i = 0x8163 then Buffer.add_string b "..."
        else if i = 0x8142 then Buffer.add_string b "."
        else if i = 0x8149 then Buffer.add_string b "!"
        else if i = 0x8148 then Buffer.add_string b "?"
        else if i = 0x8141 then Buffer.add_string b ", "
        else if i = 0x8175 then Buffer.add_string b "\""
        else if i = 0x8176 then Buffer.add_string b "\""
        else if i = 0x8177 then Buffer.add_string b "'"
        else if i = 0x8178 then Buffer.add_string b "'"
        else if i = 0x8160 then Buffer.add_string b "~"
        else (
          Buffer.add_char b (Char.unsafe_chr (0xe0 lor (i lsr 12)));
          Buffer.add_char b (Char.unsafe_chr (0x80 lor ((i lsr 6) land mask)));
          Buffer.add_char b (Char.unsafe_chr (0x80 lor (i land mask)))
        )
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
*)

(*
let to_tr_prep a = 
  Array.map
    (fun c ->
        (*if c = 0x8163 then a.[i] b "ÅE*)
        if c = 0x8163 then a.[i] b "ÅE"
        else if c = 0x8142 then 0x2E (* "." *)
        else if c = 0x8149 then 0x21 (* "!" *)
        else if c = 0x8148 then 0x3F (* b "?" *)
        else if c = 0x8141 then (* ", " *)
        else if c = 0x8175 then (* "\"" *)
        else if c = 0x8176 then (* "\"" *)
        else if c = 0x8177 then (* "'" *)
        else if c = 0x8178 then (* "'" *)
        else if c = 0x8160 then (* "~" *)
        else 
*)
        
(*
let disassemble fname (arr: Binarray.t) check =

  (* Get file header *)
  
  let rd_handler arr = 
    if !App.auto_target then App.target_version 
      := arr.{7}, arr.{6}, arr.{5}, arr.{4} in
      
  let hdr = Bytecode.read_full_header arr ~rd_handler in
  
  (* Override output encoding for non-Japanese text *)
  
  if !App.force_meta <> None || hdr.Bytecode.rldev_metadata.Metadata.text_transform <> `None then (
    if !App.verbose > 0 && Encoding.enc_type !App.enc <> `Utf8 then 
      sysInfo "Detected non-Japanese text: setting output to UTF-8";
    TextTransforms.init (Option.default hdr.Bytecode.rldev_metadata.Metadata.text_transform !App.force_meta);
    App.enc := "UTF8";
  );
  
(*
  let check = my $check = settings('action') eq 'check';
*)

  (* Get output filenames *)
  
  let bname =
    let s = Filename.basename fname in
    if Filename.check_suffix s ".uncompressed" then
      Filename.chop_suffix s ".uncompressed"
    else if Filename.check_suffix s ".rl" then
      Filename.chop_suffix s ".rl"
    else
      s
  in
  
(*
  let kename, resname =
    let base = try Filename.chop_extension bname with Invalid_argument _ -> bname in
    let base' = Filename.concat !App.outdir base in
    base' ^ ".ke",
    base' ^ (match Encoding.enc_type !App.enc with
               | `Sjs -> ".sjs"
               | `Euc -> ".euc"
               | `Utf8 -> ".utf"
               | `Other -> ".res")
*)
  let base = Filename.concat !App.outdir (
    try Filename.chop_extension bname 
    with Invalid_argument _ -> bname) 
  in
    
  let kename, resname =
    base ^ "." ^ !App.src_ext,
    base ^ (match Encoding.enc_type !App.enc with
               | `Sjs -> ".sjs"
               | `Euc -> ".euc"
               | `Utf8 -> ".utf"
               | `Other -> ".res")
  in
  
(*
  let aorg = hdr.Bytecode.data_offset in
  let amax = Binarray.dim arr in
*)
(*
  let aorg = if options.start_address >= hdr.Bytecode.data_offset 
    && options.start_address <= Binarray.dim arr then 
    options.start_address 
  else hdr.Bytecode.data_offset in
*)  
(*
  printf "aorg: %d\n" aorg;
*)
  
(*
  let amax = if options.end_address > -1 
    && options.end_address < Binarray.dim arr then
    if options.end_address > aorg then options.end_address else aorg
  else Binarray.dim arr in
*)

  let aorg = if options.start_address > hdr.Bytecode.data_offset 
    && options.start_address < Binarray.dim arr
    then options.start_address 
    else hdr.Bytecode.data_offset 
  in

  let amax = if options.end_address > -1 && 
    options.end_address < Binarray.dim arr 
    then if options.end_address > aorg  
    then options.end_address else aorg
    else Binarray.dim arr 
  in


  
(*
  printf "amax: %d\n" amax;
*)
  
  let ppos = ref aorg in
  
  (* Get target and target version *)
  
  let mode =
    if options.forced_target = `None then
      if hdr.Bytecode.header_version = 1
      then `Avg2000 else `RealLive
    else
      options.forced_target
  in
  
  let mode_version =
    if !App.target_version <> (0, 0, 0, 0) then !App.target_version
    else if hdr.Bytecode.rldev_metadata.Metadata.target_version <> (0, 0, 0, 0) then
      hdr.Bytecode.rldev_metadata.Metadata.target_version
    else match mode with
      | `Avg2000 -> 1, 0, 0, 0
      | _ -> 1, 2, 7, 0
  in

  let lexbuf =
    create
      (fun buf pos len ->
        let alen = min len (amax - !ppos) in
(*        
  printf "\n";
  printf "len: %d\n" len;
  printf "amax: %d\n" amax;
  printf "ppos: %d\n" !ppos;
  printf "alen: %d\n" alen;
  printf "\n";
*)
        if alen <= 0 then 0 else
         (if debug() > 1 then printf "-- buf %d - %d\n" !ppos (!ppos + alen);
          for i = 0 to alen - 1 do buf.(pos + i) <- arr.{!ppos + i} done;
          ppos := !ppos + alen;
          alen))
  in

  let hexd () =
    let hc = open_out (base ^ ".hex") in
    let m = Binarray.dim arr in
    (* let e = lexeme_start lexbuf in *)
    let cmd = base_cmd lexbuf in
    command cmd "hexdump";
    
    (* Output code: *)

    sysInfo "hexd() OUTPUT CODE";        
    
    DynArray.iter
      (fun cmd ->
      (* print command if visible *)
      
      (*
      if cmd.unhide && !skipping then skipping := false;
      if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
      if options.suppress_uncalled && cmd.is_jmp then skipping := true)
      *)
      
      let o = cmd.offset + !data_offset in
      let t = Binarray.read arr o (if o + 16 < m then 16 else m - o) in
      let hex, txt = Buffer.create 0, Buffer.create 0 in
(*      
      let hex = Buffer.create 0 in
      let txt = Buffer.create 0 in
*)
      
      String.iter (fun c -> 
        let i = int_of_char c in
        bprintf hex "%02x" i;
(*
        Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
*)
        Buffer.add_char txt (Char.chr (
          if i < 0x20 || i > 0x7f 
            then 0x2e else i))
      ) t;
      
(*
      fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" 
        (* o cmd.offset (Buffer.contents hex) *)
        o o (Buffer.contents hex) (Buffer.contents txt) 
          (let str = Buffer.create 0 in
          List.iter (function
            | S s | STORE s -> 
              Buffer.add_string str (Text.to_string !App.enc 
                (TextTransforms.read_cp932_compatible sysError s))
            | P i -> ()) cmd.kepago; 
          Buffer.contents str)
    ) commands;
*)

      fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" o o 
        (Buffer.contents hex) (Buffer.contents txt) 
        (String.concat "" (List.map 
          (function | S s | STORE s -> Text.to_string !App.enc 
            (TextTransforms.read_cp932_compatible sysError s)
          | P i -> "") cmd.kepago));
    ) commands;

    (try close_out hc with _ -> ())
  in
  
  let err = ref "" in
  
(*
  printf "disassembling...\n";
*)
  
  try
    reset_state ();
    data_offset := aorg;
    printf "%!";
      while true do
        let last_good = (lexeme_start lexbuf + !data_offset) - 1 in
        let abort_fun f s = (
          err := s;
          if debug() > 0 then (
            hexd ();
            f ();
                
            printf ("!!!ERROR!!!\nFailed to parse, outputting what was found.\n" ^^ 
              "Last good decode ended at 0x%06x\n!!!ERROR!!!") last_good;
                
            (*
            hexd ();
            *)
                
            raise End_of_file )
          else
            cliError s) 
        in
            
        if debug() > 1 then printf "read_command @ %d\n" last_good;
            
        try
          read_command hdr mode mode_version lexbuf        
        with
          | Optpp.Error s -> abort_fun (fun () -> cliErrorDisp s) s
          | Optpp.Trace (s, c) -> abort_fun (fun () -> printTrace s c) s
        done
  with
    End_of_file ->
      if check = true then
        if !App.verbose > 0 then
          sysInfo (sprintf "  bytecode compatibility check: %s" 
            (if !err <> "" then !err else "ok"))
        else sysInfo (if !err <> "" 
          then sprintf "  Error: %s\n" !err 
          else sprintf "%s bytecode ok\n" fname)
      else
      
        let _, labels = 
          ISet.fold(fun ptr (idx, map) -> 
            idx + 1, IMap.add ptr idx map
          ) !pointers (1, IMap.empty)
        in
        
        let skipping = ref false in
        let oc = open_out kename in
        
        let rc =
          if options.separate_strings && 
            not (DynArray.empty resstrs)
          then open_out resname
          else oc
        in
        
        begin try
          if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
            output_string oc "\xef\xbb\xbf";
          fprintf oc "{-# cp %s #- Disassembled with %s %1.2f -}\n\n#file '%s'\n"
            (String.lowercase !App.enc) App.app.name App.app.version bname;
          if oc <> rc then (
            if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
              output_string rc "\xef\xbb\xbf";
            fprintf rc "// Resources for %s\n\n" bname;
            fprintf oc "#resource '%s'\n" (Filename.basename resname)
          );
          output_char oc '\n';
          
          (match mode with
            | `Avg2000 -> output_string oc "#target AVG2000\n"
            | `Kinetic -> output_string oc "#target Kinetic\n"
            | _ -> ());
          
          if options.uses_excl_kidoku then 
            output_string oc "#kidoku_type 2\n";
          (*
          List.iter (fun s -> fprintf rc "#character '%s'\n" (Text.sjs_to_enc !App.enc s)) hdr.Bytecode.dramatis_personae;
          *)
          
(*
          if options.prep_res_file then (
            List.iter (fun s -> 
              let ss = Text.sjs_to_enc !App.enc s in
              fprintf rc "#character '%s'\t\t// '%s'\n" 
                (match Cast.get ss with | 
                  Some u -> u | None -> ss) ss)
              hdr.Bytecode.dramatis_personae
          ) else (
            List.iter (fun s -> 
              fprintf rc "#character '%s'\n" 
                (Text.sjs_to_enc !App.enc s)) 
                hdr.Bytecode.dramatis_personae;
          )
*)

(*
let get_def ?(def = key) key =
  try Cast.find key with Not_found -> def
in
*)

let get_key key =
  try Cast.find key with Not_found -> key
in

          List.iter (if options.prep_res_file then (
            fun s -> let ss = Text.sjs_to_enc !App.enc s in
              fprintf rc "#character '%s'\t\t// '%s'\n" 
                (get_key ss) ss
          ) else (
            fun s -> fprintf rc "#character '%s'\n" 
                (Text.sjs_to_enc !App.enc s)
          )) hdr.Bytecode.dramatis_personae;
          
          if rc <> oc then fprintf rc "\n";

          (* Output code: *)

          sysInfo "OUTPUT CODE 2";
          
          DynArray.iter
            (fun cmd ->
            
             (* print label if present *)
           
             (try
               let idx = IMap.find cmd.offset labels in
(*
               cmd.label = idx;
*)
               fprintf oc "\n  @%d\n" idx;
               pointers := ISet.remove cmd.offset !pointers;
               skipping := false
              with
                Not_found -> ()
              );
              
              (* print command if visible *)
            
              if cmd.unhide && !skipping then skipping := false;
              if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
              if options.suppress_uncalled && cmd.is_jmp then skipping := true
            ) commands;
            
          ISet.iter 
            (fun x -> 
              if x = amax - aorg then 
                try fprintf oc "\n  @%d\n" (IMap.find x labels) 
                with Not_found -> assert false
              else ksprintf sysWarning "label %08x not inserted" x
            ) !pointers;
          
          close_out oc;
        
          if options.hexdump then hexd ();
        
(*
          if rc <> oc then (
            DynArray.iter (fun s -> fprintf rc "%s\n" (Text.sjs_to_enc !App.enc s)) resstrs;
*)
(*
            if options.prep_res_file then
              DynArray.iter (
                if Encoding.enc_type !App.enc = `Utf8 then fun s -> 
                  fprintf rc "// %s\n%s\n" 
                    (Text.sjs_to_enc !App.enc s)
                    (Text.sjs_to_utf8_prep s)
                else fun s -> 
                  let ss = Text.sjs_to_enc !App.enc s in
                  fprintf rc "// %s\n%s\n" ss ss
              ) resstrs
            else DynArray.iter (fun s -> fprintf rc "%s\n" 
              (Text.sjs_to_enc !App.enc s)) resstrs;
*)
          if rc <> oc then (
            DynArray.iter (
              if options.prep_res_file then
                if Encoding.enc_type !App.enc = `Utf8 then fun s -> 
                  fprintf rc "// %s\n%s\n" 
                    (Text.sjs_to_enc !App.enc s)
                    (Text.sjs_to_utf8_prep s)
                else fun s -> 
                  let ss = Text.sjs_to_enc !App.enc s in
                  fprintf rc "// %s\n%s\n" ss ss
              else fun s -> fprintf rc "%s\n" 
                (Text.sjs_to_enc !App.enc s)
            ) resstrs;
            close_out rc;
          )
        with
          e -> 
            (try close_out oc with _ -> ());
            (try close_out rc with _ -> ());
            
            let hc = open_out (base ^ ".hex") in
            let max = Binarray.dim arr in
            
            (* Output code: *)
            
            sysInfo "OUTPUT CODE";
            
            DynArray.iter
              (fun cmd ->
                (* print command if visible *)
              
            (*
                if cmd.unhide && !skipping then skipping := false;
                if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
                if options.suppress_uncalled && cmd.is_jmp then skipping := true)
            *)

(*            
                let o = cmd.offset + !data_offset in
                let t = Binarray.read arr o (if o + 16 < m then 16 else m - o) in
                
                let hex = Buffer.create 0 in
                let txt = Buffer.create 0 in
              
                String.iter (fun c -> let i = int_of_char c in bprintf hex "%02x" i;
                  Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
                ) t;
*)

                
                
                let hex = Buffer.create 0 in
                let txt = Buffer.create 0 in
                let ofs = cmd.offset + !data_offset in
              
                String.iter (fun c -> let i = int_of_char c in bprintf hex "%02x" i;
                  Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
                ) (Binarray.read arr ofs (if ofs + 16 < max then 16 else max - ofs));
                
(*
                fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" 
                  o cmd.offset (Buffer.contents hex) 
                  (Buffer.contents txt) (let str = Buffer.create 0 in
                    List.iter (function
                      | S s | STORE s -> 
                        Buffer.add_string str (Text.to_string !App.enc 
                          (TextTransforms.read_cp932_compatible sysError s))
                      | P i -> ()) cmd.kepago; 
                    Buffer.contents str)
*)

                fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" ofs cmd.offset 
                  (Buffer.contents hex) (Buffer.contents txt)
                  (String.concat "" (List.map (function
                    | S s | STORE s -> Text.to_string !App.enc 
                      (TextTransforms.read_cp932_compatible sysError s)
                    | P i -> sprintf " ptr(%d)" i) cmd.kepago)
                  )
              ) commands;
            
            (try close_out hc with _ -> ());
            raise e
        end;
*)

(*
let disassemble fname (arr: Binarray.t) check =

  (* Get file header *)
  
  let rd_handler arr = 
    if !App.auto_target then App.target_version 
      := arr.{7}, arr.{6}, arr.{5}, arr.{4} in
      
  let hdr = Bytecode.read_full_header arr ~rd_handler in
  
  (* Override output encoding for non-Japanese text *)
  
  if !App.force_meta <> None || hdr.Bytecode.rldev_metadata.Metadata.text_transform <> `None then (
    if !App.verbose > 0 && Encoding.enc_type !App.enc <> `Utf8 then 
      sysInfo "Detected non-Japanese text: setting output to UTF-8";
    TextTransforms.init (Option.default hdr.Bytecode.rldev_metadata.Metadata.text_transform !App.force_meta);
    App.enc := "UTF8";
  );
  
(*
  let check = my $check = settings('action') eq 'check';
*)

  (* Get output filenames *)

  let bname =
    let s = Filename.basename fname in
    if Filename.check_suffix s ".uncompressed" then
      Filename.chop_suffix s ".uncompressed"
    else if Filename.check_suffix s ".rl" then
      Filename.chop_suffix s ".rl"
    else
      s
  in
  
  let base = Filename.concat !App.outdir (
    try Filename.chop_extension bname 
    with Invalid_argument _ -> bname) 
  in
    
  let kename, resname = base ^ "." ^ !App.src_ext,
    base ^ (match Encoding.enc_type !App.enc with
      | `Sjs -> ".sjs"
      | `Euc -> ".euc"
      | `Utf8 -> ".utf"
      | `Other -> ".res")
  in
  
  let aorg = if options.start_address > hdr.Bytecode.data_offset 
    && options.start_address < Binarray.dim arr
    then options.start_address 
    else hdr.Bytecode.data_offset 
  in

  let amax = if options.end_address > -1 && 
    options.end_address < Binarray.dim arr 
    then if options.end_address > aorg  
    then options.end_address else aorg
    else Binarray.dim arr 
  in

  let ppos = ref aorg in
  
  (* Get target and target version *)
  
  let mode =
    if options.forced_target = `None then
      if hdr.Bytecode.header_version = 1
      then `Avg2000 else `RealLive
    else
      options.forced_target
  in
  
  let mode_version =
    if !App.target_version <> (0, 0, 0, 0) then !App.target_version
    else if hdr.Bytecode.rldev_metadata.Metadata.target_version <> (0, 0, 0, 0) then
      hdr.Bytecode.rldev_metadata.Metadata.target_version
    else match mode with
      | `Avg2000 -> 1, 0, 0, 0
      | _ -> 1, 2, 7, 0
  in

  let lexbuf =
    create
      (fun buf pos len ->
        let alen = min len (amax - !ppos) in
        if alen <= 0 then 0 else
         (if debug() > 1 then printf "-- buf %d - %d\n" !ppos (!ppos + alen);
          for i = 0 to alen - 1 do buf.(pos + i) <- arr.{!ppos + i} done;
          ppos := !ppos + alen;
          alen))
  in

  let hexd () =
    let hc = open_out (base ^ ".hex") in
    let m = Binarray.dim arr in
    (* let e = lexeme_start lexbuf in *)
    let cmd = base_cmd lexbuf in
    command cmd "hexdump";
    
    (* Output code: *)

    sysInfo "hexd() OUTPUT CODE";        
    
    DynArray.iter
      (fun cmd ->
      (* print command if visible *)
      
      (*
      if cmd.unhide && !skipping then skipping := false;
      if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
      if options.suppress_uncalled && cmd.is_jmp then skipping := true)
      *)
      
      let o = cmd.offset + !data_offset in
      let t = Binarray.read arr o (if o + 16 < m then 16 else m - o) in
      let hex, txt = Buffer.create 0, Buffer.create 0 in
(*      
      let hex = Buffer.create 0 in
      let txt = Buffer.create 0 in
*)
      
      String.iter (fun c -> 
        let i = int_of_char c in
        bprintf hex "%02x" i;
(*
        Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
*)
        Buffer.add_char txt (Char.chr (
          if i < 0x20 || i > 0x7f 
            then 0x2e else i))
      ) t;
      
(*
      fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" 
        (* o cmd.offset (Buffer.contents hex) *)
        o o (Buffer.contents hex) (Buffer.contents txt) 
          (let str = Buffer.create 0 in
          List.iter (function
            | S s | STORE s -> 
              Buffer.add_string str (Text.to_string !App.enc 
                (TextTransforms.read_cp932_compatible sysError s))
            | P i -> ()) cmd.kepago; 
          Buffer.contents str)
    ) commands;
*)

      fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" o o 
        (Buffer.contents hex) (Buffer.contents txt) 
        (String.concat "" (List.map 
          (function | S s | STORE s -> Text.to_string !App.enc 
            (TextTransforms.read_cp932_compatible sysError s)
          | P i -> "") cmd.kepago));
    ) commands;

    (try close_out hc with _ -> ())
  in
  
  let err = ref "" in
  
(*
  printf "disassembling...\n";
*)
  
  try
    reset_state ();
    data_offset := aorg;
    printf "%!";
      while true do
        let last_good = (lexeme_start lexbuf + !data_offset) - 1 in
        let abort_fun f s = (
          err := s;
          if debug() > 0 then (
            hexd ();
            f ();
                
            printf ("!!!ERROR!!!\nFailed to parse, outputting what was found.\n" ^^ 
              "Last good decode ended at 0x%06x\n!!!ERROR!!!") last_good;
                
            raise End_of_file )
          else
            cliError s) 
        in
            
        if debug() > 1 then printf "read_command @ %d\n" last_good;
            
        try
          read_command hdr mode mode_version lexbuf        
        with
          | Optpp.Error s -> abort_fun (fun () -> cliErrorDisp s) s
          | Optpp.Trace (s, c) -> abort_fun (fun () -> printTrace s c) s
        done
  with
    End_of_file ->
      if check = true then
        let rlen = 
          if !err then
            let len = ref 0 in
            DynArray.iter (
              fun s -> len := !len + String.length 
                (Text.sjs_to_enc !App.enc s)
            ) resstrs;
            !len
          else -1
          
        if !App.verbose > 0 then
          sysInfo (sprintf "  bytecode compatibility check: %s" 
            (if !err <> "" then !err else "ok"))
        else sysInfo (if !err <> "" 
          then sprintf "  Error: %s\n" !err 
          else sprintf "%s bytecode ok\n" fname);
          
        !rlen
      else
      
        let _, labels = 
          ISet.fold(fun ptr (idx, map) -> 
            idx + 1, IMap.add ptr idx map
          ) !pointers (1, IMap.empty)
        in
        
        let skipping = ref false in
        let oc = open_out kename in
        
        let rc =
          if options.separate_strings && 
            not (DynArray.empty resstrs)
          then open_out resname
          else oc
        in
        
        begin try
          if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
            output_string oc "\xef\xbb\xbf";
          fprintf oc "{-# cp %s #- Disassembled with %s %1.2f -}\n\n#file '%s'\n"
            (String.lowercase !App.enc) App.app.name App.app.version bname;
          if oc <> rc then (
            if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
              output_string rc "\xef\xbb\xbf";
            fprintf rc "// Resources for %s\n\n" bname;
            fprintf oc "#resource '%s'\n" (Filename.basename resname)
          );
          output_char oc '\n';
          
          (match mode with
            | `Avg2000 -> output_string oc "#target AVG2000\n"
            | `Kinetic -> output_string oc "#target Kinetic\n"
            | _ -> ());
          
          if options.uses_excl_kidoku then 
            output_string oc "#kidoku_type 2\n";
          (*
          List.iter (fun s -> fprintf rc "#character '%s'\n" (Text.sjs_to_enc !App.enc s)) hdr.Bytecode.dramatis_personae;
          *)
          
(*
          if options.prep_res_file then (
            List.iter (fun s -> 
              let ss = Text.sjs_to_enc !App.enc s in
              fprintf rc "#character '%s'\t\t// '%s'\n" 
                (match Cast.get ss with | 
                  Some u -> u | None -> ss) ss)
              hdr.Bytecode.dramatis_personae
          ) else (
            List.iter (fun s -> 
              fprintf rc "#character '%s'\n" 
                (Text.sjs_to_enc !App.enc s)) 
                hdr.Bytecode.dramatis_personae;
          )
*)

(*
let get_def ?(def = key) key =
  try Cast.find key with Not_found -> def
in
*)

let get_key key =
  try Cast.find key with Not_found -> key
in

          List.iter (if options.prep_res_file then (
            fun s -> let ss = Text.sjs_to_enc !App.enc s in
              fprintf rc "#character '%s'\t\t// '%s'\n" 
                (get_key ss) ss
          ) else (
            fun s -> fprintf rc "#character '%s'\n" 
                (Text.sjs_to_enc !App.enc s)
          )) hdr.Bytecode.dramatis_personae;
          
          if rc <> oc then fprintf rc "\n";

          (* Output code: *)

          sysInfo "OUTPUT CODE 1";
          
          DynArray.iter
            (fun cmd ->
            
             (* print label if present *)
           
             (try
               let idx = IMap.find cmd.offset labels in
(*
               cmd.label = idx;
*)
               fprintf oc "\n  @%d\n" idx;
               pointers := ISet.remove cmd.offset !pointers;
               skipping := false
              with
                Not_found -> ()
              );
              
              (* print command if visible *)

            (*
               fprintf oc "  kepago: %d\n" (List.length cmd.kepago);
            *)
            
            (*
              sysInfo "call print_cmd()";
            *)
            
              (*
              print_cmd labels oc cmd;
              *)
(*
              sysInfo (sprintf "<%d|%d>" cmd.offset cmd.lineno);
              
              sysInfo "-----";

              fprintf oc "<%d|%d>" cmd.offset cmd.lineno;
*)          
              if cmd.unhide && !skipping then skipping := false;
              if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
              if options.suppress_uncalled && cmd.is_jmp then skipping := true;

(*
              sysInfo "=====";
              
              fprintf oc " - %s\n" cmd.opcode;

              sysInfo (sprintf " - %s\n" cmd.opcode);
*)                            
            ) commands;
            
          ISet.iter 
            (fun x -> 
              if x = amax - aorg then 
                try fprintf oc "\n  @%d\n" (IMap.find x labels) 
                with Not_found -> assert false
              else ksprintf sysWarning "label %08x not inserted" x
            ) !pointers;
          
          close_out oc;
        
          if options.hexdump then hexd ();
        
(*
          if rc <> oc then (
            DynArray.iter (fun s -> fprintf rc "%s\n" (Text.sjs_to_enc !App.enc s)) resstrs;
*)
(*
            if options.prep_res_file then
              DynArray.iter (
                if Encoding.enc_type !App.enc = `Utf8 then fun s -> 
                  fprintf rc "// %s\n%s\n" 
                    (Text.sjs_to_enc !App.enc s)
                    (Text.sjs_to_utf8_prep s)
                else fun s -> 
                  let ss = Text.sjs_to_enc !App.enc s in
                  fprintf rc "// %s\n%s\n" ss ss
              ) resstrs
            else DynArray.iter (fun s -> fprintf rc "%s\n" 
              (Text.sjs_to_enc !App.enc s)) resstrs;
*)
          if rc <> oc then (
            DynArray.iter (
              if options.prep_res_file then
                if Encoding.enc_type !App.enc = `Utf8 then fun s -> 
                  fprintf rc "// %s\n%s\n" 
                    (Text.sjs_to_enc !App.enc s)
                    (Text.sjs_to_utf8_prep s)
                else fun s -> 
                  let ss = Text.sjs_to_enc !App.enc s in
                  fprintf rc "// %s\n%s\n" ss ss
              else fun s -> fprintf rc "%s\n" 
                (Text.sjs_to_enc !App.enc s)
            ) resstrs;
            close_out rc;
          )
        with
          e -> 
            (try close_out oc with _ -> ());
            (try close_out rc with _ -> ());
            
            let hc = open_out (base ^ ".hex") in
            let max = Binarray.dim arr in
            
            (* Output code: *)
            
            sysInfo "OUTPUT CODE 2";
            
            DynArray.iter
              (fun cmd ->
                (* print command if visible *)
              
            (*
                if cmd.unhide && !skipping then skipping := false;
                if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
                if options.suppress_uncalled && cmd.is_jmp then skipping := true)
            *)

(*            
                let o = cmd.offset + !data_offset in
                let t = Binarray.read arr o (if o + 16 < m then 16 else m - o) in
                
                let hex = Buffer.create 0 in
                let txt = Buffer.create 0 in
              
                String.iter (fun c -> let i = int_of_char c in bprintf hex "%02x" i;
                  Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
                ) t;
*)

                
                
                let hex = Buffer.create 0 in
                let txt = Buffer.create 0 in
                let ofs = cmd.offset + !data_offset in
              
                String.iter (fun c -> let i = int_of_char c in bprintf hex "%02x" i;
                  Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
                ) (Binarray.read arr ofs (if ofs + 16 < max then 16 else max - ofs));
                
(*
                fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" 
                  o cmd.offset (Buffer.contents hex) 
                  (Buffer.contents txt) (let str = Buffer.create 0 in
                    List.iter (function
                      | S s | STORE s -> 
                        Buffer.add_string str (Text.to_string !App.enc 
                          (TextTransforms.read_cp932_compatible sysError s))
                      | P i -> ()) cmd.kepago; 
                    Buffer.contents str)
*)

                fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" ofs cmd.offset 
                  (Buffer.contents hex) (Buffer.contents txt)
                  (String.concat "" (List.map (function
                    | S s | STORE s -> Text.to_string !App.enc 
                      (TextTransforms.read_cp932_compatible sysError s)
                    | P i -> sprintf " ptr(%d)" i) cmd.kepago)
                  )
              ) commands;
            
            (try close_out hc with _ -> ());
            raise e
        end;
*)

let get_base fname =
  let bname =
    let s = Filename.basename fname in
    if Filename.check_suffix s ".uncompressed" then
      Filename.chop_suffix s ".uncompressed"
    else if Filename.check_suffix s ".rl" then
      Filename.chop_suffix s ".rl"
    else
      s
  in
  
  let base = Filename.concat !App.outdir (
    try Filename.chop_extension bname 
    with Invalid_argument _ -> bname) 
  in
  
  base,bname


let read_header arr = 
  let rd_handler arr = 
    if !App.auto_target then App.target_version 
      := arr.{7}, arr.{6}, arr.{5}, arr.{4} in
      
  Bytecode.read_full_header arr ~rd_handler


(*
let write_hexdump fname arr lexbuf =
*)

let write_hexdump fname arr =
  let base,_ = get_base fname in
  let hc = open_out (base ^ ".hex") in
  let m = Binarray.dim arr in
  (* let e = lexeme_start lexbuf in *)
(*
  let cmd = base_cmd lexbuf in
  command cmd "hexdump";
*)
    
  (* Output code: *)

  sysInfo "hexd() OUTPUT CODE";        
    
  DynArray.iter
    (fun cmd ->
    (* print command if visible *)
      
    (*
    if cmd.unhide && !skipping then skipping := false;
    if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
    if options.suppress_uncalled && cmd.is_jmp then skipping := true)
    *)
      
    let o = cmd.offset + !data_offset in
    let t = Binarray.read arr o (if o + 16 < m then 16 else m - o) in
    let hex, txt = Buffer.create 0, Buffer.create 0 in
      
    String.iter (fun c -> 
      let i = int_of_char c in
      bprintf hex "%02x" i;
      Buffer.add_char txt (Char.chr (
        if i < 0x20 || i > 0x7f 
          then 0x2e else i))
    ) t;
      
    fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" o o 
      (Buffer.contents hex) (Buffer.contents txt) 
      (String.concat "" (List.map 
        (function | S s | STORE s -> Text.to_string !App.enc 
          (TextTransforms.read_cp932_compatible sysError s)
        | P i -> sprintf " @ptr(%d)" (i + !data_offset)) cmd.kepago));
  ) commands;

  (try close_out hc with _ -> ())


let disassemble fname (arr: Binarray.t) =

  (* Get file header *)
  
(*
  let rd_handler arr = 
    if !App.auto_target then App.target_version 
      := arr.{7}, arr.{6}, arr.{5}, arr.{4} in
      
  let hdr = Bytecode.read_full_header arr ~rd_handler in
*)

  let hdr = read_header arr in
  
  (* Override output encoding for non-Japanese text *)
  
  if !App.force_meta <> None || hdr.Bytecode.rldev_metadata.Metadata.text_transform <> `None then (
    if !App.verbose > 0 && Encoding.enc_type !App.enc <> `Utf8 then 
      sysInfo "Detected non-Japanese text: setting output to UTF-8";
    TextTransforms.init (Option.default hdr.Bytecode.rldev_metadata.Metadata.text_transform !App.force_meta);
    App.enc := "UTF8";
  );
  
(*
  let check = my $check = settings('action') eq 'check';
*)

  (* Get output filenames *)
(*  
  let bname =
    let s = Filename.basename fname in
    if Filename.check_suffix s ".uncompressed" then
      Filename.chop_suffix s ".uncompressed"
    else if Filename.check_suffix s ".rl" then
      Filename.chop_suffix s ".rl"
    else
      s
  in
  
  let base = Filename.concat !App.outdir (
    try Filename.chop_extension bname 
    with Invalid_argument _ -> bname) 
  in
    
  let kename, resname = base ^ "." ^ !App.src_ext,
    base ^ (match Encoding.enc_type !App.enc with
      | `Sjs -> ".sjs"
      | `Euc -> ".euc"
      | `Utf8 -> ".utf"
      | `Other -> ".res")
  in
*)
  
  let aorg = if options.start_address > hdr.Bytecode.data_offset 
    && options.start_address < Binarray.dim arr
    then options.start_address 
    else hdr.Bytecode.data_offset 
  in

  let amax = if options.end_address > -1 && 
    options.end_address < Binarray.dim arr 
    then if options.end_address > aorg  
    then options.end_address else aorg
    else Binarray.dim arr 
  in

  let ppos = ref aorg in
  
  (* Get target and target version *)
  
  let mode =
    if options.forced_target = `None then
      if hdr.Bytecode.header_version = 1
      then `Avg2000 else `RealLive
    else
      options.forced_target
  in
  
  let mode_version =
    if !App.target_version <> (0, 0, 0, 0) then !App.target_version
    else if hdr.Bytecode.rldev_metadata.Metadata.target_version <> (0, 0, 0, 0) then
      hdr.Bytecode.rldev_metadata.Metadata.target_version
    else match mode with
      | `Avg2000 -> 1, 0, 0, 0
      | _ -> 1, 2, 7, 0
  in

  let lexbuf =
    create
      (fun buf pos len ->
        let alen = min len (amax - !ppos) in
        if alen <= 0 then 0 else
         (if debug() > 2 then printf "-- buf %d - %d\n" !ppos (!ppos + alen);
          for i = 0 to alen - 1 do buf.(pos + i) <- arr.{!ppos + i} done;
          ppos := !ppos + alen;
          alen))
  in

(*
  let hexd () =
    let hc = open_out (base ^ ".hex") in
    let m = Binarray.dim arr in
    (* let e = lexeme_start lexbuf in *)
    let cmd = base_cmd lexbuf in
    command cmd "hexdump";
    
    (* Output code: *)

    sysInfo "hexd() OUTPUT CODE";        
    
    DynArray.iter
      (fun cmd ->
      (* print command if visible *)
      
      (*
      if cmd.unhide && !skipping then skipping := false;
      if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
      if options.suppress_uncalled && cmd.is_jmp then skipping := true)
      *)
      
      let o = cmd.offset + !data_offset in
      let t = Binarray.read arr o (if o + 16 < m then 16 else m - o) in
      let hex, txt = Buffer.create 0, Buffer.create 0 in
(*      
      let hex = Buffer.create 0 in
      let txt = Buffer.create 0 in
*)
      
      String.iter (fun c -> 
        let i = int_of_char c in
        bprintf hex "%02x" i;
(*
        Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
*)
        Buffer.add_char txt (Char.chr (
          if i < 0x20 || i > 0x7f 
            then 0x2e else i))
      ) t;
      
(*
      fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" 
        (* o cmd.offset (Buffer.contents hex) *)
        o o (Buffer.contents hex) (Buffer.contents txt) 
          (let str = Buffer.create 0 in
          List.iter (function
            | S s | STORE s -> 
              Buffer.add_string str (Text.to_string !App.enc 
                (TextTransforms.read_cp932_compatible sysError s))
            | P i -> ()) cmd.kepago; 
          Buffer.contents str)
    ) commands;
*)

      fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" o o 
        (Buffer.contents hex) (Buffer.contents txt) 
        (String.concat "" (List.map 
          (function | S s | STORE s -> Text.to_string !App.enc 
            (TextTransforms.read_cp932_compatible sysError s)
          | P i -> "") cmd.kepago));
    ) commands;

    (try close_out hc with _ -> ())
  in
*)
  
  let err = ref "" in
  
(*
  printf "disassembling...\n";
*)
  
  try
    reset_state ();
    data_offset := aorg;
    printf "%!";
    while true do
      let last_good = (lexeme_start lexbuf + !data_offset) - 1 in
      let abort_fun f s = (
        err := s;
        if debug() > 0 then (
          write_hexdump fname arr;
          (* hexd (); *)
          f ();
                
          printf ("!!!ERROR!!!\nFailed to parse, outputting what was found.\n" ^^ 
            "Last good decode ended at 0x%06x\n!!!ERROR!!!") last_good;
                
          raise End_of_file)
        else
          cliError s)
      in
            
      if debug() > 2 then printf "read_command @ %d\n" last_good;
            
      try
        read_command hdr mode mode_version lexbuf        
      with
        | Optpp.Error s -> abort_fun (fun () -> cliErrorDisp s) s
        | Optpp.Trace (s, c) -> abort_fun (fun () -> printTrace s c) s
    done;
        
    !err
  with
    End_of_file -> !err


let check fname (arr: Binarray.t) =
  let err = disassemble fname arr in
  
(*  
  let err = try
    disassemble fname arr
  with
    End_of_file -> ""
  in
*)
  
  if !App.verbose > 0 then
    sysInfo (sprintf "  bytecode compatibility check: %s" 
      (if err <> "" then err else "ok"))
  else sysInfo (if err <> "" 
    then sprintf "  Error: %s\n" err 
    else sprintf "%s bytecode ok\n" fname);
      
  err


let source_info fname (arr: Binarray.t) =
  options.control_codes <- false;
  options.raw_strings <- true;
  
  let err = disassemble fname arr in

  let rlin,rlen = 
    if err = "" then (
      let lin = ref 0 in
      let len = ref 0 in
      DynArray.iter (
        fun s -> 
(*
          len := !len + String.length (Str.global_replace (Str.regexp "\\{[^{]*}") "" s)
*)
          let slen = (String.length s) - ((String.index s ' ') + 1) in
          if slen > 0 then incr lin;
          len := !len + slen
          
          (*
          len := !len + String.length s
          *)
          
          (* (Text.sjs_to_enc !App.enc s) *)
      ) resstrs;
      !lin,!len
    ) else -1,-1
  in
  rlin,rlen


  
(*
  (DynArray.length resstrs),rlen
*)


let format_cmd labels cmd =  
  let s = String.concat "" (List.map (function
    | S s | STORE s -> 
      if (try String.sub s 0 6 = "#line " with _ -> false) 
        then "" else Text.to_string !App.enc 
        (TextTransforms.read_cp932_compatible sysError s)
    | P i -> try sprintf " @%d" (IMap.find i labels)
      with Not_found -> sprintf " @unknown%d" i
    ) cmd.kepago)
  in
  
  if s <> "" then (
    (if options.annotate then sprintf "{-%08x-} " 
      (cmd.offset + !data_offset) else "") ^
    (if options.read_debug_symbols && cmd.ctype = "dbline" 
      then sprintf "#line %d " cmd.lineno else "") ^ s ^
    (if options.opcodes then (
      (if cmd.opcode <> "" || cmd.ctype <> "" then " // " else "") ^ 
      (if cmd.opcode <> "" then cmd.opcode else "") ^
      (if cmd.ctype <> "" then " ctype: " ^ cmd.ctype else "")
      ) else "")
    ^ "\n"
  ) else s


(*
let write_source_alt fname (arr: Binarray.t) =
  let hdr = read_header arr in
  
  (* Get output filenames *)
  
(*
  let bname =
    let s = Filename.basename fname in
    if Filename.check_suffix s ".uncompressed" then
      Filename.chop_suffix s ".uncompressed"
    else if Filename.check_suffix s ".rl" then
      Filename.chop_suffix s ".rl"
    else
      s
  in
  
  let base = Filename.concat !App.outdir (
    try Filename.chop_extension bname 
    with Invalid_argument _ -> bname) 
  in
*)

  let base,bname = get_base fname in
  let idx = int_of_string (String.sub fname 4 4) in
      
  let kename, resname = base ^ "." ^ !App.src_ext,
    base ^ (match Encoding.enc_type !App.enc with
      | `Sjs -> ".sjs"
      | `Euc -> ".euc"
      | `Utf8 -> ".utf"
      | `Other -> ".res")
  in
  
  let aorg = if options.start_address > hdr.Bytecode.data_offset 
    && options.start_address < Binarray.dim arr
    then options.start_address 
    else hdr.Bytecode.data_offset 
  in

  let amax = if options.end_address > -1 && 
    options.end_address < Binarray.dim arr 
    then if options.end_address > aorg  
    then options.end_address else aorg
    else Binarray.dim arr 
  in

  let _, labels = 
    ISet.fold(fun ptr (idx, map) -> 
      idx + 1, IMap.add ptr idx map
    ) !pointers (1, IMap.empty)
  in

  let skipping = ref false in
  let oc = open_out kename in

  let rc =
    if options.separate_strings && 
      not (DynArray.empty resstrs)
    then open_out resname
    else oc
  in

(*
  let count_lines str =    
    let rec loop s l = 
      try
        let i = String.index s '\n' in
        loop (String.sub s (i + 1) ((String.length s) - (i + 1))) (l + 1)
      with Not_found -> l
    in
    loop str 0
  in
*)

  let count_lines str =    
    let rec loop s l = 
      try
        let i = String.index s '\n' + 1 in
        loop (String.sub s i ((String.length s) - i)) (l + 1)
      with Not_found -> l
    in
    loop str 0
  in

  let line = ref 1 in
  
(*
  let write_string oc str =
    output_string oc str;
    line := !line + count_lines str
  in
*)

  let write_string str =
    output_string oc str;
    line := !line + count_lines str
  in
  
(*
  let fwritef oc fmt args =
    line := !line + count_lines (fprintf oc fmt args)
  in
*)

(*
  let fwritef oc fmt args =
    line := !line + count_lines (fprintf oc fmt args)
  in
*)
    
(*    
  let fwritef oc fmt args =
    let str = fprintf oc fmt args in
    let rec loop s l = 
      try
        let i = String.index s '\n' in
        loop (String.sub s (i + 1) ((String.length s) - (i + 1))) l + 1
      with Not_found -> l
    in
    
    line := line + (loop str 0)
  in
*)

(*
  fprintf : out_channel ->
       ('a, out_channel, unit) format -> 'a
*)       
       
  begin try
    if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
      output_string oc "\xef\xbb\xbf";
    ksprintf write_string "{-# cp %s #- Disassembled with %s %1.2f -}\n\n#file '%s'\n"
      (String.lowercase !App.enc) App.app.name App.app.version bname;
(*
    ksprintf (write_string oc) "{-# cp %s #- Disassembled with %s %1.2f -}\n\n#file '%s'\n"
      (String.lowercase !App.enc) App.app.name App.app.version bname;
    fwritef oc "{-# cp %s #- Disassembled with %s %1.2f -}\n\n#file '%s'\n"
      (String.lowercase !App.enc) App.app.name App.app.version bname;
*)
    if oc <> rc then (
      if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
        output_string rc "\xef\xbb\xbf";
      fprintf rc "// Resources for %s\n\n" bname;
      ksprintf write_string "#resource '%s'\n" 
        (Filename.basename resname)
(*
      ksprintf (write_string oc) "#resource '%s'\n" (Filename.basename resname)
      fwritef oc "#resource '%s'\n" (Filename.basename resname)
*)
    );
    
    output_char oc '\n';  incr line;

    let mode =
      if options.forced_target = `None then
        if hdr.Bytecode.header_version = 1
        then `Avg2000 else `RealLive
      else
        options.forced_target
    in
    
    (match mode with
      | `Avg2000 -> output_string oc "#target AVG2000\n"; incr line
      | `Kinetic -> output_string oc "#target Kinetic\n"; incr line
      | _ -> ());
  
    if options.uses_excl_kidoku then 
(*
      write_string oc "#kidoku_type 2\n";
*)
      output_string oc "#kidoku_type 2\n"; incr line;
      
    (*
    List.iter (fun s -> fprintf rc "#character '%s'\n" (Text.sjs_to_enc !App.enc s)) hdr.Bytecode.dramatis_personae;
    *)
  

    let get_key key =
      try Cast.find key with Not_found -> key
    in

    List.iter (if options.prep_res_file then (
      fun s -> let ss = Text.sjs_to_enc !App.enc s in
        fprintf rc "#character '%s'\t\t// '%s'\n" 
          (get_key ss) ss
    ) else (
      fun s -> fprintf rc "#character '%s'\n" 
        (Text.sjs_to_enc !App.enc s)
    )) hdr.Bytecode.dramatis_personae;
  
    if rc <> oc then fprintf rc "\n";

    (* Output code: *)

(*
    sysInfo "OUTPUT CODE 1";
*)
  
    DynArray.iter
      (fun cmd ->
    
       (* print label if present *)
   
       (try
         let idx = IMap.find cmd.offset labels in
        (*
         cmd.label = idx;
        *)
         
         ksprintf write_string "\n  @%d\n" idx;
         pointers := ISet.remove cmd.offset !pointers;
         skipping := false
        with
          Not_found -> ()
        );
      
        (* print command if visible *)

        if cmd.unhide && !skipping then skipping := false;
(*
        if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
*)
        cmd.lineno <- !line;
        
(*
        let str = format_cmd labels cmd in
        
        if String.sub str 0 12 = "#entrypoint " then
          seen_map.entries entries.
*)

(*
        if cmd.ctype = "kidoku" then
          DynArray.add seen_map.entrypoints 
            { seen = seen; line = line };
*)

(*
        if cmd.ctype = "kidoku" then
          DynArray.add !map.entrypoints !line
        if cmd.ctype = "entrypoint" then
          DynArray.add !map.entrypoints !line;
          output_char oc '\n'; incr line
*)
        if cmd.ctype = "entrypoint" then
          DynArray.add !map.entrypoints !line
        else if cmd.ctype = "call" || cmd.ctype = "jump" then (
          let s,e = if List.length cmd.args > 0 then
            (try int_of_string (List.nth cmd.args 0) with _ -> -1),
            if List.length cmd.args > 1 then
            (try int_of_string (List.nth cmd.args 1) with _ -> -1)
            else -1
            else -1,-1
          in
            
          (* need to handle variable parameters, eg 7030 $03f farcall(intD[5], intD[6]) *)
          
          if debug () > 1 then 
            sysInfo (sprintf "Seen%04d %-12s Seen%04d%s" 
              s cmd.ident s (if e > -1 then 
               (sprintf "(Z%02d)" e) else ""));

          DynArray.add (if cmd.ctype = "call" then 
            !map.calls else !map.gotos) { 
            origin = { seen = idx; line = !line };
            target = { scene = s; entry = e };
            kind = cmd.ident };
        );

        if not (!skipping || cmd.hidden) then (
          if cmd.ctype = "entrypoint" then (
            output_char oc '\n'; incr line;
            write_string (format_cmd labels cmd);
            output_char oc '\n'; incr line
          ) else write_string (format_cmd labels cmd)
          (*
          write_string (format_cmd labels cmd);
          if cmd.ctype = "entrypoint" then 
            output_char oc '\n'; incr line
          *)
        );
          
        if options.suppress_uncalled && 
          cmd.is_jmp then skipping := true;
      ) commands;

(*
          let jump = { empty_jump with 
            origin = { seen = idx; line = !line };
            target = { scene = s; entry = e };
            kind = cmd.ident }
          in
*)
        
(*
          match tl cmd.kepago with | FP s,e -> {
            origin = { seen = idx; line = !line };
            target = { scene = s; entry = e };
            kind = cmd.ident }
*)

(*
            DynArray.add (if cmd.ctype = "call" then 
              seen_map.calls else seen_map.gotos) 
*)              
(*
              let jump =
                (let j = { empty_jump with origin = 
                  { seen = idx; line = !line };
                  kind = cmd.ident } in

(*                  
                match cmd with 
                  | { kepago = _ _ :: FP (s,e) } -> 
                    { j with target = { scene = s; entry = e } }
                  | _ -> j)
              in
*)            
          
                 match tl cmd.kepago with 
(*
                  | FP (s,e) -> { j with target = 
                      { scene = s; entry = e } }
*)
(*
                  | FP fp -> let s,e = (int_of_string fp),-1 in
                      { j with target = { scene = s; entry = e } }
*)
                  | FP s -> let e = -1 in
                      { j with target = { scene = s; entry = e } }
                  | _ -> j)
              in
*)

(*
              DynArray.add (if cmd.ctype = "call" then 
                seen_map.calls else seen_map.gotos) jump;
*)
        (*
          { empty_jump with target = { scene = s; entry = e } }
            kind = cmd.ident
          
          DynArray.add (if cmd.ctype = "call" then 
            seen_map.calls else seen_map.gotos) 
            { empty_jump with target = { scene = scene; 
              entry = entry }; kind = cmd.ident };
        *)

(*
        else if cmd.type = ""
*)

                
(*        
        if not (!skipping || cmd.hidden) then write_string (format_cmd labels cmd);
        if options.suppress_uncalled && cmd.is_jmp then skipping := true;
      ) commands;
*)
    
    ISet.iter 
      (fun x -> 
        if x = amax - aorg then 
          try ksprintf write_string "\n  @%d\n" (IMap.find x labels) 
          with Not_found -> assert false
        else ksprintf sysWarning "label %08x not inserted" x
      ) !pointers;
  
    close_out oc;

(*
    if options.hexdump then hexd ();
*)

    if options.hexdump then write_hexdump fname arr;

    if rc <> oc then (
      DynArray.iter (
        if options.prep_res_file then
          if Encoding.enc_type !App.enc = `Utf8 then fun s -> 
            fprintf rc "// %s\n%s\n" 
              (Text.sjs_to_enc !App.enc s)
              (Text.sjs_to_utf8_prep s)
          else fun s -> 
            let ss = Text.sjs_to_enc !App.enc s in
            fprintf rc "// %s\n%s\n" ss ss
        else fun s -> fprintf rc "%s\n" 
          (Text.sjs_to_enc !App.enc s)
      ) resstrs;
      close_out rc;
    )
  with
    e -> 
      (try close_out oc with _ -> ());
      (try close_out rc with _ -> ());
      
      write_hexdump fname arr;
    
(*
      let hc = open_out (base ^ ".hex") in
      let max = Binarray.dim arr in
    
      (* Output hexdump: *)
    
      (*
      sysInfo "OUTPUT CODE 2";
      *)
    
      DynArray.iter
        (fun cmd ->
          (* print command if visible *)
        
          let hex = Buffer.create 0 in
          let txt = Buffer.create 0 in
          let ofs = cmd.offset + !data_offset in
      
          String.iter (fun c -> let i = int_of_char c in bprintf hex "%02x" i;
            Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
          ) (Binarray.read arr ofs (if ofs + 16 < max then 16 else max - ofs));
        
          fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" ofs cmd.offset 
            (Buffer.contents hex) (Buffer.contents txt)
            (String.concat "" (List.map (function
              | S s | STORE s -> Text.to_string !App.enc 
                (TextTransforms.read_cp932_compatible sysError s)
              | P i -> sprintf " ptr(%d)" i
            ) cmd.kepago))
        ) commands;
(*
              | FP i -> sprintf " fptr(%d)" i) cmd.kepago))
        ) commands;
*)
(*
              | P i -> sprintf " ptr(%d)" i
*)
(*
              | FP (s,e) -> "") cmd.kepago)
*)
(*
              | FP fp -> "") cmd.kepago)
*)
(*
              | FP i -> sprintf " fptr(%d)" i)
            ) cmd.kepago)
        ) commands;
*)
    
      (try close_out hc with _ -> ());
*)
      raise e
  end
*)

(* generate source file from disassembly result *)

let write_source fname (arr: Binarray.t) =
  let hdr = read_header arr in
  
  (* Get output filenames *)
  
  let base,bname = get_base fname in
  
(*
  let idx = int_of_string (String.sub fname 4 4) in
*)
      
  let kename, resname = base ^ "." ^ !App.src_ext,
    base ^ (match Encoding.enc_type !App.enc with
      | `Sjs -> ".sjs"
      | `Euc -> ".euc"
      | `Utf8 -> ".utf"
      | `Other -> ".res")
  in
  
(*
  let aorg = if options.start_address > hdr.Bytecode.data_offset 
    && options.start_address < Binarray.dim arr
    then options.start_address 
    else hdr.Bytecode.data_offset 
  in
*)

(*
  let amax = if options.end_address > -1 && 
    options.end_address < Binarray.dim arr 
    then if options.end_address > aorg  
    then options.end_address else aorg
    else Binarray.dim arr 
  in
*)

  let _, labels = 
    ISet.fold(fun ptr (idx, map) -> 
      idx + 1, IMap.add ptr idx map
    ) !pointers (1, IMap.empty)
  in

  let skipping = ref false in
  let oc = open_out kename in

  let rc =
    if options.separate_strings && 
      not (DynArray.empty resstrs)
    then open_out resname
    else oc
  in

(*
  let count_lines str =    
    let rec loop s l = 
      try
        let i = String.index s '\n' + 1 in
        loop (String.sub s i ((String.length s) - i)) (l + 1)
      with Not_found -> l
    in
    loop str 0
  in

  let line = ref 1 in
  
(*
  let write_string oc str =
    output_string oc str;
    line := !line + count_lines str
  in
*)
*)

(*
  let write_string str =
    output_string oc str;
(*
    line := !line + count_lines str
*)
  in
*)

  begin try
    if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
      output_string oc "\xef\xbb\xbf";
    output_string oc (sprintf "{-# cp %s #- Disassembled with %s %1.2f -}\n\n#file '%s'\n"
      (String.lowercase !App.enc) App.app.name App.app.version bname);
    if oc <> rc then (
      if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
        output_string rc "\xef\xbb\xbf";
      fprintf rc "// Resources for %s\n\n" bname;
      output_string oc (sprintf "#resource '%s'\n" 
        (Filename.basename resname))
    );
    
    output_char oc '\n'; (* incr line; *)

    let mode =
      if options.forced_target = `None then
        if hdr.Bytecode.header_version = 1
        then `Avg2000 else `RealLive
      else
        options.forced_target
    in
    
    (match mode with
      | `Avg2000 -> output_string oc "#target AVG2000\n"; (* incr line *)
      | `Kinetic -> output_string oc "#target Kinetic\n"; (* incr line *)
      | _ -> ());
  
    if options.uses_excl_kidoku then 
      output_string oc "#kidoku_type 2\n"; (* incr line; *)
(*
      write_string oc "#kidoku_type 2\n";
*)
      
    let get_key key =
      try Cast.find key with Not_found -> key
    in

    List.iter (if options.prep_res_file then (
      fun s -> let ss = Text.sjs_to_enc !App.enc s in
        fprintf rc "#character '%s'\t\t// '%s'\n" 
          (get_key ss) ss
    ) else (
      fun s -> fprintf rc "#character '%s'\n" 
        (Text.sjs_to_enc !App.enc s)
    )) hdr.Bytecode.dramatis_personae;
  
    if rc <> oc then fprintf rc "\n";

    (* Output code: *)

(*
    sysInfo "OUTPUT CODE 1";
*)
  
    DynArray.iter
      (fun cmd ->
    
       (* print label if present *)
   
       (try
         let idx = IMap.find cmd.offset labels in
         (*
         cmd.label = idx;
         *)
         
         (*
         ksprintf write_string "\n  @%d\n" idx;
         *)
         output_string oc (sprintf "\n  @%d\n" idx);
         pointers := ISet.remove cmd.offset !pointers;
         skipping := false
        with
          Not_found -> ()
        );
        
        (* print command if visible *)

(*
        if (cmd.unhide && !skipping) || cmd.ctype = "label" then skipping := false;
*)
        if cmd.unhide && !skipping then skipping := false;
(*
        if not (!skipping || cmd.hidden) then print_cmd labels oc cmd;
*)

(*
        cmd.lineno <- !line;
    
        if cmd.ctype = "entrypoint" then
          DynArray.add !map.entrypoints !line
        else if cmd.ctype = "call" || cmd.ctype = "jump" then (
          let s,e = if List.length cmd.args > 0 then
            (try int_of_string (List.nth cmd.args 0) with _ -> -1),
            if List.length cmd.args > 1 then
            (try int_of_string (List.nth cmd.args 1) with _ -> -1)
            else -1
            else -1,-1
          in
            
          (* need to handle variable parameters, eg 7030 $03f farcall(intD[5], intD[6]) *)
          
          if debug () > 1 then 
            sysInfo (sprintf "Seen%04d %-12s Seen%04d%s" 
              s cmd.ident s (if e > -1 then 
               (sprintf "(Z%02d)" e) else ""));

          DynArray.add (if cmd.ctype = "call" then 
            !map.calls else !map.gotos) { 
            origin = { seen = idx; line = !line };
            target = { scene = s; entry = e };
            kind = cmd.ident };
        );
*)

(*
        if not (!skipping || cmd.hidden) then (
          if cmd.ctype = "entrypoint" then (
            (*
            output_char oc '\n'; (* incr line; *)
            write_string (format_cmd labels cmd);
            output_char oc '\n' (*; incr line *)
            *)
            output_string oc ("\n" ^ (format_cmd_labels cmd) ^ "\n")
          ) else write_string (format_cmd labels cmd)
          (*
          write_string (format_cmd labels cmd);
          if cmd.ctype = "entrypoint" then 
            output_char oc '\n'; incr line
          *)
        );
*)

        if not (!skipping || cmd.hidden) then (
          if cmd.ctype = "entrypoint" then output_string 
            oc ("\n" ^ (format_cmd labels cmd) ^ "\n")
          (*
          else if cmd.ctype = "label" then (
            output_string oc ("\n" ^ (format_cmd labels cmd));
(*
            pointers := ISet.remove cmd.offset !pointers;
*)
            skipping := false
          ) else output_string oc (format_cmd labels cmd)
          *)
          else output_string oc (format_cmd labels cmd)
        );
        
(*
          else output_string oc ((if cmd.ctype 
            = "label" then "\n" else "") 
            ^ (format_cmd labels cmd))
        );
*)
        
        if options.suppress_uncalled && 
          cmd.is_jmp then skipping := true;
      ) commands;

(*    
    ISet.iter 
      (fun x -> 
        if x = amax - aorg then 
          try ksprintf write_string "\n  @%d\n" (IMap.find x labels) 
          with Not_found -> assert false
        else ksprintf sysWarning "label %08x not inserted" x
      ) !pointers;
*)
    
    close_out oc;

(*
    if options.hexdump then hexd ();
*)

    if options.hexdump then write_hexdump fname arr;

    if rc <> oc then (
      DynArray.iter (
        if options.prep_res_file then
          if Encoding.enc_type !App.enc = `Utf8 then fun s -> 
            fprintf rc "// %s\n%s\n" 
              (Text.sjs_to_enc !App.enc s)
              (Text.sjs_to_utf8_prep s)
          else fun s -> 
            let ss = Text.sjs_to_enc !App.enc s in
            fprintf rc "// %s\n%s\n" ss ss
        else fun s -> fprintf rc "%s\n" 
          (Text.sjs_to_enc !App.enc s)
      ) resstrs;
      close_out rc;
    )
  with
    e -> 
      (try close_out oc with _ -> ());
      (try close_out rc with _ -> ());
      
(*
      write_hexdump fname arr;
*)
      
      raise e
  end

(* process disassembly result; insert labels & add cmd.lineno *)

let process_source fname (arr: Binarray.t) =
  let hdr = read_header arr in
  
  (* Get output filenames *)

  let base,bname = get_base fname in
  let idx = int_of_string (String.sub fname 4 4) in
      
(*
  let kename, resname = base ^ "." ^ !App.src_ext,
    base ^ (match Encoding.enc_type !App.enc with
      | `Sjs -> ".sjs"
      | `Euc -> ".euc"
      | `Utf8 -> ".utf"
      | `Other -> ".res")
  in
*)
  
  let aorg = if options.start_address > hdr.Bytecode.data_offset 
    && options.start_address < Binarray.dim arr
    then options.start_address 
    else hdr.Bytecode.data_offset 
  in

  let amax = if options.end_address > -1 && 
    options.end_address < Binarray.dim arr 
    then if options.end_address > aorg  
    then options.end_address else aorg
    else Binarray.dim arr 
  in

  let ptrs = ref ISet.empty in 
(*
  ISet.iter (fun ptr -> ISet.add ptr !ptrs) !pointers;
*)
  ISet.iter (fun ofs -> 
    ptrs := ISet.add ofs !ptrs
  ) !pointers;

  let _, labels = 
    ISet.fold(fun ptr (idx, map) -> 
      idx + 1, IMap.add ptr idx map
    ) !pointers (1, IMap.empty)
  in

  let skipping = ref false in
(*
  let oc = open_out kename in
*)

(*
  let rc =
    if options.separate_strings && 
      not (DynArray.empty resstrs)
    then open_out resname
    else oc
  in
*)

(*
  let count_lines str =    
    let rec loop s l = 
      try
        let i = String.index s '\n' in
        loop (String.sub s (i + 1) ((String.length s) - (i + 1))) (l + 1)
      with Not_found -> l
    in
    loop str 0
  in
*)

  let count_lines str =    
    let rec loop s l = 
      try
        let i = String.index s '\n' + 1 in
        loop (String.sub s i ((String.length s) - i)) (l + 1)
      with Not_found -> l
    in
    loop str 0
  in

  let line = ref 1 in

  let write_string str =
(*
    output_string oc str;
*)
    line := !line + count_lines str
  in
  
       
  begin try
(*
    if !App.bom && Encoding.enc_type !App.enc = `Utf8 then 
      output_string oc "\xef\xbb\xbf";
*)
    ksprintf write_string "{-# cp %s #- Disassembled with %s %1.2f -}\n\n#file '%s'\n"
      (String.lowercase !App.enc) App.app.name App.app.version bname;
    
    (* output_char oc '\n'; *) 
    incr line;

    if options.separate_strings && 
      not (DynArray.empty resstrs)
    then incr line;

    let mode =
      if options.forced_target = `None then
        if hdr.Bytecode.header_version = 1
        then `Avg2000 else `RealLive
      else
        options.forced_target
    in
    
(*
    (match mode with
      | `Avg2000 -> output_string oc "#target AVG2000\n"; incr line
      | `Kinetic -> output_string oc "#target Kinetic\n"; incr line
      | _ -> ());
*)

    match mode with
      | `Avg2000 | `Kinetic -> incr line
      | _ -> ();
  
    if options.uses_excl_kidoku then 
      (* output_string oc "#kidoku_type 2\n"; *) 
      incr line;

(*
    let get_key key =
      try Cast.find key with Not_found -> key
    in
*)

(*
    List.iter (if options.prep_res_file then (
      fun s -> let ss = Text.sjs_to_enc !App.enc s in
        fprintf rc "#character '%s'\t\t// '%s'\n" 
          (get_key ss) ss
    ) else (
      fun s -> fprintf rc "#character '%s'\n" 
        (Text.sjs_to_enc !App.enc s)
    )) hdr.Bytecode.dramatis_personae;
  
    if rc <> oc then fprintf rc "\n";
*)

    (* Output code: *)

(*
    sysInfo "OUTPUT CODE 1";
*)

(*
    let larr = DynArray.create () in
    
    DynArray.iteri
      (fun i cmd ->
    
       (* print label if present *)
   
(*
       (try
         let idx = IMap.find cmd.offset labels in
         DynArray.add larr i;
         (*
         DynArray.set larr (idx - 1) i;
         *)
         
         (*
         ksprintf write_string "\n  @%d\n" idx;
         *)
         pointers := ISet.remove cmd.offset !pointers;
         skipping := false
        with
          Not_found -> ()
        );
*)

       (*
       if IMap.mem cmd.offset labels then (
         DynArray.add larr i;
         (*
         DynArray.set larr (idx - 1) i;
         *)
         
         (*
         ksprintf write_string "\n  @%d\n" idx;
         *)
(*
         pointers := ISet.remove cmd.offset !pointers;
*)
         ptrs := ISet.remove cmd.offset !ptrs;
         skipping := false
       )
       *)

       if IMap.mem cmd.offset labels then (
         DynArray.add larr i;
         ptrs := ISet.remove cmd.offset !ptrs;
         skipping := false
       )
    ) commands;
    
(*
    DynArray.iteri
      (fun li ci -> 
        let lcmd = DynArray.get commands ci in
        let cmd = { empty_cmd with 
          offset = lcmd.offset;  ctype = "label";
          kepago = [ S (sprintf "  @%d" (li + 1)) ] } in
        DynArray.insert commands (ci + li) cmd
    ) larr;
*)
*)

(*        
    DynArray.iteri
      (fun i cmd ->
       if IMap.mem cmd.offset labels then (
         DynArray.add larr i;
         ptrs := ISet.remove cmd.offset !ptrs;
         skipping := false
       )
    ) commands;
*)    
    DynArray.iter
      (fun cmd ->
    
       (* print label if present *)
       
       (*
       (try
         let idx = IMap.find cmd.offset labels in
              
         ksprintf write_string "\n  @%d\n" idx;
         pointers := ISet.remove cmd.offset !pointers;
         skipping := false
        with
          Not_found -> ()
        );
        *)
        
       (try
         let idx = IMap.find cmd.offset labels in
              
         ksprintf write_string "\n  @%d\n" idx;
         (*
         pointers := ISet.remove cmd.offset !pointers;
         *)
         ptrs := ISet.remove cmd.offset !ptrs;
         skipping := false
        with
          Not_found -> ()
        );
        
        (* print command if visible *)

        if cmd.unhide && !skipping then skipping := false;
        cmd.lineno <- !line;
        
        if cmd.ctype = "label" then
          skipping := false
        else if cmd.ctype = "entrypoint" then
          DynArray.add !map.entrypoints !line
        else if cmd.ctype = "call" || cmd.ctype = "jump" then (
          let s,e = if List.length cmd.args > 0 then
            (try int_of_string (List.nth cmd.args 0) with _ -> -1),
            if List.length cmd.args > 1 then
            (try int_of_string (List.nth cmd.args 1) with _ -> -1)
            else -1
            else -1,-1
          in
            
          (* need to handle variable parameters, eg 7030 $03f farcall(intD[5], intD[6]) *)
          
          if debug () > 1 then 
            sysInfo (sprintf "Seen%04d %-12s Seen%04d%s" 
              s cmd.ident s (if e > -1 then 
               (sprintf "(Z%02d)" e) else ""));

          DynArray.add (if cmd.ctype = "call" then 
            !map.calls else !map.gotos) { 
            origin = { seen = idx; line = !line };
            target = { scene = s; entry = e };
            kind = cmd.ident };
        );

        if not (!skipping || cmd.hidden) then (
          if cmd.ctype = "entrypoint" then (
            (* output_char oc '\n'; *) incr line;
            write_string (format_cmd labels cmd);
            (* output_char oc '\n'; *) incr line
          ) else (
            (*
            if cmd.ctype = "label" then
              (* output_char oc '\n'; *)
              incr line;
            *)
            write_string (format_cmd labels cmd)
          (*
          write_string (format_cmd labels cmd);
          if cmd.ctype = "entrypoint" then 
            output_char oc '\n'; incr line
          *)
          )
        );
          
        if options.suppress_uncalled && 
          cmd.is_jmp then skipping := true;
      ) commands;

(*
          let jump = { empty_jump with 
            origin = { seen = idx; line = !line };
            target = { scene = s; entry = e };
            kind = cmd.ident }
          in
*)
        
(*
          match tl cmd.kepago with | FP s,e -> {
            origin = { seen = idx; line = !line };
            target = { scene = s; entry = e };
            kind = cmd.ident }
*)

(*
            DynArray.add (if cmd.ctype = "call" then 
              seen_map.calls else seen_map.gotos) 
*)              
(*
              let jump =
                (let j = { empty_jump with origin = 
                  { seen = idx; line = !line };
                  kind = cmd.ident } in

(*                  
                match cmd with 
                  | { kepago = _ _ :: FP (s,e) } -> 
                    { j with target = { scene = s; entry = e } }
                  | _ -> j)
              in
*)            
          
                 match tl cmd.kepago with 
(*
                  | FP (s,e) -> { j with target = 
                      { scene = s; entry = e } }
*)
(*
                  | FP fp -> let s,e = (int_of_string fp),-1 in
                      { j with target = { scene = s; entry = e } }
*)
                  | FP s -> let e = -1 in
                      { j with target = { scene = s; entry = e } }
                  | _ -> j)
              in
*)

(*
              DynArray.add (if cmd.ctype = "call" then 
                seen_map.calls else seen_map.gotos) jump;
*)
        (*
          { empty_jump with target = { scene = s; entry = e } }
            kind = cmd.ident
          
          DynArray.add (if cmd.ctype = "call" then 
            seen_map.calls else seen_map.gotos) 
            { empty_jump with target = { scene = scene; 
              entry = entry }; kind = cmd.ident };
        *)

(*
        else if cmd.type = ""
*)

                
(*        
        if not (!skipping || cmd.hidden) then write_string (format_cmd labels cmd);
        if options.suppress_uncalled && cmd.is_jmp then skipping := true;
      ) commands;
*)
    
    ISet.iter 
      (fun x -> 
        if x = amax - aorg then 
          try ksprintf write_string "\n  @%d\n" (IMap.find x labels) 
          with Not_found -> assert false
        else ksprintf sysWarning "label %08x not inserted" x
      ) !ptrs; (* !pointers; *)
  
(*
    close_out oc;
*)

(*
    if options.hexdump then hexd ();
*)

(*
    if options.hexdump then write_hexdump fname arr;
*)

(*
    if rc <> oc then (
      DynArray.iter (
        if options.prep_res_file then
          if Encoding.enc_type !App.enc = `Utf8 then fun s -> 
            fprintf rc "// %s\n%s\n" 
              (Text.sjs_to_enc !App.enc s)
              (Text.sjs_to_utf8_prep s)
          else fun s -> 
            let ss = Text.sjs_to_enc !App.enc s in
            fprintf rc "// %s\n%s\n" ss ss
        else fun s -> fprintf rc "%s\n" 
          (Text.sjs_to_enc !App.enc s)
      ) resstrs;
      close_out rc;
    )
*)
  with
    e -> 
(*
      (try close_out oc with _ -> ());
      (try close_out rc with _ -> ());
*)
      
      write_hexdump fname arr;
    
(*
      let hc = open_out (base ^ ".hex") in
      let max = Binarray.dim arr in
    
      (* Output hexdump: *)
    
      (*
      sysInfo "OUTPUT CODE 2";
      *)
    
      DynArray.iter
        (fun cmd ->
          (* print command if visible *)
        
          let hex = Buffer.create 0 in
          let txt = Buffer.create 0 in
          let ofs = cmd.offset + !data_offset in
      
          String.iter (fun c -> let i = int_of_char c in bprintf hex "%02x" i;
            Buffer.add_char txt (Char.chr (if i >= 0x20 && i <= 0x7f then i else 0x2e))
          ) (Binarray.read arr ofs (if ofs + 16 < max then 16 else max - ofs));
        
          fprintf hc "[$%08x|%05d] %32s [%16s] %s\n%!" ofs cmd.offset 
            (Buffer.contents hex) (Buffer.contents txt)
            (String.concat "" (List.map (function
              | S s | STORE s -> Text.to_string !App.enc 
                (TextTransforms.read_cp932_compatible sysError s)
              | P i -> sprintf " ptr(%d)" i
            ) cmd.kepago))
        ) commands;
(*
              | FP i -> sprintf " fptr(%d)" i) cmd.kepago))
        ) commands;
*)
(*
              | P i -> sprintf " ptr(%d)" i
*)
(*
              | FP (s,e) -> "") cmd.kepago)
*)
(*
              | FP fp -> "") cmd.kepago)

              *)
(*
              | FP i -> sprintf " fptr(%d)" i)
            ) cmd.kepago)
        ) commands;
*)
    
      (try close_out hc with _ -> ());
*)
      raise e
  end


let process_map idx map =
(*
  Hashtbl.add seen_map idx { 
    Disassembler.empty_map with 
    Disassembler.calls = DynArray.copy map.Disassembler.calls; 
    Disassembler.gotos = DynArray.copy map.Disassembler.gotos };
*)

  if !App.verbose > 1 then (
    ksprintf sysInfo "Analyzing Seen%04d..." idx;
    ksprintf sysInfo "  enpts: %d" (DynArray.length map.entrypoints);
    ksprintf sysInfo "  calls: %d" (DynArray.length map.calls);
    ksprintf sysInfo "  gotos: %d" (DynArray.length map.gotos)
  );

  Hashtbl.add seen_map idx (copy_map map);
  
  DynArray.iter
    (fun (call : jump_t) ->
      try
        let seen = Hashtbl.find seen_map call.target.scene in
              
        Hashtbl.add seen.entries 
          call.target.entry call
      with
        Not_found -> ()
      ) map.calls;
        
  DynArray.iter
    (fun (goto : jump_t) ->
      try              
        let seen = Hashtbl.find seen_map goto.target.scene in
              
        Hashtbl.add seen.entries 
          goto.target.entry goto
        with
      Not_found -> ()
    ) map.gotos


let disassemble_source fname (arr: Binarray.t) =
  let err = disassemble fname arr in
  
  if err = "" || debug () > 0 then (
    process_source fname arr;
    write_source fname arr
  );
    
  let idx = int_of_string (String.sub fname 4 4) in
  
  process_map idx !map
  
(*
  !map
*)

(*
  if err = "" && options.hexdump then write_hexdump fname arr
*)    


let generate_map fname (arr: Binarray.t) = 
  let err = disassemble fname arr in
  
  if err = "" || debug () > 0 then (
    process_source fname arr;
  );
    
  let idx = int_of_string (String.sub fname 4 4) in
  
  process_map idx !map