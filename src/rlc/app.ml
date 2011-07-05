(*
   Rlc: RealLive-compatible compiler
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

let app = { 
    Config.app_info with
    exe_name = "rlc";
    name = "Rlc";
    usage = "<options> file";
    description = "RealLive-compatible compiler";
    year2 = 2011;
    author2 = "Richard 23"
}


type options = { 
    mutable start_line: int;
    mutable end_line: int
}


let options = { 
    start_line = -1;
    end_line = -1
}


(*
and verbose  = ref false
*)

let start_line = ref ~-1
let end_line = ref ~-1

and verbose  = ref 0
and compress = ref true
and outdir   = ref ""
and outfile  = ref ""
and gameexe  = ref ""
and enc      = ref Config.default_encoding
and old_vars = ref false
and with_rtl = ref true
and assertions = ref true
and debug_info = ref true
and metadata = ref true
and array_bounds = ref false
and flag_labels = ref false
and opt_level = ref 1l

(*
and src_ext = ref "org"
*)

let kfn_file = ref "reallive.kfn"
let cast_file = ref ""
let game_file = ref "game.cfg"
let game_id = ref "LB"

let src_ext = ref "org"
and resdir = ref ""

(*
and appdirs = DynArray.create ()
*)

(*
type options =
  { mutable separate_strings: bool;
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
    mutable start_line: int;
    mutable end_line: int; 
    mutable prep_res_file: bool; 
    mutable opcodes: bool; 
    mutable hexdump: bool }
*)


let runtime_trace = ref 0

(*
let runtime_trace = ref 1
*)

(*
ignore(runtime_trace := 1)
*)

(*
let info where what =
  ksprintf cliWarning "%s line %d: %s" where.file where.line (Text.sjs_to_err what)
*)
  
  
(*
let cliInfo = prettily_blk_ln !base_indent

let cliWarning s =
  Format.set_formatter_out_channel stderr;
  cliInfo s;
  flush stderr;
  Format.set_formatter_out_channel stdout
*) 

(*
let badc = Hashtbl.create ()
*)