(*
   CvtKpacRes: converts Kpac-format resource files to the RLdev format
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

open Optpp
open Printf
open ExtList

let dsyms = ref []

(* Option definitions *)

let options =
  [
    Opt { short = ""; long = "help"; argname = "";
          descr = "display this usage information";
          withoutarg = Some (fun () -> failwith "help");
          witharg = None };
    Opt { short = ""; long = "version"; argname = "";
          descr = "display " ^ App.app.name ^ " version information";
          withoutarg = Some (fun () -> failwith "version");
          witharg = None };
    Break;
    Opt { short = "-d"; long = "define"; argname = "SYM";
          descr = "define SYM for #ifdef tests";
          withoutarg = None;
          witharg = Some (fun sym -> dsyms := sym :: !dsyms) };
  ]

(* Main program *)

let main =
  try
    getopt options (function "" -> () | x -> ksprintf failwith "unexpected argument `%s'" x) (fun () -> ()) ~app:App.app;
    Convert.convert (List.map String.lowercase !dsyms)
  with
    | Failure "help"    -> display_help App.app options
    | Failure "version" -> display_version App.app
    | Failure e         -> sysError e
