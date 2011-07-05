(*
   Binarray: bigarray wrapper for byte twiddling
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

let buffer_size = 4_194_304
  
let broken = Sys.os_type = "Win32"

let no_mmap = ref false
exception MMapFailure

(* Caml type 'a, representation kind 'b, and memory layout 'c. *)

type t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
let blit = Bigarray.Array1.blit
let sub = Bigarray.Array1.sub
let dim = Bigarray.Array1.dim
let fill = Bigarray.Array1.fill

let map_file fd b len =
  try
    Bigarray.Array1.map_file fd Bigarray.int8_unsigned Bigarray.c_layout b len
  with Sys_error "No such device" ->
    no_mmap := true;
    raise MMapFailure      

let copy arr =
  let rv = create (dim arr) in
  blit arr rv;
  rv

external read : t -> idx:int -> len:int -> string = "string_of_binary_array"
external unsafe_read_buf : t -> buffer:string -> idx:int -> 
  len:int -> offs:int -> unit = "string_from_binary_array"
external read_sz : t -> idx:int -> len:int -> string = "cstring_of_binary_array"
external unsafe_read_sz : t -> idx:int -> string = "unsafe_cstring_of_binary_array"
external write : t -> idx:int -> string -> unit = "string_to_binary_array"
external write_sz : t -> idx:int -> len:int -> string -> unit = "cstring_to_binary_array"
external unsafe_char_of_int : int -> char = "%identity"

external digest : t -> Digest.t = "digest_array"

let put_int32 arr ~idx i =
  arr.{idx} <- Int32.to_int (Int32.logand i 0xff_l);
  arr.{idx + 1} <- Int32.to_int (Int32.logand (Int32.shift_right i 8) 0xff_l);
  arr.{idx + 2} <- Int32.to_int (Int32.logand (Int32.shift_right i 16) 0xff_l);
  arr.{idx + 3} <- Int32.to_int (Int32.logand (Int32.shift_right i 24) 0xff_l)

let put_i16 arr ~idx i =
  arr.{idx} <- i land 0xff;
  arr.{idx + 1} <- (i asr 8) land 0xff

let put_int arr ~idx i =
  put_i16 arr ~idx i;
  arr.{idx + 2} <- (i asr 16) land 0xff;
  arr.{idx + 3} <- (i asr 24) land 0xff

let get_int32 arr ~idx =
  Int32.add
    (Int32.of_int arr.{idx})
    (Int32.add
      (Int32.shift_left (Int32.of_int arr.{idx + 1}) 8)
      (Int32.add
        (Int32.shift_left (Int32.of_int arr.{idx + 2}) 16)
        (Int32.shift_left (Int32.of_int arr.{idx + 3}) 24)))

let get_i16 arr ~idx =
  arr.{idx} + (arr.{idx + 1} lsl 8)

let get_int arr ~idx =
  get_i16 arr ~idx + (arr.{idx + 2} lsl 16) + (arr.{idx + 3} lsl 24)

let rec read_input fname : t =
  if not (Sys.file_exists fname) then Printf.ksprintf 
    failwith "file `%s' not found" fname;
  if broken || !no_mmap then
    let ic = open_in_bin fname in
    let rlen = in_channel_length ic in
    let rv = create rlen in
    let buffer = String.create (min rlen buffer_size) in
    let rec loop idx len =
      if len = 0 then (
        close_in ic;
        rv
      )
      else
        let read = input ic buffer 0 (min len buffer_size) in
        write rv idx (String.sub buffer 0 read);
        loop (idx + read) (len - read)
    in
    loop 0 (in_channel_length ic)
  else
    try
      let fdescr =
	try
          Unix.openfile fname [Unix.O_RDONLY] 0
	with Unix.Unix_error (e, _, _) ->
          Printf.ksprintf failwith "cannot read file `%s': %s" 
		    fname (String.uncapitalize (Unix.error_message e))
      in
      let arr = map_file fdescr false ~-1 in
      let rv = create (dim arr) in
	blit arr rv;
	rv
    with MMapFailure ->
      read_input fname

let map_output fname len =
  try
    assert (len > 0);
    let f = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o755 in
    map_file f true len, f
  with
    | Unix.Unix_error (e, _, _)
     -> Printf.ksprintf failwith "cannot write to file `%s': %s" 
	   fname (String.uncapitalize (Unix.error_message e))

let output arr oc =
  let buffer = String.create (min (dim arr) buffer_size) in
  let rec loop idx len =
    if len > 0 then
      let left = min len buffer_size in
      unsafe_read_buf arr ~buffer ~idx ~len:left ~offs:0;
      if left = buffer_size then
        output_string oc buffer
      else
        output_string oc (String.sub buffer 0 left);
      loop (idx + left) (len - left)
  in
  loop 0 (dim arr)

let rec write_file arr fname =
  if !no_mmap then
    let oc = open_out_bin fname in
      try
	output arr oc;
	close_out oc
      with e ->
	close_out_noerr oc;
	raise e
  else
    try
      let oa, od = map_output fname (dim arr) in 
	try
	  blit arr oa;
	  Unix.close od
	with e ->
	  Unix.close od;
          raise e
    with 
	MMapFailure -> write_file arr fname
      | e -> raise e

	(*
let remove_duplicates comparison_fun string_list =
  let module StringSet =
	Set.Make(struct type t = string
					let compare = comparison_fun end) in
  StringSet.elements
	(List.fold_right StringSet.add string_list StringSet.empty)
*)

(*
module Seen =
  struct
(*
	type file = string
	type data = Binarray
	
	type t = Empty | file * Binarray
*)

	type t = Empty | file * Bytecode.file_header_t * Binarray.t;

	  type file = None | string;
	
	let file = Empty
	and header = Bytecode.empty_header 
	and data = Binarray.create(size);
	
	let create = 
		( None Bytecode.empty_header Binarray.create(size) ) t
(*
	  let make size = 
		( None Bytecode.empty_header Binarray.create(size) ) t
	  type file = None | string;
*)
v	
(*
	let hdr =
*)
	
	let read = Binarray.read_input
	let write = Binarray.write_file
	
	let read_header ?(rd_handler = (fun _ -> ())) full seen = 
		(if full then read_file_header else read_header) ~rd_handler arr
  end
*)

		
(*
	let is_bytecode arr idx =
		
		
		?(rd_handler = (fun _ -> ())) arr =
  end
*)	
	
(*
     type t = string
     let compare x y = if x = y then Equal else if x < y then Less else Greater
   end;;

   
   
	type s
	
	type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue
     let empty = Empty
     let rec insert queue prio elt =
       match queue with
         Empty -> Node(prio, elt, Empty, Empty)
       | Node(p, e, left, right) ->
           if prio <= p
           then Node(prio, elt, insert right p e, left)
           else Node(p, e, insert right prio elt, left)
     exception Queue_is_empty
     let rec remove_top = function
         Empty -> raise Queue_is_empty
       | Node(prio, elt, left, Empty) -> left
       | Node(prio, elt, Empty, right) -> right
       | Node(prio, elt, (Node(lprio, lelt, _, _) as left),
                         (Node(rprio, relt, _, _) as right)) ->
           if lprio <= rprio
           then Node(lprio, lelt, remove_top left, right)
           else Node(rprio, relt, left, remove_top right)
     let extract = function
         Empty -> raise Queue_is_empty
       | Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
   end;;
  *)
  