(*
  Kprl: SEEN.TXT archiving, encryption, and compression handling
  Copyright (C) 2007 Haeleth
  Revised 2009-2011 by Richard 23

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
  02111-1307, USA.
*)

open Printf

open Game

open Binarray
open Bytecode

open GameTypes

(*
open Game 
*)


(* Stubs for routines in C. *)

external c_decompress: Binarray.t -> Binarray.t -> unit = "rl_prim_decompress"
(* external c_compress: Binarray.t -> int -> int = "rl_prim_compress" *)
external c_compress: Binarray.t -> int = "rl_prim_compress"
external c_apply_mask: Binarray.t -> int -> unit = "rl_prim_apply_mask"
external c_decrypt: Binarray.t -> int -> int -> Binarray.t -> unit = "rl_prim_decrypt"


(* LB key *)

(*
offset of xor zone is 256
length of xor zone is 256
key xors 256 bytes of data

# key: a828fd66a0237769f945f82c7c00adf4
*)

(*
type xor_subkey = int * int * char list
type xor_key = xor_subkey list
*)

(*
let key = ref [|0xa8; 0x28; 0xfd; 0x66; 0xa0; 0x23; 0x77; 0x69; 0xf9; 0x45;
0xf8; 0x2c; 0x7c; 0x00; 0xad; 0xf4|]
*)

let key = ref [|0xa8; 0x28; 0xfd; 0x66; 0xa0; 0x23; 0x77; 0x69; 0xf9; 0x45;
0xf8; 0x2c; 0x7c; 0x00; 0xad; 0xf4|]


(* LB EX keys *)
(*
offset of xor zone is 4176 / 265
length of xor zone is 384 (128 x 3)
each key xors 128 bytes of data

# key1: 000000171400137c6f0d726f1e0e0004
# key2: 000000171400137c6f0d726f1e0e0004
# key3: 76f1b77e0f006af35566dd64a4d42253
*)

(*
let key1 = ref [|0x00; 0x00; 0x00; 0x17; 0x14; 0x00; 0x13; 0x7c; 0x6f; 0x40d;
0x72; 0x6f; 0x1e; 0x0e; 0x00; 0x04|]
*)

(*
# key1: a828fd71b423641596488a43620eadf0
# key2: ded94a18af231d9aac232548d8d48fa7
# key3: 76f1b7691b00798f3a6baf0bbada2257

# key1: 
a8 28 fd 71 b4 23 64 15
96 48 8a 43 62 0e ad f0
*)

(*
let key1 = ref [|0xa8; 0x28; 0xfd; 0x71; 0xb4; 0x23; 0x64; 0x15; 0x96; 0x48;
0x8a; 0x43; 0x62; 0x0e; 0xad; 0xf0|]
*)

(*
# key1: a828fd71b423641596488a43620eadf0
# key2: ded94a18af231d9aac232548d8d48fa7
# key3: 76f1b7691b00798f3a6baf0bbada2257

# key2:
de d9 4a 18 af 23 1d 9a 
ac 23 25 48 d8 d4 8f a7 
*)

(*
let key2 = ref [|0xde; 0xd9; 0x4a; 0x18; 0xaf; 0x23; 0x1d; 0x9a; 0xac; 0x23;
0x25; 0x48; 0xd8; 0xd4; 0x8f; 0xa7|]
*)

(*
# key1: a828fd71b423641596488a43620eadf0
# key2: ded94a18af231d9aac232548d8d48fa7
# key3: 76f1b7691b00798f3a6baf0bbada2257

# key3: 
76 f1 b7 69 1b 00 79 8f 
3a 6b af 0b ba da 22 57
*)

(*
let key3 = ref [|0xde; 0xf1; 0xb7; 0x69; 0x1b; 0x00; 0x79; 0x8f; 0x3a; 0x6b;
0xaf; 0x0b; 0xba; 0xda; 0x22; 0x57|]
*)

(*
let key3 = ref [|0xde; 0xf1; 0xb7; 0x69; 0x1b; 0x00; 0x79; 0x8f; 0x3a; 0x6b;
0xaf; 0x0b; 0xba; 0xda; 0x22; 0x57|]

let key4 = ref [|0x76; 0xf1; 0xb7; 0x69; 0x1b; 0x00; 0x79; 0x8f; 0x3a; 0x6b;
0xaf; 0x0b; 0xba; 0xda; 0x22; 0x57|]
*)

(*
let set_game verbose s2 =
    let i = match s2 with
        | "CFV"        ->    0
        | "LB"        ->    1
        | "LBEX"    ->    2
        | "LBME"    ->    -1
        | "FIVE"    ->    3
        | "SNOW"    ->    4
        | _         -> -1
    in    
    printf "set_game(%s) => %d\n" s2 i
*)

(*
let game = ref 1

let set_game verbose id =
    let idx = match id with
        | "CFV"        ->    0
        | "LB"        ->    1
        | "LBEX"    ->    2
        | "LBME"    ->    -1
        | "FIVE"    ->    3
        | "SNOW"    ->    4
        | _         -> -1
    in
    (*
    if verbose() then printf "set_game(%s) => %d\n" id idx;
    *)
    game := idx

(*        
    game = idx
*)
*)

(*
let game = ref ~0


let set_game verbose id = 
    let idx = match id with
        | CFV    ->    0
        | LB    ->    1
        | LBEX    ->    2
        | LBME    ->    
        | FIVE    ->    3
        | SNOW    ->    4
        | _     -> -1
        
    game = idx
*)

let set_key verbose s1 =
    if not ((String.length s1) = 32) then Optpp.usageError "key must be 32 characters";
    let rec loop s n =
        match String.length s with
            | 0 -> ()
            | _ -> Array.set !key n (int_of_string ("0x" ^ (String.sub s 0 2)));
            if verbose() then
                printf "0x%02X " (Array.get !key n);
            loop (String.sub s 2 ((String.length s) - 2)) (n+1) in

            if verbose() then
                printf "  Key set to: ";
            loop s1 0;
            if verbose() then
                printf "\n%!"

(*
let prep_key () =
    Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout !key

let prep_key1 () =
    Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout !key1

let prep_key2 () =
    Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout !key2

let prep_key3 () =
    Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout !key3

let prep_key4 () =
    Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout !key4
*)


let apply_xorkey dst =
(*
    Optpp.sysInfo "apply_xorkey\n";
*)
    
    let size = Binarray.dim dst in
(*
    let key = Game.get_key () in 
    
    let game = Game.get_game () in
    
    ksprintf Optpp.sysInfo "game id: %s\n" !Game.game;
    
    ksprintf Optpp.sysInfo "game: %s\n" (match game with Some g -> Game.string_of_game g | None -> "None");
    ksprintf Optpp.sysInfo "key: %s\n" (match game with Some g -> (match g.key with Some k -> Game.string_of_key k | None -> "None") | None -> "None");
    ksprintf Optpp.sysInfo "key: %s\n" (Game.string_of_key key);

    ksprintf Optpp.sysInfo "size: %d\n" size;
    ksprintf Optpp.sysInfo "key length: %d\n" (List.length key);
*)
    
    List.iter (fun sk ->
        let idx = ref 0 in
        let len = sk.length in
        let pos = ref sk.offset in
        let xor = sk.data in 
    
(*
        printf "xorkey: %s\n" (String.concat " " (List.map (fun b -> sprintf "%02x" b) xor));
        printf "offset: %d\n" !pos;
        printf "length: %d\n\n" len;
*)
        
        while (!idx < len && !pos < size) do
        (*    ignore (printf "pos:  %d\n" !pos);    *)
            
            let byte = dst.{!pos} in
            
        (*    
            printf "byte: %02x\n" byte;            
            printf "kidx: %d\n" (!idx mod 16); 
        *)

            dst.{!pos} <- byte lxor (List.nth xor (!idx mod 16));
            
        (*
            printf "xorb: %02x\n" (List.nth xor (!idx mod 16));    
            printf "xbyt: %02x\n\n" dst.{!pos};
        *)
        
            incr pos;
            incr idx
        done
    ) (Game.get_key ())
    
(*
    printf "done with apply_xorkey\n\n";
*)
    
(*
    dst
*)


(*
let apply_xorkey data key = 
*)

(*
let apply_xorkey ?(key = []) data =
*)

(*
let apply_xorkey data =
    let size = Binarray.dim data in
*)    
(*
    let xkey = if key = [] then 
        Game.get_key else key in
    let key = Game.get_key in
*)

    (*    if key <> [] then ( *)
    
(*
    let key = Game.get_key in
*)
    
(*
    List.iter (fun (ofs, len, arr) ->
*)

(*    
let apply_xorkey dst =
    Optpp.sysInfo "apply_xorkey\n";
    
    let size = Binarray.dim dst in
    let key = Game.get_key () in 
    let game = Game.get_game () in
    
    ksprintf Optpp.sysInfo "game id: %s\n" !Game.game;
    
    ksprintf Optpp.sysInfo "game: %s\n" (match game with Some g -> Game.string_of_game g | None -> "None");
    ksprintf Optpp.sysInfo "key: %s\n" (match game with Some g -> (match g.key with Some k -> Game.string_of_key k | None -> "None") | None -> "None");
    ksprintf Optpp.sysInfo "key: %s\n" (Game.string_of_key key);

    ksprintf Optpp.sysInfo "size: %d\n" size;
    ksprintf Optpp.sysInfo "key length: %d\n" (List.length key);
    
    List.iter (fun sk ->
        let idx = ref 0 in
        let len = sk.length in
        let pos = ref sk.offset in
        let xor = sk.data in 
    
        printf "xorkey: %s\n" (String.concat " " (List.map (fun b -> sprintf "%02x" b) xor));
        printf "offset: %d\n" !pos;
        printf "length: %d\n\n" len;
        
    (*
    let xor = Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout sk.data in
    *)
    
        while (!idx < len && !pos < size) do
            ignore (printf "pos:  %d\n" !pos);
            
            let byte = dst.{!pos} in
            
            ignore (printf "byte: %02x\n" byte);
            
        (*
            let i2 = !idx mod 16 in
        *)
        
            ignore (printf "kidx: %d\n" (!idx mod 16));

(*
            let xorb = xor.{!idx mod 16} in
*)

(*
            let xorb = List.nth xor (!idx mod 16) in

            dst.{!pos} <- byte lxor xorb; 
*)

(*
            let xorb = sk.{i2} in
*)            
            (*
            ignore (printf "xorb: %02x\n" xorb);
            *)
            
        (*    dst.{!pos} <- byte lxor (xor.(!idx mod 16));    *)
            dst.{!pos} <- byte lxor (List.nth xor (!idx mod 16));
            
        (*    printf "xorb: %02x\n" (xor.(!idx mod 16));    *)
            printf "xorb: %02x\n" (List.nth xor (!idx mod 16));    
            printf "xbyt: %02x\n\n" dst.{!pos};
            
        (*
            let xorb = xor.{!idx mod 16} in
        *)
        (*
            Bigarray.Array1.get (xor, !idx mod 16)
        *)
        (*
            let xorb = xor.{!idx mod 16} in
            dst.{!pos} <- byte lxor xorb;
        *)
        (*
            incr pos;
            
            printf "pos:  %d\n" !pos;
            
            incr idx;
            
            printf "idx:  %d\n" !idx;
            
            printf "\n";
        *)
        
            incr pos;
            incr idx
        done
    ) key;
    (*
    )Game.get_key;
    *)
    
    printf "done with apply_xorkey\n\n";
    
    dst
        
(*
        while (!idx < subkey.length && !pos < size) do
            let byte = data.{!pos} in
        (*
            let xorb = xor.{!idx mod 16} in
        *)
            let xorb = subkey.{!idx mod 16} in
            data.{!pos} <- byte lxor xorb;
(*
            data.{!pos} <- data.{!pos} lxor 
                subkey.data.{!idx mod 16};
            data.{!pos} <- (data.{!pos} lxor xor.{!idx mod 16});
*)

            incr pos;
            incr idx
        done
    ) Game.get_key;
    
    data
*)
(*
    ) key (* xkey *)
*)

    
(*
    data
*)
*)

(*let apply_xorkey data key = 
    int keyid = Long_val(key_id);
    
    Xor_Key *key = 0;

    if(keyid > -1) {
        key = keys[keyid];
    }
*)

(*
//    if(keyid && key) {
*)

    
(*
(* Return an equivalent to a bytecode file with decompressed and decrypted
   data.  The input array is modified. *)
let decompress arr =
  let hdr = read_file_header arr in
  
  ignore (printf "decrypting with RealLive key...");
  
  c_apply_mask arr hdr.data_offset;
  
  match hdr.compressed_size with
    | None -> arr
    | Some i -> ignore (printf "decompressing..."); let rv = create (hdr.data_offset + hdr.uncompressed_size) in
                blit (sub arr 0 hdr.data_offset) (sub rv 0 hdr.data_offset);
                (c_decompress (sub arr hdr.data_offset i)
            (sub rv hdr.data_offset hdr.uncompressed_size) 2);
(*
            (sub rv hdr.data_offset hdr.uncompressed_size) !game);
            (sub rv hdr.data_offset hdr.uncompressed_size) 2);
*)
(*        
                (c_decompress (sub arr hdr.data_offset i)
          (sub rv hdr.data_offset hdr.uncompressed_size)
        (hdr.compiler_version == 110002 || 
            hdr.compiler_version == 1110002) (prep_key()));
*)        
        
        (*
        (hdr.compiler_version == 110002) (prep_key()));
        *)
    (*    
        (hdr.compiler_version == 110002 || 
            hdr.compiler_version == 1110002) (prep_key()));
    *)
        (*
        (hdr.compiler_version == 110002 && 
            hdr.compiler_version == 1110002) (prep_key()));
        *)
        
        (*
        (hdr.compiler_version == 110002) (prep_key()));
        *)
        
(*        
            if hdr.compiler_version == 110002 then
                (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
            (256) (257) (prep_key()));
            
            if hdr.compiler_version == 1110002 then(
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (256) (8) (prep_key1()));
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (256+128) (128) (prep_key2()));
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (512) (16) (prep_key3()));
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (528) (113) (prep_key4()));
            );
*)            
            rv
*)



(*
(* Return an equivalent to a bytecode file with decompressed and decrypted
   data.  The input array is modified. *)
   
let decompress arr =
(*  printf "decompressing...\n"; *)

  let hdr = read_file_header arr in

(*
  let game = Game.get_game in
  
  let key = match game.key with
    `Key k -> k | `None -> () in
*)

(*
  let g = Game.get_game in
  let k = match g with Some g -> g.key | None -> () in
*)
  
(*
  let key = match Game.get_game with 
*)
(*
  let key = match Game.get !Game.game with 
    | Some g -> 
      match g.key with
        | `Key k -> k 
        | `None -> []
    | None -> [] in
*)
    
  let key = Game.get_key in

(*    
    Array.iter
      (fun skey -> let ofs, len, arr = skey in
        Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout arr
*)



(*
  ignore (printf "compressed size: %d\n" (match hdr.compressed_size with
    | None -> -1 | Some b -> b));
  ignore (printf "uncompressed size: %d\n" hdr.uncompressed_size);
  ignore (printf "data offset: %d\n" hdr.data_offset);

  printf "game: %d\n" !game;
*)
  
  c_apply_mask arr hdr.data_offset;
  
  match hdr.compressed_size with
    | None -> arr
    | Some i -> let rv = create (hdr.data_offset + hdr.uncompressed_size) in
                blit (sub arr 0 hdr.data_offset) (sub rv 0 hdr.data_offset);
                c_decompress (sub arr hdr.data_offset i) 
          (sub rv hdr.data_offset hdr.uncompressed_size); (* !game; *) (*key;*) (* !game; *)
          ignore (if key <> [] then apply_xorkey rv key); (* data; *)
          rv
                

(*
(*        
                (c_decompress (sub arr hdr.data_offset i)
          (sub rv hdr.data_offset hdr.uncompressed_size)
        (hdr.compiler_version == 110002 || 
            hdr.compiler_version == 1110002) (prep_key()));
*)        
        
        (*
        (hdr.compiler_version == 110002) (prep_key()));
        *)
    (*    
        (hdr.compiler_version == 110002 || 
            hdr.compiler_version == 1110002) (prep_key()));
    *)
        (*
        (hdr.compiler_version == 110002 && 
            hdr.compiler_version == 1110002) (prep_key()));
        *)
        
        (*
        (hdr.compiler_version == 110002) (prep_key()));
        *)
        
(*        
            if hdr.compiler_version == 110002 then
                (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
            (256) (257) (prep_key()));
            
            if hdr.compiler_version == 1110002 then(
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (256) (128) (prep_key1()));
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (256+128) (128) (prep_key2()));
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (512) (16) (prep_key3()));
                    (c_decrypt (sub rv hdr.data_offset hdr.uncompressed_size)
                (528) (113) (prep_key4()));
            );
*)            
            rv
*)
*)

let inflate src dst =
  c_decompress src dst;
  apply_xorkey dst;
  dst

let deflate buf offset length =
(*
  Optpp.sysInfo "deflate...";
  ksprintf Optpp.sysInfo "  size:   %d" (Binarray.dim buf);
  ksprintf Optpp.sysInfo "  offset: %d" offset;
  ksprintf Optpp.sysInfo "  length: %d" length;
*)
  
(*
  c_compress (apply_xorkey (Binarray.sub buf offset length)) + offset
*)

  let dst = Binarray.sub buf offset length in
  apply_xorkey (dst);
  c_compress (dst) + offset
  
  
(* Return an equivalent to a bytecode file with decompressed and decrypted
   data.  The input array is modified. *)
   
let decompress arr =
(*  printf "decompressing...\n"; *)

(*
  Optpp.sysInfo "decompressing...";
*)
  
  let hdr = read_file_header arr in

(*
  Optpp.sysInfo (sprintf "archived: %s" (if hdr.archived then "true" else "false"));
*)
  
(*
  let key = Game.get_key in
*)
  
  c_apply_mask arr hdr.data_offset;
  
  match hdr.compressed_size with
    | None -> arr
    | Some i -> let rv = create (hdr.data_offset + hdr.uncompressed_size) in
                blit (sub arr 0 hdr.data_offset) (sub rv 0 hdr.data_offset);
(*
                ignore (inflate (sub arr hdr.data_offset i)
                    (sub rv hdr.data_offset hdr.uncompressed_size));
*)
                let src = sub arr hdr.data_offset i in
                let dst = sub rv hdr.data_offset hdr.uncompressed_size in
                
(*
  Optpp.sysInfo (sprintf "archived: %s" (if hdr.archived then "true" else "false"));
*)
            
                ignore (if hdr.archived 
                  then inflate src dst
                  else ( c_decompress src dst; dst ));
            
            (*
                ignore (c_decompress src dst; if hdr.archived then apply_xorkey dst);
            *)
            
                rv
(*
                c_decompress (sub arr hdr.data_offset i) 
          (sub rv hdr.data_offset hdr.uncompressed_size); (* !game; *) (*key;*) (* !game; *)
*)
        (*
          ignore (if key <> [] then apply_xorkey rv key); (* data; *)
        *)
        (*
          apply_xorkey rv Game.get_key; (* data; *)
        *)
(*
          apply_xorkey rv;
          rv
*)
        (*  let rv2 = apply_xorkey rv in *)
        
(*
          ignore (apply_xorkey (sub rv hdr.data_offset hdr.uncompressed_size));
*)
          
(*
          printf "done with decompress\n\n";
          
          rv
*)

            
(* Return an equivalent to an uncompressed bytecode file with compressed and
  encrypted data.  The input array is not modified. *)
let compress arr =
  let hdr = read_file_header arr in
  let data_offset = hdr.data_offset in
  if hdr.compressed_size = None then
    let rv = create (dim arr) in
    blit arr rv;
    put_int rv 4 hdr.compiler_version;
    c_apply_mask rv data_offset;
    rv
  else
    let uncompressed_size = dim arr - data_offset in
    let buffer = create (uncompressed_size * 9 / 8 + 9) in
    blit (sub arr (data_offset - 8) (uncompressed_size + 8)) 
      (sub buffer 0 (uncompressed_size + 8));

    let compressed_size = deflate buffer 8 uncompressed_size in

    let rv = create (data_offset + compressed_size) in
    blit (sub arr 0 data_offset) (sub rv 0 data_offset);
    put_int buffer ~idx:0 compressed_size;
    put_int buffer ~idx:4 uncompressed_size;
    put_int rv ~idx:0 0x1d0;
    put_int rv ~idx:4 hdr.compiler_version;
    put_int rv ~idx:0x28 compressed_size;
    blit (sub buffer 0 compressed_size) (sub rv data_offset compressed_size);
    c_apply_mask rv data_offset;
    rv

(*
let compress arr =
  let hdr = read_file_header arr in
  let data_offset = hdr.data_offset in
  if hdr.compressed_size = None then
    let rv = create (dim arr) in
    blit arr rv;
    put_int rv 4 hdr.compiler_version;
    c_apply_mask rv data_offset;
    rv
  else
    let buffer = create ((dim arr - data_offset) * 9 / 8 + 9) in
(*
    let to_compress = sub buffer 8 (dim arr - data_offset) in
*)
    blit (sub arr (data_offset - 8) (dim arr - data_offset + 8)) 
      (sub buffer 0 (dim arr - data_offset + 8));
(*  let compressed_size = (c_compress to_compress (hdr.compiler_version == 110002) *)
(*
    let compressed_size = (c_compress to_compress (hdr.compiler_version == 110002 || hdr.compiler_version == 1110002)) ((prep_key())) + 8 in
*)
(*
    let compressed_size = (c_compress to_compress !game) + 8 in
*)

(*
    let compressed_size = (c_compress (apply_xorkey to_compress)) + 8 in
*)

(*
    let compressed_size = deflate to_compress 8 (dim arr - data_offset);
*)

    let compressed_size = deflate buffer 8 (dim arr - data_offset);

    let rv = create (data_offset + compressed_size) in
    blit (sub arr 0 data_offset) (sub rv 0 data_offset);
    put_int buffer ~idx:0 compressed_size;
(*
    put_int buffer ~idx:4 (dim to_compress);
*)
    put_int buffer ~idx:4 (dim arr - data_offset);
    put_int rv ~idx:0 0x1d0;
    put_int rv ~idx:4 hdr.compiler_version;
    put_int rv ~idx:0x28 compressed_size;
    blit (sub buffer 0 compressed_size) (sub rv data_offset compressed_size);
    c_apply_mask rv data_offset;
    rv
*)