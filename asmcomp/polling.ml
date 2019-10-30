(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Mach

(* constants *)
let lmax = 500
let k = 6
let e = lmax/k
(* let r = lmax/k *)

let insert_poll_instr instr = 
    let poll_instr = 
        Iop (Ipoll)
    in
    { instr with desc = poll_instr ; next = instr ; res = [| |] }

let rec insert_poll_aux delta instr =
    let delta = match instr.desc with
        | Iend -> delta
        | Iop _ -> delta + 1
        | _ -> delta + 1
    in
    let next = match instr.desc with
        | Iend -> instr.next
        | _ -> (if delta >= (lmax-1) then 
            (print_endline "inserted";insert_poll_instr instr.next)
            else
            insert_poll_aux delta instr.next)
    in
    { instr with next }

let insert_poll fun_body =
    insert_poll_aux (lmax-e) fun_body

let fundecl f =
    let new_body = insert_poll f.fun_body in
        { f with fun_body = new_body }
