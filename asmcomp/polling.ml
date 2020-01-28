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
open Clflags

(* constants *)
let lmax = 25
let k = 10
let e = lmax/k
(* let r = lmax/k *)

let is_addr_live i = not (Reg.Set.is_empty (Reg.Set.filter (fun f -> f.typ = Cmm.Addr) i))

let insert_poll_instr instr next = 
    let poll_instr = Iop (Ipoll) in
    { desc = poll_instr;
      next = instr;
      arg = [||];
      res = [| |] ;
      dbg = Debuginfo.none;
      live = next.live;
      available_before = next.available_before;
      available_across = next.available_across;
    }

let rec insert_poll_aux delta instr =
    let (desc, next) = match instr.desc with
        (* terminating condition *)
        | Iend -> (instr.desc, instr.next)

        (* no control-flow change operations *)
        | Iop Imove
        | Iop Ispill
        | Iop Ireload
        | Iop Iconst_int _
        | Iop Iconst_float _
        | Iop Iconst_symbol _
        | Iop Istackoffset _
        | Iop Iload _
        | Iop Istore _
        | Iop Ialloc _
        | Iop Iintop _
        | Iop Iintop_imm _
        | Iop Inegf
        | Iop Iabsf
        | Iop Iaddf
        | Iop Isubf
        | Iop Imulf
        | Iop Idivf
        | Iop Ifloatofint
        | Iop Iintoffloat
        | Iop Ispecific _
        | Iop Iname_for_debugger _ ->
            let next = 
            if (delta > lmax) then begin
                if (is_addr_live instr.next.live) then begin
                    insert_poll_aux delta instr.next end
                else begin
                    insert_poll_instr (insert_poll_aux 0 instr.next) instr.next
                end
            end else begin
                insert_poll_aux (delta + 1) instr.next
            end
            in
            (instr.desc , next)
        (* control-flow changing operations *)
        | Iop (Icall_ind _)
        | Iop (Icall_imm _)
        | Iop (Itailcall_ind _)
        | Iop (Itailcall_imm _)
        | Iop (Iextcall _) ->
            let (desc, next) = 
                if (delta > (lmax - e)) then begin
                    if (is_addr_live instr.live) then begin
                        (instr.desc, insert_poll_aux (lmax-e) instr.next)
                    end else begin
                        let updated_instr = {instr with next = insert_poll_aux (lmax-e) instr.next} in
                        (Iop (Ipoll), updated_instr)
                    end
                end else begin
                    (instr.desc, insert_poll_aux (lmax-e) instr.next)
                end
            in
            (desc, next)
        (* complex instructions *)
        | Ireturn ->
            let (desc, next) = 
                if (delta > (lmax - e)) then begin
                    if (is_addr_live instr.live) then begin
                        (instr.desc, insert_poll_aux (lmax-e) instr.next)
                    end else begin
                        let updated_instr = {instr with next = insert_poll_aux (lmax-e) instr.next} in
                        (Iop (Ipoll), updated_instr)
                    end
                end else begin
                    (instr.desc, instr.next)
                end
            in
            (desc, next)
        | Iifthenelse (test, i1, i2) ->
            (Iifthenelse (test, insert_poll_aux delta i1, insert_poll_aux delta i2), insert_poll_aux (lmax-e) instr.next)
        | Iswitch (iarr, instrs) ->
            (Iswitch (iarr, Array.map (fun i -> insert_poll_aux delta i) instrs), insert_poll_aux (lmax-e) instr.next)
        | Icatch (_rec_flag, handlers, body) ->
            let new_body = insert_poll_aux delta body in
            let new_handlers = List.map (fun (n,i) -> (n, insert_poll_aux (lmax-e) i)) handlers in
            (Icatch (_rec_flag, new_handlers, new_body), instr.next)
        | Iexit _ ->
            let (desc, next) = 
                if (delta > (lmax - e)) then begin
                    if (is_addr_live instr.live) then begin
                        (instr.desc, insert_poll_aux (lmax-e) instr.next)
                    end else begin
                        let updated_instr = {instr with next = insert_poll_aux (lmax-e) instr.next} in
                        (Iop (Ipoll), updated_instr)
                    end
                end else begin
                    (instr.desc, instr.next)
                end
            in
            (desc, next)
        | Itrywith (body, handler) ->
            (Itrywith (insert_poll_aux delta body, insert_poll_aux (lmax-e) handler) , insert_poll_aux (lmax-e) instr.next)
        | Iraise _ ->
            let (desc, next) = 
                if (delta > (lmax - e)) then begin
                    if (is_addr_live instr.live) then begin
                        (instr.desc, insert_poll_aux (lmax-e) instr.next)
                    end else begin
                        let updated_instr = {instr with next = insert_poll_aux (lmax-e) instr.next} in
                        (Iop (Ipoll), updated_instr)
                    end
                end else begin
                    (instr.desc, instr.next)
                end
            in
            (desc, next)
        | Iop (Ipoll) -> assert false
    in
    { instr with desc ; next }

let insert_poll fun_body =
    insert_poll_aux (lmax-e) fun_body

let fundecl f =
    let new_body =
      if !add_poll then
        insert_poll f.fun_body
      else
        f.fun_body
    in
      { f with fun_body = new_body }
