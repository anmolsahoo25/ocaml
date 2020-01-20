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
    let (desc, next) = match instr.desc, instr.next.desc with
        (* terminating condition *)
        | Iend, _ -> (instr.desc, instr.next)

        (* no control-flow change operations *)
        | Iop Imove , _
        | Iop Ispill , _
        | Iop Ireload , _
        | Iop Iconst_int _ , _
        | Iop Iconst_float _ , _
        | Iop Iconst_symbol _ , _
        | Iop Istackoffset _ , _
        | Iop Iload _ , _
        | Iop Istore _ , _
        | Iop Ialloc _ , _
        | Iop Iintop _ , _
        | Iop Iintop_imm _ , _
        | Iop Inegf , _
        | Iop Iabsf , _
        | Iop Iaddf , _
        | Iop Isubf , _
        | Iop Imulf , _
        | Iop Idivf , _
        | Iop Ifloatofint , _
        | Iop Iintoffloat , _
        | Iop Ispecific _ , _
        | Iop Iname_for_debugger _ , _ ->
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
        | _ , Iop (Icall_ind _)
        | _ , Iop (Icall_imm _)
        | _ , Iop (Itailcall_ind _)
        | _ , Iop (Itailcall_imm _) ->
            let next = 
            if (delta > (lmax -e)) then begin
                if (is_addr_live instr.next.live) then begin
                    insert_poll_aux delta instr.next
                end else begin
                    insert_poll_instr (insert_poll_aux (lmax - e) instr.next) instr.next
                end
            end else begin
                insert_poll_aux (lmax - e) instr.next
            end
            in
            (instr.desc, next)
        (* complex instructions *)
        | Ireturn, _ ->
            let next = 
            if (delta < e) then begin
                insert_poll_aux delta instr.next
            end else begin
                if (delta > lmax - e) then begin
                    if (is_addr_live instr.next.live) then begin
                        insert_poll_aux delta instr.next
                    end else begin
                        insert_poll_instr (insert_poll_aux 0 instr.next) instr.next
                    end
                end else begin
                    insert_poll_aux (delta + 1) instr.next
                end
            end
            in
            (instr.desc, next)
        | _,_ -> (instr.desc, instr.next)
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
