(** Native lifter of ARM instructions to the BAP IL **)

(* create and open target-specific modules *)
module McModules = McFactory.Make(McARM.McEnum)
open McModules
open McARM.McEnum

open Int64
open Ast
open BatPervasives
open Big_int_Z
open Big_int_convenience
open Type
open BatListFull
open Arch

(* Purposefully placed below BatPervasives *)
open Ast_convenience

module VH = Var.VarHash

module D = Debug.Make(struct let name = "Disasm_ARM" and default=`NoDebug end)
open D

(* Convenience aliases for var definitions *)
let new_temp = Var_temp.nt

(* MCInst-based ARM register definitions *)
module R = struct         (* TODO: functorize and go into a separate file *)
  let new_reg = Var.newvar

  (* LLVM's convention for invalid / unknown register *)
  let noregister = new_reg "NoRegister" reg_32

  (* memory definition *)
  let mem = Var.newvar "mem32" (TMem (reg_32, reg_8))

  (* arithmetic flags individually *)
  let nf = new_reg "NF" reg_1
  let zf = new_reg "ZF" reg_1
  let cf = new_reg "CF" reg_1
  let vf = new_reg "VF" reg_1
  let qf = new_reg "QF" reg_1
  let ge = new_reg "GE" reg_4

  (* Saved Program Status Register *)
  let spsr = new_reg "SPSR" reg_32

  (* Thumb if-then state register *)
  let itstate = new_reg "ITSTATE" reg_8

  (* Core registers: Link register, program counter, stack pointer *)
  let lr = new_reg "LR" reg_32
  let pc = new_reg "PC" reg_32
  let sp = new_reg "SP" reg_32

  (* 32-bit GP registers *)
  let r0 = new_reg "R0" reg_32
  let r1 = new_reg "R1" reg_32
  let r2 = new_reg "R2" reg_32
  let r3 = new_reg "R3" reg_32
  let r4 = new_reg "R4" reg_32
  let r5 = new_reg "R5" reg_32
  let r6 = new_reg "R6" reg_32
  let r7 = new_reg "R7" reg_32
  let r8 = new_reg "R8" reg_32
  let r9 = new_reg "R9" reg_32
  let r10 = new_reg "R10" reg_32
  let r11 = new_reg "R11" reg_32
  let r12 = new_reg "R12" reg_32
  (* Core registers are aliased to r13-15 *)
  let r13 = sp
  let r14 = lr
  let r15 = pc

  (* Floating point registers *)
  let fpexec = new_reg "FPEXC" reg_32
  let fpinst = new_reg "FPINST" reg_32
  let fpinst2 = new_reg "FPINST2" reg_32
  let fpscr = new_reg "FPSCR" reg_32
  let fpscr_nzcv = new_reg "FPSCR_NZCV" reg_32
  let fpsid = new_reg "FPSID" reg_32

  (* 64-bit SIMD registers *)
  let d0 = new_reg "D0" reg_64
  let d1 = new_reg "D1" reg_64
  let d2 = new_reg "D2" reg_64
  let d3 = new_reg "D3" reg_64
  let d4 = new_reg "D4" reg_64
  let d5 = new_reg "D5" reg_64
  let d6 = new_reg "D6" reg_64
  let d7 = new_reg "D7" reg_64
  let d8 = new_reg "D8" reg_64
  let d9 = new_reg "D9" reg_64
  let d10 = new_reg "D10" reg_64
  let d11 = new_reg "D11" reg_64
  let d12 = new_reg "D12" reg_64
  let d13 = new_reg "D13" reg_64
  let d14 = new_reg "D14" reg_64
  let d15 = new_reg "D15" reg_64
  let d16 = new_reg "D16" reg_64
  let d17 = new_reg "D17" reg_64
  let d18 = new_reg "D18" reg_64
  let d19 = new_reg "D19" reg_64
  let d20 = new_reg "D20" reg_64
  let d21 = new_reg "D21" reg_64
  let d22 = new_reg "D22" reg_64
  let d23 = new_reg "D23" reg_64
  let d24 = new_reg "D24" reg_64
  let d25 = new_reg "D25" reg_64
  let d26 = new_reg "D26" reg_64
  let d27 = new_reg "D27" reg_64
  let d28 = new_reg "D28" reg_64
  let d29 = new_reg "D29" reg_64
  let d30 = new_reg "D30" reg_64
  let d31 = new_reg "D31" reg_64

  (* 128-bit SIMD registers *)
  let q0 = new_reg "Q0" reg_128
  let q1 = new_reg "Q1" reg_128
  let q2 = new_reg "Q2" reg_128
  let q3 = new_reg "Q3" reg_128
  let q4 = new_reg "Q4" reg_128
  let q5 = new_reg "Q5" reg_128
  let q6 = new_reg "Q6" reg_128
  let q7 = new_reg "Q7" reg_128
  let q8 = new_reg "Q8" reg_128
  let q9 = new_reg "Q9" reg_128
  let q10 = new_reg "Q10" reg_128
  let q11 = new_reg "Q11" reg_128
  let q12 = new_reg "Q12" reg_128
  let q13 = new_reg "Q13" reg_128
  let q14 = new_reg "Q14" reg_128
  let q15 = new_reg "Q15" reg_128

  (* 32-bit floating point registers *)
  let s0 = new_reg "S0" reg_32
  let s1 = new_reg "S1" reg_32
  let s2 = new_reg "S2" reg_32
  let s3 = new_reg "S3" reg_32
  let s4 = new_reg "S4" reg_32
  let s5 = new_reg "S5" reg_32
  let s6 = new_reg "S6" reg_32
  let s7 = new_reg "S7" reg_32
  let s8 = new_reg "S8" reg_32
  let s9 = new_reg "S9" reg_32
  let s10 = new_reg "S10" reg_32
  let s11 = new_reg "S11" reg_32
  let s12 = new_reg "S12" reg_32
  let s13 = new_reg "S13" reg_32
  let s14 = new_reg "S14" reg_32
  let s15 = new_reg "S15" reg_32
  let s16 = new_reg "S16" reg_32
  let s17 = new_reg "S17" reg_32
  let s18 = new_reg "S18" reg_32
  let s19 = new_reg "S19" reg_32
  let s20 = new_reg "S20" reg_32
  let s21 = new_reg "S21" reg_32
  let s22 = new_reg "S22" reg_32
  let s23 = new_reg "S23" reg_32
  let s24 = new_reg "S24" reg_32
  let s25 = new_reg "S25" reg_32
  let s26 = new_reg "S26" reg_32
  let s27 = new_reg "S27" reg_32
  let s28 = new_reg "S28" reg_32
  let s29 = new_reg "S29" reg_32
  let s30 = new_reg "S30" reg_32
  let s31 = new_reg "S31" reg_32

  (* Feature registers *)
  let mvfr0 = new_reg "MVFR0" reg_32
  let mvfr1 = new_reg "MVFR1" reg_32
  let mvfr2 = new_reg "MVFR2" reg_32

  let regs_arm : var list =
    [r0; r1; r2; r3; r4; r5; r6; r7; r8; r9; r10; r11; r12; sp; lr; pc; nf; zf; cf; vf; qf; ge]
end

(* memory access operations *)
module M = struct (* TODO functorize into seperate file *)

  (* Our memory definition *)
  let mem = R.mem

  exception Mem_ARM_exception of string

  (* Temporary until we can break this module out *)
  let move v e = Move(v, e, [])
  let int2e i = Int(biconst i, reg_32)

  (* Temporary until we can break this module out *)
  let assn d s =
    if d = R.pc
    then Jmp(s, [])
    else move d s

  let load dest addr t = move dest (Ast.Load(Var mem, addr, little_endian, t))
  let store data addr t = move mem (Ast.Store(Var mem, addr, data, little_endian, t))

  (* Types for single register memory access *)
  type mode_r = Offset | Preindex | Postindex
  type sign = Signed | Unsigned
  type size = B | H | W | D
  type operation = Load | Store

  (* TODO check for misaligned access *)
  (* single register memory access *)
  let access_r dest1 ?dest2 base offset mode sign size operation =
    let base_e = Var base in
    let dest1_e = Var dest1 in
    let o_base = Var_temp.nt "orig_base" reg_32 in
    let o_offset = Var_temp.nt "orig_offset" reg_32 in

    (* If this load is a jump (only valid for 4byte load)
     * We need to do the write_back before the load so we
     * Use the originals
    *)
    let address = match mode,operation,size,dest1 with
      | Preindex,Load,W,d when d = R.pc -> Var o_base
      | Postindex,Load,W,d when d = R.pc -> Var o_base +* Var o_offset
      | Preindex,_,_,_ -> base_e
      | _ -> base_e +* offset in

    (* Create temps for original if this is a jump *)
    let pre_write_back = match mode, operation, size, dest1 with
      | Preindex,Load,W,d when d = R.pc ->
        [move o_base base_e;
         move base (base_e +* offset)]
      | Postindex,Load,W,d when d = R.pc ->
        [move o_base base_e;
         move o_offset offset;
         move base (base_e +* offset)]
      | Offset,_,_,_ -> []
      | _ -> [] in

    let write_back = match mode, operation, size, dest1 with
      | Preindex,Load,W,d when d = R.pc -> []
      | Postindex,Load,W,d when d = R.pc -> []
      | Offset,_,_,_ -> []
      | _ -> [move base (base_e +* offset)] in

    let t = match size with
      | B -> reg_8
      | H -> reg_16
      | W -> reg_32
      | D -> reg_32 in

    (* Get the second destination of a Dword operation *)
    let check_dest2 = function
      | Some e -> e
      | _ -> raise (Mem_ARM_exception "incorrect destination for Dword access") in

    match operation with
    | Load ->
      let temp = if size = B || size = H
        then Var_temp.nt "temp" t
        else dest1 in

      let cast = match sign with
        | Unsigned -> cast_unsigned
        | Signed -> cast_signed in

      let extend = match size with
        | B -> [move dest1 (cast reg_32 (Var temp))]
        | H -> [move dest1 (cast reg_32 (Var temp))]
        | W -> []
        | D -> [] in

      let loads =
        if size = D then
          let dest2 = check_dest2 dest2 in

          [move dest1 (Ast.Load(Var mem, address, little_endian, t));
           move dest2 (Ast.Load(Var mem, address +* Int (bi4, reg_32), little_endian, t))]
        else
          (* perform the load *)
          [assn temp (Ast.Load(Var mem, address, little_endian, t))]
      in

      pre_write_back
      @ loads
      (* sign/zero extend if necessary *)
      @ extend
      @ write_back

    | Store ->
      let temp = if size = B || size = H
        then Var_temp.nt "temp" t
        else dest1 in

      (* truncate the value if necessary *)
      let trunc = match size with
        | B -> [move temp (cast_low reg_8 dest1_e)]
        | H -> [move temp (cast_low reg_16 dest1_e)]
        | W -> []
        | D -> [] in

      let stores =
        if size = D
        then
          let dest2 = check_dest2 dest2 in

          [move mem (Ast.Store(Var mem, address, Var dest1, little_endian, t));
           move mem (Ast.Store(Var mem, address +* Int(bi4, reg_32), Var dest2, little_endian, t))]
        else
          [move mem (Ast.Store(Var mem, address, Var temp, little_endian, t))]
      in

      (* truncate the value if necessary *)
      trunc
      (* Do the store *)
      @ stores
      @ write_back

  type mode_m = IA | IB | DA | DB
  type update_m = Update | Noupdate

  let access_m dest_list base mode update operation =
    let base_e = Var base in
    let o_base = Var_temp.nt "orig_base" reg_32 in

    let calc_offset ith =
      match mode with
      | IB -> 4 * (ith + 1)
      | DB -> -4 * (ith + 1)
      | IA -> 4 * ith
      | DA -> -4 * ith in

    let writeback =
      match update,mode with
      | Update,IB
      | Update,IA -> [move base (base_e +* (int2e (4*(List.length dest_list))))]
      | Update,DB
      | Update,DA -> [move base (base_e +* (int2e (-4*(List.length dest_list))))]
      | Noupdate,_ -> [] in

    (* Don't want to be dependant on OCaml 4.0 *)
    let rec mapi i f = function
      | [] -> []
      | a :: l -> let r = f i a in r :: mapi (i + 1) f l in

    let mapi f l = mapi 0 f l in

    let create_access i dest =
      let offset_e = int2e (calc_offset i) in
      match operation with
      | Load -> assn dest (Ast.Load(Var mem, (Var o_base) +* offset_e, little_endian, reg_32))
      | Store -> move mem (Ast.Store(Var mem, (Var o_base) +* offset_e, Var dest, little_endian, reg_32)) in

    move o_base base_e
    :: writeback
    @mapi create_access dest_list

end

open McInst
open McOperand

exception Disasm_ARM_exception of string

let bi_of_string str =
  let build_bi (pos, accum) i =
    (pos + 1, accum +% (shift_left_big_int (biconst i) (pos * 8))) in
  let explode s =           (* FAQ *)
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  in
  let chars = explode str in
  let ints = List.map Char.code chars in
  let (_, instr) = List.fold_left build_bi (0,bi0) ints in
  instr

(* AST building helpers *)
let move v e =
  Move(v, e, [])

let mcreg_to_var = function
  | McR0 -> R.r0
  | McR1 -> R.r1
  | McR2 -> R.r2
  | McR3 -> R.r3
  | McR4 -> R.r4
  | McR5 -> R.r5
  | McR6 -> R.r6
  | McR7 -> R.r7
  | McR8 -> R.r8
  | McR9 -> R.r9
  | McR10 -> R.r10
  | McR11 -> R.r11
  | McR12 -> R.r12
  | McLR -> R.lr
  | McPC -> R.pc
  | McSP -> R.sp
  | _ -> raise (Disasm_ARM_exception "unimplemented McReg")

let mcreg_to_multivar = function
  | McR12_SP -> (R.r12, R.sp)
  | McR0_R1 -> (R.r0, R.r1)
  | McR2_R3 -> (R.r2, R.r3)
  | McR4_R5 -> (R.r4, R.r5)
  | McR6_R7 -> (R.r6, R.r7)
  | McR8_R9 -> (R.r8, R.r9)
  | McR10_R11 -> (R.r10, R.r11)
  | _ -> raise (Disasm_ARM_exception "unimplemented McReg")

let mcreg_to_exp r = Var(mcreg_to_var r)

let mcimm_to_exp imm size = Int(biconst64 imm, size)

let mcoperand_to_exp opr size =
  match opr with
  | McReg reg -> mcreg_to_exp reg
  | McImm imm -> mcimm_to_exp imm size
  | _ -> raise (Disasm_ARM_exception "unimplemented McOperand (exp)")

let op2e o = mcoperand_to_exp o reg_32

let mcoperand_to_var = function
  | McReg reg -> mcreg_to_var reg
  | _ -> raise (Disasm_ARM_exception "unimplemented McOperand (var)")

let op2v = mcoperand_to_var

let int2e i = Int(biconst i, reg_32)

(* Generates an expression for the given McCond *)
let set_cond mccond =
  let int0 = Int(bi0, reg_1) in
  let int1 = Int(bi1, reg_1) in
  let z = Var R.zf in
  let c = Var R.cf in
  let v = Var R.vf in
  let n = Var R.nf in
  match mccond_of_int64 mccond with
  | McCondEQ -> z ==* int1
  | McCondNE -> z ==* int0
  | McCondCS -> c ==* int1
  | McCondCC -> c ==* int0
  | McCondMI -> n ==* int1
  | McCondPL -> n ==* int0
  | McCondVS -> v ==* int1
  | McCondVC -> v ==* int0
  | McCondHI -> (c ==* int1) &* (z ==* int0)
  | McCondLS -> (c ==* int0) |* (z ==* int1)
  | McCondGE -> n ==* v
  | McCondLT -> n <>* v
  | McCondGT -> (z ==* int0) &* (n ==* v)
  | McCondLE -> (z ==* int1) |* (n <>* v)
  | McCondAL -> int1

(* the amount that the pc is ahead of the instruction *)
let pc_offset = bi8

(* executes the "stmts" if the condition holds.
 * executes the "flags" if wflag is CPSR and
 * the condition holds
*)
let exec stmts ?flags ?wflag cond =
  (* write to the flags if wflag is CPSR *)
  let stmts = match flags, wflag with
    | Some f, Some (McReg McCPSR) -> stmts@f
    | _ -> stmts in

  let c = match cond with
    | McImm c -> c
    | _ -> raise (Disasm_ARM_exception "Condition must be McImm") in

  (* We shortcut if the condition = all *)
  if mccond_of_int64 c = McCondAL
  then stmts
  else let skiplabel = newlab ~pref:"skip" () in
    ncjmp (set_cond c) (exp_of_lab skiplabel)
    @ stmts
    @ [Label(skiplabel, [])]

(* performs an assignment, turning PC writes into jumps *)
let assn d s =
  if d = R.pc
  then Jmp(s, [])
  else move d s

(* performs an assn of s to d if cond holds with the flags value *)
let simp_exec d s cond flags wflag =
  exec [assn d s] cond ~flags ~wflag

(* Adds the label and sets the appropriate value of the PC to prep an instruction *)
let add_pc_and_labels ?(asm) addr ir =
  let attr = match asm with None -> [] | Some s -> [Asm(s)] in
  Label(Addr addr, attr)
  :: Label(Name(Printf.sprintf "pc_0x%s" (Util.big_int_to_hex addr)),[])
  (* set PC = instruction addr + 8 *)
  :: move R.pc (Int(addr +% pc_offset, reg_32))
  :: ir


(** Functions for shifts **)

(* Shift types and conversion from integer to type *)
type shift = ASR | LSL | LSR | ROR | RRX

let shift_of_int64 i =
  match to_int i with
  | 1 -> ASR
  | 2 -> LSL
  | 3 -> LSR
  | 4 -> ROR
  | 5 -> RRX
  | _ -> raise (Disasm_ARM_exception ("Unimplemented shift type"))

(* Need the operand and the carry flag value *)
(* src_e - the source, if you intend to use the carry bit, this
 *   must not be the destination of the shift expression.
 *   This means it must be a temp that contains the
 *   value of the source not the actual source itself
 * shift_type - the type of shift
 * shift_e - must be a exp that is the amount of the shift (ignored for rrx)
*)
let shift_c src_e shift_type shift_e t =
  let bits = Typecheck.bits_of_width t in
  let bits_e = Int(biconst bits, t) in
  let nth_bit n e = cast_low reg_1 (e >>* n) in
  match shift_type with
  | ASR -> (* asr *)
    let shifted = src_e >>>* shift_e in
    let carry = nth_bit (shift_e -* Int(bi1,t)) src_e in
    (shifted, carry)
  | LSL -> (* lsl *)
    let shifted = binop LSHIFT src_e shift_e in
    let carry = nth_bit (bits_e -* shift_e) src_e in
    (shifted, carry)
  | LSR -> (* lsr *)
    let shifted = binop RSHIFT src_e shift_e in
    let carry = nth_bit (shift_e -* Int(bi1,t)) src_e in
    (shifted, carry)
  | ROR -> (* ror *)
    let ret1 = binop RSHIFT src_e shift_e in
    let ret2 = binop LSHIFT src_e (bits_e -* shift_e) in
    let shifted = ret1 |* ret2 in
    let carry = nth_bit (shift_e -* Int(bi1,t)) src_e in
    (shifted, carry)
  | RRX -> (* rrx *)
    let ret1 = binop RSHIFT src_e (Int (bi1, t)) in
    let carryin = binop LSHIFT (cast_unsigned t (Var R.cf)) (bits_e -* Int(bi1, t)) in
    let shifted = ret1 |* carryin in
    let carry = nth_bit (Int(bi0,t)) src_e in
    (shifted, carry)

(* decodes a register shifted operand
 * src - the operand to be shifted, cannot be the destination
 *        in practice this means it must be a temp variable.
 * shift_type - an int64, bits 2 through 0 represent the shift type
 *              valid shift types are number 1 through 5
 * shift - the value to shift by
 * t - the type
*)
let r_shift src shift_type shift t =
  match shift_type with
  | McImm i64 -> shift_c src (shift_of_int64 i64) shift t
  | _ -> raise (Disasm_ARM_exception ("Unimplemented shift_type"))

(* decodes an immediate shifted operand
 * src - the operand to be shifted, cannot be the destination
 *        in practice this means it must be a temp variable.
 * shift_type - an int64, bits 2 through 0 represent the shift type
 *              valid shift types are number 1 through 5
 *              bits 3 and higher represent the shift amount if this is an
 *              immediate shift. For register shifts these upper bits are 0.
 *              If the shift type is RRX, a shift amount of 1 is implied.
 * t - the type
*)
let i_shift src shift_type t =
  match shift_type with
  | McImm i64 ->
    (* lower three bits are type*)
    let shift_t = shift_of_int64 (logand i64 0x7L) in
    (* other bits are immediate *)
    let shift_amt = shift_right (logand i64 (lognot 0x7L)) 3 in
    shift_c src shift_t (Int(biconst64 shift_amt, t)) t
  | _ -> raise (Disasm_ARM_exception ("Unimplemented shift_type"))


(* decodes a shifted operand for a memory operation
 * src - the operand to be shifted
 * shift - an int64,
 *            bits 11 through 0 represent the shift amount
 *            bits 12 represents whether the expression is added or subtracted
 *            bits 15 through 13 represent the shift type, valid shift types
 *              are number 1 through 5
 * t - the type
*)
let mem_shift src shift t =
  (* Gets the shift type from the immediate *)
  let shift_type i =
    shift_of_int64 (shift_right (logand i 0xE000L) 13) in

  (* Gets the shift amount from the immediate *)
  let shift_amt i =
    Int(biconst64 (logand i 0xFFFL), t) in

  (* Converts the shift to a negative if the negative bit is set *)
  let to_neg i exp =
    if (logand 0x1000L i) = 0x1000L
    then Int(bim1,t) ** exp else exp in

  match shift with
  | McImm i64 ->
    let (exp, _) = shift_c src (shift_type i64) (shift_amt i64) t in
    to_neg i64 exp
  | _ -> raise (Disasm_ARM_exception ("Unimplemented mem_shift_type"))

(** Functions to help memory accesses **)

(* wrapper for memory access *)
let access_r_wrapper dest1 ?dest2 ?shift base offset mode sign size operation =
  let base = op2v base in

  (* if offset is int32_min, it is actually "negative" zero *)
  (* can't use Int32.min, because it does a signed conversion *)
  let min_32 = shift_left 1L 31 in
  let offset = match offset with
    | McImm i64 when i64 = min_32 -> Int(bi0, reg_32)
    | _ -> op2e offset in

  let offset = match shift with
    | Some shft -> mem_shift offset shft reg_32
    | None -> offset in

  (* If dest1 is a multireg, convert it into a dest1, dest2 pair *)
  match dest1, dest2, size with
  | McReg multireg, None, M.D ->
    let (d1, d2) = mcreg_to_multivar multireg in
    M.access_r d1 ~dest2:d2 base offset mode sign M.D operation
  | d1, Some d2, size ->
    M.access_r (op2v d1) ~dest2:(op2v d2) base offset mode sign size operation
  | d1, None, size  ->
    M.access_r (op2v d1) base offset mode sign size operation

(* Different wrapper for memory access *)
let access_r_wrapper2 dest1 ?dest2 base offset mode sign size operation =
  let dest1 = op2v dest1 in
  let base = op2v base in

  match dest2 with
  | Some dest2 ->
    M.access_r dest1 ~dest2:(op2v dest2) base offset mode sign size operation
  | None ->
    M.access_r dest1 base offset mode sign size operation

(* takes an int64 src and converts it to an Ast.exp that is the offset
 * for some memory instructions
 * neg_bmask - a bitmask that determines the bit in the src that is
 *              the repair bit
 * imm_bmask - a bitmask that determines which bits in the src are the immediate
 * rtype - whether a set mask indicates a positive or negative immediate.
 *
*)
type repair_type = POS | NEG
let mem_repair_imm src sign_bmask imm_bmask rtype =
  let bit_set = (logand (of_int sign_bmask) src) = (of_int sign_bmask) in
  let negate = (bit_set && (rtype = NEG)) || (not bit_set && (rtype = POS)) in
  let offset = (logand src (of_int imm_bmask)) in
  Int(biconst64 (if negate then mul minus_one offset else offset), reg_32)

(* takes an int64 immediate and a register and converts it to an Ast.exp that
 * is the offset for some memory instructions
 * neg_bmask - a bitmask that determines the bit in the src that is
 *              the negative bit
 * rtype - whether a set mask indicates a positive or negative operand.
*)
let mem_repair_reg reg imm sign_bmask rtype =
  let bit_set = (logand (of_int sign_bmask) imm) = (of_int sign_bmask) in
  let negate = (bit_set && (rtype = NEG)) || (not bit_set && (rtype = POS)) in
  if negate then Int(bim1,reg_32) ** reg else reg

(* Decides whether to use the register or immediate as the offset value
 * Also performs conversion to remove the negative bit and the
*)
let mem_offset_reg_or_imm reg_off imm_off =
  match reg_off with
  | McReg McNoRegister -> mem_repair_imm imm_off 0x100 0xff NEG
  | reg -> mem_repair_reg (op2e reg) imm_off 0x100 NEG

(* A different kind of mem_offset *)
let mem_offset_reg_or_imm2 reg_off imm_off =
  match reg_off with
  | McReg McNoRegister -> mem_repair_imm imm_off 0x100 0xff POS
  | reg -> mem_repair_reg (op2e reg) imm_off 0x1 POS

(* wrapper for memory access *)
let access_m_wrapper dest_list base mode update operation =
  let dest_list = List.map op2v dest_list in
  let base = op2v base in
  M.access_m dest_list base mode update operation


module B = struct

  let branch operand ?link ?x ?cond addr =
    let target =
      match operand with
      | McReg r -> op2e (McReg r)
      | McImm offset ->
        (* if offset is int32_min, it is actually "negative" zero *)
        (* can't use Int32.min, because it does a signed conversion *)
        let min_32 = shift_left 1L 31 in
        let offset = if offset = min_32 then zero else offset in
        Int(addr +% pc_offset +% (biconst64 offset), reg_32)
      | _ -> raise (Disasm_ARM_exception "unimplemented McOperand (exp)") in

    (* TODO detect change to thumb *)
    let _ = match x with
      | Some true -> ()
      | _ -> () in

    let jump = [Jmp(target, [])] in

    let link_instr = match link with
      | Some true -> [move R.lr (Int(addr +% pc_offset -% bi4, reg_32))]
      | _ -> [] in

    match cond with
    | Some c -> exec (link_instr@jump) c
    | None -> link_instr@jump

end

module Data = struct
  exception Data_ARM_exception of string

  (** Functions handling the computation of flags **)
  let compute_nf result = cast_high reg_1 result
  let compute_zf result t = Int(bi0, t) ==* result

  let set_nf r = move R.nf (compute_nf r)
  let set_zf r t = move R.zf (compute_zf r t)

  let set_nzf r t =
    [set_nf r;
     set_zf r t]

  let set_vnzf_add s1 s2 r t =
    move R.vf (cast_high reg_1 ((s1 =* s2) &* (s1 ^* r)))
    :: set_nzf r t

  let set_flags_add s1 s2 r t =
    move R.cf (r <* s1)
    :: set_vnzf_add s1 s2 r t

  let set_flags_sub s1 s2 r t =
    set_flags_add s1 (exp_not s2 +* Int(bi1, t)) r t

  let set_flags_adc s1 s2 r t =
    (* Computes the carry flag by performing the addition *)
    let bits = Typecheck.bits_of_width t in
    let t' = Reg (bits + 1) in
    let c = cast_unsigned t' in
    let bige = c s1 +* c s2 +* c (Var R.cf) in
    move R.cf (extract bits bits bige)
    :: set_vnzf_add s1 s2 r t

  let set_flags_sbc s1 s2 r t =
    set_flags_adc s1 (exp_not s2) r t

  let set_cf_data_imm imm raw =
    if (biconst 255 >=% biconst64 imm && biconst64 imm >=% bi0)
    then let instr = bi_of_string raw in
      if (instr &% biconst 0xf00 ==% bi0)
      then Var R.cf
      else Int(bi0, reg_1)
    else cast_high reg_1 (Int(biconst64 imm, reg_32))



  type data_instr = AND | BIC | EOR | MOV | MVN | ORR |
                    SUB | RSB | ADD | ADC | SBC | RSC

  let data_instr ?dest src1 ?src2 itype ?sreg ?simm raw cond wflag =
    let d = match dest with
      | Some dest -> op2v dest
      | None -> new_temp "dest" reg_32 in

    let s1 = op2e src1 in

    let s2 = match src2 with
      | Some src -> op2e src
      | None -> Int(bi0, reg_32) in

    (* Do the register shift *)
    let (s1, s2, stmts, carry) = match itype, sreg, simm with
      | MOV, Some sreg, Some simm
      | MVN, Some sreg, Some simm ->
        let unshifted = new_temp "unshifted" reg_32 in
        let (shifted, carry) = r_shift (Var unshifted) simm (op2e sreg) reg_32 in
        let stmts = [move unshifted s1] in
        (shifted, s2, stmts, carry)
      | _, Some sreg, Some simm ->
        let unshifted = new_temp "unshifted" reg_32 in
        let (shifted, carry) = r_shift (Var unshifted) simm (op2e sreg) reg_32 in
        let stmts = [move unshifted s2] in
        (s1, shifted, stmts, carry)
      | MOV, None, Some simm
      | MVN, None, Some simm ->
        let unshifted = new_temp "unshifted" reg_32 in
        let (shifted, carry) = i_shift (Var unshifted) simm reg_32 in
        let stmts = [move unshifted s1] in
        (shifted, s2, stmts, carry)
      | _, None, Some simm ->
        let unshifted = new_temp "unshifted" reg_32 in
        let (shifted, carry) = i_shift (Var unshifted) simm reg_32 in
        let stmts = [move unshifted s2] in
        (s1, shifted, stmts, carry)
      | _ -> (s1, s2, [], Var R.cf) in

    let (stmts, flags) = match itype, src1, src2 with
      | MOV, McImm i64, _
      | MVN, McImm i64, _
      | AND, _, Some (McImm i64)
      | BIC, _, Some (McImm i64)
      | EOR, _, Some (McImm i64)
      | ORR, _, Some (McImm i64) ->
        let flags = move R.cf (set_cf_data_imm i64 raw)
                    :: set_nzf (Var d) reg_32 in
        (stmts, flags)
      | MOV, _, _
      | MVN, _, _
      | AND, _, _
      | BIC, _, _
      | EOR, _, _
      | ORR, _, _ ->
        let flags = move R.cf carry
                    :: set_nzf (Var d) reg_32 in
        (stmts, flags)
      | itype1, _, _ ->
        let orig1 = new_temp "orig1" reg_32 and orig2 = new_temp "orig2" reg_32 in
        let new_stmts = stmts @ [move orig1 s1; move orig2 s2] in
        let flags = match itype1 with
          | SUB -> set_flags_sub (Var orig1) (Var orig2) (Var d) reg_32
          | RSB -> set_flags_sub (Var orig2) (Var orig1) (Var d) reg_32
          | ADD -> set_flags_add (Var orig1) (Var orig2) (Var d) reg_32
          | ADC -> set_flags_adc (Var orig1) (Var orig2) (Var d) reg_32
          | SBC -> set_flags_sbc (Var orig1) (Var orig2) (Var d) reg_32
          | RSC -> set_flags_sbc (Var orig2) (Var orig1) (Var d) reg_32
          | _ -> raise (Data_ARM_exception "impossible")
        in
        (new_stmts, flags) in

    let oper = match itype with
      | AND -> s1 &* s2
      | BIC -> s1 &* (exp_not s2)
      | EOR -> s1 ^* (s2)
      | MOV -> s1
      | MVN -> exp_not s1
      | ORR -> s1 |* s2
      | SUB -> s1 -* s2
      | RSB -> s2 -* s1
      | ADD -> s1 +* s2
      | ADC -> s1 +* s2 +* cast_unsigned reg_32 (Var R.cf)
      | SBC -> s1 +* (exp_not s2) +* cast_unsigned reg_32 (Var R.cf)
      | RSC -> (exp_not s1) +* s2 +* cast_unsigned reg_32 (Var R.cf) in

    exec (stmts@[assn d oper]) cond ~flags ~wflag

  (* Instructions that set the carry flag for immediates:
   * AND, BIC, EOR, MOV, MVN, ORR, TEQ, TST w/ imm
   * cf is the msb of the ror immediate
   * AND, BIC, EOR, MOV, MVN, ORR, TEQ, TST w/ reg
   * cf is unchanged
   * AND, BIC, EOR, MOV, MVN, ORR, TEQ, TST w/ rsr,rsi
   * cf is carry out

   * instructions that don't follow this are:
   * SUB, RSB, ADD, ADC, SBC, RSC, CMP, CMN
   * cf is always the cf of the addition/subtraction
  *)
end

module Bit = struct
  type sign = Signed | Unsigned
  type size = H | B
  let type_of_size = function
    | H -> reg_16
    | B -> reg_8

  let extend dest src ?src2 sign size rotation cond =
    let rot = match rotation with
      | McImm i64 -> to_int (mul i64 8L)
      | _ -> raise (Disasm_ARM_exception "incorrect operand type") in
    let (rotated, _) = if (rot <> 0)
      then shift_c (op2e src) ROR (int2e rot) reg_32
      else (op2e src, Int(bi0,reg_32)) in
    let extracted = cast_low (type_of_size size) rotated in
    let cast =
      match sign with
      | Unsigned -> cast_unsigned
      | Signed -> cast_signed in
    let extend = cast reg_32 extracted in
    let final = match src2 with
      | Some s2 -> (op2e s2) +* extend
      | None -> extend in
    exec [assn (op2v dest) final] cond

  let b_extract dest src sign lsb widthminus1 cond =
    let op2i op = match op with
      | McImm i64 -> to_int i64
      | _ -> raise (Disasm_ARM_exception "incorrect operand type for bit extract") in

    let low = op2i lsb in
    let high = low + (op2i widthminus1) in
    let extracted = extract high low (op2e src) in
    let cast =
      match sign with
      | Unsigned -> cast_unsigned
      | Signed -> cast_signed in
    let final = cast reg_32 extracted in
    exec [assn (op2v dest) final] cond

  let count_bits_set imm =
    let v = match imm with
      | McImm i64 -> i64
      | _ -> raise (Disasm_ARM_exception "bits_set takes and McImm as input") in

    let c = rem (logand (mul (logand v 0xfffL)  0x1001001001001L) 0x84210842108421L) 0x1fL in
    let c = add c (rem (logand (mul (shift_right (logand v 0xfff000L) 12) 0x1001001001001L) 0x84210842108421L) 0x1fL) in
    to_int (add c (rem (logand (mul (shift_right v 24) 0x1001001001001L) 0x84210842108421L) 0x1fL))

  let get_lsb_width raw =
    let instr = int64_of_big_int (bi_of_string raw) in
    let lsb = logand (shift_right instr 7) 0x1fL in
    let msb = logand (shift_right instr 16) 0x1fL in
    let width = abs (add (sub msb lsb) 1L) in
    (to_int lsb, to_int width)

  let bit_field_insert dest src raw cond =
    (* get bits set  *)
    let d = op2v dest and d_e = op2e dest in
    let (lsb, width) = get_lsb_width raw in
    let extracted = extract (width - 1) 0 (op2e src) in
    let ext_h b s = extract 31 b s in
    let ext_l b s = extract b 0 s in
    let inst = match lsb + width - 1, lsb with
      | 31, 0 -> extracted
      | 31, l -> concat extracted (ext_l (l-1) d_e)
      | m, 0 -> concat (ext_h (m+1) d_e) extracted
      | m, l -> concat (concat (ext_h (m+1) d_e) extracted) (ext_l (l-1) d_e) in
    exec [move d inst] cond
end


module Mul = struct
  exception Mul_ARM_exception of string

  type sign = Signed | Unsigned

  let mull lodest hidest src1 src2 sign ?addend cond wflag =
    let cast =
      match sign with
      | Unsigned -> cast_unsigned
      | Signed -> cast_signed in
    let s1_64 = cast reg_64 (op2e src1) in
    let s2_64 = cast reg_64 (op2e src2) in
    let result = new_temp "result" reg_64 in
    let flags = Data.set_nzf (Var result) reg_64 in
    let opn = match addend with
      | Some _ -> (s1_64 ** s2_64 +* concat (op2e hidest) (op2e lodest))
      | None -> (s1_64 ** s2_64) in

    let insns =
      move result opn
      :: move (op2v lodest) (extract 31 0 (Var result))
      :: move (op2v hidest) (extract 63 32 (Var result))
      :: [] in
    exec insns cond ~flags ~wflag

  (* size: BB, BT, TB, TT, D, DX, WB, WT
  *)
  type smul_size = BB | BT | TB | TT | D | DX | WB | WT
  let smul dest ?hidest src1 src2 ?accum ?hiaccum ?q size cond =
    let src1 = op2e src1 and src2 = op2e src2 in
    let top s = cast_signed reg_64 (extract 31 16 s) in
    let bot s = cast_signed reg_64 (extract 15 0 s) in
    let top32 s = cast_signed reg_64 (extract 47 16 s) in
    let res = new_temp "result_64" reg_64 in
    let result = match size with
      | BB -> bot src1 ** bot src2
      | BT -> bot src1 ** top src2
      | TB -> top src1 ** bot src2
      | TT -> top src1 ** top src2
      | D -> top src1 ** top src2 +* bot src1 ** bot src2
      | DX -> top src1 ** bot src2 +* bot src1 ** top src2
      | WB -> top32 (cast_signed reg_64 src1 ** bot src2)
      | WT -> top32 (cast_signed reg_64 src1 ** top src2)
    in
    let result = match accum, hiaccum with
      | None, None -> result
      | Some a, None -> result +* (cast_signed reg_64 (op2e a))
      | Some a, Some hia -> result +* (concat (op2e hia) (op2e a))
      | _ -> raise (Mul_ARM_exception "Cannot specify only a hi accumulator")
    in
    let qflag = match q with
      | Some true -> [move R.qf (cast_signed reg_64 (extract 31 0 (Var res)) <>* Var res)]
      | _ -> []
    in
    let instr = match hidest with
      | Some hid -> move res result
                    :: move (op2v hid) (extract 63 32 (Var res))
                    :: [move (op2v dest) (extract 31 0 (Var res))]
      | None -> move res result
                :: [move (op2v dest) (extract 31 0 (Var res))]
    in
    exec (instr@qflag) cond
end

(** Core Function to convert to IL from LLVM disassembler **)


(* convert an McInst to BAP IL *)
let bil_of_mcinst mcinst addr =
  match mcinst.mcopcode, mcinst.mcoperands with
  | McMOVi, [dest; src; cond; _; wflag]
  | McMOVr, [dest; src; cond; _; wflag] ->
    Data.data_instr ~dest src Data.MOV mcinst.raw_bytes cond wflag

  | McMOVsr, [dest; src; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.MOV ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McMOVsi, [dest; src; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.MOV ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McMVNi, [dest; src; cond; _; wflag]
  | McMVNr, [dest; src; cond; _; wflag] ->
    Data.data_instr ~dest src Data.MVN mcinst.raw_bytes cond wflag

  | McMVNsr, [dest; src; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.MVN ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McMVNsi, [dest; src; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.MVN ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McANDri, [dest; src1; src2; cond; _; wflag]
  | McANDrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.AND mcinst.raw_bytes cond wflag

  | McANDrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.AND ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McANDrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.AND ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McBICri, [dest; src1; src2; cond; _; wflag]
  | McBICrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.BIC mcinst.raw_bytes cond wflag

  | McBICrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.BIC ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McBICrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.BIC ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McEORri, [dest; src1; src2; cond; _; wflag]
  | McEORrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.EOR mcinst.raw_bytes cond wflag

  | McEORrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.EOR ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McEORrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.EOR ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McORRri, [dest; src1; src2; cond; _; wflag]
  | McORRrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ORR mcinst.raw_bytes cond wflag

  | McORRrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ORR ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McORRrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ORR ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McTEQri, [src1; src2; cond; _]
  | McTEQrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.EOR mcinst.raw_bytes cond (McReg McCPSR)

  | McTEQrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.EOR ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  | McTEQrsi, [dest; src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.EOR ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  | McTSTri, [src1; src2; cond; _]
  | McTSTrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.AND mcinst.raw_bytes cond (McReg McCPSR)

  | McTSTrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.AND ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  | McTSTrsi, [src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.AND ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  | McADDri, [dest; src1; src2; cond; _; wflag]
  | McADDrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ADD mcinst.raw_bytes cond wflag

  | McADDrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ADD ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McADDrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ADD ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McSUBri, [dest; src1; src2; cond; _; wflag]
  | McSUBrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.SUB mcinst.raw_bytes cond wflag

  | McSUBrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.SUB ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McSUBrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.SUB ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McADCri, [dest; src1; src2; cond; _; wflag]
  | McADCrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ADC mcinst.raw_bytes cond wflag

  | McADCrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ADC ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McADCrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.ADC ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McSBCri, [dest; src1; src2; cond; _; wflag]
  | McSBCrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.SBC mcinst.raw_bytes cond wflag

  | McSBCrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.SBC ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McSBCrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.SBC ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McRSBri, [dest; src1; src2; cond; _; wflag]
  | McRSBrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.RSB mcinst.raw_bytes cond wflag

  | McRSBrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.RSB ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McRSBrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.RSB ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McRSCri, [dest; src1; src2; cond; _; wflag]
  | McRSCrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.RSC mcinst.raw_bytes cond wflag

  | McRSCrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.RSC ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McRSCrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.RSC ~simm:shift_imm mcinst.raw_bytes cond wflag

  | McCMPri, [src1; src2; cond; _]
  | McCMPrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.SUB mcinst.raw_bytes cond (McReg McCPSR)

  | McCMPrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.SUB ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  | McCMPrsi, [src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.SUB ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  | McCMNri, [src1; src2; cond; _]
  | McCMNzrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.ADD mcinst.raw_bytes cond (McReg McCPSR)

  | McCMNzrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.ADD ~sreg:shift_reg ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  | McCMNzrsi, [src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.ADD ~simm:shift_imm mcinst.raw_bytes cond (McReg McCPSR)

  (* Special Data Instructions *)
  | McMOVi16, [dest; src; cond; wflag] ->
    let insns = [move (op2v dest) (op2e src)] in
    exec insns cond

  | McMOVTi16, [dest; _; src; cond; wflag] ->
    let insns = [move (op2v dest) (op2e dest |* (binop LSHIFT (op2e src) (Int (biconst 16, reg_32))))] in
    exec insns cond

  (* Additional Data Instructions *)

  (* extends *)
  | McUXTB, [dest; src; rot; cond; _] ->
    Bit.extend dest src Bit.Unsigned Bit.B rot cond

  | McUXTH, [dest; src; rot; cond; _] ->
    Bit.extend dest src Bit.Unsigned Bit.H rot cond

  | McSXTB, [dest; src; rot; cond; _] ->
    Bit.extend dest src Bit.Signed Bit.B rot cond

  | McSXTH, [dest; src; rot; cond; _] ->
    Bit.extend dest src Bit.Signed Bit.H rot cond

  | McUXTAB, [dest; src; shft; rot; cond; _] ->
    Bit.extend dest shft ~src2:src Bit.Unsigned Bit.B rot cond

  | McUXTAH, [dest; src; shft; rot; cond; _] ->
    Bit.extend dest shft ~src2:src Bit.Unsigned Bit.H rot cond

  | McSXTAB, [dest; src; shft; rot; cond; _] ->
    Bit.extend dest shft ~src2:src Bit.Signed Bit.B rot cond

  | McSXTAH, [dest; src; shft; rot; cond; _] ->
    Bit.extend dest shft ~src2:src Bit.Signed Bit.H rot cond
  (* extracts *)
  | McUBFX, [dest; src; lsb; widthminus1; cond; _] ->
    Bit.b_extract dest src Bit.Unsigned lsb widthminus1 cond

  | McSBFX, [dest; src; lsb; widthminus1; cond; _] ->
    Bit.b_extract dest src Bit.Signed lsb widthminus1 cond
  (* reverses *)
  | McREV, [dest; src; cond; _] ->
    let s = op2e src in
    let i24 = int2e 24 in
    let i8 = int2e 8 in
    let umask = int2e 0xff0000 in
    let lmask = int2e 0xff00 in
    let rev = (binop LSHIFT s i24) |*
              (binop RSHIFT s i24 ) |*
              (binop RSHIFT (s &* umask) i8) |*
              (binop LSHIFT (s &* lmask) i8) in
    exec [assn (op2v dest) rev] cond

  | McREV16, [dest; src; cond; _] ->
    let s = op2e src in
    let i16 = int2e 16 in
    let rev = (binop LSHIFT s i16) |*
              (binop RSHIFT s i16) in
    exec [assn (op2v dest) rev] cond

  (* count leading zeroes *)
  (* XXX There may be ways to optimize *)
  | McCLZ, [dest; src; cond; _] ->
    let shift = new_temp "shift" reg_32 in
    let accum = new_temp "accum" reg_32 in
    let l1 = newlab ~pref:"clz_exit" () in
    let l2 = newlab ~pref:"clz_loop" () in

    let insns = move shift (op2e src)
                :: move accum (int2e 32)
                :: Label(l2, [])
                :: cjmp (Var shift ==* int2e 0) (exp_of_lab l1)
                @ move shift (Var shift >>* int2e 1)
                  :: move accum (Var accum -* int2e 1)
                  :: Jmp(exp_of_lab l2, [])
                  :: Label(l1, [])
                  :: move (op2v dest) (Var accum)
                  :: [] in
    exec insns cond

  (* bit field *)
  | McBFI, [dest; unknown; src; bmask; cond; _] ->
    Bit.bit_field_insert dest src mcinst.raw_bytes cond

  | McBFC, [dest; unknown; bmask; cond; _] ->
    Bit.bit_field_insert dest (McImm 0L) mcinst.raw_bytes cond

  (* bit reverse *)
  (* XXX There may be ways to optimize *)
  | McRBIT, [dest; src; cond; _] ->
    let v = new_temp "v" reg_32 in
    let r = new_temp "r" reg_32 in
    let s = new_temp "s" reg_32 in
    let l1 = newlab ~pref:"rbit_exit" () in
    let l2 = newlab ~pref:"rbit_loop" () in

    let insns =
      move v (binop RSHIFT (op2e src) (int2e 1))
      :: move r (op2e src)
      :: move s (int2e 31)
      :: Label(l2, [])
      :: cjmp (Var v ==* int2e 0) (exp_of_lab l1)
      @ move r (binop LSHIFT (Var r) (int2e 1))
        :: move r (Var r |* (Var v &* int2e 1))
        :: move s (Var s -* int2e 1)
        :: Jmp(exp_of_lab l2, [])
        :: Label(l1, [])
        :: move (op2v dest) (binop LSHIFT (Var r) (Var s))
        :: [] in
    exec insns cond

  (* Swap bytes *)
  | McSWPB, [dest; src1; src2; cond; _] ->
    let temp = new_temp "temp" reg_8 in
    let insns = M.load temp (op2e src2) reg_8
                :: M.store (extract 7 0 (op2e src1)) (op2e src2) reg_8
                :: [assn (op2v dest) (cast_unsigned reg_32 (Var temp))] in
    exec insns cond

  (* Pack half *)
  | McPKHTB, [dest; src1; src2; shift; cond; _] ->
    (* shift is always asr *)
    let (shifted, _) = shift_c (op2e src2) ASR (op2e shift) reg_32 in
    let insns = [assn (op2v dest) (concat (extract 31 16 (op2e src1)) (extract 15 0 shifted))] in

    exec insns cond

  (* Multiply *)

  | McMUL, [dest; src1; src2; cond; rflag; wflag] ->
    let flags = Data.set_nzf (op2e dest) reg_32 in
    simp_exec (op2v dest) (op2e src1 ** op2e src2) cond flags wflag

  | McMLA, [dest; src1; src2; addend; cond; rflag; wflag] ->
    let flags = Data.set_nzf (op2e dest) reg_32 in
    simp_exec (op2v dest) ((op2e src1 ** op2e src2) +* op2e addend) cond flags wflag

  | McMLS, [dest; src1; src2; addend; cond; _] ->
    exec [move (op2v dest) (op2e addend -* (op2e src1 ** op2e src2))] cond

  | McUMULL, [lodest; hidest; src1; src2; cond; rflag; wflag] ->
    Mul.mull lodest hidest src1 src2 Mul.Unsigned cond wflag

  | McSMULL, [lodest; hidest; src1; src2; cond; rflag; wflag] ->
    Mul.mull lodest hidest src1 src2 Mul.Signed cond wflag

  | McUMLAL, [lodest; hidest; src1; src2; loadd; hiadd; cond; rflag; wflag] ->
    Mul.mull lodest hidest src1 src2 Mul.Unsigned ~addend:true cond wflag

  | McSMLAL, [lodest; hidest; src1; src2; loadd; hiadd; cond; rflag; wflag] ->
    Mul.mull lodest hidest src1 src2 Mul.Signed ~addend:true cond wflag

  (* signed 16bit mul plus a 32bit bit accum, Q *)
  | McSMLABB, [dest; src1; src2; accum; cond; wflag] ->
    Mul.smul dest src1 src2 ~accum ~q:true Mul.BB cond

  (* signed 16bit mul *)
  | McSMULBB, [dest; src1; src2; cond; wflag] ->
    Mul.smul dest src1 src2 Mul.BB cond

  (* two signed 16bit muls plus 32bit accum and optional xchg, Q*)
  | McSMLAD, [dest; src1; src2; accum; cond; wflag] ->
    Mul.smul dest src1 src2 ~accum ~q:true Mul.D cond

  (* two signed 16bit muls and optional xchg, Q *)
  | McSMUAD, [dest; src1; src2; cond; wflag] ->
    Mul.smul dest src1 src2 ~q:true Mul.D cond

  (* signed 16bit times signed 32bit added to 32bit accum, Q *)
  | McSMLAWB, [dest; src1; src2; accum; cond; wflag] ->
    Mul.smul dest src1 src2 ~accum ~q:true Mul.WB cond

  (* signed 16bit mul *)
  | McSMULTB, [dest; src1; src2; cond; wflag] ->
    Mul.smul dest src1 src2 Mul.TB cond

  (* signed 16bit mul plus 64bit accum *)
  | McSMLALBT, [dest; hidest; src1; src2; cond; wflag] ->
    Mul.smul dest ~hidest src1 src2 ~accum:dest ~hiaccum:hidest Mul.BT cond

  (* Load/Store operations *)

  | McSTRD, [dest1; dest2; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 ~dest2 base offset M.Offset M.Unsigned M.D M.Store in
    exec insns cond

  | McLDRD, [dest1; dest2; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 ~dest2 base offset M.Offset M.Unsigned M.D M.Load in
    exec insns cond

  | McSTRD_POST, [dest1; dest2; base; unknown; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 ~dest2 base offset M.Postindex M.Unsigned M.D M.Store in
    exec insns cond

  | McLDRD_POST, [dest1; dest2; base; unknown; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 ~dest2 base offset M.Postindex M.Unsigned M.D M.Load in
    exec insns cond

  | McSTRD_PRE, [unknown; dest1; dest2; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 ~dest2 base offset M.Preindex M.Unsigned M.D M.Store in
    exec insns cond

  | McLDRD_PRE, [dest1; dest2; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 ~dest2 base offset M.Preindex M.Unsigned M.D M.Load in
    exec insns cond

  | McSTRH, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Offset M.Unsigned M.H M.Store in
    exec insns cond

  | McLDRH, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Offset M.Unsigned M.H M.Load in
    exec insns cond

  | McSTRH_PRE, [unknown; dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Preindex M.Unsigned M.H M.Store in
    exec insns cond

  | McLDRH_PRE, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Preindex M.Unsigned M.H M.Load in
    exec insns cond

  | McSTRH_POST, [unknown; dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.H M.Store in
    exec insns cond

  (* Unlike the convention of all other load and store instructions, for some
   * instructions the sign bit is set in the immediate when the operand
   * is POSITIVE. Insructions that are affected by this are marked with
   * "POS_SIGN_BIT"
  *)
  (* POS_SIGN_BIT *)
  | McSTRHTr, [unknown; dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm2 reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.H M.Store in
    exec insns cond

  | McLDRH_POST, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.H M.Load in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRHTr, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm2 reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.H M.Load in
    exec insns cond

  | McLDRSH, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Offset M.Signed M.H M.Load in
    exec insns cond

  | McLDRSH_PRE, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Preindex M.Signed M.H M.Load in
    exec insns cond

  | McLDRSH_POST, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Signed M.H M.Load in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRSHTr, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm2 reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Signed M.H M.Load in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRSHTi, [dest1; unknown; base; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm2 (McReg McNoRegister) imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Signed M.H M.Load in
    exec insns cond

  | McLDRSB, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Offset M.Signed M.B M.Load in
    exec insns cond

  | McLDRSB_PRE, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Preindex M.Signed M.B M.Load in
    exec insns cond

  | McLDRSB_POST, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Signed M.B M.Load in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRSBTr, [dest1; unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = mem_offset_reg_or_imm2 reg_off imm_off in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Signed M.B M.Load in
    exec insns cond

  | McSTRi12, [dest1; base; offset; cond; _] ->
    let insns = access_r_wrapper dest1 base offset M.Offset M.Unsigned M.W M.Store in
    exec insns cond

  | McLDRi12, [dest1; base; offset; cond; _] ->
    let insns = access_r_wrapper dest1 base offset M.Offset M.Unsigned M.W M.Load in
    exec insns cond

  | McSTRBi12, [dest1; base; offset; cond; _] ->
    let insns = access_r_wrapper dest1 base offset M.Offset M.Unsigned M.B M.Store in
    exec insns cond

  | McLDRBi12, [dest1; base; offset; cond; _] ->
    let insns = access_r_wrapper dest1 base offset M.Offset M.Unsigned M.B M.Load in
    exec insns cond

  | McSTRrs, [dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Offset M.Unsigned M.W M.Store in
    exec insns cond

  | McLDRrs, [dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Offset M.Unsigned M.W M.Load in
    exec insns cond

  | McSTRBrs, [dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Offset M.Unsigned M.B M.Store in
    exec insns cond

  | McLDRBrs, [dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Offset M.Unsigned M.B M.Load in
    exec insns cond

  | McSTR_POST_IMM, [unknown; dest1; base; invalid; McImm offset; cond; _] ->
    let offset = mem_repair_imm offset 0x1000 0xfff NEG in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.W M.Store in
    exec insns cond

  | McLDR_POST_IMM, [dest1; unknown; base; invalid; McImm offset; cond; _] ->
    let offset = mem_repair_imm offset 0x1000 0xfff NEG in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.W M.Load in
    exec insns cond

  | McSTRB_POST_IMM, [unknown; dest1; base; invalid; McImm offset; cond; _]
  | McSTRBT_POST_IMM, [unknown; dest1; base; invalid; McImm offset; cond; _] ->
    let offset = mem_repair_imm offset 0x1000 0xfff NEG in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.B M.Store in
    exec insns cond

  | McLDRB_POST_IMM, [dest1; unknown; base; invalid; McImm offset; cond; _]
  | McLDRBT_POST_IMM, [dest1; unknown; base; invalid; McImm offset; cond; _] ->
    let offset = mem_repair_imm offset 0x1000 0xfff NEG in
    let insns = access_r_wrapper2 dest1 base offset M.Postindex M.Unsigned M.B M.Load in
    exec insns cond

  | McSTR_POST_REG, [unknown; dest1; base; offset; shift; cond; _]
  | McSTRT_POST_REG, [unknown; dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Postindex M.Unsigned M.W M.Store in
    exec insns cond

  | McLDR_POST_REG, [dest1; unknown; base; offset; shift; cond; _]
  | McLDRT_POST_REG, [dest1; unknown; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Postindex M.Unsigned M.W M.Load in
    exec insns cond

  | McSTRB_POST_REG, [unknown; dest1; base; offset; shift; cond; _]
  | McSTRBT_POST_REG, [unknown; dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Postindex M.Unsigned M.B M.Store in
    exec insns cond

  | McLDRB_POST_REG, [dest1; unknown; base; offset; shift; cond; _]
  | McLDRBT_POST_REG, [dest1; unknown; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Postindex M.Unsigned M.B M.Load in
    exec insns cond

  | McSTR_PRE_IMM, [unknown; dest1; base; McImm offset; cond; _] ->
    let insns = access_r_wrapper dest1 base (McImm offset) M.Preindex M.Unsigned M.W M.Store in
    exec insns cond

  | McLDR_PRE_IMM, [dest1; unknown; base; McImm offset; cond; _] ->
    let insns = access_r_wrapper dest1 base (McImm offset) M.Preindex M.Unsigned M.W M.Load in
    exec insns cond

  | McSTRB_PRE_IMM, [unknown; dest1; base; McImm offset; cond; _] ->
    let insns = access_r_wrapper dest1 base (McImm offset) M.Preindex M.Unsigned M.B M.Store in
    exec insns cond

  | McLDRB_PRE_IMM, [dest1; unknown; base; McImm offset; cond; _] ->
    let insns = access_r_wrapper dest1 base (McImm offset) M.Preindex M.Unsigned M.B M.Load in
    exec insns cond

  | McSTR_PRE_REG, [unknown; dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Preindex M.Unsigned M.W M.Store in
    exec insns cond

  | McLDR_PRE_REG, [dest1; unknown; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Preindex M.Unsigned M.W M.Load in
    exec insns cond

  | McSTRB_PRE_REG, [unknown; dest1; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Preindex M.Unsigned M.B M.Store in
    exec insns cond

  | McLDRB_PRE_REG, [dest1; unknown; base; offset; shift; cond; _] ->
    let insns = access_r_wrapper dest1 base offset ~shift M.Preindex M.Unsigned M.B M.Load in
    exec insns cond

  (* Exclusive access, we may later want to do something special to these *)

  | McLDREX, [dest1; base; cond; _] ->
    let insns = access_r_wrapper dest1 base (McImm 0L) M.Offset M.Unsigned M.W M.Load in
    exec insns cond

  | McLDREXB, [dest1; base; cond; _] ->
    let insns = access_r_wrapper dest1 base (McImm 0L) M.Offset M.Unsigned M.B M.Load in
    exec insns cond

  | McLDREXH, [dest1; base; cond; _] ->
    let insns = access_r_wrapper dest1 base (McImm 0L) M.Offset M.Unsigned M.H M.Load in
    exec insns cond

  (* multidest is one of the multireg combinations *)
  | McLDREXD, [multidest; base; cond; _] ->
    let insns = access_r_wrapper multidest base (McImm 0L) M.Offset M.Unsigned M.D M.Load in
    exec insns cond

  | McSTREX, [dest1; src1; base; cond; _] ->
    let insns = access_r_wrapper src1 base (McImm 0L) M.Offset M.Unsigned M.W M.Store in
    let result = [move (op2v dest1) (Int(bi0, reg_32))] in
    exec (insns@result) cond

  | McSTREXB, [dest1; src1; base; cond; _] ->
    let insns = access_r_wrapper src1 base (McImm 0L) M.Offset M.Unsigned M.B M.Store in
    let result = [move (op2v dest1) (Int(bi0, reg_32))] in
    exec (insns@result) cond

  | McSTREXH, [dest1; src1; base; cond; _] ->
    let insns = access_r_wrapper src1 base (McImm 0L) M.Offset M.Unsigned M.H M.Store in
    let result = [move (op2v dest1) (Int(bi0, reg_32))] in
    exec (insns@result) cond

  (* multisrc is one of the multireg combinations *)
  | McSTREXD, [dest1; multisrc; base; cond; _] ->
    let insns = access_r_wrapper multisrc base (McImm 0L) M.Offset M.Unsigned M.D M.Store in
    let result = [move (op2v dest1) (Int(bi0, reg_32))] in
    exec (insns@result) cond

  | McLDMIA, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IA M.Noupdate M.Load in
    exec insns cond

  | McLDMIA_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IA M.Update M.Load in
    exec insns cond

  | McSTMIA, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IA M.Noupdate M.Store in
    exec insns cond

  | McSTMIA_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IA M.Update M.Store in
    exec insns cond

  | McLDMDA, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DA M.Noupdate M.Load in
    exec insns cond

  | McLDMDA_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DA M.Update M.Load in
    exec insns cond

  | McSTMDA, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DA M.Noupdate M.Store in
    exec insns cond

  | McSTMDA_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DA M.Update M.Store in
    exec insns cond

  | McLDMIB, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IB M.Noupdate M.Load in
    exec insns cond

  | McLDMIB_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IB M.Update M.Load in
    exec insns cond

  | McSTMIB, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IB M.Noupdate M.Store in
    exec insns cond

  | McSTMIB_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.IB M.Update M.Store in
    exec insns cond

  | McLDMDB, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DB M.Noupdate M.Load in
    exec insns cond

  | McLDMDB_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DB M.Update M.Load in
    exec insns cond

  | McSTMDB, base :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DB M.Noupdate M.Store in
    exec insns cond

  | McSTMDB_UPD, base :: unknown :: cond :: wr_flag :: dest_list ->
    let insns = access_m_wrapper dest_list base M.DB M.Update M.Store in
    exec insns cond

  (* Branching instructions *)
  | McBcc, [offset; cond; _] ->
    B.branch offset ~cond:cond addr

  | McBL, [offset; cond; _]
  | McBL_pred, [offset; cond; _] ->
    B.branch offset ~cond:cond ~link:true addr

  | McBX_RET, [cond; _] ->
    B.branch (McReg McLR) ~cond:cond ~x:true addr

  | McBX, [target] ->
    B.branch target ~x:true addr

  | McBX_pred, [target; cond; _] ->
    B.branch target ~cond:cond ~x:true addr

  | McBLX, [target] ->
    B.branch target ~link:true ~x:true addr

  | McBLX_pred, [target; cond; _] ->
    B.branch target ~cond:cond ~link:true ~x:true addr

  | McBLXi, [offset] ->
    B.branch offset ~link:true ~x:true addr

  (* supervisor call *)
  | McSVC, [McImm i64; cond; _] ->
    exec [Special(Printf.sprintf "svc 0x%Lx" i64, None, [])] cond

  | McMRS, [dest; cond; _] ->
    let set_bits flag src lsb = src |* (binop LSHIFT (cast_unsigned reg_32 (Var flag)) (int2e lsb)) in
    let d = op2v dest in
    let vd = Var d in
    let insns = move d (int2e 0)
                :: move d (set_bits R.nf vd 31)
                :: move d (set_bits R.zf vd 30)
                :: move d (set_bits R.cf vd 29)
                :: move d (set_bits R.vf vd 28)
                :: move d (set_bits R.qf vd 27)
                :: [move d (set_bits R.ge vd 16)]
    in
    exec insns cond

  (* Move to special from register
   * For MSR an immediate with bit x set means:
   *  bit 0 is CPSR_c (is not valid in ARMv7)
   *  bit 1 is CPSR_x (is not valid in ARMv7)
   *  bit 2 is APSR_g
   *  bit 3 is APSR_nzcvq
  *)
  | McMSR, [McImm imm; src; cond; _] ->
    let insns = if logand imm 0x8L = 0x8L
      then move R.nf (extract 31 31 (op2e src))
           :: move R.zf (extract 30 30 (op2e src))
           :: move R.cf (extract 29 29 (op2e src))
           :: move R.vf (extract 28 28 (op2e src))
           :: [move R.qf (extract 27 27 (op2e src))]
      else []
    in
    let insns = if logand imm 0x4L = 0x4L
      then move R.ge (extract 19 16 (op2e src))
           :: insns
      else insns
    in
    exec insns cond

  (* All of these are nops in User mode *)
  | McCPS2p, _ | McDMB, _ | McDSB, _ | McHINT, _ | McPLDi12, _ ->
    [Special(McUtil.string_of_mcinst mcinst, None, [])]

  | _ -> [Special("Unimplemented instruction: "^(McUtil.string_of_mcinst mcinst), None, [])]

(* generate the bap for the instruction at address addr *)
let disasm_instr get_byte addr =
  let bytes_to_read = 4 in (* XXX: ARM - read as many bytes as necessary *)
  let str = String.create bytes_to_read in
  for i = 0 to bytes_to_read - 1 do
    str.[i] <- get_byte (addr +% (biconst i))
  done;

  let size, mcinst = McDisassembler.get_mcinst ~disassemble:true str in

  (* size 0 indicates an invalid mcinst *)
  let il = if size = 0 then []
    else
      let asm = match mcinst.assembly with
        | Some x -> x
        | _ -> failwith "Impossible: we requested disassemble above"
      in
      let bil = bil_of_mcinst mcinst addr in
      add_pc_and_labels ~asm addr bil
  in

  il, (addr +% bi4)
