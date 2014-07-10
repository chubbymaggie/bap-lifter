(** Native lifter of ARM instructions to the BAP IL *)

(** Global TODO:

    - Turn comments into ocamldoc and add doc gen to _oasis.

    - Improve naming convention. (Look at [offset] in [access_r_wrapper] for a
    scary example.)

    - Align conceptually parallel code to expose pattern for refactoring.

 **)

exception Disasm_ARM_exception of string

module B = Bitvector            (* more fundamental than Bil; named like Z *)
module McModules = Mc.Factory.Make (Mc.ARM.Enum) (* create target submodules *)

(* llvm-mc interop *)
open McModules              (* expose target-specific MC submodules *)
open Mc.ARM.Enum            (* open target-specific enum *)
open McOperand              (* lifter needs McOperand (McImm, ...) everywhere *)

(* BAP 1.x is based on Core; in the future, we could have a Bap.Std module that
   `include Core_kernel.Std` and have everyone `open Bap.Std` instead *)
open Core_kernel.Std

(* Pending: I am upstreaming this into Core. *)
(* also good showcase of why we want Bap.Std *)
module Int64 = struct
  include Int64
  let (land) = bit_and
  let (lor)  = bit_or
  let (lxor) = bit_xor
  let (lsl)  = shift_left
  let (lsr)  = shift_right
  let (asr)  = shift_right_logical
end

(* Bil interop *)
open Bil             (* Bil types; also allow using Bop.(expr) directly *)
open Type            (* TODO: Can the present type.ml can merge into bil.ml? *)

(** Things that should live in some other module *)
module SomeOtherModule = struct

  (** Return a Z.t from a string specifying the low-to-high bytes; should live
      in our new B module *)
  let bi_of_string str =
    (* TODO: should preallocate result, though str is never long in our code *)
    let build_bi (pos, accum) i =
      let bits = pos * 8 in
      pos + 1, Z.(accum + ~$i lsl bits)
    in
    let chars = String.to_list str in
    let ints = List.map ~f:Char.to_int chars in
    let _, instr = List.fold_left ~f:build_bi ~init:(0, Z.(~$0)) ints in
    instr

  (** A constant from the propose Mc.ARM.Target module; can also help writing
      64-bit lifter *)
  let pointer_size = 4

end
open SomeOtherModule

(** Things that I wish to make obsolete *)
module MakeMeObsolete = struct

  (** Return the width of a Type.typ *)
  let rbits = function
    | Reg n -> n
    | _ -> raise (Disasm_ARM_exception "Non-register type bits requested")

  (** Make a literal of value n in the same size as t *)
  let rlit n t = B.lit n (rbits t)

end
open MakeMeObsolete

(** ARM-specific constants *)
let pc_offset = Z.(~$8)         (* PC is ahead by some bytes in ARM *)
let int2e i = Int (B.lit i (8 * pointer_size))

(** ARM registers

    One design is for each ARM lifter to have its own register module. Then
    parameterize the other modules on this by functorization.

 **)
(* need to define this before helpers such as `assn` *)
module R = struct
  let new_var = Var.new_var

  (* LLVM's convention for invalid / unknown register *)
  let noregister = new_var "NoRegister" (Reg 32)

  (* Memory definition *)
  let mem = new_var "mem32" (TMem (Reg 32, Reg 8))

  (* Arithmetic flags, individually *)
  let nf = new_var "NF" (Reg 1)
  let zf = new_var "ZF" (Reg 1)
  let cf = new_var "CF" (Reg 1)
  let vf = new_var "VF" (Reg 1)
  let qf = new_var "QF" (Reg 1)
  let ge = new_var "GE" (Reg 4)

  (* Saved Program Status Register *)
  let spsr = new_var "SPSR" (Reg 32)

  (* Thumb if-then state register *)
  let itstate = new_var "ITSTATE" (Reg 8)

  (* Core registers: link register, program counter, stack pointer *)
  let lr = new_var "LR" (Reg 32)
  let pc = new_var "PC" (Reg 32)
  let sp = new_var "SP" (Reg 32)

  (* 32-bit general-purpose registers *)
  let r0  = new_var "R0"  (Reg 32)
  let r1  = new_var "R1"  (Reg 32)
  let r2  = new_var "R2"  (Reg 32)
  let r3  = new_var "R3"  (Reg 32)
  let r4  = new_var "R4"  (Reg 32)
  let r5  = new_var "R5"  (Reg 32)
  let r6  = new_var "R6"  (Reg 32)
  let r7  = new_var "R7"  (Reg 32)
  let r8  = new_var "R8"  (Reg 32)
  let r9  = new_var "R9"  (Reg 32)
  let r10 = new_var "R10" (Reg 32)
  let r11 = new_var "R11" (Reg 32)
  let r12 = new_var "R12" (Reg 32)

  (* Core registers are aliased to r13-15 in ARM *)
  let r13 = sp
  let r14 = lr
  let r15 = pc

  (* Floating point registers *)
  let fpexc      = new_var "FPEXC"      (Reg 32)
  let fpinst     = new_var "FPINST"     (Reg 32)
  let fpinst2    = new_var "FPINST2"    (Reg 32)
  let fpscr      = new_var "FPSCR"      (Reg 32)
  let fpscr_nzcv = new_var "FPSCR_NZCV" (Reg 32)
  let fpsid      = new_var "FPSID"      (Reg 32)

  (* 64-bit SIMD registers *)
  let d0  = new_var "D0"  (Reg 64)
  let d1  = new_var "D1"  (Reg 64)
  let d2  = new_var "D2"  (Reg 64)
  let d3  = new_var "D3"  (Reg 64)
  let d4  = new_var "D4"  (Reg 64)
  let d5  = new_var "D5"  (Reg 64)
  let d6  = new_var "D6"  (Reg 64)
  let d7  = new_var "D7"  (Reg 64)
  let d8  = new_var "D8"  (Reg 64)
  let d9  = new_var "D9"  (Reg 64)
  let d10 = new_var "D10" (Reg 64)
  let d11 = new_var "D11" (Reg 64)
  let d12 = new_var "D12" (Reg 64)
  let d13 = new_var "D13" (Reg 64)
  let d14 = new_var "D14" (Reg 64)
  let d15 = new_var "D15" (Reg 64)
  let d16 = new_var "D16" (Reg 64)
  let d17 = new_var "D17" (Reg 64)
  let d18 = new_var "D18" (Reg 64)
  let d19 = new_var "D19" (Reg 64)
  let d20 = new_var "D20" (Reg 64)
  let d21 = new_var "D21" (Reg 64)
  let d22 = new_var "D22" (Reg 64)
  let d23 = new_var "D23" (Reg 64)
  let d24 = new_var "D24" (Reg 64)
  let d25 = new_var "D25" (Reg 64)
  let d26 = new_var "D26" (Reg 64)
  let d27 = new_var "D27" (Reg 64)
  let d28 = new_var "D28" (Reg 64)
  let d29 = new_var "D29" (Reg 64)
  let d30 = new_var "D30" (Reg 64)
  let d31 = new_var "D31" (Reg 64)

  (* 128-bit SIMD registers *)
  let q0  = new_var "Q0"  (Reg 128)
  let q1  = new_var "Q1"  (Reg 128)
  let q2  = new_var "Q2"  (Reg 128)
  let q3  = new_var "Q3"  (Reg 128)
  let q4  = new_var "Q4"  (Reg 128)
  let q5  = new_var "Q5"  (Reg 128)
  let q6  = new_var "Q6"  (Reg 128)
  let q7  = new_var "Q7"  (Reg 128)
  let q8  = new_var "Q8"  (Reg 128)
  let q9  = new_var "Q9"  (Reg 128)
  let q10 = new_var "Q10" (Reg 128)
  let q11 = new_var "Q11" (Reg 128)
  let q12 = new_var "Q12" (Reg 128)
  let q13 = new_var "Q13" (Reg 128)
  let q14 = new_var "Q14" (Reg 128)
  let q15 = new_var "Q15" (Reg 128)

  (* 32-bit floating point registers *)
  (* Because these are floating point registers,
   * we cannot define them properly. *)
  (*
  let s0  = new_var "S0"  (Reg 32)
  let s1  = new_var "S1"  (Reg 32)
  let s2  = new_var "S2"  (Reg 32)
  let s3  = new_var "S3"  (Reg 32)
  let s4  = new_var "S4"  (Reg 32)
  let s5  = new_var "S5"  (Reg 32)
  let s6  = new_var "S6"  (Reg 32)
  let s7  = new_var "S7"  (Reg 32)
  let s8  = new_var "S8"  (Reg 32)
  let s9  = new_var "S9"  (Reg 32)
  let s10 = new_var "S10" (Reg 32)
  let s11 = new_var "S11" (Reg 32)
  let s12 = new_var "S12" (Reg 32)
  let s13 = new_var "S13" (Reg 32)
  let s14 = new_var "S14" (Reg 32)
  let s15 = new_var "S15" (Reg 32)
  let s16 = new_var "S16" (Reg 32)
  let s17 = new_var "S17" (Reg 32)
  let s18 = new_var "S18" (Reg 32)
  let s19 = new_var "S19" (Reg 32)
  let s20 = new_var "S20" (Reg 32)
  let s21 = new_var "S21" (Reg 32)
  let s22 = new_var "S22" (Reg 32)
  let s23 = new_var "S23" (Reg 32)
  let s24 = new_var "S24" (Reg 32)
  let s25 = new_var "S25" (Reg 32)
  let s26 = new_var "S26" (Reg 32)
  let s27 = new_var "S27" (Reg 32)
  let s28 = new_var "S28" (Reg 32)
  let s29 = new_var "S29" (Reg 32)
  let s30 = new_var "S30" (Reg 32)
  let s31 = new_var "S31" (Reg 32)
  *)

  (* Feature registers *)
  let mvfr0 = new_var "MVFR0" (Reg 32)
  let mvfr1 = new_var "MVFR1" (Reg 32)
  let mvfr2 = new_var "MVFR2" (Reg 32)

  let regs = [
    r0; r1; r2; r3; r4; r5; r6; r7;
    r8; r9; r10; r11; r12; sp; lr; pc;
    nf; zf; cf; vf; qf; ge;
    spsr; itstate; fpexc; fpinst; fpinst2; fpscr; fpscr_nzcv; fpsid;
    d0; d1; d2; d3; d4; d5; d6; d7;
    d8; d9; d10; d11; d12; d13; d14; d15;
    d16; d17; d18; d19; d20; d21; d22; d23;
    d24; d25; d26; d27; d28; d29; d30; d31;
    q0; q1; q2; q3; q4; q5; q6; q7;
    q8; q9; q10; q11; q12; q13; q14; q15;
    mvfr0; mvfr1; mvfr2;
  ]

  let of_mcreg = function
    | McR0  -> r0
    | McR1  -> r1
    | McR2  -> r2
    | McR3  -> r3
    | McR4  -> r4
    | McR5  -> r5
    | McR6  -> r6
    | McR7  -> r7
    | McR8  -> r8
    | McR9  -> r9
    | McR10 -> r10
    | McR11 -> r11
    | McR12 -> r12
    | McLR  -> lr
    | McPC  -> pc
    | McSP  -> sp
    | _ -> raise (Disasm_ARM_exception "unimplemented McReg")

  let of_mcmultireg = function
    | McR0_R1   -> r0,  r1
    | McR2_R3   -> r2,  r3
    | McR4_R5   -> r4,  r5
    | McR6_R7   -> r6,  r7
    | McR8_R9   -> r8,  r9
    | McR12_SP  -> r12, sp
    | McR10_R11 -> r10, r11
    | _         -> raise (Disasm_ARM_exception "unimplemented McReg")

end

(** AST building helpers; everyone should be carefully vetted *)
module Helper = struct

  (** ARM-specific semantics: write to PC is a jump *)
  let assn (d, s) =  (* turn PC writes into jumps; takes tuple to mirror Move *)
    if d = R.pc then Jmp s else Move (d, s)

  let op2e = function
    | McReg reg -> Var (R.of_mcreg reg)
    | McImm imm -> Int (B.lit64 imm 32)
    | _ -> raise (Disasm_ARM_exception "unimplemented McOperand (exp)")

  let op2v = function
    | McReg reg -> R.of_mcreg reg
    | _ -> raise (Disasm_ARM_exception "unimplemented McOperand (var)")

  (* executes the "stmts" if the condition holds.
   * executes the "flags" if wflag is CPSR and the condition holds
   **)
  let exec (stmts : stmt list) ?(flags : stmt list option)
      ?(wflag : t option) (cond : t) =
    (* write to the flags if wflag is CPSR *)
    let stmts =
      match flags, wflag with
      | Some f, Some (McReg McCPSR) -> stmts @ f
      | _ -> stmts
    in
    let c =
      match cond with
      | McImm c -> c
      | _ -> raise (Disasm_ARM_exception "Condition must be McImm")
    in
    (* generates an expression for the given McCond *)
    let set_cond mccond =
      let z = Var R.zf in
      let c = Var R.cf in
      let v = Var R.vf in
      let n = Var R.nf in
      let f = Int (B.lit 0 1) in
      let t = Int (B.lit 1 1) in
      match mccond_of_int64 mccond with
      | McCondEQ -> Bop.(z = t)
      | McCondNE -> Bop.(z = f)
      | McCondCS -> Bop.(c = t)
      | McCondCC -> Bop.(c = f)
      | McCondMI -> Bop.(n = t)
      | McCondPL -> Bop.(n = f)
      | McCondVS -> Bop.(v = t)
      | McCondVC -> Bop.(v = f)
      | McCondHI -> Bop.(c = t land z = f)
      | McCondLS -> Bop.(c = f lor  z = t)
      | McCondGE -> Bop.(n =  v)
      | McCondLT -> Bop.(n <> v)
      | McCondGT -> Bop.(z = f land n =  v)
      | McCondLE -> Bop.(z = t lor  n <> v)
      | McCondAL -> t
    in
    (* We shortcut if the condition = all *)
    match mccond_of_int64 c with
    | McCondAL -> stmts
    | _ -> [If (set_cond c, stmts, [])]

end
open Helper

(** Memory access operations *)
module M = struct

  exception Mem_ARM_exception of string

  (** Types for single-register memory access *)
  type mode_r = Offset | PreIndex | PostIndex
  type sign = Signed | Unsigned
  type size = B | H | W | D
  type operation = Ld | St

  (** Types for multiple-register memory access *)
  type mode_m = IA | IB | DA | DB
  type update_m = Update | NoUpdate

  (* Doug TODO check for misaligned access *)
  (* Single-register memory access *)
  let access_r ~(dst1 : Var.t) ?(dst2 : Var.t option) ~(base : Var.t)
      (offset : exp) mode sign size operation =
    let o_base   = Var.new_tmp "orig_base"   (Reg 32) in
    let o_offset = Var.new_tmp "orig_offset" (Reg 32) in
    (* If this load is a jump (only valid for 4-byte load)
     * We need to do the write_back before the load so we
     * Use the originals
     **)
    let address =
      match mode, operation, size, dst1 with
      | PreIndex,  Ld, W, d when d = R.pc -> Var o_base
      | PostIndex, Ld, W, d when d = R.pc -> Bop.(Var o_base + Var o_offset)
      | PreIndex,  _,  _, _               -> Var base
      | _                                 -> Bop.(Var base + offset)
    in
    (* Create temps for original if this is a jump *)
    let pre_write_back =
      match mode, operation, size, dst1 with
      | PreIndex,  Ld, W, d when d = R.pc ->
        Move (o_base, Var base) ::
        Move (base, Bop.(Var base + offset)) ::
        []
      | PostIndex, Ld, W, d when d = R.pc ->
        Move (o_base, Var base) ::
        Move (o_offset, offset) ::
        Move (base, Bop.(Var base + offset)) ::
        []
      | Offset, _, _, _ -> []
      | _ -> []
    in
    let write_back =
      match mode, operation, size, dst1 with
      | PreIndex,  Ld, W, d when d = R.pc -> []
      | PostIndex, Ld, W, d when d = R.pc -> []
      | Offset,    _,  _, _               -> []
      | _ -> [Move (base, Bop.(Var base + offset))]
    in
    let typ =
      match size with
      | B -> Reg 8
      | H -> Reg 16
      | W -> Reg 32
      | D -> Reg 32
    in
    (* Get the second destination of a Dword operation *)
    let check_dst2 = function
      | Some e -> e
      | _ -> raise (Mem_ARM_exception "incorrect destination for Dword access")
    in
    match operation with
    | Ld ->
      let temp =
        match size with
        | B | H -> Var.new_tmp "temp" typ
        | _ -> dst1
      in
      let cast l r =
        match sign with
        | Signed   -> Cast (CAST_SIGNED,   l, r)
        | Unsigned -> Cast (CAST_UNSIGNED, l, r)
      in
      let extend =
        match size with
        | B -> [Move (dst1, cast (Reg 32) (Var temp))]
        | H -> [Move (dst1, cast (Reg 32) (Var temp))]
        | W -> []
        | D -> []
      in
      let loads =
        if size = D then
          let dest2 = check_dst2 dst2 in
          [
            Move (dst1,  Load (Var R.mem, address,
                               LittleEndian, typ));
            Move (dest2, Load (Var R.mem, Bop.(address + Int (B.lit 4 32)),
                               LittleEndian, typ));
          ]
        else
          (* perform the load *)
          [
            assn (temp,  Load (Var R.mem, address,
                               LittleEndian, typ));
          ]
      in
      pre_write_back @
      loads @
      extend @                  (* sign/zero extend if necessary *)
      write_back
    | St ->
      let temp =
        match size with
        | B | H -> Var.new_tmp "store_temp" typ
        | _ -> dst1
      in

      (* truncate the value if necessary *)
      let trunc =
        match size with
        | B -> [Move (temp, Cast (CAST_LOW, Reg 8,  Var dst1))]
        | H -> [Move (temp, Cast (CAST_LOW, Reg 16, Var dst1))]
        | _ -> []
      in
      let stores =
        match size with
        | D ->
          let dest2 = check_dst2 dst2 in
          [
            Move (R.mem, Store (Var R.mem, address,
                                Var dst1, LittleEndian, typ));
            Move (R.mem, Store (Var R.mem, Bop.(address + Int (B.lit 4 32)),
                                Var dest2, LittleEndian, typ));
          ]
        | _ ->
          [
            Move (R.mem, Store (Var R.mem, address,
                                Var temp, LittleEndian, typ));
          ]
      in
      trunc @                   (* truncate the value if necessary *)
      stores @
      write_back

  let access_m dest_list base mode update operation =
    let o_base = Var.new_tmp "orig_base" (Reg 32) in
    let calc_offset ith =
      match mode with
      | IB ->  4 * (ith + 1)
      | DB -> -4 * (ith + 1)
      | IA ->  4 * ith
      | DA -> -4 * ith
    in
    let writeback =
      let dest_len = 4 * List.length dest_list in
      match update with
      | NoUpdate -> []
      | Update ->
        let f =
          match mode with
          | IB | IA -> Bop.(+)
          | DB | DA -> Bop.(-)
        in
        [Move (base, f (Var base) (int2e dest_len))]
    in
    let create_access i dest =
      let offset_e = int2e (calc_offset i) in
      match operation with
      | Ld -> assn (dest,
                    Load  (Var R.mem, Bop.(Var o_base + offset_e),
                           LittleEndian, Reg 32))
      | St -> Move (R.mem,
                    Store (Var R.mem, Bop.(Var o_base + offset_e), Var dest,
                           LittleEndian, Reg 32))
    in
    Move (o_base, Var base) ::
    writeback @
    List.mapi ~f:create_access dest_list

end

module Shift = struct

  (* shift types and conversion from integer to type *)
  type shift_type = ASR | LSL | LSR | ROR | RRX

  let shift_of_int64 i64 =
    match Int64.to_int_exn i64 with
    | 1 -> ASR
    | 2 -> LSL
    | 3 -> LSR
    | 4 -> ROR
    | 5 -> RRX
    | _ -> raise (Disasm_ARM_exception "Unimplemented shift type")

  (* Need the operand and the carry flag value *)
  (* src - the source, if you intend to use the carry bit, this
   *   must not be the destination of the shift expression.
   *   This means it must be a temp that contains the
   *   value of the source not the actual source itself
   * shift_type - the type of shift
   * shift - must be a exp that is the amount of the shift (ignored for rrx)
   **)
  let shift_c ~src shift_type ~shift t =
    let bits = rbits t in
    let bits_e = Int (B.lit bits bits) in
    let nth_bit n e = Cast (CAST_LOW, Reg 1, Bop.(e lsr n)) in
    let e1 = Int (B.lit 1 bits) in
    match shift_type with
    | ASR ->
      let shifted = Bop.(src asr shift) in
      let carry = nth_bit Bop.(shift - e1) src in
      shifted, carry
    | LSL ->
      let shifted = Bop.(src lsl shift) in
      let carry = nth_bit Bop.(bits_e - shift) src in
      shifted, carry
    | LSR ->
      let shifted = Bop.(src lsr shift) in
      let carry = nth_bit Bop.(shift - e1) src in
      shifted, carry
    | ROR ->
      let ret1 = Bop.(src lsr shift) in
      let ret2 = Bop.(src lsl (bits_e - shift)) in
      let shifted = Bop.(ret1 lor ret2) in
      let carry = nth_bit Bop.(shift - e1) src in
      shifted, carry
    | RRX ->
      let ret1 = Bop.(src lsr e1) in
      let carryin = Bop.(Cast (CAST_UNSIGNED, t, Var R.cf) lsl (bits_e - e1)) in
      let shifted = Bop.(ret1 lor carryin) in
      let carry = nth_bit (Int (B.lit 0 bits)) src in
      shifted, carry

  (* decodes a register shifted operand
   * src - the operand to be shifted, cannot be the destination
   *        in practice this means it must be a temp variable.
   * shift_type - an int64, bits 2 through 0 represent the shift type
   *              valid shift types are number 1 through 5
   * shift - the value to shift by
   * t - the type
   **)
  let r_shift ~src shift_type ~shift t =
    match shift_type with
    | McImm i64 -> shift_c ~src (shift_of_int64 i64) ~shift t
    | _ -> raise (Disasm_ARM_exception "Unimplemented shift_type")

  (* decodes an immediate shifted operand
   * src - the operand to be shifted, cannot be the destination
   *        in practice this means it must be a temp variable.
   * shift_type - an int64, bits 2 through 0 represent the shift type
   *              valid shift types are number 1 through 5
   *              bits 3 and higher represent the shift amount if this is an
   *              immediate shift. For register shifts these upper bits are 0.
   *              If the shift type is RRX, a shift amount of 1 is implied.
   * t - the type
   **)
  let i_shift ~src shift_type t =
    let bits = rbits t in
    match shift_type with
    | McImm i64 ->
      (* lower three bits are type*)
      let shift_t = shift_of_int64 Int64.(bit_and i64 0x7L) in
      (* other bits are immediate *)
      let shift_amt = Int64.(shift_right (bit_and i64 (bit_not 0x7L)) 3) in
      shift_c ~src shift_t ~shift:(Int (B.lit64 shift_amt bits)) t
    | _ -> raise (Disasm_ARM_exception "Unimplemented shift_type")

  (* decodes a shifted operand for a memory operation
   * src - the operand to be shifted
   * shift - an int64,
   *            bits 11 through 0 represent the shift amount
   *            bits 12 represents whether the expression is added or subtracted
   *            bits 15 through 13 represent the shift type, valid shift types
   *              are number 1 through 5
   * typ - the type
   **)
  let mem_shift ~src shift typ =
    (* Gets the shift type from the immediate *)
    let shift_typ i64 =
      shift_of_int64 Int64.(shift_right (bit_and i64 0xE000L) 13)
    in
    (* Gets the shift amount from the immediate *)
    let shift_amt i64 = Int (B.lit64 Int64.(bit_and i64 0xFFFL) (rbits typ)) in
    (* Converts the shift to a negative if the negative bit is set *)
    let to_neg i64 exp =
      if Int64.(bit_and 0x1000L i64) = 0x1000L then
        Bop.(Int (B.lit ~-1 (rbits typ)) * exp)
      else
        exp
    in
    match shift with
    | McImm i64 ->
      let exp, _ = shift_c ~src (shift_typ i64) ~shift:(shift_amt i64) typ in
      to_neg i64 exp
    | _ -> raise (Disasm_ARM_exception "Unimplemented mem_shift_type")

end

(** Combine M and Shift *)
module MS = struct

  (* FIXME: wrapper and wrapper2 need better names! *)

  (* FIXME: dest2 seems to be never used in any call; keeping for the moment *)
  (* wrapper for memory access *)
  let access_r_wrapper ~(dest1: t) ?(dest2 : t option) ?(shift : t option)
      ~(base: t) ~(offset : t) mode sign size operation =
    let base = op2v base in
    (* FIXME: HORROR: offset change type! Need to make this uniform. *)
    (* if offset is int32_min, it is actually "negative" zero *)
    (* can't use Int32.min_int, because it does a signed conversion *)
    let (offset : exp) =
      let min_32 = Int64.(shift_left 1L 31) in
      match (offset : t) with
      | McImm i64 when i64 = min_32 -> Int (B.lit 0 32)
      | _ -> op2e offset
    in
    let offset =
      match shift with
      | Some s -> Shift.mem_shift ~src:offset s (Reg 32)
      | None -> offset
    in
    (* If dest1 is a multireg, convert it into a dest1, dest2 pair *)
    match dest1, dest2, size with
    | McReg multireg, None, M.D ->
      let d1, d2 = R.of_mcmultireg multireg in
      M.access_r ~dst1:d1        ~dst2:d2
        ~base offset mode sign size operation
    | d1, Some d2, size ->
      M.access_r ~dst1:(op2v d1) ~dst2:(op2v d2)
        ~base offset mode sign size operation
    | d1, None, size  ->
      M.access_r ~dst1:(op2v d1)
        ~base offset mode sign size operation

  (* different wrapper for memory access *)
  let access_r_wrapper2 ~(dest1 : t) ?(dest2 : t option) ~(base : t)
      ~(offset : exp) mode sign size operation =
    let dest1 = op2v dest1 in   (* FIXME: this is extremely unclean *)
    let base = op2v base in
    match dest2 with
    | Some dest2 ->
      M.access_r ~dst1:dest1 ~dst2:(op2v dest2)
        ~base offset mode sign size operation
    | None ->
      M.access_r ~dst1:dest1
        ~base offset mode sign size operation

  (* takes an int64 src and converts it to an exp that is the offset
   * for some memory instructions
   * sign_mask - a bitmask that determines the bit in src that is
   *             the repair bit
   * imm_mask - a bitmask that determines which bits in src are the immediate
   * rtype - whether a set mask indicates a positive or negative immediate.
   *
   **)
  type repair_type = POS_repair | NEG_repair
  let mem_repair_imm src ~sign_mask ~imm_mask rtype =
    let bit_set = Int64.(bit_and (of_int sign_mask) src = of_int sign_mask) in
    let negate =
      (bit_set && rtype = NEG_repair) || (not bit_set && rtype = POS_repair)
    in
    let offset = Int64.(bit_and src (of_int imm_mask)) in
    Int (B.lit64 (if negate then Int64.neg offset else offset) 32)

  (* takes an int64 immediate and a register and converts it to an exp that
   * is the offset for some memory instructions
   * sign_mask - a bitmask that determines the bit in src that is the negative
                 bit
   * rtype - whether a set mask indicates a positive or negative operand.
   **)
  let mem_repair_reg reg imm ~sign_mask rtype =
    let bit_set = Int64.(bit_and (of_int sign_mask) imm = of_int sign_mask) in
    let negate =
      (bit_set && rtype = NEG_repair) || (not bit_set && rtype = POS_repair)
    in
    if negate then Bop.(int2e ~-1 * reg) else reg

  (* Decides whether to use the register or immediate as the offset value
   * Also performs conversion to remove the negative bit and the
   **)
  let mem_offset_reg_or_imm reg_off imm_off =
    match reg_off with
    | McReg McNoRegister ->
      mem_repair_imm imm_off            ~sign_mask:0x100 ~imm_mask:0xff
        NEG_repair
    | reg ->
      mem_repair_reg (op2e reg) imm_off ~sign_mask:0x100
        NEG_repair

  (* A different kind of mem_offset *)
  let mem_offset_reg_or_imm2 reg_off imm_off =
    match reg_off with
    | McReg McNoRegister ->
      mem_repair_imm imm_off            ~sign_mask:0x100 ~imm_mask:0xff
        POS_repair
    | reg ->
      mem_repair_reg (op2e reg) imm_off ~sign_mask:0x1
        POS_repair

  (* wrapper for memory access *)
  let access_m_wrapper dest_list base mode update operation =
    let dest_list = List.map ~f:op2v dest_list in
    let base = op2v base in
    M.access_m dest_list base mode update operation

end

module Branch = struct

  let branch operand ?link ?x ?cond addr =
    let target =
      match operand with
      | McReg r -> op2e (McReg r)
      | McImm offset ->
        (* FIXME: catching "negative zero" is the perfect job for Bitvector *)
        (* if offset is int32_min, it is actually "negative" zero *)
        (* can't use Int32.min, because it does a signed conversion *)
        let min_32 = Int64.(shift_left 1L 31) in
        let offset = if offset = min_32 then 0L else offset in
        Int (B.litz Z.(addr + pc_offset + Z.of_int64 offset) 32)
      | _ -> raise (Disasm_ARM_exception "unimplemented McOperand (exp)")
    in
    (* TODO detect change to thumb in `x` *)
    let _ = x in                (* side benefit: quench warning 27 *)
    let jump_instr = [Jmp target] in
    let link_instr =
      let next_addr = B.litz Z.(addr + pc_offset - ~$4) 32 in
      match link with
      | Some true -> [Move (R.lr, Int next_addr)]
      | _         -> []
    in
    let stmts = link_instr @ jump_instr in
    match cond with
    | Some c -> exec stmts c
    | None -> stmts

end

module Data = struct

  type data_instr =
    | D_AND | D_BIC | D_EOR | D_MOV | D_MVN | D_ORR
    | D_SUB | D_RSB | D_ADD | D_ADC | D_SBC | D_RSC

  exception Data_ARM_exception of string

  (** Functions handling the computation of flags *)
  let compute_nf result = Cast (CAST_HIGH, Reg 1, result)
  let compute_zf result t = Bop.(Int (B.lit 0 (rbits t)) = result)

  let set_nf r   = Move (R.nf, compute_nf r)
  let set_zf r t = Move (R.zf, compute_zf r t)

  let set_nzf r t = [
    set_nf r;
    set_zf r t;
  ]

  let set_vnzf_add s1 s2 r t =
    Move (R.vf, Cast (CAST_HIGH, Reg 1, Bop.((s1 = s2) land (s1 lxor r)))) ::
    set_nzf r t

  let set_flags_add s1 s2 r t =
    Move (R.cf, Bop.(r < s1)) ::
    set_vnzf_add s1 s2 r t

  let set_flags_sub s1 s2 r t =
    set_flags_add s1 Bop.(lnot (s2 + Int (rlit 1 t))) r t

  let set_flags_adc s1 s2 r t =
    (* Computes the carry flag by performing the addition *)
    let bits = rbits t in
    let t' = Reg (bits + 1) in
    let bige =
      let open Bop in
      Cast (CAST_UNSIGNED, t', s1      ) +
      Cast (CAST_UNSIGNED, t', s2      ) +
      Cast (CAST_UNSIGNED, t', Var R.cf)
    in
    Move (R.cf, Extract (bits, bits, bige)) ::
    set_vnzf_add s1 s2 r t

  let set_flags_sbc s1 s2 r t =
    set_flags_adc s1 Bop.(lnot s2) r t

  let set_cf_data_imm imm raw =
    if Z.(~$255 >= of_int64 imm && of_int64 imm >= ~$0) then
      if Z.(equal (bi_of_string raw land ~$ 0xf00) ~$0) then
        Var R.cf
      else
        Int (B.lit 0 1)
    else
      Cast (CAST_HIGH, Reg 1, Int (B.lit64 imm 32))

  let data_instr ?dest src1 ?src2 itype ?sreg ?simm raw ~wflag cond =
    let d =
      match dest with
      | Some dest -> op2v dest
      | None      -> Var.new_tmp "dest" (Reg 32)
    in
    let s1 = op2e src1 in
    let s2 =
      match src2 with
      | Some src -> op2e src
      | None     -> Int (B.lit 0 32)
    in
    (* Do the register shift *)
    let s1, s2, stmts, carry =
      match itype, sreg, simm with
      | D_MOV, Some sreg, Some simm
      | D_MVN, Some sreg, Some simm ->
        let unshifted = Var.new_tmp "unshifted" (Reg 32) in
        let shifted, carry =
          Shift.r_shift ~src:(Var unshifted) simm ~shift:(op2e sreg) (Reg 32)
        in
        let stmts = [Move (unshifted, s1)] in
        shifted, s2, stmts, carry
      | _, Some sreg, Some simm ->
        let unshifted = Var.new_tmp "unshifted" (Reg 32) in
        let shifted, carry =
          Shift.r_shift ~src:(Var unshifted) simm ~shift:(op2e sreg) (Reg 32)
        in
        let stmts = [Move (unshifted, s2)] in
        s1, shifted, stmts, carry
      | D_MOV, None, Some simm
      | D_MVN, None, Some simm ->
        let unshifted = Var.new_tmp "unshifted" (Reg 32) in
        let shifted, carry =
          Shift.i_shift ~src:(Var unshifted) simm (Reg 32)
        in
        let stmts = [Move (unshifted, s1)] in
        shifted, s2, stmts, carry
      | _, None, Some simm ->
        let unshifted = Var.new_tmp "unshifted" (Reg 32) in
        let shifted, carry =
          Shift.i_shift ~src:(Var unshifted) simm (Reg 32)
        in
        let stmts = [Move (unshifted, s2)] in
        s1, shifted, stmts, carry
      | _ ->
        s1, s2, [], Var R.cf
    in
    let stmts, flags =
      match itype, src1, src2 with
      | D_MOV, McImm i64, _
      | D_MVN, McImm i64, _
      | D_AND, _,         Some (McImm i64)
      | D_BIC, _,         Some (McImm i64)
      | D_EOR, _,         Some (McImm i64)
      | D_ORR, _,         Some (McImm i64) ->
        stmts, Move (R.cf, set_cf_data_imm i64 raw) :: set_nzf (Var d) (Reg 32)
      | D_MOV, _, _
      | D_MVN, _, _
      | D_AND, _, _
      | D_BIC, _, _
      | D_EOR, _, _
      | D_ORR, _, _ ->
        stmts, Move (R.cf, carry) :: set_nzf (Var d) (Reg 32)
      | itype1, _, _ ->
        let orig1 = Var.new_tmp "orig1" (Reg 32) in
        let orig2 = Var.new_tmp "orig2" (Reg 32) in
        let flags =
          match itype1 with
          | D_SUB -> set_flags_sub (Var orig1) (Var orig2) (Var d) (Reg 32)
          | D_RSB -> set_flags_sub (Var orig2) (Var orig1) (Var d) (Reg 32)
          | D_ADD -> set_flags_add (Var orig1) (Var orig2) (Var d) (Reg 32)
          | D_ADC -> set_flags_adc (Var orig1) (Var orig2) (Var d) (Reg 32)
          | D_SBC -> set_flags_sbc (Var orig1) (Var orig2) (Var d) (Reg 32)
          | D_RSC -> set_flags_sbc (Var orig2) (Var orig1) (Var d) (Reg 32)
          | _ -> raise (Data_ARM_exception "impossible")
        in
        stmts @ [Move (orig1, s1); Move (orig2, s2)], flags
    in
    let oper =
      let open Bop in
      match itype with
      | D_AND -> s1 land s2
      | D_BIC -> s1 land lnot s2
      | D_EOR -> s1 lxor s2
      | D_MOV -> s1
      | D_MVN -> lnot s1
      | D_ORR -> s1 lor s2
      | D_SUB -> s1 - s2
      | D_RSB -> s2 - s1
      | D_ADD -> s1 + s2
      | D_ADC -> s1 + s2 + Cast (CAST_UNSIGNED, Reg 32, Var R.cf)
      | D_SBC -> s1 + lnot s2 + Cast (CAST_UNSIGNED, Reg 32, Var R.cf)
      | D_RSC -> lnot s1 + s2 + Cast (CAST_UNSIGNED, Reg 32, Var R.cf)
    in
    exec (stmts @ [assn (d, oper)]) ~flags ~wflag cond

  (* Instructions that set the carry flag for immediates:
   * AND, BIC, EOR, MOV, MVN, ORR, TEQ, TST w/ imm
   * cf is the msb of the ror immediate
   * AND, BIC, EOR, MOV, MVN, ORR, TEQ, TST w/ reg
   * cf is unchanged
   * AND, BIC, EOR, MOV, MVN, ORR, TEQ, TST w/ rsr, rsi
   * cf is carry out

   * instructions that don't follow this are:
   * SUB, RSB, ADD, ADC, SBC, RSC, CMP, CMN
   * cf is always the cf of the addition/subtraction
   **)
end

module Bit = struct

  type sign = Signed | Unsigned
  type size = H | B

  let typ_of_size = function
    | H -> Reg 16
    | B -> Reg 8

  let extend ~dest ~src ?src2 sign size ~rot cond =
    let amount =
      match rot with
      | McImm i64 -> Int64.(to_int_exn (i64 * 8L))
      | _ -> raise (Disasm_ARM_exception "incorrect operand type")
    in
    let rotated, _ =
      if amount <> 0 then
        Shift.shift_c ~src:(op2e src) Shift.ROR ~shift:(int2e amount) (Reg 32)
      else
        op2e src, int2e 0
    in
    let extracted = Cast (CAST_LOW, typ_of_size size, rotated) in
    let extent =
      match sign with
      | Signed   -> Cast (CAST_SIGNED,   Reg 32, extracted)
      | Unsigned -> Cast (CAST_UNSIGNED, Reg 32, extracted)
    in
    let final =
      match src2 with
      | Some s2 -> Bop.(op2e s2 + extent)
      | None    -> extent
    in
    exec [assn (op2v dest, final)] cond

  let bit_extract ~dest ~src sign ~lsb ~widthminus1 cond =
    let op2i = function
      | McImm i64 -> Int64.to_int_exn i64
      | _ -> raise (Disasm_ARM_exception "incorrect operand type for\
                                          bit_extract")
    in
    let low = op2i lsb in
    let high = low + (op2i widthminus1) in
    let extracted = Extract (high, low, op2e src) in
    let final =
      match sign with
      | Signed   -> Cast (CAST_SIGNED,   Reg 32, extracted)
      | Unsigned -> Cast (CAST_UNSIGNED, Reg 32, extracted)
    in
    exec [assn (op2v dest, final)] cond

  (* population count: many other algorithms in
     http://www.hackersdelight.org/hdcodetxt/pop.c.txt *)
  let count_bits_set imm =
    let v =
      match imm with
      | McImm i64 -> i64
      | _ -> raise (Disasm_ARM_exception "bits_set takes and McImm as input")
    in
    let open Int64 in
    let magic1 = 0x1001001001001L in
    let magic2 = 0x84210842108421L in
    (v land 0xfffL   )        * magic1 land magic2 % 0x1fL +
    (v land 0xfff000L) lsr 12 * magic1 land magic2 % 0x1fL +
    (v               ) lsr 24 * magic1 land magic2 % 0x1fL |>
    to_int

  let get_lsb_width raw =
    let open Int64 in
    let instr = Z.to_int64 (bi_of_string raw) in
    let lsb = bit_and (shift_right instr 7) 0x1fL in
    let msb = bit_and (shift_right instr 16) 0x1fL in
    let width = abs (msb - lsb + 1L) in
    to_int_exn lsb, to_int_exn width

  let bit_field_insert ~dest ~src raw cond =
    (* get bits set *)
    let d   = op2v dest in
    let d_e = op2e dest in
    let lsb, width = get_lsb_width raw in
    let extracted = Extract (width - 1, 0, op2e src) in
    let ext_h b s = Extract (31, b, s) in
    let ext_l b s = Extract (b, 0, s) in
    let inst =
      match lsb + width - 1, lsb with
      | 31, 0 -> extracted
      | 31, l -> Concat (extracted, ext_l (l - 1) d_e)
      | m,  0 -> Concat (ext_h (m + 1) d_e, extracted)
      | m,  l -> Concat (Concat (ext_h (m + 1) d_e, extracted),
                         ext_l (l - 1) d_e)
    in
    exec [Move (d, inst)] cond
end

module Mul = struct

  type sign = Signed | Unsigned
  type smul_size = BB | BT | TB | TT | D | DX | WB | WT

  exception Mul_ARM_exception of string

  let mull ~lodest ~hidest ~src1 ~src2 sign ?addend ~wflag cond =
    let s1_64 =
      match sign with
      | Signed   -> Cast (CAST_SIGNED,   Reg 64, op2e src1)
      | Unsigned -> Cast (CAST_UNSIGNED, Reg 64, op2e src1)
    in
    let s2_64 =
      match sign with
      | Signed   -> Cast (CAST_SIGNED,   Reg 64, op2e src2)
      | Unsigned -> Cast (CAST_UNSIGNED, Reg 64, op2e src2)
    in
    let result = Var.new_tmp "result" (Reg 64) in
    let flags = Data.set_nzf (Var result) (Reg 64) in
    let opn =
      match addend with
      | Some _ -> Bop.(s1_64 * s2_64 + Concat (op2e hidest, op2e lodest))
      | None   -> Bop.(s1_64 * s2_64)
    in
    let insns =
      Move (result, opn) ::
      Move (op2v lodest, Extract (31, 0,  Var result)) ::
      Move (op2v hidest, Extract (63, 32, Var result)) ::
      []
    in
    exec insns ~flags ~wflag cond

  let smul ~dest ?hidest ~src1 ~src2 ?accum ?hiaccum ?q size cond =
    let src1 = op2e src1 in
    let src2 = op2e src2 in
    let top   s = Cast (CAST_SIGNED, Reg 64, Extract (31, 16, s)) in
    let bot   s = Cast (CAST_SIGNED, Reg 64, Extract (15, 0,  s)) in
    let top32 s = Cast (CAST_SIGNED, Reg 64, Extract (47, 16, s)) in
    let res = Var.new_tmp "result_64" (Reg 64) in
    let result =
      let open Bop in
      match size with
      | BB -> bot src1 * bot src2
      | BT -> bot src1 * top src2
      | TB -> top src1 * bot src2
      | TT -> top src1 * top src2
      | D  -> top src1 * top src2 + bot src1 * bot src2
      | DX -> top src1 * bot src2 + bot src1 * top src2
      | WB -> top32 (Cast (CAST_SIGNED, Reg 64, src1 * bot src2))
      | WT -> top32 (Cast (CAST_SIGNED, Reg 64, src1 * top src2))
    in
    let result =
      let open Bop in
      match accum, hiaccum with
      | None,   None     -> result
      | Some a, None     -> result + Cast (CAST_SIGNED, Reg 64, op2e a)
      | Some a, Some hia -> result + Concat (op2e hia, op2e a)
      | _ -> raise (Mul_ARM_exception "Cannot specify only a hi accumulator")
    in
    let qflag =
      match q with
      | Some true ->
        [Move (R.qf,
               Bop.(Cast (CAST_SIGNED, Reg 64, Extract (31, 0, Var res)) <>
                    Var res))]
      | _ -> []
    in
    let instr =
      match hidest with
      | Some hid ->
        Move (res,       result) ::
        Move (op2v hid,  Extract (63, 32, Var res)) ::
        Move (op2v dest, Extract (31, 0,  Var res)) ::
        []
      | None ->
        Move (res,       result) ::
        Move (op2v dest, Extract (31, 0, Var res)) ::
        []
    in
    exec (instr @ qflag) cond
end

(* TODO: expose organization through better commenting *)
(** Obtain BIL from an McInst *)
let bil_of_mcinst (mcinst : McInst.t) (addr : Z.t) : stmt list =
  let raw_bytes = McInst.(mcinst.raw_bytes) in
  match McInst.(mcinst.mcopcode, mcinst.mcoperands) with

  | McMOVi, [dest; src; cond; _; wflag]
  | McMOVr, [dest; src; cond; _; wflag] ->
    Data.data_instr ~dest src Data.D_MOV raw_bytes cond ~wflag

  | McMOVsr, [dest; src; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.D_MOV ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McMOVsi, [dest; src; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.D_MOV ~simm:shift_imm raw_bytes cond ~wflag

  | McMVNi, [dest; src; cond; _; wflag]
  | McMVNr, [dest; src; cond; _; wflag] ->
    Data.data_instr ~dest src Data.D_MVN raw_bytes cond ~wflag

  | McMVNsr, [dest; src; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.D_MVN ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McMVNsi, [dest; src; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src Data.D_MVN ~simm:shift_imm raw_bytes cond ~wflag

  | McANDri, [dest; src1; src2; cond; _; wflag]
  | McANDrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_AND raw_bytes cond ~wflag

  | McANDrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_AND ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McANDrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_AND ~simm:shift_imm
      raw_bytes cond ~wflag

  | McBICri, [dest; src1; src2; cond; _; wflag]
  | McBICrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_BIC raw_bytes cond ~wflag

  | McBICrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_BIC ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McBICrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_BIC ~simm:shift_imm
      raw_bytes cond ~wflag

  | McEORri, [dest; src1; src2; cond; _; wflag]
  | McEORrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_EOR raw_bytes cond ~wflag

  | McEORrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_EOR ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McEORrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_EOR ~simm:shift_imm
      raw_bytes cond ~wflag

  | McORRri, [dest; src1; src2; cond; _; wflag]
  | McORRrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ORR raw_bytes cond ~wflag

  | McORRrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ORR ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McORRrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ORR ~simm:shift_imm
      raw_bytes cond ~wflag

  | McTEQri, [src1; src2; cond; _]
  | McTEQrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_EOR raw_bytes cond ~wflag:(McReg McCPSR)

  | McTEQrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_EOR ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  | McTEQrsi, [_dest; src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_EOR ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  | McTSTri, [src1; src2; cond; _]
  | McTSTrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_AND raw_bytes cond ~wflag:(McReg McCPSR)

  | McTSTrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_AND ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  | McTSTrsi, [src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_AND ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  | McADDri, [dest; src1; src2; cond; _; wflag]
  | McADDrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ADD raw_bytes cond ~wflag

  | McADDrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ADD ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McADDrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ADD ~simm:shift_imm
      raw_bytes cond ~wflag

  | McSUBri, [dest; src1; src2; cond; _; wflag]
  | McSUBrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_SUB raw_bytes cond ~wflag

  | McSUBrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_SUB ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McSUBrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_SUB ~simm:shift_imm
      raw_bytes cond ~wflag

  | McADCri, [dest; src1; src2; cond; _; wflag]
  | McADCrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ADC raw_bytes cond ~wflag

  | McADCrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ADC ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McADCrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_ADC ~simm:shift_imm
      raw_bytes cond ~wflag

  | McSBCri, [dest; src1; src2; cond; _; wflag]
  | McSBCrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_SBC raw_bytes cond ~wflag

  | McSBCrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_SBC ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McSBCrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_SBC ~simm:shift_imm
      raw_bytes cond ~wflag

  | McRSBri, [dest; src1; src2; cond; _; wflag]
  | McRSBrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_RSB raw_bytes cond ~wflag

  | McRSBrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_RSB ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McRSBrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_RSB ~simm:shift_imm
      raw_bytes cond ~wflag

  | McRSCri, [dest; src1; src2; cond; _; wflag]
  | McRSCrr, [dest; src1; src2; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_RSC raw_bytes cond ~wflag

  | McRSCrsr, [dest; src1; src2; shift_reg; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_RSC ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag

  | McRSCrsi, [dest; src1; src2; shift_imm; cond; _; wflag] ->
    Data.data_instr ~dest src1 ~src2 Data.D_RSC ~simm:shift_imm
      raw_bytes cond ~wflag

  | McCMPri, [src1; src2; cond; _]
  | McCMPrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_SUB raw_bytes cond ~wflag:(McReg McCPSR)

  | McCMPrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_SUB ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  | McCMPrsi, [src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_SUB ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  | McCMNri, [src1; src2; cond; _]
  | McCMNzrr, [src1; src2; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_ADD raw_bytes cond ~wflag:(McReg McCPSR)

  | McCMNzrsr, [src1; src2; shift_reg; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_ADD ~sreg:shift_reg ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  | McCMNzrsi, [src1; src2; shift_imm; cond; _] ->
    Data.data_instr src1 ~src2 Data.D_ADD ~simm:shift_imm
      raw_bytes cond ~wflag:(McReg McCPSR)

  (** Special Data Instructions *)

  | McMOVi16, [dest; src; cond; _wflag] ->
    let insns = [Move (op2v dest, op2e src)] in
    exec insns cond

  | McMOVTi16, [dest; _; src; cond; _wflag] ->
    let insns = [Move (op2v dest, Bop.(op2e dest lor op2e src lsl int2e 16))] in
    exec insns cond

  (** Additional Data Instructions *)

  (* extends *)
  | McUXTB, [dest; src; rot; cond; _] ->
    Bit.extend ~dest ~src Bit.Unsigned Bit.B ~rot cond

  | McUXTH, [dest; src; rot; cond; _] ->
    Bit.extend ~dest ~src Bit.Unsigned Bit.H ~rot cond

  | McSXTB, [dest; src; rot; cond; _] ->
    Bit.extend ~dest ~src Bit.Signed Bit.B ~rot cond

  | McSXTH, [dest; src; rot; cond; _] ->
    Bit.extend ~dest ~src Bit.Signed Bit.H ~rot cond

  | McUXTAB, [dest; src; shift; rot; cond; _] ->
    Bit.extend ~dest ~src:shift ~src2:src Bit.Unsigned Bit.B ~rot cond

  | McUXTAH, [dest; src; shift; rot; cond; _] ->
    Bit.extend ~dest ~src:shift ~src2:src Bit.Unsigned Bit.H ~rot cond

  | McSXTAB, [dest; src; shift; rot; cond; _] ->
    Bit.extend ~dest ~src:shift ~src2:src Bit.Signed Bit.B ~rot cond

  | McSXTAH, [dest; src; shift; rot; cond; _] ->
    Bit.extend ~dest ~src:shift ~src2:src Bit.Signed Bit.H ~rot cond

  (* extracts *)
  | McUBFX, [dest; src; lsb; widthminus1; cond; _] ->
    Bit.bit_extract ~dest ~src Bit.Unsigned ~lsb ~widthminus1 cond

  | McSBFX, [dest; src; lsb; widthminus1; cond; _] ->
    Bit.bit_extract ~dest ~src Bit.Signed   ~lsb ~widthminus1 cond

  (* reverses *)
  | McREV, [dest; src; cond; _] ->
    let s = op2e src in
    let i24 = int2e 24 in
    let i8 = int2e 8 in
    let umask = int2e 0xff0000 in
    let lmask = int2e 0xff00 in
    let rev =
      let open Bop in
      s              lsl i24 lor
      s              lsr i24 lor
      (s land umask) lsr i8  lor
      (s land lmask) lsl i8
    in
    exec [assn (op2v dest, rev)] cond

  | McREV16, [dest; src; cond; _] ->
    let s = op2e src in
    let i16 = int2e 16 in
    let rev = Bop.(s lsl i16 lor s lsr i16) in
    exec [assn (op2v dest, rev)] cond

  (* count leading zeroes *)
  (* XXX There may be ways to optimize *)
  | McCLZ, [dest; src; cond; _] ->
    let shift = Var.new_tmp "shift" (Reg 32) in
    let accum = Var.new_tmp "accum" (Reg 32) in
    let insns =
      let open Bop in
      Move (shift, op2e src) ::
      Move (accum, int2e 32) ::
      While (Var shift <> int2e 0, begin
          Move (shift, Var shift lsr int2e 1) ::
          Move (accum, Var accum - int2e 1) ::
          []
        end) ::
      Move (op2v dest, Var accum) ::
      []
    in
    exec insns cond

  (* bit field *)
  | McBFI, [dest; _unknown; src; _bmask; cond; _] ->
    Bit.bit_field_insert ~dest ~src raw_bytes cond

  | McBFC, [dest; _unknown; _bmask; cond; _] ->
    Bit.bit_field_insert ~dest ~src:(McImm 0L) raw_bytes cond

  (* bit reverse *)
  (* XXX There may be ways to optimize *)
  | McRBIT, [dest; src; cond; _] ->
    let v = Var.new_tmp "v" (Reg 32) in
    let r = Var.new_tmp "r" (Reg 32) in
    let s = Var.new_tmp "s" (Reg 32) in
    let insns =
      let open Bop in
      Move (v, op2e src lsr int2e 1) ::
      Move (r, op2e src) ::
      Move (s, int2e 31) ::
      While (Var v <> int2e 0, begin
          Move (r, Var r lsl int2e 1) ::
          Move (r, Var r lor (Var v land int2e 1)) ::
          Move (s, Var s - int2e 1) ::
          []
        end) ::
      Move (op2v dest, Var r lsl Var s) ::
      []
    in
    exec insns cond

  (* Swap bytes *)
  | McSWPB, [dest; src1; src2; cond; _] ->
    let temp = Var.new_tmp "temp" (Reg 8) in
    let insns =
      assn (temp,  Load  (Var R.mem, op2e src2,
                          LittleEndian, Reg 8)) ::
      Move (R.mem, Store (Var R.mem, op2e src2, Extract (7, 0, op2e src1),
                          LittleEndian, Reg 8)) ::
      assn (op2v dest, Cast (CAST_UNSIGNED, Reg 32, Var temp)) ::
      []
    in
    exec insns cond

  (* Pack half *)
  | McPKHTB, [dest; src1; src2; shift; cond; _] ->
    (* shift is always asr *)
    let shifted, _ =
      Shift.shift_c ~src:(op2e src2) Shift.ASR ~shift:(op2e shift) (Reg 32)
    in
    let insns =
      [assn (op2v dest, Concat (Extract (31, 16, op2e src1),
                                Extract (15, 0,  shifted  )))]
    in
    exec insns cond

  (* Multiply *)
  | McMUL, [dest; src1; src2; cond; _rflag; wflag] ->
    let flags = Data.set_nzf (op2e dest) (Reg 32) in
    exec [assn (op2v dest, Bop.(op2e src1 * op2e src2))] ~flags ~wflag cond

  | McMLA, [dest; src1; src2; addend; cond; _rflag; wflag] ->
    let flags = Data.set_nzf (op2e dest) (Reg 32) in
    exec [assn (op2v dest, Bop.(op2e addend + op2e src1 * op2e src2))]
      ~flags ~wflag cond

  | McMLS, [dest; src1; src2; addend; cond; _] ->
    exec [Move (op2v dest, Bop.(op2e addend - op2e src1 * op2e src2))] cond

  | McUMULL, [lodest; hidest; src1; src2; cond; _rflag; wflag] ->
    Mul.mull ~lodest ~hidest ~src1 ~src2 Mul.Unsigned ~wflag cond

  | McSMULL, [lodest; hidest; src1; src2; cond; _rflag; wflag] ->
    Mul.mull ~lodest ~hidest ~src1 ~src2 Mul.Signed ~wflag cond

  | McUMLAL, [lodest; hidest; src1; src2;
              _loadd; _hiadd; cond; _rflag; wflag] ->
    Mul.mull ~lodest ~hidest ~src1 ~src2 Mul.Unsigned ~addend:true ~wflag cond

  | McSMLAL, [lodest; hidest; src1; src2;
              _loadd; _hiadd; cond; _rflag; wflag] ->
    Mul.mull ~lodest ~hidest ~src1 ~src2 Mul.Signed ~addend:true ~wflag cond

  (* signed 16bit mul plus a 32bit bit accum, Q *)
  | McSMLABB, [dest; src1; src2; accum; cond; _wflag] ->
    Mul.smul ~dest ~src1 ~src2 ~accum ~q:true Mul.BB cond

  (* signed 16bit mul *)
  | McSMULBB, [dest; src1; src2; cond; _wflag] ->
    Mul.smul ~dest ~src1 ~src2 Mul.BB cond

  (* two signed 16bit muls plus 32bit accum and optional xchg, Q*)
  | McSMLAD, [dest; src1; src2; accum; cond; _wflag] ->
    Mul.smul ~dest ~src1 ~src2 ~accum ~q:true Mul.D cond

  (* two signed 16bit muls and optional xchg, Q *)
  | McSMUAD, [dest; src1; src2; cond; _wflag] ->
    Mul.smul ~dest ~src1 ~src2 ~q:true Mul.D cond

  (* signed 16bit times signed 32bit added to 32bit accum, Q *)
  | McSMLAWB, [dest; src1; src2; accum; cond; _wflag] ->
    Mul.smul ~dest ~src1 ~src2 ~accum ~q:true Mul.WB cond

  (* signed 16bit mul *)
  | McSMULTB, [dest; src1; src2; cond; _wflag] ->
    Mul.smul ~dest ~src1 ~src2 Mul.TB cond

  (* signed 16bit mul plus 64bit accum *)
  | McSMLALBT, [dest; hidest; src1; src2; cond; _wflag] ->
    Mul.smul ~dest ~hidest ~src1 ~src2 ~accum:dest ~hiaccum:hidest Mul.BT cond

  (** Load/Store operations *)

  | McSTRD, [dest1; dest2; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~dest2 ~base ~offset
        M.Offset M.Unsigned M.D M.St
    in
    exec insns cond

  | McLDRD, [dest1; dest2; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~dest2 ~base ~offset
        M.Offset M.Unsigned M.D M.Ld
    in
    exec insns cond

  | McSTRD_POST, [dest1; dest2; base; _unknown; reg_off; McImm imm_off;
                  cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~dest2 ~base ~offset
        M.PostIndex M.Unsigned M.D M.St in
    exec insns cond

  | McLDRD_POST, [dest1; dest2; base; _unknown; reg_off; McImm imm_off;
                  cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~dest2 ~base ~offset
        M.PostIndex M.Unsigned M.D M.Ld
    in
    exec insns cond

  | McSTRD_PRE, [_unknown; dest1; dest2; base; reg_off; McImm imm_off;
                 cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~dest2 ~base ~offset
        M.PreIndex M.Unsigned M.D M.St
    in
    exec insns cond

  | McLDRD_PRE, [dest1; dest2; _unknown; base; reg_off; McImm imm_off;
                 cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~dest2 ~base ~offset
        M.PreIndex M.Unsigned M.D M.Ld
    in
    exec insns cond

  | McSTRH, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset
        M.Offset M.Unsigned M.H M.St
    in
    exec insns cond

  | McLDRH, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset
        M.Offset M.Unsigned M.H M.Ld
    in
    exec insns cond

  | McSTRH_PRE, [_unknown; dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset
        M.PreIndex M.Unsigned M.H M.St
    in
    exec insns cond

  | McLDRH_PRE, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset
        M.PreIndex M.Unsigned M.H M.Ld
    in
    exec insns cond

  | McSTRH_POST, [_unknown; dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset
        M.PostIndex M.Unsigned M.H M.St
    in
    exec insns cond

  (* Unlike the convention of all other load and store instructions, for some
   * instructions the sign bit is set in the immediate when the operand
   * is POSITIVE. Insructions that are affected by this are marked with
   * "POS_SIGN_BIT"
   **)
  (* POS_SIGN_BIT *)
  | McSTRHTr, [_unknown; dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm2 reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset
        M.PostIndex M.Unsigned M.H M.St
    in
    exec insns cond

  | McLDRH_POST, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset
        M.PostIndex M.Unsigned M.H M.Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRHTr, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm2 reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Unsigned M.H M.Ld
    in
    exec insns cond

  | McLDRSH, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.Offset M.Signed M.H M.Ld
    in
    exec insns cond

  | McLDRSH_PRE, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PreIndex M.Signed M.H M.Ld
    in
    exec insns cond

  | McLDRSH_POST, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Signed M.H M.Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRSHTr, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm2 reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Signed M.H M.Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRSHTi, [dest1; _unknown; base; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm2 (McReg McNoRegister) imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Signed M.H M.Ld
    in
    exec insns cond

  | McLDRSB, [dest1; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.Offset M.Signed M.B M.Ld
    in
    exec insns cond

  | McLDRSB_PRE, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PreIndex M.Signed M.B M.Ld
    in
    exec insns cond

  | McLDRSB_POST, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Signed M.B M.Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | McLDRSBTr, [dest1; _unknown; base; reg_off; McImm imm_off; cond; _] ->
    let offset = MS.mem_offset_reg_or_imm2 reg_off imm_off in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Signed M.B M.Ld
    in
    exec insns cond

  | McSTRi12, [dest1; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.Offset M.Unsigned M.W M.St
    in
    exec insns cond

  | McLDRi12, [dest1; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.Offset M.Unsigned M.W M.Ld
    in
    exec insns cond

  | McSTRBi12, [dest1; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.Offset M.Unsigned M.B M.St
    in
    exec insns cond

  | McLDRBi12, [dest1; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.Offset M.Unsigned M.B M.Ld
    in
    exec insns cond

  | McSTRrs, [dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.Offset M.Unsigned M.W M.St
    in
    exec insns cond

  | McLDRrs, [dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.Offset M.Unsigned M.W M.Ld
    in
    exec insns cond

  | McSTRBrs, [dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.Offset M.Unsigned M.B M.St
    in
    exec insns cond

  | McLDRBrs, [dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.Offset M.Unsigned M.B M.Ld
    in
    exec insns cond

  | McSTR_POST_IMM, [_unknown; dest1; base; _invalid; McImm offset; cond; _] ->
    let offset =
      MS.mem_repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff MS.NEG_repair
    in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Unsigned M.W M.St
    in
    exec insns cond

  | McLDR_POST_IMM, [dest1; _unknown; base; _invalid; McImm offset; cond; _] ->
    let offset =
      MS.mem_repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff MS.NEG_repair
    in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Unsigned M.W M.Ld
    in
    exec insns cond

  | McSTRB_POST_IMM,  [_unknown; dest1; base; _invalid; McImm offset; cond; _]
  | McSTRBT_POST_IMM, [_unknown; dest1; base; _invalid; McImm offset; cond; _]
    ->
    let offset =
      MS.mem_repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff MS.NEG_repair
    in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Unsigned M.B M.St
    in
    exec insns cond

  | McLDRB_POST_IMM,  [dest1; _unknown; base; _invalid; McImm offset; cond; _]
  | McLDRBT_POST_IMM, [dest1; _unknown; base; _invalid; McImm offset; cond; _]
    ->
    let offset =
      MS.mem_repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff MS.NEG_repair
    in
    let insns =
      MS.access_r_wrapper2 ~dest1 ~base ~offset M.PostIndex M.Unsigned M.B M.Ld
    in
    exec insns cond

  | McSTR_POST_REG,  [_unknown; dest1; base; offset; shift; cond; _]
  | McSTRT_POST_REG, [_unknown; dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PostIndex M.Unsigned M.W M.St
    in
    exec insns cond

  | McLDR_POST_REG,  [dest1; _unknown; base; offset; shift; cond; _]
  | McLDRT_POST_REG, [dest1; _unknown; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PostIndex M.Unsigned M.W M.Ld
    in
    exec insns cond

  | McSTRB_POST_REG,  [_unknown; dest1; base; offset; shift; cond; _]
  | McSTRBT_POST_REG, [_unknown; dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PostIndex M.Unsigned M.B M.St
    in
    exec insns cond

  | McLDRB_POST_REG,  [dest1; _unknown; base; offset; shift; cond; _]
  | McLDRBT_POST_REG, [dest1; _unknown; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PostIndex M.Unsigned M.B M.Ld
    in
    exec insns cond

  | McSTR_PRE_IMM, [_unknown; dest1; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.PreIndex M.Unsigned M.W M.St
    in
    exec insns cond

  | McLDR_PRE_IMM, [dest1; _unknown; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.PreIndex M.Unsigned M.W M.Ld
    in
    exec insns cond

  | McSTRB_PRE_IMM, [_unknown; dest1; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.PreIndex M.Unsigned M.B M.St
    in
    exec insns cond

  | McLDRB_PRE_IMM, [dest1; _unknown; base; offset; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset M.PreIndex M.Unsigned M.B M.Ld
    in
    exec insns cond

  | McSTR_PRE_REG, [_unknown; dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PreIndex M.Unsigned M.W M.St
    in
    exec insns cond

  | McLDR_PRE_REG, [dest1; _unknown; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PreIndex M.Unsigned M.W M.Ld
    in
    exec insns cond

  | McSTRB_PRE_REG, [_unknown; dest1; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PreIndex M.Unsigned M.B M.St
    in
    exec insns cond

  | McLDRB_PRE_REG, [dest1; _unknown; base; offset; shift; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset ~shift
        M.PreIndex M.Unsigned M.B M.Ld
    in
    exec insns cond

  (* Exclusive access, we may later want to do something special to these *)

  | McLDREX, [dest1; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.W M.Ld
    in
    exec insns cond

  | McLDREXB, [dest1; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.B M.Ld
    in
    exec insns cond

  | McLDREXH, [dest1; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1 ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.H M.Ld
    in
    exec insns cond

  (* multidest is one of the multireg combinations *)
  | McLDREXD, [multidest; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1:multidest ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.D M.Ld
    in
    exec insns cond

  | McSTREX, [dest1; src1; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1:src1 ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.W M.St
    in
    let result = [Move (op2v dest1, int2e 0)] in
    exec (insns @ result) cond

  | McSTREXB, [dest1; src1; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1:src1 ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.B M.St
    in
    let result = [Move (op2v dest1, int2e 0)] in
    exec (insns @ result) cond

  | McSTREXH, [dest1; src1; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1:src1 ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.H M.St
    in
    let result = [Move (op2v dest1, int2e 0)] in
    exec (insns @ result) cond

  (* multisrc is one of the multireg combinations *)
  | McSTREXD, [dest1; multisrc; base; cond; _] ->
    let insns =
      MS.access_r_wrapper ~dest1:multisrc ~base ~offset:(McImm 0L)
        M.Offset M.Unsigned M.D M.St
    in
    let result = [Move (op2v dest1, int2e 0)] in
    exec (insns @ result) cond

  | McLDMIA, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IA M.NoUpdate M.Ld in
    exec insns cond

  | McLDMIA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IA M.Update M.Ld in
    exec insns cond

  | McSTMIA, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IA M.NoUpdate M.St in
    exec insns cond

  | McSTMIA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IA M.Update M.St in
    exec insns cond

  | McLDMDA, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DA M.NoUpdate M.Ld in
    exec insns cond

  | McLDMDA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DA M.Update M.Ld in
    exec insns cond

  | McSTMDA, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DA M.NoUpdate M.St in
    exec insns cond

  | McSTMDA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DA M.Update M.St in
    exec insns cond

  | McLDMIB, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IB M.NoUpdate M.Ld in
    exec insns cond

  | McLDMIB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IB M.Update M.Ld in
    exec insns cond

  | McSTMIB, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IB M.NoUpdate M.St in
    exec insns cond

  | McSTMIB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.IB M.Update M.St in
    exec insns cond

  | McLDMDB, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DB M.NoUpdate M.Ld in
    exec insns cond

  | McLDMDB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DB M.Update M.Ld in
    exec insns cond

  | McSTMDB, base :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DB M.NoUpdate M.St in
    exec insns cond

  | McSTMDB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = MS.access_m_wrapper dest_list base M.DB M.Update M.St in
    exec insns cond

  (** Branching instructions *)

  | McBcc, [offset; cond; _] ->
    Branch.branch offset ~cond addr

  | McBL, [offset; cond; _]
  | McBL_pred, [offset; cond; _] ->
    Branch.branch offset ~cond ~link:true addr

  | McBX_RET, [cond; _] ->
    Branch.branch (McReg McLR) ~cond ~x:true addr

  | McBX, [target] ->
    Branch.branch target ~x:true addr

  | McBX_pred, [target; cond; _] ->
    Branch.branch target ~cond ~x:true addr

  | McBLX, [target] ->
    Branch.branch target ~link:true ~x:true addr

  | McBLX_pred, [target; cond; _] ->
    Branch.branch target ~cond ~link:true ~x:true addr

  | McBLXi, [offset] ->
    Branch.branch offset ~link:true ~x:true addr

  (* supervisor call *)
  | McSVC, [McImm i64; cond; _] ->
    exec [Special (Printf.sprintf "svc 0x%Lx" i64)] cond

  | McMRS, [dest; cond; _] ->
    let set_bits flag src lsb =
      Bop.(src lor (Cast (CAST_UNSIGNED, Reg 32, Var flag) lsl int2e lsb))
    in
    let d = op2v dest in
    let vd = Var d in
    let insns =
      Move (d, int2e 0) ::
      Move (d, set_bits R.nf vd 31) ::
      Move (d, set_bits R.zf vd 30) ::
      Move (d, set_bits R.cf vd 29) ::
      Move (d, set_bits R.vf vd 28) ::
      Move (d, set_bits R.qf vd 27) ::
      Move (d, set_bits R.ge vd 16) ::
      []
    in
    exec insns cond

  (* Move to special from register
   * For MSR an immediate with bit x set means:
   * bit 0 is CPSR_c (is not valid in ARMv7)
   * bit 1 is CPSR_x (is not valid in ARMv7)
   * bit 2 is APSR_g
   * bit 3 is APSR_nzcvq
   **)
  | McMSR, [McImm imm; src; cond; _] ->
    let insns =
      if Int64.(bit_and imm 0x8L) = 0x8L then
        Move (R.nf, Extract (31, 31, op2e src)) ::
        Move (R.zf, Extract (30, 30, op2e src)) ::
        Move (R.cf, Extract (29, 29, op2e src)) ::
        Move (R.vf, Extract (28, 28, op2e src)) ::
        Move (R.qf, Extract (27, 27, op2e src)) ::
        []
      else
        []
    in
    let insns =
      if Int64.(bit_and imm 0x4L) = 0x4L then
        Move (R.ge, Extract (19, 16, op2e src)) ::
        insns
      else
        insns
    in
    exec insns cond

  (* All of these are nops in User mode *)
  | McCPS2p, _ | McDMB, _ | McDSB, _ | McHINT, _ | McPLDi12, _ ->
    [Special (McUtil.string_of_mcinst mcinst)]

  | _ -> [Special ("Unimplemented instruction: " ^
                   McUtil.string_of_mcinst mcinst)]

(** API to obtain BIL of the first instruction from a byte provider *)
let disasm_instr (byte_at_offset : Z.t -> char) (addr : Z.t)
  : string * stmt list * Z.t =
  let bytes_to_read = 4 in      (* upperbound on #bytes in an ARM instruction *)
  let str = String.create bytes_to_read in
  for i = 0 to bytes_to_read - 1 do
    str.[i] <- byte_at_offset Z.(addr + ~$i)
  done;
  let size, mcinst = McDisassembler.get_mcinst str in
  let asm, bil =
    if size = 0 then            (* 0 indicates an invalid mcinst *)
      "", []
    else
      McInst.(mcinst.assembly), bil_of_mcinst mcinst addr
  in
  let inc = Z.of_int size in
  (* 4.01 complains about ~$size or even substitute rhs of inc into below *)
  asm, bil, Z.(addr + inc)
