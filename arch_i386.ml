open Arch
open Disasm_i386
open Type
let subregs =
  [
    (* x86 subregisters *)
    ("R_AL_32", ("R_EAX_32", 0, Reg 32));
    ("R_BL_32", ("R_EBX_32", 0, Reg 32));
    ("R_CL_32", ("R_ECX_32", 0, Reg 32));
    ("R_DL_32", ("R_EDX_32", 0, Reg 32));

    ("R_AH_32", ("R_EAX_32", 8, Reg 32));
    ("R_BH_32", ("R_EBX_32", 8, Reg 32));
    ("R_CH_32", ("R_ECX_32", 8, Reg 32));
    ("R_DH_32", ("R_EDX_32", 8, Reg 32));

    ("R_DI_32", ("R_EDI_32", 0, Reg 32));
    ("R_SI_32", ("R_ESI_32", 0, Reg 32));
    ("R_BP_32", ("R_EBP_32", 0, Reg 32));
    ("R_SP_32", ("R_ESP_32", 0, Reg 32));
    ("R_AX_32", ("R_EAX_32", 0, Reg 32));

    ("R_BX_32", ("R_EBX_32", 0, Reg 32));
    ("R_CX_32", ("R_ECX_32", 0, Reg 32));
    ("R_DX_32", ("R_EDX_32", 0, Reg 32));
    ("R_BP_32", ("R_EBP_32", 0, Reg 32));
    ("R_SI_32", ("R_ESI_32", 0, Reg 32));
    ("R_DI_32", ("R_EDI_32", 0, Reg 32));
    ("R_SP_32", ("R_ESP_32", 0, Reg 32));

    (* x64 subregisters *)
    ("R_AL_64", ("R_RAX", 0, Reg 64));
    ("R_BL_64", ("R_RBX", 0, Reg 64));
    ("R_CL_64", ("R_RCX", 0, Reg 64));
    ("R_DL_64", ("R_RDX", 0, Reg 64));

    ("R_AH_64", ("R_RAX", 8, Reg 64));
    ("R_BH_64", ("R_RBX", 8, Reg 64));
    ("R_CH_64", ("R_RCX", 8, Reg 64));
    ("R_DH_64", ("R_RDX", 8, Reg 64));

    ("R_DIL", ("R_RDI", 0, Reg 64));
    ("R_SIL", ("R_RSI", 0, Reg 64));
    ("R_BPL", ("R_RBP", 0, Reg 64));
    ("R_SPL", ("R_RSP", 0, Reg 64));

    ("R_DI_64", ("R_RDI", 0, Reg 64));
    ("R_SI_64", ("R_RSI", 0, Reg 64));
    ("R_BP_64", ("R_RBP", 0, Reg 64));
    ("R_SP_64", ("R_RSP", 0, Reg 64));

    ("R_AX_64", ("R_RAX", 0, Reg 64));
    ("R_BX_64", ("R_RBX", 0, Reg 64));
    ("R_CX_64", ("R_RCX", 0, Reg 64));
    ("R_DX_64", ("R_RDX", 0, Reg 64));
    ("R_BP_64", ("R_RBP", 0, Reg 64));
    ("R_SI_64", ("R_RSI", 0, Reg 64));
    ("R_DI_64", ("R_RDI", 0, Reg 64));
    ("R_SP_64", ("R_RSP", 0, Reg 64));

    ("R_EAX_64", ("R_RAX", 0, Reg 64));
    ("R_EBX_64", ("R_RBX", 0, Reg 64));
    ("R_ECX_64", ("R_RCX", 0, Reg 64));
    ("R_EDX_64", ("R_RDX", 0, Reg 64));
    ("R_EBP_64", ("R_RBP", 0, Reg 64));
    ("R_ESI_64", ("R_RSI", 0, Reg 64));
    ("R_EDI_64", ("R_RDI", 0, Reg 64));
    ("R_ESP_64", ("R_RSP", 0, Reg 64));

  ] @ Array.to_list (Array.init 16 (fun i -> (Printf.sprintf "R_XMM%d" i, (Printf.sprintf "R_YMM%d" i, 0, Reg 256))))
  @ Array.to_list (Array.init 16 (fun i -> (Printf.sprintf "R_MM%d" i, (Printf.sprintf "R_YMM%d" i, 0, Reg 256))))
  @ Array.to_list (Array.init 8 (fun i -> (Printf.sprintf "R_R%dL" (i+8), (Printf.sprintf "R_R%d" (i+8), 0, Reg 64))))
  @ Array.to_list (Array.init 8 (fun i -> (Printf.sprintf "R_R%dW" (i+8), (Printf.sprintf "R_R%d" (i+8), 0, Reg 64))))
  @ Array.to_list (Array.init 8 (fun i -> (Printf.sprintf "R_R%dD" (i+8), (Printf.sprintf "R_R%d" (i+8), 0, Reg 64))))

module X86_32 : ARCH =
struct
  type cpustate = mode
  let arch = X86_32
  let mem_index_type = Type.Reg 32
  let mem = R32.mem
  let sp  = R32.esp
  let ip  = R32.eip
  let init_state = X86
  let state_with_addr m _ = m
  let regs = regs_x86
  let disasm m g a =
    let stmts, addr = Disasm_i386.disasm_instr m (fun x -> g (Bitvector.litz x 32)) (Bitvector.to_zarith a) in
    (m, stmts, (Bitvector.litz addr 32))
end

module X86_64 : ARCH =
struct
  type cpustate = mode
  let arch = X86_64
  let mem_index_type = Type.Reg 64
  let mem = R64.mem
  let sp = R64.rsp
  let ip = R64.rip
  let init_state = X8664
  let state_with_addr m _ = m
  let regs = regs_x86_64
  let disasm m g a =
    let stmts, addr = Disasm_i386.disasm_instr m (fun x -> g (Bitvector.litz x 64)) (Bitvector.to_zarith a) in
    (m, stmts, (Bitvector.litz addr 64))
end
