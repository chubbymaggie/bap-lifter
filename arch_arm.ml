open Arch
open Disasm_ARM

module ARM : ARCH =
struct
  type cpustate = ()
  let arch = ARM
  let mem_index_type = Type.Reg 32
  let mem = R.mem
  let sp  = R.sp
  let ip  = R.pc
  let init_state : cpustate = ()
  let state_with_addr m _ = m
  let regs = R.regs
  let disasm m g a =
    let asm, stmts, addr = Disasm_ARM.disasm_instr (fun x -> g (Bitvector.litz x 32)) (Bitvector.to_zarith a) in
    (m, stmts, (Bitvector.litz addr 32), Some asm)
end
