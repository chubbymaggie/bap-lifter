open Core_kernel.Std
open Unix
let usage = "converts raw bytes to BIL."

let sock_name = ref "/tmp/bapd"

let arg_specs = [
  "--sock", Arg.String (function x -> sock_name := x), "UNIX socket path"
]

let rec serveConn conn =
  let len = (1 + 16 + 16) in (* 1 arch byte, 16 raw bytes, 16 address hex bytes *)
  let buf = String.create len in
  let recvlen = recv conn buf 0 len [] in
  let buf' = String.sub buf ~pos:0 ~len:recvlen in
  if (recvlen <> 0) then (
    let c = String.get buf' 0 in
    let module LocalArch = (val 
                             (match Core_kernel.Core_char.to_int c with
                              | 0 -> (module Arch_i386.X86_32 : Arch.ARCH)
                              | 1 -> (module Arch_i386.X86_64 : Arch.ARCH)
                              | 2 -> (module Arch_arm.ARM     : Arch.ARCH)
                              | _ -> raise (Arg.Bad "Invalid arch"))
                             : Arch.ARCH) in
    let raw = String.sub buf' ~pos:1 ~len:16 in
    let address = Int64.of_string ("0x" ^ String.sub buf' ~pos:17 ~len:16) in
    let _, bil, ft, asm =
      LocalArch.disasm LocalArch.init_state
        (fun i -> String.get raw
            Int64.(to_int_exn ((Z.to_int64 (Bitvector.to_zarith i)) -
                               address)))
        (Bitvector.lit64 address
           (Conceval.width_of LocalArch.mem_index_type)) in
    let sendC str =
      let str' = str ^ "\n" in
      let _ = send conn str' 0 (String.length str') [] in
      () in
    sendC ((Bil_piqi.json_of_stmts bil) ^ "\n" ^
           (match asm with
            | Some asm -> asm
            | None     -> "None") ^ "\n" ^
           (Bitvector.to_string ft));
    serveConn conn) else ()

let rec serve fd =
  let conn, _ = accept fd in
  serveConn conn;
  serve fd

let () = Arg.parse arg_specs (fun _ -> ()) usage;
  let fd = socket PF_UNIX SOCK_SEQPACKET 0 in
  bind fd (ADDR_UNIX !sock_name);
  print_endline !sock_name;
  listen fd 3;
  serve fd
