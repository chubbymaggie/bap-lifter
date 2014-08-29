open Core_kernel.Std

let usage = "converts ASM (from stdin) to BIL."

let address = ref (Int64.of_int 0)
let arch = ref None
let dump_asm = ref false

let text_format stmts =
  String.concat ~sep:"\n" (List.map stmts ~f:Pp.string_of_bil)

let formatter = ref text_format

let arg_specs = [
  "--addr", Arg.String (fun x -> address := Int64.of_string x),
  "memory address of first instruction";
  "--arch", Arg.String (function
      | "x86" -> arch := Some (module Arch_i386.X86_32 : Arch.ARCH)
      | "x86_64" -> arch := Some (module Arch_i386.X86_64 : Arch.ARCH)
      | "arm" -> arch := Some (module Arch_arm.ARM : Arch.ARCH)
      | _ -> raise (Arg.Bad "Invalid architecture: try x86, x86_64, arm")
    ), "one of x86, x86_64, arm";
  "--dump-asm", Arg.Set dump_asm, "show assembly (if available)";
  "--format", Arg.String (function
      | "json"  -> formatter := Bil_piqi.json_of_stmts
      | "text"  -> formatter := text_format
      | "proto" -> formatter := Bil_piqi.pb_of_stmts
      | "xml"   -> formatter := Bil_piqi.xml_of_stmts
      | _ -> raise (Arg.Bad "Invalid format. Available: json text proto xml")
    ), "Data format. Available: json text proto xml";
]

let () = Arg.parse arg_specs (fun _ -> ()) usage;
  match !arch with
  | None -> Arg.usage arg_specs usage;
  | Some arch -> (
      let module LocalArch = (val arch : Arch.ARCH) in
      let bytes = In_channel.(input_all stdin) in
      let _, bil, _, asm =
        LocalArch.disasm LocalArch.init_state
          (fun i -> String.get bytes
              Int64.(to_int_exn ((Z.to_int64 (Bitvector.to_zarith i)) -
                                 !address)))
          (Bitvector.lit64 !address
             (Conceval.width_of LocalArch.mem_index_type)) in
      print_string (!formatter bil);
      if !dump_asm then match asm with
        | Some asm -> print_endline asm
        | None -> print_endline "Assembly not available for this architecture."
      else ()
    )
