open OUnit
open Test_common

let trace_dir = "traces/traces/x86/"

(* FIXME Start architecture specific code *)
module Test = TestArch(Arch_i386.X86_32)
open Test
(* FIXME End architecture specific code *)

let tests =
  "Arch_i386" >:::
  [
    "Loop through all traces" >:: (fun () ->
        match !Config.arch with
        | None | Some "i386" | Some "x86" | Some "x86_32" ->
          handle_traces trace_dir
        | _ -> ()
      )
  ]
