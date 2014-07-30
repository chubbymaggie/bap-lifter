open OUnit
open Test_common

let trace_dir = "traces/x86_64/"

(* FIXME Start architecture specific code *)
module Test = TestArch(Arch_i386.X86_64)
open Test
(* FIXME End architecture specific code *)

let tests =
  "Arch_i386_64" >:::
  [
    "Loop through all traces" >:: (fun () ->
        handle_traces trace_dir
      )
  ]
