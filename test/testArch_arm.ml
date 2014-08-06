open OUnit
open Test_common

let trace_dir = "traces/traces/arm/"

(* FIXME Start architecture specific code *)
module Test = TestArch(Arch_arm.ARM)
open Test
(* FIXME End architecture specific code *)

let tests =
  "Arch_arm" >:::
  [
    "Loop through all traces" >:: (fun () ->
        handle_traces trace_dir
      )
  ]
