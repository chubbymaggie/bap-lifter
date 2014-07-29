open OUnit
open Core_kernel.Std
open Test_common

let trace_dir = "traces/arm/"

(* FIXME Start architecture specific code *)
module Test = TestArch(Arch_arm.ARM)
open Test
(* FIXME End architecture specific code *)

let tests =
  "Arch_arm" >:::
  [
    "Loop through all traces" >:: (fun () ->
        Array.iter (Sys.readdir trace_dir) ~f:(fun filename ->
            Log.log (Printf.sprintf "Checking file %s" filename);
            if String.is_suffix filename ~suffix:"trace"
            then handle_trace (trace_dir ^ filename)
            else Log.warning "File is not a trace."
          );
        Log.pp_results ()
      )
  ]
