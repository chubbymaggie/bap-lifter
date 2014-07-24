open OUnit
open Core_kernel.Std
open Test_common

let trace_dir = "traces/x86/"

(* FIXME Start architecture specific code *)
module Test = TestArch(Arch_i386.X86_32)
open Test
(* FIXME End architecture specific code *)

let tests =
  "Arch_i386" >:::
  [
    "Loop through all traces" >:: (fun () ->
        Array.iter (Sys.readdir trace_dir) ~f:(fun filename ->
            Log.log (Printf.sprintf "Checking file %s" filename);
            if String.is_suffix filename ~suffix:".trace"
            then handle_trace (trace_dir ^ filename)
            else Log.warning "File is not a trace."
          );
        Log.pp_results ()
      )
  ]
