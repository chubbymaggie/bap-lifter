open Core_kernel.Std

module Config = struct
  let trace : string option ref = ref None
  let frame_start : int option ref = ref None
  let frame_end : int option ref = ref None
  let dump_hex = ref false
  let dump_stmts = ref false
  (* TODO: dump expressions twice each, on entry and exit. *)
  let dump_exps = ref false
  let show_warnings = ref false
  let summary : string option ref = ref None
  let arch = ref None (* e.g. run all *)
  let arg_specs = [
    "--trace", Arg.String (fun x -> trace := Some x),
    "file run a single trace (default is to run all)";
    "--arch", Arg.String (fun x -> arch := Some x),
    "arch run only given arch (default is to run all)";
    "--range", Arg.Tuple [Arg.Int (fun x -> frame_start := Some x);
                          Arg.Int (fun x -> frame_end := Some x)],
    (* Pardon my alignment. *)
    "a \b\b\b\b\b\bz     only run frames 'a' to 'z' inclusive";
    "--dump-hex", Arg.Set dump_hex, " dump instructions as hex";
    "--dump-stmts", Arg.Set dump_stmts, " dump BIL stmts";
    "--summary", Arg.String (fun x -> summary := Some x), "file write summary to file";
  ]
end

module TestArch(LocalArch: Arch.ARCH) = struct

  (* Configuration derived from LocalArch *)
  let mem_index_width = Conceval.width_of LocalArch.mem_index_type

  (* State derived from LocalArch, Conceval *)
  let global_state = ref Conceval.State.empty
  let cpu_state = ref LocalArch.init_state
  let initialize_state () =
    global_state := Conceval.State.move LocalArch.mem
        (Conceval.Mem Conceval.Memory.empty)
        Conceval.State.empty;
    List.iter LocalArch.regs ~f:(fun var ->
        global_state := Conceval.State.move var
            (Conceval.Un ("Uninitialized", Var.typ var))
            !global_state)

  (* Helper functions *)

  module Summary = struct
    let runtime = ref 0.0
    let n_traces = ref 0
    let n_instructions = ref 0
    let n_unknown_instructions = ref 0
    let n_wrong_instructions = ref 0
    let n_exceptions = ref 0
    let errors : string list ref = ref []
    let incorrect : string list ref = ref []
    let exceptions : string list ref = ref []
    let lift_error = ref false
    let discrepancy = ref false

    let new_instr () = lift_error := false; discrepancy := false

    let set x () =
      x := true

    let log f =
      match !Config.summary with
      | Some _ -> f ()
      | None -> ()

    let incr x () =
      x := !x + 1

    let prepend x elem () =
      x := elem :: !x

    let write_summary () =
      Printf.fprintf (Out_channel.create "foo.out")
        "General Run Stats:\n\
         Total run time : %.2f seconds\n\
         Number of traces : %d\n\
         Total number of instructions: %d\n\
         Total number of BAP unknown instructions : %d\n\
         Percentage of BAP unknown instructions : %.2f%%\n\
         Total number of wrong instructions : %d\n\
         Percentage of wrong instructions : %.2f%%\n\
         Exceptions raised : %d\n\
         Errors and Unknowns:\n%s\n\
         Instructions BAP Evaluation got Incorrect:\n%s\n\
         Exceptions raised:\n%s\n"
        !runtime
        !n_traces
        !n_instructions
        !n_unknown_instructions
        (100.0 *. (float !n_unknown_instructions) /. (float !n_instructions))
        !n_wrong_instructions
        (100.0 *. (float !n_wrong_instructions) /. (float !n_instructions))
        !n_exceptions
        (String.concat ~sep:"\n" !errors)
        (* FIXME *)
        (String.concat ~sep:"\n"
           (List.map ~f:(fun (a, b) -> a ^ " : " ^ Int.to_string b)
              (List.sort ~cmp:(fun (_, x) (_, y) -> Int.compare y x)
                 (List.map ~f:(function (x::xs) -> x, (1 + List.length xs)
                                      | _ -> "", 0)
                     (List.group ~break:(<>)
                        (List.sort ~cmp:String.compare !incorrect))))))
        (String.concat ~sep:"\n" !exceptions)

  end

  module Log = struct
    let frame_num = ref 1
    let reset_frame_num () =
      frame_num := 1
    let incr_frame_num () =
      frame_num := !frame_num + 1

    let last_addr = ref (Int64.of_int (-1))
    let reset_last_addr () =
      last_addr := Int64.of_int (-1)
    let set_last_addr n =
      last_addr := n

    let good_data = ref 0
    let all_data = ref 0
    let incr_good () =
      good_data := !good_data + 1;
      all_data := !all_data + 1
    let incr_bad () =
      all_data := !all_data + 1
    let pp_results () =
      if !all_data < 0 then ()
      else Printf.printf "%.2f%% memory bytes and registers correct.\n"
          (100.0 *. (Int.to_float !good_data) /. (Int.to_float !all_data))

    let warning s =
      if !Config.show_warnings
      then Printf.printf "Warning (#%d:%s): %s\n" !frame_num
          (Int64.to_string !last_addr) s
      else ()

    let log s =
      Printf.printf "Log (#%d:%s): %s\n" !frame_num (Int64.to_string !last_addr) s
  end

  (** Given a bitvector, set a register in the global state. *)
  let set_register_bv name bv_value =
    match List.find LocalArch.regs ~f:(fun reg -> Var.name reg = name) with
    | Some var ->
      let bv_value = Bitvector.cast_low bv_value
          (Conceval.width_of (Var.typ var)) in
      global_state :=
        Conceval.State.move var (Conceval.BV bv_value) !global_state;
    | None -> Log.warning ("Unknown register " ^ name)

  (** Given a register name and a string (e.g. rawbytes),
    * set a register in the global state. *)
  let set_register name value =
    set_register_bv name (Bitvector.of_bytes (String.to_list value))

  (** Given a string (e.g. rawbytes) and Int64 address,
    * set that byte of memory in the global state. *)
  let set_mem addr value =
    let addr = Conceval.BV (Bitvector.lit64 addr mem_index_width) in
    let value = Bitvector.of_bytes (String.to_list value) in
    let prev_mem = Conceval.State.peek_exn LocalArch.mem !global_state in
    let mem = Conceval.Memory.store
        prev_mem addr (Conceval.BV value) Bil.LittleEndian
        (Type.Reg (Bitvector.width_of value)) in
    global_state := Conceval.State.move
        LocalArch.mem mem !global_state

  (** Check that a given register in global state has a given value. *)
  let check_register name trace_value =
    let trace_value = Bitvector.of_bytes (String.to_list trace_value) in
    match List.find LocalArch.regs ~f:(fun reg -> Var.name reg = name) with
    | Some var ->
      let trace_value = Bitvector.cast_low trace_value
          (Conceval.width_of (Var.typ var)) in
      (match Conceval.State.peek var !global_state with
       | Some (Conceval.BV value) ->
         if (Bitvector.bool_of (Bitvector.eq value trace_value))
         then Log.incr_good ()
         else (Log.log (Printf.sprintf "discrepancy: %s expected %s not %s" name
                          (Bitvector.to_hex trace_value) (Bitvector.to_hex value));
               Log.incr_bad ();
               Summary.log (Summary.set Summary.discrepancy))
       | Some (Conceval.Un (s, _)) ->
         Log.warning (Printf.sprintf "Value of %s was unknown: %s" name s);
         (* Log.incr_bad () *)
       | Some (Conceval.Mem _) ->
         raise (Conceval.Abort (name ^ " should be a register, not memory"))
       | None -> Log.warning ("Unknown register " ^ name)
      )
    | None -> Log.warning ("Unknown register " ^ name)

  (** Check that a given byte in global state memory has a given value. *)
  let check_mem addr trace_value =
    let bare_addr = (Bitvector.lit64 addr mem_index_width) in
    let addr = Conceval.BV bare_addr in
    let trace_value = Bitvector.of_bytes (String.to_list trace_value) in
    match Conceval.State.peek LocalArch.mem !global_state with
    | Some (Conceval.Mem mem) ->
      (match Conceval.Memory.load (Conceval.Mem mem) addr
               Bil.LittleEndian (Type.Reg (Bitvector.width_of trace_value)) with
      | Some (Conceval.BV value) ->
        if (Bitvector.bool_of (Bitvector.eq value trace_value))
        then Log.incr_good ()
        else (Log.log (Printf.sprintf "discrepancy: %s expected %s not %s"
                         (Bitvector.to_hex bare_addr)
                         (Bitvector.to_hex trace_value) (Bitvector.to_hex value));
              Log.incr_bad ();
              Summary.log (Summary.set Summary.discrepancy))
      | Some (Conceval.Un (s, _)) ->
        Log.warning (Printf.sprintf "Value of %s was unknown: %s"
                       (Bitvector.to_hex bare_addr) s);
        (* Log.incr_bad () *)
      | Some (Conceval.Mem _) ->
        raise (Conceval.Abort
                 (Printf.sprintf "%s should be a byte, not memory"
                    (Bitvector.to_hex bare_addr)))
      | None -> Log.warning (Printf.sprintf "Value of %s was uninitialized."
                               (Bitvector.to_hex bare_addr))
      )
    | Some _ -> raise (Conceval.Abort "Memory has wrong type.")
    | None -> raise (Conceval.Abort "Could not find memory.")

  (** Given two consecutive frames,
    * compare the operand_pre_list of the second
    * to the result of evaluating the first.
    * Ignore if one frame is not a `std_frame. *)
  let handle_frame_pair a b =
    match a, b with
    | `std_frame from_frame, `std_frame to_frame ->
      Summary.new_instr ();
      Summary.log (Summary.incr Summary.n_instructions);
      Log.set_last_addr from_frame.Frame_piqi.Std_frame.address;
      Log.warning ("addr: " ^
                   (Int64.to_string from_frame.Frame_piqi.Std_frame.address));
      (* First, set the state. *)
      List.iter from_frame.Frame_piqi.Std_frame.operand_pre_list
        ~f:(fun operand ->
            match operand.Frame_piqi.Operand_info.operand_info_specific with
            | `mem_operand mem_info ->
              set_mem mem_info.Frame_piqi.Mem_operand.address
                operand.Frame_piqi.Operand_info.value
            | `reg_operand reg_info ->
              set_register reg_info.Frame_piqi.Reg_operand.name
                operand.Frame_piqi.Operand_info.value
          );
      (* Next, lift from_frame to BIL. *)
      let bytes = from_frame.Frame_piqi.Std_frame.rawbytes in
      (* TODO: Which parts of this are lifter-dependent?
       * make sure this part is happy with ARM and x86(-64). *)
      let hex_of s = String.concat
          (List.map ~f:(fun c -> Printf.sprintf "%02x" (Char.to_int c))
             (String.to_list s)) in
      if !Config.dump_hex
      then Log.log (Printf.sprintf "0x%s" (hex_of bytes))
      else ();
      let next_cpu_state, bil, fallthrough_addr, _ =
        (try
           LocalArch.disasm !cpu_state
             (fun i -> String.get bytes
                 Int64.(to_int_exn ((Z.to_int64 (Bitvector.to_zarith i)) -
                                    from_frame.Frame_piqi.Std_frame.address)))
             (Bitvector.lit64 from_frame.Frame_piqi.Std_frame.address
                mem_index_width)
         with exn ->
           Summary.log (Summary.incr Summary.n_unknown_instructions);
           Summary.log (Summary.prepend Summary.errors (Exn.to_string exn));
           Summary.log (Summary.set Summary.lift_error);
           raise exn) in
      (* Step through the BIL. *)
      if !Config.dump_stmts
      then List.iter bil ~f:(fun instr -> Log.log ("\t" ^ (Pp.string_of_bil instr)))
      else ();
      let state, next = Conceval.eval_asm !global_state bil in
      cpu_state := next_cpu_state;
      global_state := Conceval.State.move LocalArch.ip
          (match next with
           | Some ip -> ip
           | _ -> Conceval.BV fallthrough_addr)
          state;
      (* Use operand_post_list, or to_frame operand_pre_list if unavailable. *)
      let post_list =
        (match from_frame.Frame_piqi.Std_frame.operand_post_list with
         | Some operand_post_list -> operand_post_list
         | None -> to_frame.Frame_piqi.Std_frame.operand_pre_list
        ) in
      (* Check state against post_list, then update it. *)
      List.iter post_list
        ~f:(fun operand ->
            match operand.Frame_piqi.Operand_info.operand_info_specific with
            | `mem_operand mem_info ->
              check_mem mem_info.Frame_piqi.Mem_operand.address
                operand.Frame_piqi.Operand_info.value;
              set_mem mem_info.Frame_piqi.Mem_operand.address
                operand.Frame_piqi.Operand_info.value
            | `reg_operand reg_info ->
              check_register reg_info.Frame_piqi.Reg_operand.name
                operand.Frame_piqi.Operand_info.value;
              set_register reg_info.Frame_piqi.Reg_operand.name
                operand.Frame_piqi.Operand_info.value
          );
      Summary.log (fun () -> if !Summary.discrepancy
                    then
                      (Summary.incr Summary.n_wrong_instructions ();
                       Summary.prepend Summary.incorrect ("0x" ^ (hex_of bytes)) ())
                    else ())
    | _, _ -> initialize_state ();
      Log.warning "Frames are not both `std_frame."

  (** Step through a trace and evaluate each frame,
      checking for discrepancies. *)
  (* TODO: Right now we only handle old-style (no operand_post_list)
   * traces; they are forward-compatible, but inferior.
   * Detect which one we're dealing with, or better yet
   * get rid of the old style. *)
  let handle_trace path =
    Log.reset_frame_num ();
    cpu_state := LocalArch.init_state;
    initialize_state ();

    let reader = new Trace_container.reader path in
    (* If given a frame range (inclusive), only test those frames. *)
    let condition = (
      match !Config.frame_start, !Config.frame_end with
      | Some a, Some z ->
        (* reader#seek seems to treat seek to 0 and 1 identically,
           so to avoid confusion we must use 1-indexing. *)
        let a = if a < 1 then 1 else a in
        Log.frame_num := a;
        reader#seek (Int64.of_int_exn a);
        (fun () -> not reader#end_of_trace && !Log.frame_num <= z)
      | (_, _) -> (fun () -> not reader#end_of_trace)) in
    let a = ref reader#get_frame in
    let b = ref reader#get_frame in
    while condition () do
      (* Any exception while stepping should be logged;
       * nothing that comes up at this time should halt testing. *)
      (try
         handle_frame_pair !a !b
       with
       | Conceval.Abort "Aborting with Special 'int 0x80'"
       | Conceval.Abort "Aborting with Special 'syscall'"
       | Conceval.Abort "Aborting with Special 'svc 0x0'" ->
         (initialize_state ();
          Log.warning "System call; dropping state.")
       | Conceval.Abort x when
           String.is_prefix x ~prefix:"Aborting with Special 'Unimplemented" ->
         Summary.log (Summary.incr Summary.n_unknown_instructions);
         Summary.log (Summary.prepend Summary.errors x)
       | exn ->
         (let trace = Exn.backtrace () in
          Log.log ("Error: " ^ (Exn.to_string exn) ^ "\n" ^ trace);
          if !Summary.lift_error then () else
            (Summary.log (Summary.incr Summary.n_exceptions);
             Summary.log (Summary.prepend Summary.exceptions (Exn.to_string exn)))
         )
      );
      a := !b;
      b := reader#get_frame;
      Log.incr_frame_num ();
    done

  let handle_traces trace_dir =
    let start_time = Sys.time() in
    Array.iter (Sys.readdir trace_dir) ~f:(fun filename ->
        if Some filename = !Config.trace ||
           (!Config.trace = None && String.is_suffix filename ~suffix:".trace")
        then (Log.log (Printf.sprintf "Checking file %s" filename);
              Summary.log (Summary.incr Summary.n_traces);
              (try handle_trace (trace_dir ^ filename)
               with exn ->
                 let trace = Exn.backtrace () in
                 (Log.log ("Error: While reading trace: " ^
                           (Exn.to_string exn)); print_endline trace)
              ))
        else Log.warning (Printf.sprintf "Ignoring file %s" filename)
      );
    Log.pp_results ();
    Summary.log (fun () ->
        Summary.runtime := Sys.time() -. start_time;
        Summary.write_summary ())

end
