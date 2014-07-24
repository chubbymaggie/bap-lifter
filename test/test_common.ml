open Core_kernel.Std

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

  module Log = struct
    let show_warnings = ref false

    let frame_num = ref 0
    let reset_frame_num () =
      frame_num := 0
    let incr_frame_num () =
      frame_num := !frame_num + 1

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
      if !show_warnings
      then Printf.printf "Warning (#%d): %s\n" !frame_num s
      else ()

    let log s =
      Printf.printf "Log (#%d): %s\n" !frame_num s
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
    let value = Conceval.BV (Bitvector.of_bytes (String.to_list value)) in
    let prev_mem = Conceval.State.peek_exn LocalArch.mem !global_state in
    let mem = Conceval.Memory.store
        prev_mem addr value Bil.LittleEndian (Type.Reg 8) in
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
               Log.incr_bad ())
       | Some (Conceval.Un (s, _)) ->
         Log.log (Printf.sprintf "Value of %s was unknown: %s" name s);
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
               Bil.LittleEndian (Type.Reg 8) with
      | Some (Conceval.BV value) ->
        if (Bitvector.bool_of (Bitvector.eq value trace_value))
        then Log.incr_good ()
        else (Log.log (Printf.sprintf "discrepancy: %s expected %s not %s"
                         (Bitvector.to_hex bare_addr)
                         (Bitvector.to_hex trace_value) (Bitvector.to_hex value));
              Log.incr_bad ())
      | Some (Conceval.Un (s, _)) ->
        Log.log (Printf.sprintf "Value of %s was unknown: %s"
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
      Log.log ("addr: " ^
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
      let next_cpu_state, bil, fallthrough_addr =
        LocalArch.disasm !cpu_state
          (fun i -> String.get bytes
              Int64.(to_int_exn ((Z.to_int64 (Bitvector.to_zarith i)) -
                                 from_frame.Frame_piqi.Std_frame.address)))
          (Bitvector.lit64 from_frame.Frame_piqi.Std_frame.address
             mem_index_width) in
      (* List.iter bil ~f:(fun instr -> Log.log ("\t" ^ (Pp.string_of_bil instr))); *)
      (* Step through the BIL. *)
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
          )

    | _, _ -> Log.warning "Frames are not both `std_frame."

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
    (* Log.frame_num := 104080;
       reader#seek 104080L; *)
    let a = ref reader#get_frame in
    let b = ref reader#get_frame in
    while (* !Log.frame_num < 104100 *) not reader#end_of_trace do
      (* Any exception while stepping should be logged;
       * nothing that comes up at this time should halt testing. *)
      (try
         (* if !Log.frame_num > 104080 && !Log.frame_num < 104100 *)
         (* then *) handle_frame_pair !a !b
       (* else () *)
       with 
       | Conceval.Abort "Aborting with Special 'int 0x80'" 
       | Conceval.Abort "Aborting with Special 'syscall'" ->
         initialize_state ();
         Log.warning "System call; dropping state."
       | exn ->
         (let trace = Exn.backtrace () in
          Log.log ("Error: " ^ (Exn.to_string exn)); print_endline trace)
      );
      a := !b;
      b := reader#get_frame;
      Log.incr_frame_num ();
      (* Log.pp_results (); *)
    done

end
