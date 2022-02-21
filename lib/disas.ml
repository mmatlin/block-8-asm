open Stdint
open B8_ins

let read_to_bytes (name : string) =
  let in_chan = open_in_bin name in
  let rec read_byte_and_continue chan =
    match input_byte chan with
    | new_byte -> new_byte::(read_byte_and_continue chan)
    | exception End_of_file -> [] in
  read_byte_and_continue in_chan

type disas_mode =
  | Instruction
  | Argument

type disas_state = {
  current_ins : b8_ins option;
  mode : disas_mode;
  byte_num : int;
  ins_byte_num : int;
  ins_acc : b8_ins list;
}

exception Internal_disassembler_error of string
exception Invalid_B8_code of string

let reducer (state : disas_state) (byte : int) : disas_state =
  match state.mode with
  | Instruction -> (
    let op_num = byte lsr 4 in
    let lower_half = byte land 15 in
    let lower_three = byte land 7 in
    let lower_quarter = byte land 3 in
    let bit_5 = lower_half lsr 3 in
    let bit_6 = (byte land 4) lsr 2 in
    let bit_8 = (byte land 1) in
    let empty_reg_option = B8Reg None in
    let empty_imm_option = Imm None in
    let empty_b8_val_of_r_i_indicator (r_i : int) : b8_val =
      if r_i = 0 then empty_reg_option else empty_imm_option in
    let empty_b8_val_of_bit_5 = empty_b8_val_of_r_i_indicator bit_5 in
    let dest_register = b8_reg_of_reg_num lower_quarter in
    let ins, next_mode = (
      match op_num with
      | 1 -> B8Move(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 4 -> B8Add(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 5 -> B8Sub(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 6 -> B8Mul(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 7 -> B8Div(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 8 -> B8And(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 9 -> B8Or(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 10 -> B8Xor(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 12 -> B8ShiftLeft(
        empty_b8_val_of_bit_5,
        dest_register
      ), Argument
      | 2 -> (
        if bit_6 = 0 then (
          let next_mode = if bit_5 = 0 then Instruction else Argument in
          B8Push(
            if next_mode = Instruction then (B8Reg dest_register) else empty_imm_option
          ), next_mode
        ) else (B8Pop dest_register), Instruction
      )
      | 3 -> B8Comp(
        empty_b8_val_of_r_i_indicator bit_5,
        empty_b8_val_of_r_i_indicator bit_6,
        if bit_8 = 0 then Diff else Bit
      ), Argument
      | 11 -> B8Not(dest_register), Instruction
      | 13 -> B8ShiftRight(
        empty_b8_val_of_bit_5,
        dest_register,
        if bit_6 = 0 then Log else Arith
      ), Argument
      | 0 -> (if bit_8 = 0 then B8Nop else B8Pause), Instruction
      | _ -> if (op_num = 14 || op_num = 15) then (
        let jump_condition = b8_jump_condition_of_condition_num lower_three in
        match jump_condition with
        | Some c ->
          B8Jump(
            empty_b8_val_of_bit_5,
            c,
            if op_num = 14 then Backward else Forward
          ), Argument
        | None -> raise (Internal_disassembler_error
            "Impossible jump condition number was checked")
      ) else raise (Internal_disassembler_error "Impossible operation number")
    )
    in {
      current_ins = if next_mode = Argument then Some ins else None;
      mode = next_mode;
      byte_num = succ state.byte_num;
      ins_byte_num = if next_mode = Argument then 1 else 0;
      ins_acc = if next_mode = Argument then state.ins_acc else ins::state.ins_acc
    }
  )
  | Argument -> (
    let lower_quarter = byte land 3 in
    let current_ins =
      match state.current_ins with
      | Some ins -> ins
      | None -> raise (Internal_disassembler_error
          "Current instruction not set before moving to argument mode")
    in
    let b8_reg_of_lower_quarter = B8Reg(b8_reg_of_reg_num lower_quarter) in
    let imm_of_byte = Imm(Some(Uint8.of_int byte)) in
    let val_of_placeholder_val (placeholder : b8_val) =
      match placeholder with
      | B8Reg(_) -> b8_reg_of_lower_quarter
      | Imm(_) -> imm_of_byte in
    let ins, next_mode =
      match current_ins with
      | B8Move(src, dest) -> B8Move(val_of_placeholder_val src, dest), Instruction
      | B8Add(src, dest) -> B8Add(val_of_placeholder_val src, dest), Instruction
      | B8Sub(src, dest) -> B8Sub(val_of_placeholder_val src, dest), Instruction
      | B8Mul(src, dest) -> B8Mul(val_of_placeholder_val src, dest), Instruction
      | B8Div(src, dest) -> B8Div(val_of_placeholder_val src, dest), Instruction
      | B8And(src, dest) -> B8And(val_of_placeholder_val src, dest), Instruction
      | B8Or(src, dest) -> B8Or(val_of_placeholder_val src, dest), Instruction
      | B8Xor(src, dest) -> B8Xor(val_of_placeholder_val src, dest), Instruction
      | B8ShiftLeft(src, dest) -> B8ShiftLeft(val_of_placeholder_val src, dest), Instruction
      | B8Push(src) -> (
        match src with
        | B8Reg(_) -> raise (Internal_disassembler_error
            "Argument mode reached for current_ins B8Push with source value of B8Reg")
        | Imm(_) -> B8Push(imm_of_byte), Instruction
      )
      | B8Comp(first, second, comparison_mode) -> (
        if state.ins_byte_num = 1 then (
          B8Comp(val_of_placeholder_val first, second, comparison_mode), Argument
        ) else if state.ins_byte_num = 2 then (
          B8Comp(first, val_of_placeholder_val second, comparison_mode), Instruction
        ) else raise (Internal_disassembler_error
            "Argument mode reached with invalid instruction byte number")
      )
      | B8ShiftRight(src, dest, shift_right_type) ->
        B8ShiftRight(val_of_placeholder_val src, dest, shift_right_type), Instruction
      | B8Jump(offset, jump_condition, jump_type) ->
        B8Jump(val_of_placeholder_val offset, jump_condition, jump_type), Instruction
      | _ -> raise (Internal_disassembler_error
        "Argument mode reached with invalid current_ins")
    in {
      current_ins = if next_mode = Argument then Some ins else None;
      mode = next_mode;
      byte_num = succ state.byte_num;
      ins_byte_num = if next_mode = Argument then succ state.ins_byte_num else 0;
      ins_acc = if next_mode = Argument then state.ins_acc else ins::state.ins_acc;
    }
  )

let disas_bytes (l : int list) : b8_ins list =
  let initial_state = {
    current_ins = None;
    mode = Instruction;
    byte_num = 0;
    ins_byte_num = 0;
    ins_acc = [];
  } in
  let disas = List.fold_left reducer initial_state l in
  match disas with
  | { mode = Argument; _ } -> raise (Invalid_B8_code
      "Not all arguments were provided to the final instruction given")
  | { mode = Instruction; _ } -> List.rev disas.ins_acc

let disas_file (path : string) : b8_ins list =
  disas_bytes (read_to_bytes path)
