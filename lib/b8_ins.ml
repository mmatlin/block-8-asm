open Printf
open Stdint

type b8_reg =
  | RR
  | R1
  | R2
  | R3

type b8_imm = uint8

type b8_val =
  | B8Reg of b8_reg option
  | Imm of b8_imm option

type b8_jump_condition =
  | Direct
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq

type b8_comparison_mode =
  | Diff
  | Bit

type b8_shift_right_type =
  | Log
  | Arith

type b8_jump_type =
  | Backward
  | Forward

type b8_ins =
  | B8Move of b8_val * b8_reg option
  | B8Push of b8_val
  | B8Pop of b8_reg option
  | B8Comp of b8_val * b8_val * b8_comparison_mode
  | B8Add of b8_val * b8_reg option
  | B8Sub of b8_val * b8_reg option
  | B8Mul of b8_val * b8_reg option
  | B8Div of b8_val * b8_reg option
  | B8And of b8_val * b8_reg option
  | B8Or of b8_val * b8_reg option
  | B8Xor of b8_val * b8_reg option
  | B8Not of b8_reg option
  | B8ShiftLeft of b8_val * b8_reg option
  | B8ShiftRight of b8_val * b8_reg option * b8_shift_right_type
  | B8Jump of b8_val * b8_jump_condition * b8_jump_type
  | B8Pause
  | B8Nop

let b8_reg_of_reg_num = function
  | 0 -> Some RR
  | 1 -> Some R1
  | 2 -> Some R2
  | 3 -> Some R3
  | _ -> None

let b8_jump_condition_of_condition_num = function
  | 0 -> Some Direct
  | 1 -> Some Eq
  | 2 -> Some Less
  | 3 -> Some LessEq
  | 5 -> Some NotEq
  | 6 -> Some GreaterEq
  | 7 -> Some Greater
  | _ -> None

let sprint_reg (r : b8_reg) =
  match r with
  | RR -> "rr"
  | R1 -> "r1"
  | R2 -> "r2"
  | R3 -> "r3"

let sprint_reg_option (o : b8_reg option) =
  match o with
  | Some(r) -> sprint_reg r
  | None -> "None"

let sprint_imm_option (o : b8_imm option) =
  (* "0x%02x" is used instead of "%#04x". This is to avoid the
     `sprintf "%#04x" 0` case, which returns "0000" and not "0x00" *)
  match o with
  | Some(i) -> sprintf "0x%02x (%d)" (Uint8.to_int i) (Uint8.to_int i)
  | None -> "None"

let sprint_val (v : b8_val) =
  match v with
  | B8Reg(o) -> sprint_reg_option o
  | Imm(o) -> sprint_imm_option o

let sprint_comparison_mode (c : b8_comparison_mode) =
  match c with
  | Diff -> "difference"
  | Bit -> "bitwise"

let sprint_shift_right_type (t : b8_shift_right_type) =
  match t with
  | Log -> "logical"
  | Arith -> "arithmetic"

let sprint_jump_condition (c : b8_jump_condition) =
  match c with
  | Direct -> "direct"
  | Eq -> "equal"
  | NotEq -> "not equal"
  | Less -> "less"
  | LessEq -> "less or equal"
  | Greater -> "greater"
  | GreaterEq -> "greater or equal"

let sprint_jump_type (t : b8_jump_type) =
  match t with
  | Backward -> "backward"
  | Forward -> "forward"

let sprint_ins (i : b8_ins) =
  let generate_typical_str = sprintf "%s (src: %s, dest: %s)" in
  match i with
  | B8Move(src, dest) -> generate_typical_str "move" (sprint_val src) (sprint_reg_option dest)
  | B8Add(src, dest) -> generate_typical_str "add" (sprint_val src) (sprint_reg_option dest)
  | B8Sub(src, dest) -> generate_typical_str "sub" (sprint_val src) (sprint_reg_option dest)
  | B8Mul(src, dest) -> generate_typical_str "mul" (sprint_val src) (sprint_reg_option dest)
  | B8Div(src, dest) -> generate_typical_str "div" (sprint_val src) (sprint_reg_option dest)
  | B8And(src, dest) -> generate_typical_str "and" (sprint_val src) (sprint_reg_option dest)
  | B8Or(src, dest) -> generate_typical_str "or" (sprint_val src) (sprint_reg_option dest)
  | B8Xor(src, dest) -> generate_typical_str "xor" (sprint_val src) (sprint_reg_option dest)
  | B8Not(dest) -> sprintf "not (dest: %s)" (sprint_reg_option dest)
  | B8ShiftLeft(src, dest) -> generate_typical_str "shl" (sprint_val src) (sprint_reg_option dest)
  | B8Push(src) -> sprintf "push (src: %s)" (sprint_val src)
  | B8Pop(dest) -> sprintf "pop (dest: %s)" (sprint_reg_option dest)
  | B8Comp(first, second, comparison_mode) ->
    sprintf "comp (src: %s, dest: %s, mode: %s)" (sprint_val first) (sprint_val second) (sprint_comparison_mode comparison_mode)
  | B8ShiftRight(src, dest, shift_right_type) ->
    sprintf "shr (src: %s, dest: %s, type: %s)" (sprint_val src) (sprint_reg_option dest) (sprint_shift_right_type shift_right_type)
  | B8Jump(offset, jump_condition, jump_type) ->
    sprintf "jump (offset: %s, condition: %s, type: %s)" (sprint_val offset) (sprint_jump_condition jump_condition) (sprint_jump_type jump_type)
  | B8Pause -> "pause"
  | B8Nop -> "nop"
