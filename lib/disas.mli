open B8_ins

exception Internal_disassembler_error of string
exception Invalid_B8_code of string

val disas_bytes : int list -> b8_ins list
val disas_file : string -> b8_ins list
