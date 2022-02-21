open Block_8_asm

let () =
  print_string (String.concat "\n" (List.map B8_ins.sprint_ins (Disas.disas_file "test_b8_program")));
  print_string "\n"
