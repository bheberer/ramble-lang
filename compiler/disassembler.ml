open Instr

let reg o (`L_Reg r) = Printf.fprintf o "r%d" r

let value o = function
  | `L_Int n -> Printf.fprintf o "%d" n
  | `L_Str s -> Printf.fprintf o "\"%s\"" (String.escaped s)
  | `L_Id s -> Printf.fprintf o "%s" s
  | `L_Loc n -> Printf.fprintf o "ptr(%d)" n

let id o (`L_Id s) = Printf.fprintf o "%s" s

let dis_instr o = function
  | I_const (r, v) -> Printf.fprintf o "  const %a, %a" reg r value v
  | I_mov (r1, r2) -> Printf.fprintf o "  mov %a, %a" reg r1 reg r2
  | I_add (r1, r2, r3) -> Printf.fprintf o "  add %a, %a, %a" reg r1 reg r2 reg r3
  | I_sub (r1, r2, r3) -> Printf.fprintf o "  sub %a, %a, %a" reg r1 reg r2 reg r3
  | I_mul (r1, r2, r3) -> Printf.fprintf o "  mul %a, %a, %a" reg r1 reg r2 reg r3
  | I_div (r1, r2, r3) -> Printf.fprintf o "  div %a, %a, %a" reg r1 reg r2 reg r3
  | I_eq (r1, r2, r3) -> Printf.fprintf o "  eq %a, %a, %a" reg r1 reg r2 reg r3
  | I_lt (r1, r2, r3) -> Printf.fprintf o "  lt %a, %a, %a" reg r1 reg r2 reg r3
  | I_leq (r1, r2, r3) -> Printf.fprintf o "  leq %a, %a, %a" reg r1 reg r2 reg r3
  | I_jmp n -> Printf.fprintf o "  jmp %d" n
  | I_if_zero (r, n) -> Printf.fprintf o "  if_zero %a, %d" reg r n
  | I_rd_glob (r, x) -> Printf.fprintf o "  rd_glob %a, %a" reg r id x
  | I_wr_glob (x, r) -> Printf.fprintf o "  wr_glob %a, %a" id x reg r
  | I_mk_tab r -> Printf.fprintf o "  mk_tab %a" reg r
  | I_rd_tab (r1, r2, r3) -> Printf.fprintf o "  rd_tab %a, %a, %a" reg r1 reg r2 reg r3
  | I_wr_tab (r1, r2, r3) -> Printf.fprintf o "  wr_tab %a, %a, %a" reg r1 reg r2 reg r3
  | I_has_tab (r1, r2, r3) -> Printf.fprintf o "  has_tab %a, %a, %a" reg r1 reg r2 reg r3
  | I_call (r, n1, n2) -> Printf.fprintf o "  call %a, %d, %d" reg r n1 n2
  | I_ret r -> Printf.fprintf o "  ret %a" reg r
  | I_halt r-> Printf.fprintf o "  halt %a" reg r
  | I_is_int (r0, r1) -> Printf.fprintf o "  is_int %a, %a" reg r0 reg r1
  | I_is_str (r0, r1) -> Printf.fprintf o "  is_str %a, %a" reg r0 reg r1
  | I_is_tab (r0, r1) -> Printf.fprintf o "  is_tab %a, %a" reg r0 reg r1

let rec dis_instrs o (is:instr array) =
  Array.iter (fun i -> Printf.fprintf o "%a\n" dis_instr i) is

let disassemble (out:out_channel) (p:prog) =
  Hashtbl.iter (fun f is -> Printf.fprintf out "%s:\n" f; dis_instrs out is) p

let print_heap h =
  Printf.printf "Heap mappings:\n";
  Hashtbl.iter (fun l v -> Printf.printf "  %a -> %a\n" value l value v) h
