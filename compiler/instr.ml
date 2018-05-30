type reg = [ `L_Reg of int ]
type value = [ `L_Int of int | `L_Str of string | `L_Id of string | `L_Loc of int ]
type id = [ `L_Id of string ]
type returnType = [`Halt of value | `Reg of value]

type instr =
  | I_const of reg * value (* dst, src *)
  | I_mov of reg * reg (* dst, src *)
  | I_add of reg * reg * reg (* dst, src1, src2 *)
  | I_sub of reg * reg * reg (* dst, src1, src2 *)
  | I_mul of reg * reg * reg (* dst, src1, src2 *)
  | I_div of reg * reg * reg (* dst, src1, src2 *)
  | I_eq of reg * reg * reg (* dst, src1, src2 *)
  | I_lt of reg * reg * reg (* dst, src1, src2 *)
  | I_leq of reg * reg * reg (* dst, src1, src2 *)
  | I_jmp of int (* offset *)
  | I_if_zero of reg * int (* src, offset *)
  | I_rd_glob of reg * id (* dst, src *)
  | I_wr_glob of id * reg (* dst, src *)
  | I_mk_tab of reg (* dst *)
  | I_rd_tab of reg * reg * reg (* dst, tab, key *)
  | I_wr_tab of reg * reg * reg (* tab, key, value *)
  | I_has_tab of reg * reg * reg (* dst, tab, key *)
  | I_call of reg * int * int (* dst, first, last *)
  | I_ret of reg (* src *)
  | I_halt of reg (* src *)
  | I_is_int of reg * reg (* dst, src *)
  | I_is_str of reg * reg (* dst, src *)
  | I_is_tab of reg * reg (* dst, src *)

type fn = instr array
type prog = (string, fn) Hashtbl.t
type heap = (value, value) Hashtbl.t
type regs = (int, value) Hashtbl.t
type stack = (string * int * regs) list
type config = heap * stack
