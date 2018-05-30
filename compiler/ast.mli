type expr =
  | EInt of int
  | ENil
  | ESelf
  | EString of string
  | ELocRd of string    (* Read a local variable *)
  | ELocWr of string * expr  (* Write a local variable *)
  | EFldRd of string    (* Read a field *)
  | EFldWr of string * expr  (* Write a field *)
  | EIf of expr * expr * expr
  | EWhile of expr * expr
  | ESeq of expr * expr
  | ENew of string
  | EInstanceOf of expr * string
  | EInvoke of expr * string * (expr list)

(*  meth name * arg name list * method body *)
type meth = { meth_name : string;
	      meth_args : string list;
	      meth_body : expr }

(* class name * superclass name * methods *)
type cls = { cls_name : string;
	     cls_super : string;
	     cls_meths : meth list }

(* classes * top-level expression *)
type ramble_prog = { prog_clss : cls list;
	           prog_main : expr }
