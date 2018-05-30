open Ast
open Instr
open Disassembler

exception NewObj
exception WriteToSelf

(*********************************************************************)

let rec output_expr o = function
  | EInt i -> Printf.fprintf o "%d" i
  | ENil -> Printf.fprintf o "nil"
  | ESelf -> Printf.fprintf o "self"
  | EString s -> Printf.fprintf o "\"%s\"" s
  | ELocRd x -> output_string o x
  | ELocWr (x, e) ->
      Printf.fprintf o "%s = (%a)" x output_expr e
  | EFldRd x -> output_string o x
  | EFldWr (x, e) ->
      Printf.fprintf o "%s = (%a)" x output_expr e
  | EIf (e1, e2, e3) ->
      Printf.fprintf o "if %a then %a else %a end" output_expr e1
	output_expr e2 output_expr e3
  | EWhile (e1, e2) ->
     Printf.fprintf o "while %a do %a end" output_expr e1 output_expr e2
  | ESeq (e1, e2) -> Printf.fprintf o "%a; %a" output_expr e1 output_expr e2
  | ENew x ->
      Printf.fprintf o "new %s" x
  | EInstanceOf (e, s) -> Printf.fprintf o "%a instanceof %s" output_expr e s 
  | EInvoke (e, m, es) ->
      Printf.fprintf o "%a.%s(%a)" output_expr e m output_exprs es

and output_exprs o = function
    [] -> ()
  | [e] -> output_expr o e
  | e::es -> Printf.fprintf o "%a, %a" output_expr e output_exprs es

and output_arg o = function
    s -> Printf.fprintf o "%s" s

and output_args o = function
  | [] -> ()
  | [a] -> output_arg o a
  | a::aa -> Printf.fprintf o "%a, %a" output_arg a output_args aa

and output_locals o = function
  | [] -> ()
  | [l] -> output_arg o l
  | l::ls -> Printf.fprintf o "%a\n%a" output_arg l output_locals ls

and output_meth o ({meth_name=name; meth_args=args; meth_body=body}:meth) =
  Printf.fprintf o "  def %s(%a)\n %a\n  end\n" name output_args args output_expr body

and output_meths o = function
    [] -> ()
  | [m] -> Printf.fprintf o "%a" output_meth m
  | m::ms -> Printf.fprintf o "%a\n%a" output_meth m output_meths ms

and output_cls o ({cls_name=name; cls_super=super; cls_meths=meths}:cls) =
  Printf.fprintf o "class %s < %s\n %a\nend\n" name super output_meths meths

and output_clss o = function
    [] -> ()
  | [c] -> Printf.fprintf o "%a" output_cls c
  | c::cs -> Printf.fprintf o "%a\n%a" output_cls c output_clss cs

and print_program ({prog_clss=clss; prog_main=main}:ramble_prog) = match clss with
  | [] -> Printf.printf "%a\n" output_expr main
  | _ -> Printf.printf "%a\n%a\n" output_clss clss output_expr main

(*********************************************************************)
let next_reg =
    let n = ref 0 in
    fun () -> (let temp = !n in n := !n + 1; temp) 
;;

let add_super_entry_to_vtable = [|I_wr_tab ((`L_Reg 2), (`L_Reg 0), (`L_Reg 1));
                                  I_ret (`L_Reg 2)|]
;;


(* ALL to dos:
 * TODO: control flow stuff: if, while. nil in guards.
 * TODO: object equal
 *)


(* Vtables for nil, obj, str, int, and map.
 * These are used for all the built-in stuff for
 * the compiler.
 * These get stored in the global classtable.
 * They contain the built-in methods, as described in the spec.
 *
 * All of these having object as super class is hardcoded in.
 * For user-defined classes, iter is used.
 *)
let nil_vtable =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    (r, [I_mk_tab (`L_Reg r); 
         I_const ((`L_Reg r1), (`L_Str "equal?"));
         I_const ((`L_Reg r2), (`L_Id "obj_equal"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "to_s"));
         I_const ((`L_Reg r2), (`L_Id "obj_to_s"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "say"));
         I_const ((`L_Reg r2), (`L_Id "obj_print"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2))])
;;

let obj_vtable =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    (r, [I_mk_tab (`L_Reg r); 
         I_const ((`L_Reg r1), (`L_Str "equal?"));
         I_const ((`L_Reg r2), (`L_Id "obj_equal"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "to_s"));
         I_const ((`L_Reg r2), (`L_Id "obj_to_s"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "say"));
         I_const ((`L_Reg r2), (`L_Id "obj_print"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2))])
;;

let str_vtable = 
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    (r, [I_mk_tab (`L_Reg r); 
         I_const ((`L_Reg r1), (`L_Str "+"));
         I_const ((`L_Reg r2), (`L_Id "str_concat"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "length"));
         I_const ((`L_Reg r2), (`L_Id "str_length"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "equal?"));
         I_const ((`L_Reg r2), (`L_Id "obj_equal"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "to_s"));
         I_const ((`L_Reg r2), (`L_Id "obj_to_s"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "say"));
         I_const ((`L_Reg r2), (`L_Id "obj_print"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2))])

;;

let int_vtable =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    (r, [I_mk_tab (`L_Reg r); 
         I_const ((`L_Reg r1), (`L_Str "+"));
         I_const ((`L_Reg r2), (`L_Id "int_add"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "-"));
         I_const ((`L_Reg r2), (`L_Id "int_sub"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "*"));
         I_const ((`L_Reg r2), (`L_Id "int_mul"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "/"));
         I_const ((`L_Reg r2), (`L_Id "int_div"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "equal?"));
         I_const ((`L_Reg r2), (`L_Id "obj_equal"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "to_s"));
         I_const ((`L_Reg r2), (`L_Id "obj_to_s"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "say"));
         I_const ((`L_Reg r2), (`L_Id "obj_print"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2))])
;;


let map_vtable = 
 let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    (r, [I_mk_tab (`L_Reg r); 
         I_const ((`L_Reg r1), (`L_Str "find"));
         I_const ((`L_Reg r2), (`L_Id "map_find"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "insert"));
         I_const ((`L_Reg r2), (`L_Id "map_insert"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "has"));
         I_const ((`L_Reg r2), (`L_Id "map_has"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "iter"));
         I_const ((`L_Reg r2), (`L_Id "map_iter"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "equal?"));
         I_const ((`L_Reg r2), (`L_Id "obj_equal"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "to_s"));
         I_const ((`L_Reg r2), (`L_Id "obj_to_s"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "say"));
         I_const ((`L_Reg r2), (`L_Id "obj_print"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2))])
;;

(* Global classtable maps classes to vtables.
 * Stored in register 0.
 * Returns register it's in (0) + instructions for setting up
 * classtable. Also, this is initialized with vtables for
 * nil, obj, str, int, map, which are built into the compiler.
 *
 * New vtables can easily be added for different methods,
 * by accessing the table at register 0, and then writing
 * "class_name" -> vtable.
 * 
 * CURRENTLY: vtables are just fn_name_str -> fn_name_id, which
 * seems dumb but doesn't really cause any problems. The actual function
 * instructions are part of the main prog, and shouldn't be in 
 * the vtables.
 *)
let classtable =
    let (obj_tbl, obj_ins) = obj_vtable in
    let (nil_tbl, nil_ins) = nil_vtable in
    let (str_tbl, str_ins) = str_vtable in
    let (int_tbl, int_ins) = int_vtable in
    let (map_tbl, map_ins) = map_vtable in
    let r = next_reg () in
    let r' = next_reg () in
    let ins = 
        [I_mk_tab (`L_Reg r');
         I_const ((`L_Reg r), (`L_Str "#obj"));
         I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg obj_tbl));
         I_const ((`L_Reg r), (`L_Str "#nil"));
         I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg nil_tbl));
         I_const ((`L_Reg r), (`L_Str "#str"));
         I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg str_tbl));
         I_const ((`L_Reg r), (`L_Str "#int"));
         I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg int_tbl));
         I_const ((`L_Reg r), (`L_Str "#map"));
         I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg map_tbl));
         I_wr_glob ((`L_Id "classtable"), (`L_Reg r'))
        ]
    in
    (r', nil_ins @ obj_ins @ str_ins @ int_ins @ map_ins @ ins)
;;

(*******
 * Object instatiating. 
 *******)

let nil_obj =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    let r3 = next_reg () in
    let ins = 
        [I_mk_tab (`L_Reg r);
         I_rd_glob ((`L_Reg r1), (`L_Id "classtable"));
         I_const ((`L_Reg r2), (`L_Str "#nil"));
         I_rd_tab ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#vtable"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r3));
         I_const ((`L_Reg r1), (`L_Str "#type"));
         I_const ((`L_Reg r2), (`L_Str "#nil"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#contents"));
         I_const ((`L_Reg r2), (`L_Str "nil"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2))
        ]
    in
    (r,ins)
;;

(* Instantiations of built-in types.
 * String and Int have #contents field.
 *
 * Also, add a field that has class name? Would help.
 * (Noting of course reserved name, so #str and #int, etc.
 *
 * Steps:
     * make table for this object
     * get location of classtable
     * get vtable from class table
     * put into this table, under #vtable
     * put contents in this table, under #contents
     * put class in this table, under #str
 *)
let str_obj s =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    let r3 = next_reg () in
    let ins = 
        [I_mk_tab (`L_Reg r);
         I_rd_glob ((`L_Reg r1), (`L_Id "classtable"));
         I_const ((`L_Reg r2), (`L_Str "#str"));
         I_rd_tab ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#vtable"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r3));
         I_const ((`L_Reg r1), (`L_Str "#contents"));
         I_const ((`L_Reg r2), (`L_Str s));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#type"));
         I_const ((`L_Reg r2), (`L_Str "#str"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
        ]
    in
    (r,ins)
;;

let int_obj n =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    let r3 = next_reg () in
    let ins = 
        [I_mk_tab (`L_Reg r);
         I_rd_glob ((`L_Reg r1), (`L_Id "classtable"));
         I_const ((`L_Reg r2), (`L_Str "#int"));
         I_rd_tab ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#vtable"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r3));
         I_const ((`L_Reg r1), (`L_Str "#contents"));
         I_const ((`L_Reg r2), (`L_Int n));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#type"));
         I_const ((`L_Reg r2), (`L_Str "#int"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
        ]
    in
    (r,ins)
;;

let map_obj s =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    let r3 = next_reg () in
    let ins = 
        [I_mk_tab (`L_Reg r);
         I_rd_glob ((`L_Reg r1), (`L_Id "classtable"));
         I_const ((`L_Reg r2), (`L_Str "#map"));
         I_rd_tab ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#vtable"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r3));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#type"));
         I_const ((`L_Reg r2), (`L_Str "#map"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#contents"));
         I_const ((`L_Reg r2), (`L_Str s));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#mapcontents"));
         I_mk_tab (`L_Reg r2);
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2))
        ]
    in
    (r,ins)
;;

(* Instantiate a class, passing in class name.
 * Assumes that vtable has already been created, 
 * which should have happened during setup.
 *
 * General idea:
     * Read global table, if it exists, link vtable to this
     * object. If not, raise the correct Halt error. Use some
     * jumps and if_zeros, etc. 
 *)
let generic_obj s =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    let r3 = next_reg () in
    let ins = 
        [I_mk_tab (`L_Reg r);
         I_rd_glob ((`L_Reg r1), (`L_Id "classtable"));
         I_const ((`L_Reg r2), (`L_Str s));
         I_rd_tab ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#vtable"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r3));
         I_const ((`L_Reg r1), (`L_Str "#type"));
         I_const ((`L_Reg r2), (`L_Str s));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
         I_const ((`L_Reg r1), (`L_Str "#contents"));
         I_const ((`L_Reg r2), (`L_Str "object"));
         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
        ]
    in
    (r,ins)
;;

let add_built_in_functions h =
    let r = next_reg () in
    let r1 = next_reg () in
    let r2 = next_reg () in
    let r3 = next_reg () in
    let r4 = next_reg () in
    let r5 = next_reg () in

    (* object methods *)                  
    let (r1', ins1') = nil_obj in
    let (r2', ins2') = int_obj 1 in
    let obj_equal = ins1' @ ins2' @
                    [I_const ((`L_Reg r), (`L_Str "#type"));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_rd_tab ((`L_Reg r3), (`L_Reg 1), (`L_Reg r));
                    I_eq ((`L_Reg r), (`L_Reg r2), (`L_Reg r3));
                    I_if_zero ((`L_Reg r), 12);
                    I_const ((`L_Reg r), (`L_Str "object"));
                    I_eq ((`L_Reg r4), (`L_Reg r), (`L_Reg r2));
                    I_if_zero ((`L_Reg r4), 2);
                    I_eq ((`L_Reg r4), (`L_Reg 0), (`L_Reg 1));
                    I_jmp 5;
                    I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_rd_tab ((`L_Reg r3), (`L_Reg 1), (`L_Reg r));
                    I_eq ((`L_Reg r4), (`L_Reg r2), (`L_Reg r3));
                    I_if_zero ((`L_Reg r4), 2);
                    I_mov ((`L_Reg r5), (`L_Reg r2'));
                    I_jmp 1;
                    I_mov ((`L_Reg r5), (`L_Reg r1'));
                    I_mov ((`L_Reg 0), (`L_Reg r5));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "obj_equal" (Array.of_list obj_equal);

    (* to_s *)
    let (r', ins) = str_obj "" in
    let obj_to_s = [I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r1), (`L_Reg 0), (`L_Reg r))]
                    @ ins @
                   [I_const ((`L_Reg r), (`L_Id "to_s"));
                    I_call ((`L_Reg r),r1,r1);
                    I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r1));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "obj_to_s" (Array.of_list obj_to_s);

    let (r',ins) = nil_obj in
    let obj_print = [I_const ((`L_Reg r), (`L_Str "#contents"));
                     I_rd_tab ((`L_Reg r1), (`L_Reg 0), (`L_Reg r))]
                    @ ins @
                    [I_const ((`L_Reg r), (`L_Id "to_s"));
                     I_call ((`L_Reg r),r1,r1);
                     I_const ((`L_Reg r), (`L_Id "print_string"));
                     I_call ((`L_Reg r),r1,r1);
                     I_mov ((`L_Reg 0), (`L_Reg r'));
                     I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "obj_print" (Array.of_list obj_print);
    
    (* string methods *)
    (* + *)
    let (r', ins) = str_obj ""  in
    let str_concat = [I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r1), (`L_Reg 1), (`L_Reg r));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_const ((`L_Reg r3), (`L_Id "concat"));
                    I_call ((`L_Reg r3), r1, r2)]
                  @ ins @
                   [I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r1));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "str_concat" (Array.of_list str_concat); 

    (* length *)
    let (r', ins) = int_obj 0  in
    let str_length = [I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r1), (`L_Reg 0), (`L_Reg r));
                    I_const ((`L_Reg r3), (`L_Id "length"));
                    I_call ((`L_Reg r3), r1, r1)]
                  @ ins @
                   [I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r1));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "str_length" (Array.of_list str_length); 

    (* int methods *)
    (* + *)
    let (r', ins) = int_obj 0 in
    let int_add = [I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r1), (`L_Reg 1), (`L_Reg r));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_add ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2))]
                  @ ins @
                   [I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r3));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "int_add" (Array.of_list int_add); 

    (* - *)
    let (r', ins) = int_obj 0 in
    let int_sub = [I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r1), (`L_Reg 1), (`L_Reg r));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_sub ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2))]
                  @ ins @
                   [I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r3));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "int_sub" (Array.of_list int_sub); 

    (* * *)
    let (r', ins) = int_obj 0 in
    let int_mul = [I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r1), (`L_Reg 1), (`L_Reg r));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_mul ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2))]
                  @ ins @
                   [I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r3));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "int_mul" (Array.of_list int_mul); 

    (* / *)
    let (r', ins) = int_obj 0 in
    let int_div = [I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r1), (`L_Reg 1), (`L_Reg r));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_div ((`L_Reg r3), (`L_Reg r1), (`L_Reg r2))]
                  @ ins @
                   [I_wr_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r3));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "int_div" (Array.of_list int_div); 

    (* map methods *)
    (* unboxes strings, ints, and nils, as they are "primitives".
     * for other objects, just compares pointers. Similar to
     * code from "equal?".
     *
     * A hacky way of doing this is: 
         * read "#contents" field
         * if it's equal to obj, do some stuff
         * otherwise, unbox
     *)

    let r4 = next_reg () in
    let r5 = next_reg () in
    let r6 = next_reg () in
    (* find *)
    let map_find = [I_const ((`L_Reg r), (`L_Str "#mapcontents"));
                    I_rd_tab ((`L_Reg r3), (`L_Reg 1), (`L_Reg r));
                    I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_const ((`L_Reg r), (`L_Str "object"));
                    I_const ((`L_Reg r4), (`L_Id "to_s"));
                    I_mov ((`L_Reg r5), (`L_Reg r2));
                    I_call ((`L_Reg r4), r5, r5);
                    I_eq ((`L_Reg r1), (`L_Reg r5), (`L_Reg r));
                    I_if_zero ((`L_Reg r1), 2);
                    I_rd_tab ((`L_Reg r1), (`L_Reg r3), (`L_Reg 1));
                    I_jmp 1;
                    I_rd_tab ((`L_Reg r1), (`L_Reg r3), (`L_Reg r2));
                    I_mov ((`L_Reg 0), (`L_Reg r1));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "map_find" (Array.of_list map_find);
    
    (* insert *)
    let (r', ins) = nil_obj in
    let map_insert = 
             ins @ [I_const ((`L_Reg r), (`L_Str "#mapcontents"));
                    I_rd_tab ((`L_Reg r3), (`L_Reg 2), (`L_Reg r));
                    I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_const ((`L_Reg r4), (`L_Id "to_s"));
                    I_mov ((`L_Reg r5), (`L_Reg r2));
                    I_call ((`L_Reg r4), r5, r5);
                    I_const ((`L_Reg r), (`L_Str "object"));
                    I_eq ((`L_Reg r1), (`L_Reg r5), (`L_Reg r));
                    I_if_zero ((`L_Reg r1), 2);
                    I_wr_tab ((`L_Reg r3), (`L_Reg 0), (`L_Reg 1));
                    I_jmp 1;
                    I_wr_tab ((`L_Reg r3), (`L_Reg r2), (`L_Reg 1));
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "map_insert" (Array.of_list map_insert);

    (* has *)
    let (r',ins') = nil_obj in
    let (r1', ins1') = int_obj 1 in
    let map_has = ins' @ ins1' @ [I_const ((`L_Reg r), (`L_Str "#mapcontents"));
                    I_rd_tab ((`L_Reg r3), (`L_Reg 1), (`L_Reg r));
                    I_const ((`L_Reg r), (`L_Str "#contents"));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r));
                    I_const ((`L_Reg r), (`L_Str "object"));
                    I_const ((`L_Reg r4), (`L_Id "to_s"));
                    I_mov ((`L_Reg r5), (`L_Reg r2));
                    I_call ((`L_Reg r4), r5, r5);
                    I_eq ((`L_Reg r1), (`L_Reg r5), (`L_Reg r));
                    I_if_zero ((`L_Reg r1), 2);
                    I_has_tab ((`L_Reg r1), (`L_Reg r3), (`L_Reg 1));
                    I_jmp 1;
                    I_has_tab ((`L_Reg r1), (`L_Reg r3), (`L_Reg r2));
                    I_if_zero ((`L_Reg r1), 2);
                    I_mov ((`L_Reg r1), (`L_Reg r1'));
                    I_jmp 1;
                    I_mov ((`L_Reg r1), (`L_Reg r'));
                    I_mov ((`L_Reg 0), (`L_Reg r1));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "map_has" (Array.of_list map_has);

    (* iter *)
    let (r',ins') = nil_obj in
    let r4 = next_reg () in
    let r5 = next_reg () in
    let r6 = next_reg () in
    let map_iter = ins' @ [I_const ((`L_Reg r), (`L_Str "#mapcontents"));
                    I_rd_tab ((`L_Reg r3), (`L_Reg 1), (`L_Reg r));
                    I_rd_glob ((`L_Reg r), (`L_Id "classtable"));
                    I_const ((`L_Reg r1), (`L_Str "#type"));
                    I_rd_tab ((`L_Reg r2), (`L_Reg 0), (`L_Reg r1));
                    I_rd_tab ((`L_Reg r1), (`L_Reg r), (`L_Reg r2));
                    I_const ((`L_Reg r2), (`L_Str "call"));
                    I_rd_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
                    I_const ((`L_Reg r1), (`L_Id "iter"));
                    I_mov ((`L_Reg r4), (`L_Reg r3));
                    I_mov ((`L_Reg r5), (`L_Reg r));
                    I_mov ((`L_Reg r6), (`L_Reg 0));
                    I_call ((`L_Reg r1), r4, r6);
                    I_mov ((`L_Reg 0), (`L_Reg r'));
                    I_ret (`L_Reg 0)]
    in
    Hashtbl.add h "map_iter" (Array.of_list map_iter);
;;

type state = (string*int) list (*assoc list of strings (variable names/ids) to register numbers*)
type reduction = (int*state*(instr list)) 

(* Compile an expression.
 * Pass in: 
     * st: state
     * e: expr
 * Return: reduction
 *)
let rec comp_expr (st:state) (e:expr):reduction = match e with
  | EInt n ->
          let (r,ins) = int_obj n in
          (r, ("self",r)::st, ins)
  | ENil ->
          let (r,ins) = nil_obj in
          (r, ("self",r)::st, ins)
  | ESelf ->
          let r = next_reg () in
          let r1 = (List.assoc "self" st) in
          (r,("self",r)::st,[I_mov (`L_Reg r, `L_Reg r1)]) 
  | EString s ->
          let (r,ins) = str_obj s in
          (r, ("self",r)::st, ins) 
  | ELocRd s -> 
          if (List.mem_assoc s st) then
            let r = next_reg () in
            let r1 = List.assoc s st in
            (r,st,[I_mov (`L_Reg r, `L_Reg r1)]) 
          else
            let r = next_reg () in
            (r,st,[I_const (`L_Reg r, `L_Id s)])
            (*raise (Unbounded s)*)
  | ELocWr (s,e) -> 
          if (s = "self") then
             raise WriteToSelf
          else

          if (List.mem_assoc s st) then
            let r1 = List.assoc s st in
            let (r2,st2,p1) = comp_expr st e in
            (r1,st,p1 @ [I_mov (`L_Reg r1, `L_Reg r2)] ) 
          else
            let r1 = next_reg () in
            let (r2,st1,p1) = comp_expr st e in 
            (r1,(s,r2)::st1,p1 @ [I_mov (`L_Reg r1, `L_Reg r2)])
  | EFldRd s ->
          let r = next_reg () in
          let r' = next_reg () in
          let (r1, ins) = nil_obj in
          let slf = (List.assoc "self" st) in
          let ins' = [I_const ((`L_Reg r'), (`L_Str s)); 
                      I_has_tab ((`L_Reg r), (`L_Reg slf), (`L_Reg r'));
                      I_if_zero ((`L_Reg r), 2);
                      I_const ((`L_Reg r'), (`L_Str s));
                      I_rd_tab ((`L_Reg r1), (`L_Reg slf), (`L_Reg r'))]
          in
          (r1, st, ins @ ins')
  | EFldWr (s,e1) ->
          let r' = next_reg () in
          let (r1,st1,p1) = comp_expr st e1 in
          let slf = (List.assoc "self" st) in
          (r1, ("self",slf)::st1, p1 @ [I_const ((`L_Reg r'), (`L_Str s)); I_wr_tab ((`L_Reg slf), (`L_Reg r'), (`L_Reg r1))])        
  | EIf (e1,e2,e3) -> 
          let (r1, st1, p1) = comp_expr st e1 in
          let (r2, st2, p2) = comp_expr st1 e2 in
          let (r3, st3, p3) = comp_expr st2 e3 in
          let r = next_reg () in
          let r' = next_reg () in
          let r'' = next_reg () in

          let ins = p1 @ [I_const ((`L_Reg r), (`L_Str "#type"));
                          I_rd_tab ((`L_Reg r'), (`L_Reg r1), (`L_Reg r));
                          I_const ((`L_Reg r), (`L_Str "#nil"));
                          I_const ((`L_Reg r''), (`L_Id "to_s"));
                          I_call ((`L_Reg r''), r', r');
                          I_eq ((`L_Reg r''), (`L_Reg r), (`L_Reg r'));
                          I_if_zero ((`L_Reg r''), 2);
                          I_const ((`L_Reg r''), (`L_Int 1));
                          I_jmp 1;
                          I_const ((`L_Reg r''), (`L_Int 0));
                          I_if_zero (`L_Reg r'', (2 + (List.length p3)))] @
                    p3 @ [I_mov (`L_Reg r, `L_Reg r3); I_jmp (1 + (List.length p2))] @
                    p2 @ [I_mov (`L_Reg r, `L_Reg r2)] in
            
          (r,st3,ins)
  | EWhile (e1,e2) ->
          let (r1,st1,p1) = comp_expr st e1 in
          let (r2,st2,p2) = comp_expr st1 e2 in
          let r = next_reg () in
          let r' = next_reg () in
          let r'' = next_reg () in
          let (r1', ins1') = nil_obj in

          let ins = ins1' @ p1 @ [I_const ((`L_Reg r), (`L_Str "#type"));
                          I_rd_tab ((`L_Reg r'), (`L_Reg r1), (`L_Reg r));
                          I_const ((`L_Reg r), (`L_Str "#nil"));
                          I_const ((`L_Reg r''), (`L_Id "to_s"));
                          I_call ((`L_Reg r''), r', r');
                          I_eq ((`L_Reg r''), (`L_Reg r), (`L_Reg r'));
                          I_if_zero ((`L_Reg r''), 2);
                          I_const ((`L_Reg r''), (`L_Int 0));
                          I_jmp 1;
                          I_const ((`L_Reg r''), (`L_Int 1));
                          I_if_zero (`L_Reg r'', (4 + (List.length p2)))] @
                    p2 @ [I_mov (`L_Reg r, `L_Reg r2); I_jmp (-((List.length p2) + 3 + 12))] in

          (r1', st2, ins)
  | ESeq (e1,e2) ->
          let (r1,st1,p1) = comp_expr st e1 in 
          let (r2,st2,p2) = comp_expr st1 e2 in
          (r2, st2, p1 @ p2) 
  | ENew s ->
          if (s = "Object") then
              let (r,ins) = generic_obj "#obj" in
              let r' = next_reg () in
              (r',("self",r')::st,ins@[I_mov ((`L_Reg r'), (`L_Reg r))])
          else if (s = "Bot") then
              let r = next_reg () in
              (r,("self",r)::st,[I_const ((`L_Reg r), (`L_Str "Cannot instantiate Bot")); I_halt (`L_Reg r)])
          else if (s = "String") then
              let (r,ins) = str_obj "" in
              let r' = next_reg () in
              (r',("self",r')::st,ins@[I_mov ((`L_Reg r'), (`L_Reg r))])
          else if (s = "Integer") then
              let (r,ins) = int_obj 0 in
              let r' = next_reg () in
              (r',("self",r')::st,ins@[I_mov ((`L_Reg r'), (`L_Reg r))])
          else if (s = "Map") then
              let (r,ins) = map_obj "map" in
              let r' = next_reg () in
              (r',("self",r')::st,ins@[I_mov ((`L_Reg r'), (`L_Reg r))])
          else
              let (r,ins) = generic_obj s in
              let r' = next_reg () in
              (r',("self",r')::st,ins@[I_mov ((`L_Reg r'), (`L_Reg r))])
  | EInstanceOf (e1,s) ->
          let (r1,st1,p1) = comp_expr st e1 in
          let r = next_reg () in
          let r2 = next_reg () in
          let r3 = next_reg () in
          let s' = (if (s = "Bot") then "#nil"
                    else if (s = "Object") then "#obj"
                    else if (s = "String") then "#str" 
                    else if (s = "Integer") then "#int"
                    else if (s = "Map") then "#map"
                    else s) in
          let ins =
              [I_const ((`L_Reg r), (`L_Str "#type"));
               I_rd_tab ((`L_Reg r2), (`L_Reg r1), (`L_Reg r));
               I_const ((`L_Reg r), (`L_Str s'));
               I_eq ((`L_Reg r3), (`L_Reg r), (`L_Reg r2));]
          in 
          (r3,st1,p1 @ ins)
  | EInvoke (e1, s, es) ->
        let (r2,st2,self_ins) = comp_expr st e1 in
        let r = next_reg () in
        let rec comp_list st' ins l rl = (match l with
            | [] -> ((r,st,[]),rl)
            | [h] ->  let (r1,st1,p1) = comp_expr st' h in
                    ((r1,st1, (p1 @ ins)), [r1] @ rl)
            | h::t -> let (r1,st1,p1) = comp_expr st' h in
                    comp_list st1 (p1 @ ins) t ([r1] @ rl)
                     
        ) in

        let ((r1,st1,param_ins),rl) = comp_list st2 [] es [] in
        (* once reg-list is built by comp_list, 
         * generate a move instruction for each
         * register s.t. parameters are consecutive *)
        let start = next_reg () in
        let rec mov_regs lst = (match lst with
            | [] -> []
            | (h::t) -> let r = next_reg () in
                        [I_mov ((`L_Reg r), (`L_Reg h))] @ (mov_regs t)
        ) in
        
        let mov_ins = mov_regs ((List.rev rl) @ [r2]) in

        let r = next_reg () in
        let r' = next_reg () in
        let r'' = next_reg () in
        let r1' = next_reg () in
        let ins = [I_rd_glob ((`L_Reg r), (`L_Id "classtable"));
                   I_const ((`L_Reg r'), (`L_Str "#type"));
                   I_rd_tab ((`L_Reg r''), (`L_Reg r2), (`L_Reg r')); 
                   I_rd_tab ((`L_Reg r'), (`L_Reg r), (`L_Reg r''));
                   I_const ((`L_Reg r''), (`L_Str s));
                   I_has_tab ((`L_Reg r), (`L_Reg r'), (`L_Reg r''));
                   I_if_zero ((`L_Reg r), 3);
                   I_rd_tab ((`L_Reg r1'), (`L_Reg r'), (`L_Reg r''));
                   I_call ((`L_Reg r1'), (start+1), (start + (List.length es) + 1));
                   I_jmp 2;
                   I_const ((`L_Reg r), (`L_Str "No such method"));
                   I_halt (`L_Reg r);
                  ] in
        ((start+1), st1, self_ins @ param_ins @ mov_ins @ ins)
;;

(* This makes sure that all parameters of a function are 
 * initialized so they exist in the state before a function
 * is called. Copied from p4 main.ml, don't remember exact
 * deets.
 *)
let rec args_as_state l i = match l with
    | [] -> []
    | (h::t) -> (h,i)::(args_as_state t (i+1))
;;

(* Build vtable from class while also 
 * putting methods in prog hashtable. 
 *
 * idea:
     * traverse thru cls_meths for given class
     * add each meth to global program hashtable
        * mapping of meth_name -> compiled instruction array,
        * ensuring that arguments are part of state when compiling
        * (where args_as_state comes in)
     * simultaneously, build a vtable for this class
        * do this by initializing vtable with "I_mk_tab"
        * then for each method, put it in the vtable
            * (currently vtables are name_string -> name_id)
        * at the end, write the vtable to the global classtable
            * do this by reading the global class table into register,
            * then table-writing into that register
            * should be class_name (id) -> table location (from mk_tab, the r)
 *
 * What about states?
 * Each def ... end method should get its own state, correct? I think so.
 * So we don't care about state at the end of a method, it's not used.
 * The instructions ae important and so is the register returned to. 
 *
 * Does it work? Who knows. But idea is good.
 *
 * Everything having object as a superclass is hardcoded in.
 *)
let vtable_from_class h (c:cls) =
    let methods = c.cls_meths in
    let r = next_reg () in
    let rec build_vtable ml vt = match ml with
        | [] -> let r1 = next_reg () in
                let r2 = next_reg () in
                vt @
            [I_rd_glob ((`L_Reg r1), (`L_Id "classtable")); 
             I_const ((`L_Reg r2), (`L_Str c.cls_name));
             I_wr_tab ((`L_Reg r1), (`L_Reg r2), (`L_Reg r))] 
        | (m::t) -> let state = args_as_state m.meth_args 0 in
                    let (obj,ins) = generic_obj c.cls_name in
                    let (r1,s1,p1) = comp_expr (("self",obj)::state) m.meth_body in
                    let p1' = ins @ p1 @ [I_ret (`L_Reg r1)] in
                    Hashtbl.add h (c.cls_name ^ "_" ^ m.meth_name) (Array.of_list p1');
                    let r1 = next_reg () in
                    let r2 = next_reg () in
                    let r3 = next_reg () in
                    let r4 = next_reg () in

                    let s = (if (c.cls_super = "Bot") then "#nil"
                        else if (c.cls_super = "Object") then "#obj"
                        else if (c.cls_super = "String") then "#str" 
                        else if (c.cls_super = "Integer") then "#int"
                        else if (c.cls_super = "Map") then "#map"
                        else c.cls_super) in

                    let super_tab = next_reg () in
                    let funct = next_reg () in
                    let this_tab = next_reg () in
                    (*print_string m.meth_name; print_string ":\n";
                    dis_instrs stdout (Array.of_list p1');*)
                    build_vtable t (vt @ 
                        [I_const ((`L_Reg r1), (`L_Str m.meth_name));
                         I_const ((`L_Reg r2), (`L_Id (c.cls_name ^ "_" ^ m.meth_name)));
                         I_wr_tab ((`L_Reg r), (`L_Reg r1), (`L_Reg r2));
                         I_rd_glob ((`L_Reg r3), (`L_Id "classtable"));
                         I_const ((`L_Reg r2), (`L_Str s));
                         I_rd_tab ((`L_Reg r4), (`L_Reg r3), (`L_Reg r2));
                         I_const ((`L_Reg r2), (`L_Id "iter"));
                         I_mov ((`L_Reg super_tab), (`L_Reg r4));
                         I_const ((`L_Reg funct), (`L_Id "add_super_entry_to_vtable"));
                         I_mov ((`L_Reg this_tab), (`L_Reg r));
                         I_call ((`L_Reg r2), super_tab, this_tab) 
                         ])
    in
    build_vtable methods [I_mk_tab (`L_Reg r)] 
;; 

(* Traverse through each class, getting the vtable
 * set-up instructions from each one. Then, put all
 * of these instructions together, and return that.
 *
 * Result is all vtables are set up, global #classtable
 * should be updated to reflect that, and prog hashtable
 * should have all functions in it.
 *)
let rec build_instrs ht (c:cls list) =
    match c with
        | [] -> [] 
        | (cl::t) -> let ins = vtable_from_class ht cl in
                     ins @ (build_instrs ht t) 
;; 

(* All preliminary set up is:
    * Build vtables for all classes, store in one  
        * main classtable, which is in a global variable ("#classtable").
    * Put in all built-in functions, which also get
        * put in the global classtable. 
 * Compile main, then add that to prog.
 *
 *)
let compile_prog (p:ramble_prog):prog =
  (* make main htable *)  
  let h = Hashtbl.create 17 in
  
  Hashtbl.add h "add_super_entry_to_vtable" add_super_entry_to_vtable; 
  add_built_in_functions h;

  (* put all functions in main htable *)
  let class_ins = build_instrs h p.prog_clss in

  (* get classtable setup instructions *) 
  let (_,classtable_ins) = classtable in

  (* compile "main", top-level code, with superclass as object *)
  let (obj,obj_ins) = generic_obj "#obj" in
  let (main_r, _, main_instrs) = comp_expr [("self",obj)] p.prog_main in

  let r = next_reg () in
  let r1 = next_reg () in
  
  let final_ins = [I_const ((`L_Reg r), (`L_Id "obj_to_s"));
                   I_call ((`L_Reg r), main_r, main_r);
                   I_const ((`L_Reg r), (`L_Str "#contents"));
                   I_rd_tab ((`L_Reg r1), (`L_Reg main_r), (`L_Reg r));
                   I_const ((`L_Reg r), (`L_Id "to_s"));
                   I_call ((`L_Reg r), r1, r1);
                   I_ret (`L_Reg r1)]
  in
                 

  let main_instrs' = classtable_ins @ class_ins @ obj_ins @ main_instrs @ final_ins in 
  let _ = Hashtbl.add h "main" (Array.of_list main_instrs') in
  match p with {prog_clss = cl; prog_main = ex} ->
    (match ex with
       _ -> ());
    h

(*********************************************************************)

let parse_file name =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let (p:ramble_prog) = Parser.main Lexer.token lexbuf in
    close_in chan;
    p

let main () =
  let p = parse_file Sys.argv.(1) in
  let (p':Instr.prog) = compile_prog p in
  let out_chan = open_out "a.out" in
    disassemble out_chan p'

;;

main ()
