{
 open Parser
 exception Eof

  let keywords = Hashtbl.create 37
  let _ =
    List.iter (fun (k, t) -> Hashtbl.add keywords k t)
              [ "const", CONST;
		"mov", MOV;
		"add", ADD;
		"sub", SUB;
		"mul", MUL;
		"div", DIV;
		"eq", EQ;
		"lt", LT;
		"leq", LEQ;
		"is_int", IS_INT;
		"is_str", IS_STR;
		"is_tab", IS_TAB;
		"jmp", JMP;
		"if_zero", IF_ZERO;
		"rd_glob", RD_GLOB;
		"wr_glob", WR_GLOB;
		"mk_tab", MK_TAB;
		"rd_tab", RD_TAB;
		"wr_tab", WR_TAB;
		"has_tab", HAS_TAB;
		"call", CALL;
		"ret", RET;
		"halt", HALT;
	      ]
}

let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule token = parse
  [' ' '\t' '\r']		{ token lexbuf } (* skip whitespace *)
| '#' [^ '\n']* '\n'		{ token lexbuf } (* skip #'d lines *)
| '\n'				{ EOL }
| '-'? ['0'-'9']+ as s		{ INT(int_of_string s) }
| id ':' as s			{ FN(String.sub s 0 ((String.length s) - 1)) }
| 'r' ['0'-'9']+ as r   	{ REG(int_of_string (Str.string_after r 1)) }
| '"' [^'"' '\n']* '"' as s	{ STR(Scanf.unescaped(String.sub s 1 ((String.length s) - 2))) }
| id as s			{ try Hashtbl.find keywords s with Not_found -> ID s }
| ','				{ COMMA }
| eof				{ EOF }
| _ as lxm { Printf.printf "Illegal character %c" lxm; failwith "Bad input" }

