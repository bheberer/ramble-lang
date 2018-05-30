{
 open Parser
 exception Eof
}
rule token = parse
  [' ' '\t' '\n' '\r'] { token lexbuf }
| ['U''u']['M''m']+ {  token lexbuf } 
| ['U''u']['H''h']+[^ '\n']* '\n' { token lexbuf }
| "class" { CLASS }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "you know?" { END }
| "while" { WHILE }
| "do" { DO }
| "def" { DEF }
| "let" { DEF }
| "nil" { NIL }
| "self" { SELF }
| "begin" { BEGIN }
| "new" { NEW }
| "instanceof" { INSTANCEOF }
| "subclasses" { SUBCLASSES }
| '(' { LP }
| ')' { RP }
| ',' { COMMA }
| " yeah" { SEMI }
| '.' { DOT }
| '=' { EQ }
| '"' [^'"']* '"' as lxm { STR(Scanf.unescaped(String.sub lxm 1 ((String.length lxm) - 2))) }
| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
| ['A'-'Z''a'-'z''+''-''*''/''_''!''?']+ as lxm { ID(lxm) }
| '@'['A'-'Z''a'-'z''+''-''*''/''_''!''?']+ as lxm { FID(lxm) }
| eof { EOF }
| _ as lxm { Printf.printf "Illegal character %c" lxm; failwith "Bad input" }
