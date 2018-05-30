%{
  open Ast
%}

%token <int> INT
%token <string> STR ID FID
%token CLASS IF THEN ELSE DEF END WHILE DO NEW NIL INSTANCEOF
%token LT LP RP COMMA SEMI DOT EQ EOF SELF BEGIN SUBCLASSES
%token LET
%start main
%type <Ast.expr> expr exprs
%type <Ast.expr list> params
%type <string list> ids
%type <Ast.meth> meth
%type <Ast.meth list> meths
%type <Ast.cls> cls
%type <Ast.cls list> clss
%type <Ast.ramble_prog> main
%right SEMI
%right EQ
%nonassoc INSTANCEOF
%nonassoc DOT
%%
main:
 clss exprs EOF { { prog_clss=$1; prog_main=$2 } }
 | clss EOF { {prog_clss=$1; prog_main=ENil} }
;
clss:
  { [] }
| cls clss { $1::$2 }
cls:
  CLASS ID SUBCLASSES ID BEGIN meths END {
    { cls_name = $2;
      cls_super = $4;
      cls_meths = $6 }
  }

meths:
  { [] }
| meth meths { $1::$2 }
;
meth:
  DEF ID LP ids RP exprs END {
    { meth_name = $2;
      meth_args = $4;
      meth_body = $6 }
  }
;
ids:
  { [] }
| ID  { [$1] }
| ID COMMA ids { $1::$3 }
;
exprs:
| expr { $1 }
| expr SEMI exprs { ESeq($1, $3) }
;
expr:
| INT { EInt $1 }
| NIL { ENil }
| SELF { ESelf }
| STR { EString $1 }
| ID { ELocRd $1 }
| ID EQ expr { ELocWr($1, $3) }
| FID { EFldRd $1 }
| FID EQ expr { EFldWr($1, $3) }
| IF expr THEN exprs ELSE exprs END { EIf($2, $4, $6) }
| WHILE expr DO exprs END { EWhile($2, $4) }
| expr DOT ID LP params RP { EInvoke($1, $3, $5) }
| NEW ID { ENew $2 }
| expr INSTANCEOF ID {EInstanceOf($1, $3) }
| LP exprs RP { $2 }
;
params:
  { [] }
| expr { [$1] }
| expr COMMA params { $1::$3 }
