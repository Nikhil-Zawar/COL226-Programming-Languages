%{
open A4_types

%}

%token <string> IDEN VAR INT BOOL_CONST BOOL_OPER LOGICAL_OPER ARITH_OPER
%token LEFT_BRACKET RIGHT_BRACKET LEFT_SQUARE RIGHT_SQUARE
%token COMMA UNDERSCORE SEMICOLON EXCLAMATION_MARK VERTICAL_BAR End_reached
%token DEF END_COMMAND

%start main
%type <A4_types.program> main

%%

main:
  // | clause { [$1] }
  | clause main { $1 :: $2 }
  | error {raise ( A4_types.ParseError "Error was detected")}
  | End_reached {[]}

clause:
  | atomic_formula END_COMMAND{ FACT($1) }
  | atomic_formula DEF atomic_formulalist END_COMMAND{ RULE($1 , $3) }

atomic_formulalist: 
  | atomic_formula {[$1]}
  | atomic_formula COMMA atomic_formulalist {[$1] @ $3}

atomic_formula:
  | IDEN LEFT_BRACKET terms RIGHT_BRACKET   {($1,$3)}

terms:
  | term { [$1] }
  | term COMMA terms { [$1] @ $3 }

term:
  | VAR { VARIABLE($1) }
  | IDEN { IDENTIFIER($1) }
  | ARITH_OPER {BINARY_OPERATOR($1)}
  | BOOL_OPER {BINARY_OPERATOR($1)}
  | LOGICAL_OPER {BINARY_OPERATOR($1)}
  | INT { STRING_CONST($1)}
  | BOOL_CONST {STRING_CONST($1)}
  | IDEN LEFT_BRACKET terms RIGHT_BRACKET { ATOM($1,$3) }
  | ARITH_OPER LEFT_BRACKET terms RIGHT_BRACKET {ATOM($1,$3)}
  // | ARITH_OPER {BINARY_OPERAND($1)}
  // | term ARITH_OPER term {BINARY_OPERAND($3)}
  | BOOL_OPER LEFT_BRACKET terms RIGHT_BRACKET {ATOM($1,$3)}
  // | term BOOL_OPER term {BINARY_OPERAND($1)}
  // | term BOOL_OPER term {BINARY_OPERAND($3)}
  | LOGICAL_OPER LEFT_BRACKET terms RIGHT_BRACKET{ATOM($1,$3)}
  // | term LOGICAL_OPER term {BINARY_OPERAND($1)}
  // | term LOGICAL_OPER term {BINARY_OPERAND($3)}
%%
