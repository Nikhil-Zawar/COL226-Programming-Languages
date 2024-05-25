{
  open A4_parser
}

rule token = parse
    [' ' '\t' '\n']+     { token lexbuf }
    | "true" | "false" as arg {BOOL_CONST(arg)}
    | "equality" | "not_equal" | "gte" | "lte" | "gt" | "lt" as arg {BOOL_OPER(arg)}
    | "and" | "or" | "xor" | "nand" | "nor" as arg {LOGICAL_OPER(arg)}
    | "+" | "-" | "**" as arg {ARITH_OPER(arg)}
    | ['.'] {END_COMMAND}
    | ['('] {LEFT_BRACKET}
    | [')'] {RIGHT_BRACKET}
    | [','] {COMMA}
    | ['!'] {EXCLAMATION_MARK}
    | ":-" {DEF}
    | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as identi {IDEN (identi) }
    | ['A'-'Z' ]['a'-'z' 'A'-'Z' '0'-'9']* as var {VAR (var) }
    | ['0'-'9']+ as num {INT(num)}
    | ['_'] {UNDERSCORE}
    | ['['] {LEFT_SQUARE}
    | [']'] {RIGHT_SQUARE}
    | [';'] {SEMICOLON}
    | ['|'] {VERTICAL_BAR}
    | eof { End_reached}
    | "(* |" {comment lexbuf}

and comment = parse
    | "*)" {token lexbuf}
    | eof { End_reached}
    | _ {comment lexbuf}

