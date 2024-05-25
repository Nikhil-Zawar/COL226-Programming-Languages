open A4_types
open A4_lexer
open A4_parser


let rec print_terms_list tlist = match tlist with
  | [] -> Printf.printf ""
  | VARIABLE hd :: tl -> Printf.printf "\t \t Variable : %s  \n" hd; print_terms_list tl;
  | IDENTIFIER hd::tl -> Printf.printf "\t \t Identifier : %s \n" hd; print_terms_list tl;
  | BINARY_OPERATOR hd::tl -> Printf.printf "\t \t Binary Operator : %s \n" hd; print_terms_list tl;
  | ATOM (a,b)::c -> Printf.printf "\t \t Predicate : %s \n" a; print_terms_list b; print_terms_list c; 
  | STRING_CONST hd :: tl -> Printf.printf "\t \t Numeral : %s  \n" hd; print_terms_list tl;
;;

let print_atomic_formula atmic = match atmic with
  | (a,tlist) -> Printf.printf "\t Predicate : %s \n" a ;
      print_terms_list tlist
;;

let rec print_atomic_formula_list atmic_list = match atmic_list with
  | [] -> Printf.printf "Printing atomic formula list completed -------------------------- \n"
  | hd::tl -> 
    print_atomic_formula hd ; 
    print_atomic_formula_list tl
;;


let rec print_clause clas = match clas with
  | FACT atmic -> print_atomic_formula atmic
  | RULE(atmic, atmic_list) -> 
    print_atomic_formula atmic;
    print_atomic_formula_list atmic_list
  ;;


let rec print_ast ast = match ast with
  | [] -> Printf.printf "Printing Complete ------------------------------------------------------------------\n"
  | head :: tail ->
    print_clause head;
    print_ast tail

let parse_error lexbuf =
  let curr = lexbuf.Lexing.lex_curr_p in
  let line = curr.Lexing.pos_lnum in
  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let tok = Lexing.lexeme lexbuf in
  Printf.eprintf "Syntax error at line %d, character %d, token '%s'\n" line cnum tok;
  exit 1
;;

let parse_with_error lexbuf =
  (* try *)
    A4_parser.main A4_lexer.token lexbuf
  (* with *)
  (* | A4_lexer.End_reached -> (A4_types.ParseError "Unexpected end of file") *)
;;

let parse_file  =
  try
    let file_name = Sys.argv.(1) in 
    Printf.eprintf "%s \n" file_name;
    let handling_file = open_in file_name in
    let lexbuf = Lexing.from_channel handling_file in
    Printf.eprintf "Printing Abstract Syntax Tree Starts --------------------------------------------- \n";
    let result_tree = parse_with_error lexbuf in
    close_in handling_file;
    print_ast result_tree
  with
  | A4_types.ParseError msg ->
    Printf.eprintf "Parse Error: %s\n" msg;
    exit 1
  | Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
;;

parse_file;;
