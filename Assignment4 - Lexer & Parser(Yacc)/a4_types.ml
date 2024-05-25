exception ParseError of string

type predicate = string ;;

type term = 
  | VARIABLE of string
  | IDENTIFIER of string
  | BINARY_OPERATOR  of string
  | ATOM of predicate * term list
  | STRING_CONST of string
;;

type atomic_formula = predicate * term list;;

type clause =
  | FACT of atomic_formula
  | RULE of atomic_formula * (atomic_formula list)
;;
type program = clause list ;;

