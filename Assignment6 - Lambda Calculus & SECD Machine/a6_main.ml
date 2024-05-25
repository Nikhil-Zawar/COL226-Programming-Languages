type var = string;;
type exp = Variable of var | Number of int | Boolean of bool |
  Arith_Plus of exp*exp | Arith_Times of exp*exp | Arith_Minus of exp*exp | Arith_Divide of exp*exp |
  Logical_And of exp*exp | Logical_Or of exp*exp | Greater of exp*exp | Equality of exp*exp | Logical_Not of exp |
  Conditional_ifte of exp*exp*exp | Abstract of var*exp | Apply of exp*exp 
;;

type opcode = LOAD_NUM of int | LOAD_BOOL of bool 
  | ARITH_PLUS | ARITH_TIMES | ARITH_MINUS | ARITH_DIVIDE 
  | LOGICAL_AND | LOGICAL_OR | GREATER | EQUALITY
  | LOGICAL_NOT | CONDITIONAL_IFTE of opcode list*opcode list
  | FIND_VARIABLE of var | RETURN | MAKE_CLOSURE of var*opcode list | APPLY 
;;

let rec compile e = match e with
  | Variable x -> [FIND_VARIABLE x]
  | Number n -> [LOAD_NUM n]
  | Boolean b -> [LOAD_BOOL b]
  | Arith_Plus (e1,e2) -> (compile e1) @ (compile e2) @ [ARITH_PLUS]
  | Arith_Times (e1,e2) -> (compile e1) @ (compile e2) @ [ARITH_TIMES]
  | Arith_Minus (e1,e2) -> (compile e1) @ (compile e2) @ [ARITH_MINUS]
  | Arith_Divide (e1,e2) -> (compile e1) @ (compile e2) @ [ARITH_DIVIDE]
  | Logical_And (e1,e2) -> (compile e1) @ (compile e2) @ [LOGICAL_AND]
  | Logical_Or (e1,e2) -> (compile e1) @ (compile e2) @ [LOGICAL_OR]
  | Greater (e1,e2) -> (compile e1) @ (compile e2) @ [GREATER]
  | Equality (e1,e2) -> (compile e1) @ (compile e2) @ [EQUALITY]
  | Logical_Not e1 -> (compile e1) @ [LOGICAL_NOT]
  | Conditional_ifte (e1,e2,e3) -> (compile e1)@[CONDITIONAL_IFTE ((compile e2), (compile e3))]
  | Abstract (x, e1) -> [MAKE_CLOSURE (x, (compile e1)@[RETURN])]
  | Apply (e1,e2) -> (compile e1)@(compile e2)@[APPLY]
;;

let rec find_exp_by_string key lst =
  match lst with
  | [] -> failwith "Key not found in the list"
  | (s, e) :: rest -> if s = key then e else find_exp_by_string key rest

type com_code = opcode list;;

type answers = 
  | Num of int
  | Bool of bool 
  | Closure of var*com_code*table
and table = (string*answers) list ;;

type stack_type = answers list;;
type table_type = table;;
type compile_type = com_code;;
type dump_type = (answers list * table * com_code) list ;;

exception Stuck of (stack_type * table_type * compile_type * dump_type);;

let rec stkmc (s,g,c,d) = match (s,g,c,d) with
  | (hd::_,g1,[],d1) -> hd 
  | (s1, g1, (LOAD_NUM n)::c1, d1) -> stkmc ((Num n)::s1, g1, c1, d1)
  | (s1, g1, (LOAD_BOOL b)::c1, d1) -> stkmc ((Bool b)::s1, g1, c1, d1)
  | ((Num n1)::(Num n2)::s1, g1, (ARITH_PLUS)::c1, d1) -> stkmc((Num (n1+n2))::s1, g1, c1, d1)
  | ((Num n1)::(Num n2)::s1, g1, (ARITH_TIMES)::c1, d1) -> stkmc((Num (n1*n2))::s1, g1, c1, d1)
  | ((Num n1)::(Num n2)::s1, g1, (ARITH_MINUS)::c1, d1) -> stkmc((Num (n1-n2))::s1, g1, c1, d1)
  | ((Num n1)::(Num n2)::s1, g1, (ARITH_DIVIDE)::c1, d1) -> stkmc((Num (n1/n2))::s1, g1, c1, d1)
  | ((Bool b1)::(Bool b2)::s1, g1, (LOGICAL_AND)::c1, d1) -> stkmc((Bool (b1 && b2))::s1, g1, c1, d1)
  | ((Bool b1)::(Bool b2)::s1, g1, (LOGICAL_OR)::c1, d1) -> stkmc((Bool (b1 || b2))::s1, g1, c1, d1)
  | ((Num n1)::(Num n2)::s1, g1, (GREATER)::c1, d1) -> stkmc((Bool (n1>n2))::s1, g1, c1, d1)
  | ((Num n1)::(Num n2)::s1, g1, (EQUALITY)::c1, d1) -> stkmc((Bool (n1=n2))::s1, g1, c1, d1)
  | ((Bool b1)::s1, g1, (LOGICAL_NOT)::c1, d1) -> stkmc ((Bool (not b1))::s1, g1, c1, d1)
  | ((Bool true)::s1, g1, (CONDITIONAL_IFTE(c1,c2))::c3, d1) -> stkmc (s1, g1, (c1 @ c3), d1)
  | ((Bool false)::s1, g1, (CONDITIONAL_IFTE(c1,c2))::c3, d1) -> stkmc (s1, g1, (c2 @ c3), d1)
  | (s1, g1, (FIND_VARIABLE x)::c1, d1) -> stkmc ((find_exp_by_string x g1)::s1, g1, c1, d1)
  | ((Num n1)::s1, g1, (RETURN)::c1, (s2, g2, c2)::d1) -> stkmc ((Num n1)::s2, g2, c2, d1)
  | ((Bool b1)::s1, g1, (RETURN)::c1, (s2, g2, c2)::d1) -> stkmc ((Bool b1)::s2, g2, c2, d1)
  | ((Num n1)::(Closure (x,c1,g1))::s2, g2, (APPLY)::c2, d1) -> stkmc ([], g1@[(x,Num n1)], c1, (s2,g2,c2)::d1)
  | ((Bool b1)::(Closure (x,c1,g1))::s2, g2, (APPLY)::c2, d1) -> stkmc ([], g1@[(x,Bool b1)], c1, (s2,g2,c2)::d1)
  | (s1,g1,(MAKE_CLOSURE(x,c1))::c2, d1) -> stkmc((Closure (x,c1,g1)::s1), g1, c2, d1)
  | (_,_,_,_) -> raise (Stuck (s, g, c, d)) 
;;

let rec print_com_code comm_code = match comm_code with
  | [] -> Printf.printf "\nCompleted printing the compiled code. \n"
  | (LOAD_NUM n)::tl -> (Printf.printf ", INT %d " n) ; print_com_code tl
  | (LOAD_BOOL b)::tl -> (Printf.printf ", BOOL %B " b) ; print_com_code tl
  | (ARITH_PLUS)::tl -> (Printf.printf ", PLUS" ) ; print_com_code tl
  | (ARITH_TIMES)::tl -> (Printf.printf ", TIMES" ) ; print_com_code tl
  | (ARITH_MINUS)::tl -> (Printf.printf ", MINUS" ) ; print_com_code tl
  | (ARITH_DIVIDE)::tl -> (Printf.printf ", DIVIDE" ) ; print_com_code tl
  | (LOGICAL_AND)::tl -> (Printf.printf ", AND" ) ; print_com_code tl
  | (LOGICAL_OR)::tl -> (Printf.printf ", OR" ) ; print_com_code tl
  | (EQUALITY)::tl -> (Printf.printf ", EQ" ) ; print_com_code tl
  | (GREATER)::tl -> (Printf.printf ", GT" ) ; print_com_code tl
  | (LOGICAL_NOT)::tl -> (Printf.printf ", NOT" ) ; print_com_code tl
  | (CONDITIONAL_IFTE  (c1,c2))::tl -> Printf.printf ", IF_THEN_ELSE"; print_com_code c1; print_com_code c2
  | (MAKE_CLOSURE (x, c1))::tl -> Printf.printf ", MAKE_CLOSURE : "; print_string x; print_com_code c1 ; print_com_code tl
  | (RETURN)::tl -> Printf.printf ", RETURN"; print_com_code tl
  | (APPLY)::tl -> Printf.printf ", APPLICATION"; print_com_code tl
  | (FIND_VARIABLE x)::tl -> Printf.printf ", LOOKUP VARIABLE %s" x; print_com_code tl 
;;

let print_answer val_ans = match val_ans with
  | Num n -> Printf.printf "The answer is a integer : %d \n\n" n
  | Bool b -> Printf.printf "The answer is a boolean : %B \n\n" b
  | Closure (v,c,t) -> Printf.printf "The answer is of invalid type, the top element of the stack is a closure"
;;

(* TEST CASE 1 *)
(* e1 = 12*3 *)
let e1 = Arith_Times(Number 12, Number 3);;
let stack = [];;
let gamma_table = [];;
let compiled_code = compile e1;;
let dump = [];;
let answer_value = stkmc (stack, gamma_table, compiled_code, dump);;
Printf.printf "\nCompiled code is as follows : \n";;
print_com_code compiled_code;;
Printf.printf "\nAnswer is as follows : \n";;
print_answer answer_value;;

(* TEST CASE 2 *)
(* e2 = (5=2+3)&&((7-2)>4) *)
let e2 = Logical_And(  
  Equality(Number 5, Arith_Plus(Number 2, Number 3)), 
  Greater(Arith_Minus(Number 7, Number 2), Number 4)
  ) ;;
let stack = [];;
let gamma_table = [];;
let compiled_code = compile e2;;
let dump = [];;
let answer_value = stkmc (stack, gamma_table, compiled_code, dump);;
Printf.printf "\nCompiled code is as follows : \n";;
print_com_code compiled_code;;
Printf.printf "\nAnswer is as follows : \n";;
print_answer answer_value;;


(* TEST CASE 3 *)
(* e3 = x+5, where x=y+10, where y = 5 *)
let e3 = Apply(Abstract("x", Arith_Plus(Variable "x", Number 5)), Arith_Plus(Variable "y", Number 10));;
let stack = [];;
let gamma_table = [("y",Num 5)];;
let compiled_code = compile e3;;
let dump = [];;
let answer_value = stkmc (stack, gamma_table, compiled_code, dump);;
Printf.printf "\nCompiled code is as follows : \n";;
print_com_code compiled_code;;
Printf.printf "\nAnswer is as follows : \n";;
print_answer answer_value;;

(* TEST CASE 4 *)
(* e4 =x+y, where y=12*3 ,where x = 10+5 *)
let e5 = Apply(Abstract("x", Apply (Abstract("y", Arith_Plus(Variable "x", Variable "y")), Arith_Times(Number 12, Number 3))), Arith_Plus(Number 10, Number 5));;
let stack = [];;
let gamma_table = [];;
let compiled_code = compile e5;;
let dump = [];;
let answer_value = stkmc (stack, gamma_table, compiled_code, dump);;
Printf.printf "\nCompiled code is as follows : \n";;
print_com_code compiled_code;;
Printf.printf "\nAnswer is as follows : \n";;
print_answer answer_value;;

(* TEST CASE 5 *)
(* e5 = x*x where w=2+3 *)
let e5 = Apply(Abstract("x", Arith_Times(Variable "x", Variable "x")), Arith_Plus(Number 2, Number 3));;
let stack = [];;
let gamma_table = [];;
let compiled_code = compile e5;;
let dump = [];;
let answer_value = stkmc (stack, gamma_table, compiled_code, dump);;
Printf.printf "\nCompiled code is as follows : \n";;
print_com_code compiled_code;;
Printf.printf "\nAnswer is as follows : \n";;
print_answer answer_value;;

(* TEST CASE 6 *)
(* e6 = (4*(x+3)) where x=y where y is given gamma table *)
let e6 = Arith_Times(Number 4, Apply(Abstract("x", Arith_Plus(Variable "x",Number 3)), Variable "y"))
let stack = [];;
let gamma_table = [("y",Num 3)];;
let compiled_code = compile e6;;
let dump = [];;
let answer_value = stkmc (stack, gamma_table, compiled_code, dump);;
Printf.printf "\nCompiled code is as follows : \n";;
print_com_code compiled_code;;
Printf.printf "\nAnswer is as follows : \n";;
print_answer answer_value;;

(* TEST CASE 7 *)
(* e7 =  if x>5 then 10+15 else 20+15, where x = 3+7*)
let e7 = Apply(Abstract("x", Conditional_ifte(Greater(Variable "x", Number 5), Arith_Plus(Number 10, Number 15), Arith_Plus(Number 20, Number 15))), Arith_Plus(Number 3, Number 7));;
let stack = [];;
let gamma_table = [];;
let compiled_code = compile e7;;
let dump = [];;
let answer_value = stkmc (stack, gamma_table, compiled_code, dump);;
Printf.printf "\nCompiled code is as follows : \n";;
print_com_code compiled_code;;
Printf.printf "\nAnswer is as follows : \n";;
print_answer answer_value;;

