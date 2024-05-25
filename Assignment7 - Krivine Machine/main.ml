type exp = 
  | Number of int
  | Boolean of bool
  | Variable of string
  | Plus of exp*exp | Times of exp*exp | Minus of exp*exp | Div of exp*exp
  | Mod of exp*exp | Power of exp*exp
  | And of exp*exp | Or of exp*exp
  | Eq of exp*exp | Gt of exp*exp | Lt of exp*exp
  | Gte of exp*exp | Lte of exp*exp
  | Not of exp
  | Abstract of string*exp
  | Apply of exp*exp
  | Ifthenelse of exp*exp*exp
;;

type closure = CLOS of exp*table
and table = (string*closure) list
;;

let add_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Number (n1+n2), [])
  | _ -> failwith "Addition failed\n"
;;

let mult_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Number (n1*n2), [])
  | _ -> failwith "Multiplication failed\n"
;;

let sub_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Number (n1-n2), [])
  | _ -> failwith "Subtraction failed\n"
;;

let div_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Number (n1/n2), [])
  | _ -> failwith "Division failed\n"
;;

let mod_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Number (n1 mod n2), [])
  | _ -> failwith "Mod function failed\n"
;;

let rec power_fun a b = match (a,b) with
  | (_,0) -> 1
  | (aa,1) -> aa
  | (aa, bb) -> power_fun (aa*aa) (bb-1)
;;

let exponent_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Number (power_fun n1 n2), [])
  | _ -> failwith "Exponent function failed\n"
;;

let and_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Boolean b1, t1),CLOS (Boolean b2, t2)) -> CLOS (Boolean (b1 && b2), [])
  | _ -> failwith "And operation failed\n"
;;

let or_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Boolean b1, t1),CLOS (Boolean b2, t2)) -> CLOS (Boolean (b1 || b2), [])
  | _ -> failwith "Or operation failed\n"
;;

let eq_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Boolean (n1=n2), [])
  | _ -> failwith "Equality operation failed\n"
;;

let gt_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Boolean (n1>n2), [])
  | _ -> failwith "Greater than operation failed\n"
;;

let lt_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Boolean (n1<n2), [])
  | _ -> failwith "Less than operation failed\n"
;;

let gte_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Boolean (n1>=n2), [])
  | _ -> failwith "Greater than or equal to operation failed\n"
;;

let lte_closures(c1, c2) = match (c1, c2) with 
  | (CLOS (Number n1, t1),CLOS (Number n2, t2)) -> CLOS (Boolean (n1<=n2), [])
  | _ -> failwith "Less than or equal to operation failed\n"
;;

let rec find_exp_by_string key lst =
  match lst with
  | [] -> failwith "Key not found in the list"
  | (s, e) :: rest -> if s = key then e else find_exp_by_string key rest
;;

let rec kriv_mach cls stk = match (cls,stk) with
  | (CLOS (Number n, g1), s) -> CLOS (Number n, g1)
  | (CLOS (Boolean b, g1), s) -> CLOS (Boolean b, g1)
  | (CLOS (Plus(n1, n2),g1),s) -> kriv_mach (add_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Minus(n1, n2),g1),s) -> kriv_mach (sub_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Times(n1, n2),g1),s) -> kriv_mach (mult_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  
  | (CLOS (Div(n1, n2),g1),s) -> kriv_mach (div_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Mod(n1, n2),g1),s) -> kriv_mach (mod_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Power(n1, n2),g1),s) -> kriv_mach (exponent_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (And(n1, n2),g1),s) -> kriv_mach (and_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Or(n1, n2),g1),s) -> kriv_mach (or_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Gt(n1, n2),g1),s) -> kriv_mach (gt_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Lt(n1, n2),g1),s) -> kriv_mach (lt_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Gte(n1, n2),g1),s) -> kriv_mach (gte_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Lte(n1, n2),g1),s) -> kriv_mach (lte_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Eq(n1, n2),g1),s) -> kriv_mach (eq_closures(
                                                        (kriv_mach (CLOS (n1,g1)) []),
                                                        (kriv_mach (CLOS (n2,g1)) []) )) s
  | (CLOS (Variable x, g1),s) -> kriv_mach (find_exp_by_string x g1) s
  | (CLOS (Abstract(x,e1), g1), c1::s) -> kriv_mach (CLOS (e1, (x,c1)::g1)) s
  | (CLOS (Apply(e1,e2), g1)), s -> kriv_mach (CLOS (e1,g1)) ((CLOS (e2,g1))::s)
  (* | (CLOS (Ifthenelse(e0,e1,e2), g1)), s -> kriv_mach (
    match (kriv_mach (CLOS (e0,g1)) []) with
    CLOS (Boolean True, _) -> CLOS (e1, g1)
    | CLOS (Boolean False, _) -> CLOS (e2, g1)
  ) s *)
  | _ -> failwith "Error in the krivine machine(Note that if else not made)\n"
;;

let rec print_answer cls = match cls with
  | CLOS (Number n,_) -> Printf.printf "The answer is a integer : %d \n" n 
  | CLOS (Boolean b, _) -> if (b = true) then Printf.printf "The answer is a boolean : True \n" else Printf.printf "The answer is a boolean : False \n"
  | _ -> failwith "No correct answer"
;; 

(* TEST CASE 1 *)
(* e1 = 12*3 *)
let e1 = Times(Number 12, Number 3);;
let c1 = CLOS (e1, []);;
let k1 = kriv_mach c1 [];;
print_answer k1;;

(* TEST CASE 2 *)
(* e2 = (5=2+3)&&((7-2)>4) *)
let e2 = And(  
  Eq(Number 5, Plus(Number 2, Number 3)), 
  Gt(Minus(Number 7, Number 2), Number 4)
  ) ;;
let c2 = CLOS (e2, []);;
let k2 = kriv_mach c2 [];;
print_answer k2;;

(* TEST CASE 3 *)
(* e3 = x+5, where x=y+10, where y = 5 *)
let e3 = Apply(Abstract("x", Plus(Variable "x", Number 5)), Plus(Variable "y", Number 10));;
let initial_gamma_table = [("y", CLOS (Number 5,[]))];;
let c3 = CLOS (e3, initial_gamma_table);;
let k3 = kriv_mach c3 [];;
print_answer k3;;

(* TEST CASE 4 *)
(* e4 =x+y, where y=12*3 ,where x = 10+5 *)
let e4 = Apply(Abstract("x", Apply (Abstract("y", Plus(Variable "x", Variable "y")), Times(Number 12, Number 3))), Plus(Number 10, Number 5));;
let initial_gamma_table = [];;
let c4 = CLOS (e4, initial_gamma_table);;
let k4 = kriv_mach c4 [];;
print_answer k4;;

(* TEST CASE 5 *)
(* e5 = x*x where x=2+3 *)
let e5 = Apply(Abstract("x", Times(Variable "x", Variable "x")), Plus(Number 2, Number 3));;
let initial_gamma_table = [];;
let c5 = CLOS (e5, initial_gamma_table);;
let k5 = kriv_mach c5 [];;
print_answer k5;;

(* TEST CASE 6 *)
(* e6 = (4*(x+3)) where x=y where y is given gamma table *)
let e6 = Times(Number 4, Apply(Abstract("x", Plus(Variable "x",Number 3)), Variable "y"))
let initial_gamma_table = [("y", CLOS (Number 3,[]))];;
let c6 = CLOS (e6, initial_gamma_table);;
let k6 = kriv_mach c6 [];;
print_answer k6;;
