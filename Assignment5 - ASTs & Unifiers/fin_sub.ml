open List;;

type symbol = string * int ;;
type signature = symbol list;;
exception Fail
type tree = N of int | V of string | C of {node : symbol; children : tree list} ;;

let x = V "x";;
let y = V "y";;
let z = V "z";;

let zero = N 0;;
let one = N 1;;

let plus_zero_one = C {node = ("+",2); children = [zero; one]};;
let times_one_x = C {node = ("*",2); children = [one; x]};;
let plus_zero_y = C {node = ("+",2); children = [zero; y]};;
let plus_timesonex_pluszeroy = C {node = ("+",2); children = [times_one_x; plus_zero_y]};;
let plus_timesonex_z = C {node = ("+",2); children = [times_one_x; z]};;

let rec print_tree tr = match tr with
  | N n -> print_string ("\nNumeral: " ^ string_of_int n)
  | V v -> print_string ("\nVariable: " ^ v)
  | C { node = (sym,arity); children = child_list } ->
    begin
      match child_list with
      | [] -> ()
      | child :: rest ->
        print_string ("\nSymbol: " ^ sym ^ " [");
        print_tree child;
        List.iter (fun ch -> print_string "; "; print_tree ch) rest;
        print_string "]\n"
    end
;;

let rec print_sigma sgm = 
  match sgm with
   | [] -> Printf.printf  "\n"
   | (sym,tree)::tail -> Printf.printf "\n [  Variable : %s ;" sym;
   print_tree tree; print_string " ]  \n"; print_sigma tail 
;;

let rec len lst = match lst with
   [] -> 0
  | x::t -> 1 + len t
;;

let check_sig sg = 
  let rec check_sig_internal sgn visited = match sgn with
  | [] -> true
  | (sym,arity)::tail -> if arity <0 then false
  else if List.mem sym visited then false
  else check_sig_internal tail (sym::visited)
in
check_sig_internal sg [] 
;;

let rec member sym arity signature = match signature with
| [] -> false
| (s,a)::rest_sgn -> if (s=sym && a=arity) then true else member sym arity rest_sgn
;;

let rec wftree signature tree = match tree with
  | N n -> true
  | V b -> true
  | C {node = (sym,arity); children = children} -> 
    if ( member sym arity signature && arity = List.length children) then
      fold_left (&&) true (List.map ( wftree signature) children)
    else false
;;

let rec ht t = match t with
  V _ -> 0
  | N _ -> 0
  | C r -> let (s,n) = r.node in (if n=0 then 0 else 1+(fold_left max 0 (map ht r.children)))
;;

let rec size t = match t with
  V _ -> 1
  | N  _ -> 1
  | C r -> 1 + (fold_left (+) 0 (map size r.children) )
;;

let rec vars tree =
  let rec vars_helper res tree =
    match tree with
    | N n -> List.append res [string_of_int n]
    | V v -> List.append res [v]
    | C { node = _; children } -> List.fold_left vars_helper res children
  in
vars_helper [] tree
;;

let mirror tree = 
  let mirtree = tree in
  let rec mirror_helper mirtree = match mirtree with
  | V v -> V v
  | N n -> N n
  | C {node = (s,n); children = child_list} ->
    let reversed_children = List.map mirror_helper (List.rev child_list) in
    C { node = (s, n); children = reversed_children }
  in
  mirror_helper mirtree
;;

let id_sub x = V x ;;

(* The following subst function is used when sigma is a function mapped from variable to tree *)
(* let rec subst sigma t = match t with
 | V x -> sigma x
 | N n -> N n
 | C r -> C {node= r.node; children = map (subst sigma) r.children}
;; *)

let sigma111 v = match v with
 "x" -> one
 | "y" -> plus_timesonex_pluszeroy
 | _ -> V v
;;

(* The following subst function is used when sigma is a collection of pairs of type string*tree *)
let rec subst sigma t_tree =
  match t_tree with
  | V x -> (try List.assoc x sigma with Not_found -> t_tree)
  | N n -> N n
  | C { node; children } -> C { node; children = List.map (subst sigma) children }
;;

let compose_subst sigma1 sigma2 =
  List.map (fun (x, t) -> (x, subst sigma2 t)) (sigma1@sigma2)
;;

let rec is_element_in_list x lst = match lst with
  | [] -> false   
  | hd :: tl -> if hd = x then true else is_element_in_list x tl

(* sigma is a list of pairs type = string*tree *)
(* if we assume the trees are well formed then we can remove this line  List.length children1 <> List.length children2*)
let rec mgu t_tree u_tree =
  match t_tree, u_tree with
  | V x, V y -> if x = y then [] else [(x, u_tree)]

  | V x, C { node = (sym, arity) ; children = clist } ->
    if is_element_in_list x (vars u_tree) then raise Fail
    else [(x, u_tree)]

  | C { node = _; _ }, V x ->mgu u_tree t_tree

  | C { node = (sym1, art1); children = children1 }, C { node = (sym2, art2); children = children2 } ->
    if sym1 <> sym2 || List.length children1 <> List.length children2 then
     ( Printf.printf "CASE 1 MGU Does not exist\n" ; raise Fail)
    else
      let rec unify_pairs sigma_unified t_children u_children = match t_children, u_children with
        | [], [] -> sigma_unified
        (* Here we have to recurisivily call on each of the children the same mgu function *)
        | t_head::t_tail, u_head::u_tail ->
          let s' = mgu (subst sigma_unified t_head) (subst sigma_unified u_head) in
          unify_pairs (compose_subst s' sigma_unified) t_tail u_tail
        | _ -> Printf.printf "CASE 2 MGU Does not exist\n" ; raise Fail
      in
      unify_pairs [] children1 children2
  | _ -> []
;;


(* TEST CASES *)

(* For check_sig function*)
(* let sgn1 = [("+",2); ("1",0); ("-",2)];;
let sgn2 = [("+",2); ("1",0); ("+",3)];;
let sgn3 = [("+",2); ("1",0); ("-",-3)];;
let bool_checksig1 = check_sig sgn1;;
let bool_checksig2 = check_sig sgn2;;
let bool_checksig3 = check_sig sgn3;;
print_string (string_of_bool bool_checksig1 ^ "\n") ;;
print_string (string_of_bool bool_checksig2 ^ "\n") ;;
print_string (string_of_bool bool_checksig3 ^ "\n") ;; *)

(* For wftree function *)
(* let sign1 = [("+",2); ("not",1); ("*",2)];;
let sign2 = [("+",2); ("not",1); ("*",3)];;
let sign3 = [("+",2); ("not",1); ("-",2)];;
let bool_wftree1 = wftree sign1 plus_timesonex_pluszeroy;;
let bool_wftree2 = wftree sign2 plus_timesonex_pluszeroy;;
let bool_wftree3 = wftree sign3 plus_timesonex_pluszeroy;;
print_string (string_of_bool bool_wftree1 ^ "\n");;
print_string (string_of_bool bool_wftree2 ^ "\n");;
print_string (string_of_bool bool_wftree3 ^ "\n");; *)

(* For mirror function *)
(* let revtree1 = mirror plus_timesonex_pluszeroy;;
let revtree2 = mirror plus_timesonex_z;;
print_tree revtree1;;
print_tree revtree2;; *)


(* let tree1 = C {node = ("*", 2); children = [V "h"; N 42] };;
let tree2 = C {node = ("*", 2); children = [ C {node = ("/", 2); children = [N 51; V "k"]} ; N 23] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";; *)



(* 
Test case 1:- 

Test case when both the trees are same Variables

let tree1 = V "abc";;
let tree2 = V "abc";;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;

ANSWER:-
The mgu of tree1 and tree2 is : 
The substituted tree1 is using mgu as sigma : 
Variable: abc
The substituted tree2 is using mgu as sigma : 
Variable: abc

*)

(* 
Test case 2:- 

Test case when both trees are different variables

let tree1 = V "abc";;
let tree2 = V "pqr";;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";; 

ANSWER:
The mgu of tree1 and tree2 is : 
 [  Variable : abc ;
Variable: pqr ]  

The substituted tree1 is using mgu as sigma : 
Variable: pqr
The substituted tree2 is using mgu as sigma : 
Variable: pqr
*)

(* 
Test case 3:- 
One of the tree is variable and other is not
Occurs-check is false, the variable is not in the other tree

let tree1 = V "x";;
let tree2 = C {node = ("+", 2); children = [N 1; N 6] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;

ANSWER:-
The mgu of tree1 and tree2 is : 
 [  Variable : x ;
Symbol: + [
Numeral: 1; 
Numeral: 6]
 ]  

The substituted tree1 is using mgu as sigma : 
Symbol: + [
Numeral: 1; 
Numeral: 6]

The substituted tree2 is using mgu as sigma : 
Symbol: + [
Numeral: 1; 
Numeral: 6]
*)

(* 
Test case 4:-
One of the tree is variable and other is not
Occurs-check is true, the variable is present in the other tree

let tree1 = V "a";;
let tree2 = C {node = ("and", 2); children = [V "a"; V "b"] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;

ANSWER : 
Fatal error: exception A5_main.Fail

*)

(* 
Test case 5:- 
Both the expressions are trees, but the root is different

let tree1 = C {node = ("-", 2); children = [N 42; N 69] };;
let tree2 = C {node = ("+", 2); children = [N 68; N 41] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;

MGU Does not exist
Fatal error: exception A5_main.Fail
*)

(* 
Test case 6:- 
Both the trees are exactly the same

let tree1 = C {node = ("+", 2); children = [N 68; N 41] };;
let tree2 = C {node = ("+", 2); children = [N 68; N 41] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;

The mgu of tree1 and tree2 is : 
The substituted tree1 is using mgu as sigma : 
Symbol: + [
Numeral: 68; 
Numeral: 41]

The substituted tree2 is using mgu as sigma : 
Symbol: + [
Numeral: 68; 
Numeral: 41]
*)

(* 
Test case 7:- 
Both are trees with same root

let tree1 = C {node = ("+", 2); children = [V "a"; N 61] };;
let tree2 = C {node = ("+", 2); children = [ C {node = ("-", 2); children = [N 12; V "b"]} ; N 6] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;


The mgu of tree1 and tree2 is : 
 [  Variable : a ;
Symbol: - [
Numeral: 12; 
Variable: b]
 ]  

The substituted tree1 is using mgu as sigma : 
Symbol: + [
Symbol: - [
Numeral: 12; 
Variable: b]
; 
Numeral: 61]

The substituted tree2 is using mgu as sigma : 
Symbol: + [
Symbol: - [
Numeral: 12; 
Variable: b]
; 
Numeral: 6]

*)

(* 
Test case 8:- 
Arbitrary well formed trees

let tree1 = C {node = ("*", 2); children = [V "h"; N 42] };;
let tree2 = C {node = ("*", 2); children = [ C {node = ("/", 2); children = [N 51; V "k"]} ; N 23] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;

ANSWER :-

The mgu of tree1 and tree2 is : 
 [  Variable : h ;
Symbol: / [
Numeral: 51; 
Variable: k]
 ]  

The substituted tree1 is using mgu as sigma : 
Symbol: * [
Symbol: / [
Numeral: 51; 
Variable: k]
; 
Numeral: 42]

The substituted tree2 is using mgu as sigma : 
Symbol: * [
Symbol: / [
Numeral: 51; 
Variable: k]
; 
Numeral: 23]

*)

(* TEST CASE 9
Possibly a edge case

let tree1 = C { node = ("f",2); children = [V "x"; V "y"] }
let tree2 = C { node = ("f",2); children = [V "y"; C { node = ("g",1); children = [V "x"] }] };;
let mgu_sigma = mgu tree1 tree2;;
Printf.printf "The mgu of tree1 and tree2 is : " ;; 
print_sigma mgu_sigma;;
let tree1_substituted = subst mgu_sigma tree1;;
let tree2_substituted = subst mgu_sigma tree2;;
Printf.printf "The substituted tree1 is using mgu as sigma : ";;
print_tree tree1_substituted;;
Printf.printf "\n";;
Printf.printf "The substituted tree2 is using mgu as sigma : ";;
print_tree tree2_substituted;;
Printf.printf "\n";;

ANSWER :-
Fatal error: exception A5_main.Fail 
*)


let sig1 = [("0", 0); ("1", 0); ("0", 1)];;
let sig2 = [("0", 0); ("1", 0); ("+", 2)];;
let t = C {node = ("+", 2); children = [(V "x"); (V "y"); (V "z")]} ;;
let t2 = C {node = ("+", 2); children = [(V "x"); (V "y")]} ;;
let t3 = C {node = ("+", 2); children = [(V "z"); t2]} ;;

let ans1 = check_sig sig1;;
print_string (string_of_bool ans1 ^ "\n") ;;

let ans2 = wftree sig2 t;;
print_string (string_of_bool ans2 ^ "\n");;

let ans3 = ht t2;;
print_string (string_of_int ans3 ^ "\n" );;

let ans4 = size t2;;
print_string (string_of_int ans4 ^ "\n");;

print_tree t3;;
let ans5 = mirror t3;;
print_tree ans5;;