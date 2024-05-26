{
type token =
    | Integer of int
    | Next_line
    | Arithematic_Op of string
    | Boolean_Op of string
    | Boolean_Const of string
    | Logical_Op of string
    | Keyword of string
    | Identifier of string
    | Brackets of string
    | Comma of string
    | String_Const of string
    | String_Op of string
    | Error of string
    | Property_of_object of string
    | End_command of string
exception End_reached
exception Not_found 
}

rule token = parse
(* let acc = "" in  *)
    | [' ' '\t' ] { token lexbuf }
    | ['\n']          {Next_line}
    | ['.'] as argument {Property_of_object(String.make 1 argument)}
    | ['0' - '9'] + as argument {Integer(int_of_string argument)}
    | "+" | "-" | "/" | "%" | "**" | "*" as argument {Arithematic_Op ("" ^ argument )}
    | "=" | ">" | "<" |"==" | ">=" | "<=" |"!=" as argument { Boolean_Op ("" ^ argument) }
    | "&&" | "||"  as argument {Logical_Op ("" ^ argument)}
    | "true" | "false" as argument {Boolean_Const ("" ^ argument)}
    | "if" | "then" | "else" | "func_def" | "is_there" | "int" |"while"|"for"| "double" | "return" | "elsif" | "size" | "scan_input" | "line_break" as argument {Keyword ("" ^ argument)}
    | ['(' '{' '}' '[' ']' ')'] as argument { Brackets (String.make 1 argument)}
    | "./" as argument { End_command ("" ^ argument)}
    | [','] as argument { Comma (String.make 1 argument)}
    | ['a' - 'z' '_'] ['a' - 'z' '0' - '9' '_' '\'']* as argument { Identifier argument }
    | ['"'] ('"' | [^'"']* as str) '"' {  String_Const str  }
    | '"' {Error ("String length undetermined")}
    | ['0' - '9'] ['a' - 'z' 'A' - 'Z' '_' '\'']* as str { Error str }
    | ['1' - '9' 'A' - 'Z'] ['0' - '9' 'a' - 'z' 'A' - 'Z' '_' '\'']* as str { Error str }
    | "concat" | "contains" | "ends_with" | "starts_with" as argument { String_Op ("" ^ argument) }
    | _  {raise Not_found}
    | eof { raise End_reached }

{
let tokeniser () = begin
    try
        let file_name = Sys.argv.(1) in 
        let handling_file = open_in file_name in
        let lexbuf = Lexing.from_channel handling_file in
        while true do
            let result = token lexbuf in
            match result with
            | Next_line -> Printf.printf "";
            | Property_of_object(i) ->Printf.printf "\"%s\"\t : Property function of the object \n" i;
            | Arithematic_Op(i) -> Printf.printf "%s \t : Arithematic Operator \n" i;
            | Integer(i) -> Printf.printf "%d \t : integer\n" i;
            | Boolean_Op(i) -> Printf.printf "%s \t : Boolean Operator \n" i;
            | Boolean_Const(i) -> Printf.printf "%s \t : Boolean Constant \n" i;
            | Logical_Op(i) -> Printf.printf "%s \t : Logical Operator \n" i;
            | Keyword(i) -> Printf.printf "%s \t : Keyword \n" i;
            | Identifier(i) -> Printf.printf "%s \t : Identifier \n" i;
            | Brackets(i)  -> Printf.printf "%s \t : Parenthesis\n"  i;
            | Comma(i) -> Printf.printf "\"%s\"\t : Comma\n" i;
            | End_command(i) -> Printf.printf "%s \t : Command Ended\n" i;
            | String_Const(i) -> Printf.printf "\"%s\" \t : String\n" i;
            | String_Op(i) -> Printf.printf "%s\t : String Operator\n" i;
            | Error(i) -> Printf.printf "%s \t : Error\n" i;
        done
    with 
    | End_reached -> exit 0
    | Not_found -> Printf.printf "Token not defined in the syntax\n";
end;;
tokeniser();; 
}



(* c = "Hello World"
print(c)
if c.size == 5 then return 5 else return 6
concat("Hello", "World")
123a
function_definition newfun(a, b) { return a + b ./ }
if contains("hello world", "hello") || starts_with("foo", "f") then true else false
function_definition(a, b) {
    if a > b then
        return a ./
    else
        return b ./
}
if 123a then true else false
if concat(123, "abc") == "123abc" then
    scan_input() ./
else
    is_there(3) ./
c = "HELLO$$%%@@#"
let aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = 10./
let max_int_value = 2147483647./
"my nae
if a1 + b2 * c3 >= d4 / e5 then "hello, world!" else false./

&&&
##%@##$@$@#%32 *)