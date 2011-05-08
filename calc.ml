type data = int
type op = data list -> data
type arbre = 
    | Data of data
    | Op of op * (arbre list)

(* Si f n'est pas une opération algébrique (par ex - ou /), neutre est le neutre de la loi associée
(par ex + ou * ). Dans ce cas, f neutre x prend l'inverse de x pour la loi associée *)
let make_op f neutre = function
                        | [] -> neutre
                        | [x] -> f neutre x
                        | x::xs -> List.fold_left f x xs

let sum = make_op ( + ) 0
let diff = make_op ( - ) 0
let prod = make_op ( * ) 1
let div = make_op ( / ) 1

let rec parse = parser
    | [< '( ' ' | '\n' | '\t' ); rest >] -> parse rest
    | [< a = parse_data >] -> Data a
    | [< ''('; f = parse_func; args = parse_args [] >] -> Op (f, args)

and parse_data = parser
    | [< chiffre = parse_chiffre ; rest >] -> parse_num chiffre rest
and parse_chiffre = parser
    | [< ' ('0'..'9') as chiffre >]-> (int_of_char chiffre) - (int_of_char '0')
and parse_num n = parser
    | [< k = parse_chiffre; rest >] -> parse_num (n*10 + k) rest
    | [< >] -> n

and parse_func = parser
    | [< '( ' ' | '\n' | '\t' ); rest >] -> parse_func rest
    | [< ''+' >] -> sum
    | [< ''-' >] -> diff
    | [< ''*' >] -> prod
    | [< ''/' >] -> div
and parse_args l = parser
    | [< '( ' ' | '\n' | '\t' ); rest >] -> parse_args l rest
    | [< '')' >] -> List.rev l
    | [< a = parse ; rest >] -> parse_args (a::l) rest


let rec eval = function
    | Data x -> x
    | Op (f, args) -> f (List.map eval args)

let () =
    let input = Stream.of_channel stdin in
    let arbre_sortie = parse input in
    print_int (eval arbre_sortie); print_newline ()
