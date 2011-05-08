type data = int
type op = data list -> data
type arbre = 
    | Data of data
    | Op of op * (arbre list)

let rec sum = function
    | [] -> 0
    | x :: xs -> x + (sum xs);;

let diff args =
    let n = List.length args in 
    if n = 0 then failwith "Pas assez d'arguments"
    else if n = 1 then - (List.hd args)
    else List.fold_left (fun acc x -> acc-x) (List.hd args) (List.tl args);;

let rec prod = function
    | [] -> 1
    | x :: xs -> x * (prod xs);;

let div args = 
    let n = List.length args in
    if n = 0 then failwith "Pas assez d'arguments"
    else if n = 1 then 1 / (List.hd args)
    else List.fold_left (fun acc x -> acc/x) (List.hd args) (List.tl args);;

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
