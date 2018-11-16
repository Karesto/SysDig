open Netlist_ast
open Netlist
open Scheduler





let general_error     = "Entrée invalide, veuillez réessayer."
let error_wrong_entry = "Entrée invalide. Entrez uniquement des bits 1 ou 0. Veuillez réessayer :"
let error_taille i    = "Entrée invalide. Entrez une succession de "^ string_of_int i ^" bits. Veuillez réessayer."




let bool_of_int i = if i=1 then true else false
let int_of_bool x =  if x then 1 else 0


let extract_val = function
    |VBit b -> b
    |_-> true

let int_of_barray t=
    Array.fold_right(fun b n -> (2*n) + (if b then 1 else 0)) t 0

let calc_binop op x1 x2=
    match op with
    |And  -> x1&&x2
    |Or   -> x1||x2
    |Xor  -> (x1||x2)&& not(x1&&x2)
    |Nand -> not(x1 && x2)




let calc_mux a b c = if a  then c else b


exception Not_Valid_Argument of int

let bitarray_of_string s n =
    let a =  Array.make n false in
    let aux i c =
        let c2 = int_of_string(Char.escaped c) in
        if (c2 = 0 || c2 = 1) then
        a.(i) <- bool_of_int(c2)
        else raise (Not_Valid_Argument i)
    in
    String.iteri aux s;
    VBitArray a







let string_of_array vect n =

    Array.fold_left (fun a x -> string_of_int(int_of_bool x) ^ a) "" vect








let rec get_bit env nom =

    try
    let a = int_of_string (read_line()) in
    if (a = 0 || a = 1) then
    (Hashtbl.replace env nom (VBit (bool_of_int a)))
    else begin
    print_endline (error_wrong_entry);
    (get_bit env nom)
    end
    with _ ->
    begin
    print_endline (general_error);
    get_bit env nom end



let rec get_array env nom n =
    try
    (
        let a = read_line() in
        let long  = String.length a   in
        if  long <> n then ( print_endline (error_taille n); get_array env nom n )
        else begin
            try
                (let res = bitarray_of_string a n in

                Hashtbl.replace env nom res)
            with
                |Not_Valid_Argument _ -> (print_endline(error_wrong_entry);get_array env nom n)
                | _                   -> (print_endline(general_error)    ;get_array env nom n)
        end
    )
    with _   ->   begin    print_endline (error_wrong_entry); get_array env nom n  end

let get env var_type nom =

    match Env.find nom var_type with
    |TBit -> print_endline ("Entrez la valeur de la variable  "^nom^"  : (un bit)") ;get_bit env nom
    |TBitArray n -> print_endline ("Entrez la valeur de la variable  "^nom^"  : ("^string_of_int(n) ^" bit)"); get_array env nom n


let print_res env a =
    match Hashtbl.find env a with
    |VBit b      -> print_endline(a ^ " = " ^string_of_int   (int_of_bool b )  )
    |VBitArray b -> print_endline(a ^ " = " ^(string_of_array b (Array.length b)) )
