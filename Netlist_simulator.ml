open Netlist_ast
open Netlist
open Scheduler
open Helping_funs


exception Unable_to_cast_operands_boolop
(*Notre Environment, càd ou on stock les variables*)

let env = Hashtbl.create 30
let ram = Hashtbl.create 30
let ram_stack = Stack.create()
let rom = Hashtbl.create 30
let reg = ref (Hashtbl.create 30)

(*J'ajouterai une fonction insère ram/reg plus tard, pour l'instant, on initialise TOUT a false*)


(* Opérations D'executions d'équations*)
(* On supppose que les variables sont toutes initialisées *)

let extract_arg = function
    | Avar   x -> Hashtbl.find env x
    | Aconst y -> y




let calc eq =
  let calc_expr = function
  | Earg a                     -> extract_arg a

  | Enot a                     -> begin
                                 match extract_arg a with
                                 | VBit      b   -> VBit      (not b)
                                 | VBitArray tab -> VBitArray (Array.map (fun x -> (not x)) tab)
                                 end
  | Ereg     (a)               -> Hashtbl.find !reg a

  | Eslice   (i,j,a)           -> begin
                                match extract_arg a with
                                | VBit      b   -> VBit b
                                | VBitArray tab -> VBitArray ( Array.sub tab i (j - i + 1))
                                end


  | Eselect  (i,a)             -> begin
                                match extract_arg a with
                                | VBit      b   ->  VBit b
                                | VBitArray tab ->  VBit tab.(i)
                                end


  | Ebinop   (op,a,b)         -> begin
                                match extract_arg a, extract_arg b with
                                | VBit      va,  VBit      vb                                         ->  VBit (calc_binop op va vb)
                                | VBitArray ta,  VBitArray tb  when Array.length ta = Array.length tb ->  VBitArray (Array.map2 (calc_binop op) ta tb)
                                | _ -> raise (Unable_to_cast_operands_boolop)
                                end

  | Econcat  (a,b)            -> begin
                                match extract_arg a, extract_arg b with
                                | VBit b1 , VBit b2 ->
                            			VBitArray [| b1 ; b2 |]
                            		| VBit b1 , VBitArray t2 ->
                            			VBitArray ( Array.append [| b1|] t2 )
                            		| VBitArray t1 , VBit b2 ->
                            			VBitArray ( Array.append t1 [|b2|] )
                            		| VBitArray t1 , VBitArray t2 ->
                            			VBitArray ( Array.append t1 t2)
                            		end

  | Emux     (a1,a2,a3)       -> begin
                                match extract_arg a1 , extract_arg a2, extract_arg a3 with
                                | VBit v1      , VBit v2      , VBit v3      -> VBit      (calc_mux v1 v2 v3)
                                | VBitArray tm , VBitArray t1 , VBitArray t2
                              			when ((Array.length tm) = (Array.length t1)&& (Array.length t1) = (Array.length t2) ) ->
                              			let aux i tmi =calc_mux tmi t1.(i) t2.(i) in
                              			VBitArray (Array.mapi aux tm)
                              	| _ ->  failwith "Type_Error_length"
                            		end

  | Erom     (_,_,a)         -> begin
                                 match extract_arg a with
                                 |VBit      v -> (try
                                                  Hashtbl.find rom (int_of_bool   v)
                                                  with _ -> VBit false)
                                 |VBitArray v -> (try
                                                Hashtbl.find rom (int_of_barray v)
                                                with _ -> VBitArray (Array.make (Array.length v) false))
                                end


  | Eram  (_,n,a1,a2,a3,a4)     -> begin
                                  (match extract_arg a2  with
                                 |VBit true -> ()
                                 |VBitArray (v) when v.(0) = true  -> ()
                                 |_ -> Stack.push (extract_arg a3,extract_arg a4) ram_stack)
                                ;
                                try
                                 (
                                match extract_arg a1 with
                                 |VBit      v -> Hashtbl.find ram (int_of_bool   v)
                                 |VBitArray v -> Hashtbl.find ram (int_of_barray v)
                                 )
                                 with _ ->  (match extract_arg a1 with
                                            |VBit      v -> Hashtbl.replace ram (int_of_bool   v) (VBitArray (Array.make n false))
                                            |VBitArray v -> Hashtbl.replace ram (int_of_barray v) (VBitArray (Array.make n false))
                                            ); (VBitArray (Array.make n false))
                                 end

        in

      (*print_endline (fst eq);
        print_endline (string_of_bool(extract_val (calc_expr (snd eq)) ) ); *)

        Hashtbl.replace env (fst eq) (calc_expr (snd eq))


(*
Coder initialisation reg, ram rom
*)

let maj_ram =
    while not(Stack.is_empty ram_stack) do
    begin
    let adr,data = Stack.pop ram_stack in
    match adr with
    |VBit      v -> Hashtbl.replace ram (int_of_bool v) data
    |VBitArray v -> Hashtbl.replace ram (int_of_barray v) data
    end
    done

let ini nom =
    Hashtbl.add env nom (VBit false);
    Hashtbl.add !reg nom (VBit false)

let interpretons p nb =
    print_endline("Le nombre d'itérations est :" ^ string_of_int(nb) );
    List.iter ini        (List.map (fst) (Env.bindings p.p_vars));
    for i =1 to nb
    do
    print_endline("Itération n° :" ^string_of_int(i));
    List.iter (get env p.p_vars)    p.p_inputs;
    List.iter calc       p.p_eqs;
    print_endline ("Résultats:");
    List.iter (print_res env)  p.p_outputs;
    reg := (Hashtbl.copy env);
    maj_ram
    done;

(*--------------------------- Old ass functions----------------------------*)

(*--------------------NE MARCHE PAS POUR VBITARRAY -----------------------

*)

(* Ancienne Fonction Get, ne sert plus a grande chose mais bon

let rec get nom = print_string (" "^nom^" ? :");
                  let a = Pervasives.read_int() in
                  if (a = 0 || a = 1)
                  then
                  (Hashtbl.add env nom (VBit (bool_of_int a)))
                  else
                  begin
                  print_string(error_wrong_entry);
                  (get nom)
                  end

*)
(*
let string_of_array vect n =
    let a =  String.make n '0' in
    let aux i b =
        let carac = char_of_int (int_of_bool b) in
        a.[i] <- carac
    in
    Array.iteri aux vect;
    a
*)


(*RAM ROM A IDENTIFIER? QQCHOSE ?
PB AVEC RAM.NET*)
