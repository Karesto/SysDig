open Netlist_ast
open Graph


exception Combinational_cycle



module Idset = Set.Make(struct type t = ident let compare = compare end)

let kes a set = match a with
                |Avar x -> Idset.add x set
                |_ -> Idset.empty


let read_exp eq =

    let parcours = function
    |Earg (a) -> kes a Idset.empty
    |Enot (a) -> kes a Idset.empty
    |Ebinop (b,a1,a2)  -> kes a2 (kes a1 Idset.empty)
    |Emux  (a1,a2,a3)  -> kes a1 (kes a2 (kes a1 Idset.empty))
    |Econcat  (a1,a2)  -> kes a2 (kes a1 Idset.empty)
    |Eslice   (_,_,a)  -> kes a Idset.empty
    |Eselect  (_, a)   -> kes a Idset.empty
    |_ -> Idset.empty

    in
    parcours (snd eq)



let vary eq = Idset.add (fst eq) (read_exp eq)



let rec lis_eq eqs a =
    match eqs with
    | [] -> []
    | x::q when fst x = a -> [x]
    | x::q -> lis_eq q a

let rec detect_topo liste eqs =
    match  liste with
    | [] -> []
    |x::q -> (lis_eq eqs x )@ (detect_topo q eqs)

let schedule p =
    print_endline ("0");
    let  eqs = p.p_eqs in
    let  g = {g_nodes = []} in

    let look_expr a = List.find (fun elem -> a = (fst elem)) eqs in
    let dependance expr = Idset.iter (add_edge2 g (fst expr)) (read_exp expr) in
    print_endline ("1");
    List.iter (add_node g) (Idset.elements (List.fold_left  Idset.union   Idset.empty   (List.map vary eqs)));
    List.iter dependance eqs;
    print_endline ("1.5");

    if has_cycle g then raise Combinational_cycle
    else ( let tm = { p_eqs = detect_topo (topological g) eqs ; (* equations *)
          p_inputs = p.p_inputs; (* inputs *)
          p_outputs = p.p_outputs; (* outputs *)
          p_vars = p.p_vars; } in
    print_endline ("2"); tm )
