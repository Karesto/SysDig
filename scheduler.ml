open Netlist_ast
open Graph

(*
-----------------------------------------------------------------------------------------------------
Note: ça compile et donne un résultat, reste à vérifier que c'est le bon et réparer les fonctions lis_eq et detect_topo pour ne pas avoir de
concaténation, soit avec une récursivité mutuelle (pas sur que ça marche), soit avec une autre fonction qui lit pour savoir si c'est dedans (moche)




----------------------------------------------------------------
*)

exception Combinational_cycle


module Idset = Set.Make(struct type t = ident let compare = compare end)

let kes a set =
(* Ajoute le nom de la variable a au set si a en est une *)
      match a with
      |Avar x -> Idset.add x set
      |_ -> Idset.empty


let read_exp eq =
    match snd eq with
    (* Donne la liste des variables d'une equation en dans un set*)
    |Earg     (a)                         -> kes a Idset.empty
    |Enot     (a)                         -> kes a Idset.empty
    |Eslice   (_,_,a)                     -> kes a Idset.empty
    |Erom     (_,_,a)                     -> kes a Idset.empty
    |Eselect  (_,a)                       -> kes a Idset.empty
    |Ebinop   (_,a1,a2) |Econcat  (a1,a2) -> kes a2 (kes a1 Idset.empty)
    |Emux     (a1,a2,a3)                  -> kes a1 (kes a2 (kes a1 Idset.empty))
    |Eram     (_,_,a1,_,_,_)              -> kes a1 Idset.empty
    |Ereg     (_)                         -> Idset.empty



(* Vary: Renvoie les variables présentes dans une expression *)

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
    let  eqs = p.p_eqs in
    let  g = {g_nodes = []} in

    (*let look_expr a = List.find (fun elem -> a = (fst elem)) eqs in*)
    let dependance expr = Idset.iter (add_edge2 g (fst expr)) (read_exp expr) in
    List.iter (add_node g) (Idset.elements (List.fold_left  Idset.union   Idset.empty   (List.map vary eqs)));
    List.iter dependance eqs;

    if has_cycle g then raise Combinational_cycle
    else ( let tm = { p_eqs = detect_topo (topological g) eqs ; (* equations *)
          p_inputs = p.p_inputs; (* inputs *)
          p_outputs = p.p_outputs; (* outputs *)
          p_vars = p.p_vars; } in
    tm )
