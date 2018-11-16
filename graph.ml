exception Cycle
type mark = NotVisited | InProgress | Visited



type 'a graph =
    { mutable g_nodes : 'a node list }

and 'a node =
{
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}


(* Outils Débuggage :*)


let rec print_list = function
    | [] -> ()
    | e::l -> print_string e ; print_string " " ; print_list l


let rec print_nodes = function
| e::l when e.n_mark = NotVisited -> print_string "NotVisited" ; print_string " " ; print_nodes l
| e::l when e.n_mark = Visited    -> print_string "Visited"    ; print_string " " ; print_nodes l
| e::l when e.n_mark = InProgress -> print_string "InProgress" ; print_string " " ; print_nodes l
| _ -> ()

(* Fin Outils Débuggage*)



let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

  let add_edge2 g id2 id1 =
    let n1 = node_for_label g id1 in
    let n2 = node_for_label g id2 in
    n1.n_link_to <- n2::n1.n_link_to;
    n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
    let truth = ref false in
    let rec study_cycle elems=
      match elems with
      | [] -> ()
      |a::q when a.n_mark = NotVisited -> a.n_mark <- InProgress; study_cycle a.n_link_to ; a.n_mark <- Visited ; study_cycle q
      |a::q when a.n_mark = InProgress -> truth := true
      |a::q -> study_cycle  q
    in
    study_cycle g.g_nodes;
    !truth


let rec reset nodlist =
match nodlist with
| []   -> ()
| a::q -> a.n_mark <- NotVisited;reset q


let topological g =
  if has_cycle g then failwith "ayyy lmao your graph has cycles";
  reset g.g_nodes;
  let tri_topo = ref [] in

  let rec tryons elems=
    match elems with
    | [] -> ()
    | a::q when a.n_mark = NotVisited -> a.n_mark <- InProgress; tryons a.n_link_to ; a.n_mark <- Visited; tri_topo := a.n_label::!tri_topo ; tryons q
    | a::q -> tryons  q
  in

  tryons g.g_nodes;
  !tri_topo
(*
-----------DEBUG---------------

let () = let g = mk_graph() in   add_node g "1"; add_node g "21"; add_node g "22"; add_node g "333";
  add_edge g "1" "21"; add_edge g "1" "22";
  add_edge g "21" "333"; add_edge g "22" "333"; List.iter (print_string) (topological g)


*)
