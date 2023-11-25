open BtreeS
open Bst
open Random

(*Création d'un arbre d'une taille size contenant comme valeur maximale maxValue*)
let rec bst_rnd_create_aux(t, size, maxValue : 'a t_tree * int * int) : 'a t_tree =
  if (size = 0)
  then t
  else
    let rd : int = Random.int(maxValue) in
    bst_rnd_create_aux(bst_linsert(t, rd), size - 1, maxValue)
;;

let bst_rnd_create(size, maxValue : int * int) : 'a t_tree =
  bst_rnd_create_aux(tree_empty(), size, maxValue)
;;

(*Calcul d'un déséquilibre d'un arbre*)
let bst_imbalance(t : 'a t_tree) : int =
  height(tree_subleft(t)) - height(tree_subright(t))
;;

let rec bst_sum_imbalance(t : 'a t_tree) : int =
  if (tree_isempty(t))
  then 0
  else bst_imbalance(t) + bst_sum_imbalance(tree_subleft(t)) + bst_sum_imbalance(tree_subright(t))
;;

let bst_avg_imbalance(nb, size, maxValue : int * int * int) : float =
  let imb : float ref = ref 0. in

  for i = 0 to nb - 1 do
    let abr : 'a t_tree = bst_rnd_create(size, maxValue) in 
    imb := !imb +. float_of_int(bst_sum_imbalance(abr)) /. float_of_int(size);
  done;

  (!imb /. float_of_int(nb))
;;

(*Génération d'une liste croissante*)
let rec generate_aux(list, size, limit, array : 'a list * int * int * bool array) : ('a list * bool array) =
  if size = 0
  then (List.sort (fun a b -> compare a b) list, array)
  else
    let rd : int = Random.int(limit) + 1 in
  
    if array.(rd)
    then
      generate_aux(list, size, limit, array)
    else begin
      array.(rd) <- true;
      generate_aux(rd::list, size - 1, limit, array)
    end
;;

let generateList(size, limit, array : int * int * bool array) : ('a list * bool array) =
  generate_aux([], size, limit, array)
;;

(*Génération d'une liste avec des sous suites de taille croissante, décroissante, aléatoire ou fixe contenant des entiers croissants*)
let generateListWithSubList(nbSousSuite, ordre : int * string) : 'a list =
  let maxSizeSuite : int ref = ref 10 in

  let tmp : int ref = ref (Random.int(3)) in
  let list : 'a list ref = ref [] in

  let maxValue : int = 999 in
  let unique : bool array ref = ref (Array.make (maxValue + 1) false) in

  for i = 0 to nbSousSuite - 1 do
    match ordre with
    | "aleatoire" ->
      let size : int = Random.int(!maxSizeSuite) in
      let (l, arr) : ('a list * bool array) = generateList(size, maxValue, !unique) in
      
      unique := arr;
      list := !list @ l

    | "croissant" ->
      tmp := !tmp + 1;

      let size : int = !tmp in
      let (l, arr) : ('a list * bool array) = generateList(size, maxValue, !unique) in

      unique := arr;
      list := !list @ l

    | "decroissant" ->
      let size : int = nbSousSuite - i in
      let (l, arr) : ('a list * bool array) = generateList(size, maxValue, !unique) in

      unique := arr;
      list := !list @ l

    | "fixe" ->
      let (l, arr) : ('a list * bool array) = generateList(nbSousSuite, maxValue, !unique) in
      
      unique := arr;
      list := !list @ l

    | _ -> Printf.printf "Mauvais ordre spécifié\n"
  done;

  !list
;;

let rec createBstByList(tree, list, size : 'a t_tree * 'a list * int) : ('a t_tree * int) =
  if list = []
  then (tree, size)
  else createBstByList(bst_linsert(tree, List.hd(list)), List.tl(list), size + 1)
;;

let bst_avg_imbalance_V2(nbTrees, nbSousSuite, order : int * int * string) : float =
  let imb : float ref = ref 0. in

  for i = 0 to nbTrees - 1 do
    let list : 'a list  = generateListWithSubList(nbSousSuite, order) in
    let (abr, size) : ('a t_tree * int) = createBstByList(tree_empty(), list, 0) in

    imb := !imb +. float_of_int(bst_sum_imbalance(abr)) /. float_of_int(size);
  done;

  (!imb /. float_of_int(nbTrees))
;;

let test() =
  Random.self_init();
  
  bst_avg_imbalance_V2(100, 30, "aleatoire");; (*100 arbres, 30 sous-suites, ordre des sous-suites*)
;;

test();;
