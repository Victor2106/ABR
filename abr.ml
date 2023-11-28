open BtreeS
open Bst
open Random

let rangeRandom(min, max : int * int) : int =
  min + Random.int(max - min + 1)
;;

(*Question 1*)
let rec bst_rnd_create_aux(t, size, maxValue, array : 'a t_tree * int * int * bool array) : 'a t_tree =
  if size = 0
  then t
  else
    let rd : int = rangeRandom(1, maxValue) in
  
    if array.(rd)
    then
      bst_rnd_create_aux(t, size, maxValue, array)
    else begin
      array.(rd) <- true;
      bst_rnd_create_aux(bst_linsert(t, rd), size - 1, maxValue, array)
    end
;;

let bst_rnd_create(size, maxValue : int * int) : 'a t_tree =
  let unique : bool array ref = ref (Array.make (maxValue + 1) false) in
  bst_rnd_create_aux(tree_empty(), size, maxValue, !unique)
;;

(*Question 2*)
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

(*Question 3*)

(*Génération d'une liste croissante sans doublon*)
let rec generate_aux(list, size, limit, array : 'a list * int * int * bool array) : ('a list * bool array) =
  if size = 0
  then (List.sort compare list, array)
  else
    let rd : int = rangeRandom(1, limit) in
  
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

(*Génration d'une liste avec sous-suites fixes*)
let generateWithFixSubList(nbSubList, sizeSubList, maxValue : int * int * int) : 'a list =
  let list : 'a list ref = ref [] in
  let unique : bool array ref = ref (Array.make (maxValue + 1) false) in

  for i = 0 to nbSubList - 1
  do
    let (l, arr) : ('a list * bool array) = generateList(sizeSubList, maxValue, !unique) in
      
      unique := arr;
      list := !list @ l
  done;

  !list
;;

(*Génration d'une liste avec sous-suites aléatoires*)
let generateWithRandomSubList(nbSubList, maxValue : int * int) : 'a list =
  let list : 'a list ref = ref [] in
  let unique : bool array ref = ref (Array.make (maxValue + 1) false) in

  for i = 0 to nbSubList - 1
  do
    let rand = rangeRandom(1, 100) in
    let (l, arr) : ('a list * bool array) = generateList(rand, maxValue, !unique) in
      
    unique := arr;
    list := !list @ l
  done;

  !list
;;

(*Génration d'une liste avec sous-suites croissantes*)
let generateWithIncreaseSubList(borneLength, nbSubList, maxValue : int * int * int) : 'a list =
  let list : 'a list ref = ref [] in
  let unique : bool array ref = ref (Array.make (maxValue + 1) false) in
  let currentSize : int ref = ref 0 in
  let i : int ref = ref 0 in

  while (!i <= nbSubList)
  do
    let size : int ref = ref (Random.int(borneLength) + 1) in
    if (!size >= !currentSize)
    then 
      (
        let (l, arr) : ('a list * bool array) = generateList(!size, maxValue, !unique) in
        unique := arr;
        list := !list @ l;
        i:= !i + 1;
      );

     currentSize := !size;
  done;

  !list
;;

(*Génration d'une liste avec sous-suites décroissantes*)
let generateWithDecreasingSubList(borneLength, nbSubList, maxValue : int * int * int) : 'a list =
  let list : 'a list ref = ref [] in
  let unique : bool array ref = ref (Array.make (maxValue + 1) false) in
  let currentSize : int ref = ref 0 in
  let i : int ref = ref 0 in

  while (!i <= nbSubList)
  do
    let size : int ref = ref (Random.int(borneLength) + 1) in
    if (!size <= !currentSize)
    then 
      (
        let (l, arr) : ('a list * bool array) = generateList(!size, maxValue, !unique) in
        unique := arr;
        list := !list @ l;
        i:= !i + 1;
      );

     currentSize := !size;
  done;

  !list
;;

(*Génration d'un arbre à partir d'une liste*)
let rec createBstByList(tree, list, size : 'a t_tree * 'a list * int) : ('a t_tree * int) =
  if list = []
  then (tree, size)
  else createBstByList(bst_linsert(tree, List.hd(list)), List.tl(list), size + 1)
;;

(*Calcul du déséquilibre moyen d'un arbre avec des sous-suites croissantes*)
let bst_avg_imbalance_increase(nbTrees, borneLength, nbSubList, maxValue : int * int * int * int) : float =
  Random.self_init();

  let imb : float ref = ref 0. in
  for i = 0 to nbTrees - 1
  do
    let list : 'a list  = generateWithIncreaseSubList(borneLength, nbSubList, maxValue) in
    let (abr, size) : ('a t_tree * int) = createBstByList(tree_empty(), list, 0) in

    imb := !imb +. float_of_int(bst_sum_imbalance(abr)) /. float_of_int(size);
  done;

  (!imb /. float_of_int(nbTrees))
;;

(*Calcul du déséquilibre moyen d'un arbre avec des sous-suites décroissantes*)
let bst_avg_imbalance_decreasing(nbTrees, borneLength, nbSubList, maxValue : int * int * int * int) : float =
  Random.self_init();

  let imb : float ref = ref 0. in
  for i = 0 to nbTrees - 1
  do
    let list : 'a list  = generateWithDecreasingSubList(borneLength, nbSubList, maxValue) in
    let (abr, size) : ('a t_tree * int) = createBstByList(tree_empty(), list, 0) in

    imb := !imb +. float_of_int(bst_sum_imbalance(abr)) /. float_of_int(size);
  done;

  (!imb /. float_of_int(nbTrees))
;;

(*Calcul du déséquilibre moyen d'un arbre avec des sous-suites aléatoires*)
let bst_avg_imbalance_random(nbTrees, nbSubList, maxValue : int * int * int) : float =
  Random.self_init();

  let imb : float ref = ref 0. in
  for i = 0 to nbTrees - 1
  do
    let list : 'a list  = generateWithRandomSubList(nbSubList, maxValue) in
    let (abr, size) : ('a t_tree * int) = createBstByList(tree_empty(), list, 0) in

    imb := !imb +. float_of_int(bst_sum_imbalance(abr)) /. float_of_int(size);
  done;

  (!imb /. float_of_int(nbTrees))
;;

(*Calcul du déséquilibre moyen d'un arbre avec des sous-suites fixes*)
let bst_avg_imbalance_fixe(nbTrees, nbSubList, sizeSubList, maxValue : int * int * int * int) : float =
  Random.self_init();

  let imb : float ref = ref 0. in
  for i = 0 to nbTrees - 1
  do
    let list : 'a list  = generateWithFixSubList(nbSubList, sizeSubList, maxValue) in
    let (abr, size) : ('a t_tree * int) = createBstByList(tree_empty(), list, 0) in

    imb := !imb +. float_of_int(bst_sum_imbalance(abr)) /. float_of_int(size);
  done;

  (!imb /. float_of_int(nbTrees))
;;

(*let test() =

  (*Quelques exemples*)
  (*Sous-suites décroissantes :*)
  (*Exemple : *)

  let imbalance_decreasing : float = bst_avg_imbalance_decreasing(1000, 50, 40, 2000) in (*1000 arbres, 25 sous-suites, valeurs comprises entre 1 et 999*)
  print_float(imbalance_decreasing);;
  
  (*Sous-suites croissantes :*)
  (*Exemple : *)
  (*let imbalance_increase : float = bst_avg_imbalance_increase(1000, 50, 40, 2000) in (*1000 arbres, 25 sous-suites, valeurs comprises entre 1 et 999*)
  print_float(imbalance_increase)*)

  (*Sous-suites aléatoires :*)
  (*Exemple : *)
  (*let imbalance_random : float = bst_avg_imbalance_random(1000, 25, 999) in (*1000 arbres, 25 sous-suites, valeurs comprises entre 1 et 999*)
  print_float(imbalance_random);;*)

  (*Sous-suites fixes :*)
  (*Exemple : *)
  (*let imbalance_fix : float = bst_avg_imbalance_fix(1000, 25, 999) in (*1000 arbres, 25 sous-suites, valeurs comprises entre 1 et 999*)
  print_float(imbalance_fix);;*)
;;

test();;*)