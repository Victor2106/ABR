open BtreeS;;

let max(a,b : int * int) : int = 
  if a <= b 
  then b 
  else a
;;

let rec height(t : 'a t_tree) : int = 
  if (tree_isempty(t))
  then 0
  else 1 + max(height(tree_subleft(t)), height(tree_subright(t)))
;;

(*L'arbre est-il une feuille ?*)
let t_isleaf(t : 'a t_tree) : bool =
  height(t) = 1
;;

let t_isinnode(t : 'a t_tree) : bool =
  if t_isleaf(t)
  then false
  else height(tree_subright(t)) > 0 || height(tree_subleft(t)) > 0
;;

(*Calcul de la taille de l'arbre*)
let rec size(t : 'a t_tree) : int =
  if tree_isempty(t)
  then 0
  else
    let (l,r) : 'a t_tree * 'a t_tree =(tree_subleft(t),tree_subright(t)) in
    if t_isinnode(t)
    then 1 + size(l) + size(r)
    else 0
;;

let rec bst_seek(t, v : 'a t_tree * 'a) : bool =
  if(tree_isempty(t))
  then false
  else 
    let (n, fg, fd) : 'a * 'a t_tree * 'a t_tree = (tree_root(t), tree_subleft(t), tree_subright(t)) in
    if n = v
    then true
    else
      if v >= n
      then bst_seek(fd, v)
      else bst_seek(fg, v)
;;

(*Insérer un élément dans l'arbre*)
let rec bst_linsert(t, v : 'a t_tree * 'a) : 'a t_tree =
  if (tree_isempty(t))
  then tree_rooting(v, tree_empty(), tree_empty())
  else
    let (n, fg, fd) : 'a * 'a t_tree * 'a t_tree = (tree_root(t), tree_subleft(t), tree_subright(t)) in
    if v <= n
    then tree_rooting(n, bst_linsert(fg, v), fd)
    else tree_rooting(n, fg, bst_linsert(fd, v))
;;

(*Afficher l'arbre dans une chaine de caractères*)
let rec btree_to_string(t : 'a t_tree) : string =
  if tree_isempty(t)
  then "empty"
  else
    let (fg, fd) = (tree_subleft(t), tree_subright(t)) in
    "(" ^ string_of_int(tree_root(t)) ^ "," ^ btree_to_string(fg) ^ "," ^ btree_to_string(fd) ^ ")"
;;

let rec bst_lbuild_aux(l, t : 'a list * 'a t_tree) : 'a t_tree =
  if l == []
  then t
  else
    let n : 'a = List.hd(l) in
    let (new_t, new_l) : 'a t_tree * 'a list = (bst_linsert(t, n), List.tl(l)) in
    bst_lbuild_aux(new_l, new_t)
;;

(*Construire un arbre (en fait on utilise la fonction d'insérer un élément...)*)
let bst_lbuild(l : 'a list) : 'a t_tree =
  bst_lbuild_aux(l, tree_empty())
;;

let rec find_min (t : 'a t_tree) : 'a * 'a t_tree =
  if (tree_isempty(t))
  then failwith("Not_Found")
  else
    let (n, fg, fd) : 'a * 'a t_tree * 'a t_tree = (tree_root(t), tree_subleft(t), tree_subright(t)) in
    if (tree_isempty(fg))
    then (n, fd)
    else
      let (min, new_t) : 'a * 'a t_tree = find_min(fg) in
      (min, tree_rooting(n, new_t, fd))
;;

let rec bst_delete(t, x : 'a t_tree * 'a) : 'a t_tree =
  if(tree_isempty(t))
  then tree_empty()
  else
    let (n, fg, fd) : 'a * 'a t_tree * 'a t_tree = (tree_root(t), tree_subleft(t), tree_subright(t)) in
    if x < n
    then tree_rooting(n, bst_delete(fg, x), fd)
    else if x > n
    then tree_rooting(n, fg, bst_delete(fd, x))
    else
      if (tree_isempty(fg) && not(tree_isempty(fd)))
      then fd
      else
        if (tree_isempty(fd) && not(tree_isempty(fg)))
        then fg
        else
          let (min, new_t) : 'a * 'a t_tree = find_min(fd) in
          tree_rooting(min, fg, new_t)
;;
