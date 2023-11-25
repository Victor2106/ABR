type 'a t_tree = T_EMPTY | T_ROOTING of ('a * 'a t_tree * 'a t_tree) ;;

let tree_empty() : 'a t_tree =
  T_EMPTY
;;

let tree_isempty(t : 'a t_tree) : bool =
  match t with
    T_EMPTY -> true
  | T_ROOTING(_,_,_) -> false
;;

let tree_rooting(v, t1, t2 : 'a * 'a t_tree * 'a t_tree ) : 'a t_tree =
  T_ROOTING(v, t1, t2)
;;

let tree_root(t : 'a t_tree) : 'a =
  match t with
    T_EMPTY -> failwith "error : empty"
  | T_ROOTING(n,_,_) -> n
;;

let tree_subleft(t : 'a t_tree) : 'a t_tree =
  match t with
    T_EMPTY -> failwith "error : empty"
  | T_ROOTING(_,t1,_) -> t1
;;

let tree_subright(t : 'a t_tree) : 'a t_tree =
  match t with
    T_EMPTY -> failwith "error : empty"
  | T_ROOTING(_,_,t1) -> t1
;;

let rec t_findval(t, v : 'a t_tree * 'a) : bool =
  match t with
    T_EMPTY -> false
  | T_ROOTING(n, t1, t2) ->
     if n = v
     then true
     else
       if v >= n
       then t_findval(t2, v)
       else t_findval(t1, v)
;;
