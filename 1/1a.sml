type id = string;

datatype binop = Plus | Minux | Times | Div;

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list
     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp;

(********************)

type key = string;
datatype 'a tree = Leaf | Tree of 'a tree * key * 'a * 'a tree;

val empty = Leaf;

fun insert (key, v, Leaf) = Tree(Leaf, key, v, Leaf)
  | insert (key, value, Tree(l, k, v, r)) =
    if key < k
    then Tree(insert(key, value, l), k, v, r)
    else if key > k
    then Tree(l, k, v, insert(key, value, r))
    else Tree(l, key, value, r);

fun member (key, Leaf) = false
  | member (key, Tree(l, k, _, r)) =
    if key < k
    then member(key, l)
    else if key > k
    then member(key, r)
    else true

fun lookup (key, Leaf) = NONE
  | lookup (key, Tree(l, k, v, r)) =
    if key < k
    then lookup(key, l)
    else if key > k
    then lookup(key, r)
    else SOME v;

fun size Leaf = 0
  | size (Tree(l, k, v, r)) = 1 + size(l) + size(r);

fun left (Tree(l, _, _, _)) = l
fun right (Tree(_, _, _, r)) = r

(************)
val t1 = insert("t", 2, insert("s", 2, insert("b", 2, insert("f", 2, insert("i", 2, insert("p", 4, insert("s", 3, insert("t", 1, empty))))))));
val t2 = insert("h", 2, insert("g", 2, insert("f", 2, insert("e", 2, insert("d", 2, insert("c", 4, insert("b", 3, insert("a", 1, empty))))))));


(** d. まだ特徴がよくわからん。赤黒木で良い？ **)
