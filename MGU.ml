(* variable = string and symbol  = string*int *)

type symbol = string*int;;
type variable = string;;
type term = V of variable | Node of symbol*(term list);;
type signature = symbol list;;


let rec length l = match l with
    [ ] -> 0
  | x :: xs -> 1 + (length xs)
;;

let rec isFound v l =
  match l with 
    [] -> false;
  | (v1,a) :: t -> if (v1 = v) then true else isFound v t;;

let rec check_sig s = 
  match s with
    [] -> true
  | (v,a) :: t -> if(isFound v t || a < 0) then false else check_sig t;; 

let rec foldl f e l = match l with
    [ ] -> e
  | x :: xs -> foldl f (f e x) xs
;;

let rec wfterm pt = match pt with
    V x -> true;
  | Node ((a,b),l) -> foldl (fun x y -> ((wfterm y) && x)) (b = length l) l;;

let rec map f l =
  match l with
    [] -> []
  | x :: xs -> f x :: map f xs
 
let rec ht pt = match pt with
    V x -> 0
  | Node ((a,b),l) -> 1 + foldl (fun x y -> max x (ht y)) 0 l;;

let rec size pt = match pt with
    V x -> 1
  | Node ((a,b),l) -> 1 + foldl (fun x y -> x+ (size y)) 0 l;;

let rec member x s =
  match s with
    [] -> false
  | h::t -> if(x = h) then true else member x t;;

let rec union s1 s2 =
  match s1 with
    []-> s2
  | x::xs -> if(member x s2 = false) then union xs (x::s2) else union xs s2;;

let rec vars pt = match pt with
    V x -> x::[]
  | Node ((a,b),l) -> foldl (fun x y -> union x (vars y)) [] l;;

let rec subst pt sigma = match pt with
    V x -> if (Hashtbl.mem sigma x = true) then Hashtbl.find sigma x else (V x) 
  | Node ((a,b),l) -> Node ((a,b),map (fun x -> subst x sigma) l)
;;


let kliesli sigma1 sigma2 pt = subst (subst pt sigma1) sigma2;;

exception NOT_UNIFIABLE;;

let rec foldl2 f a l1 l2 = match l1,l2 with
    [],[] -> a
    
  | h1::t1,h2::t2 -> foldl2 f (f a h1 h2) t1 t2
  | _,_ -> raise NOT_UNIFIABLE;;


let rec hasVariable v l =
  match l with
    [] -> false
  | h::t -> if h=v then true else hasVariable v t;;


let printTerm term =
  match term with
  | V x -> Printf.printf "%s \n" x 
  | Node((a,b), l) -> Printf.printf "%s \n" a;;


(*let term1 = Node(("A",2),[(Node(("Add",2),[V "X";V "Y"])); (Node(("Sub",2),[Node(("A",0),[]); V "Y"]))]);;*)

let mgu t1 t2 = 
  let sigma = Hashtbl.create 10000 in
  let rec mguutil sigma t1 t2 =
    match t1, t2 with
    | V x, V y -> if x != y then Hashtbl.add sigma x (V y); sigma
    | V x, Node((a,0),l) -> Hashtbl.add sigma x (Node((a,0),l)); sigma
    | Node ((a,0),l), V y -> Hashtbl.add sigma y (Node((a,0),l)); sigma
    | V x, Node((a,b),l) -> if hasVariable x (vars (Node((a,b),l))) then raise NOT_UNIFIABLE else Hashtbl.add sigma x (Node((a,b),l)); sigma
    | Node((a,b),l), V y -> if hasVariable y (vars (Node((a,b),l))) then raise NOT_UNIFIABLE else Hashtbl.add sigma y (Node((a,b),l)); sigma
    | Node((a,0),l1), Node((b,0),l2) -> if a = b then sigma else raise NOT_UNIFIABLE 
    | Node((a,b),l1), Node((p,q),l2) -> if a = p && b = q then foldl2 (fun x y1 y2 -> mguutil x (subst y1 sigma) (subst y2 sigma)) sigma l1 l2 else raise NOT_UNIFIABLE in
  mguutil sigma t1 t2;;

(*
let c = Node(("B",0),[]);;
let term1 = Node(("A",2),[Node(("D",2),[c;Node(("D",2),[V "X";c])]);Node(("C",1),[c])]);;
let term3 = Node(("A",2),[c;c]);;
let term2 = Node(("A",2),[Node(("D",2),[c;Node(("D",2),[ Node (("A",2),[V "Z"]);c])]);Node(("C",1),[V "Z"])]);; 
let term4 = Node(("A",2),[Node(("B",1),[V "X"]); Node(("C",1),[V "X"])]);;
let term5 = Node(("A",2),[Node(("B",1),[V "P"]); Node(("C",1),[V "Q"])]);;
let sigma1 = mgu term4 term5;;
let sigma2 = mgu term5 term4;;
Hashtbl.iter (fun x y -> Print f.printf "%s ->" x; (printTerm y)) sigma1;;
Hashtbl.iter (fun x y -> Printf.printf "%s ->" x; (printTerm y)) sigma2;;
(subst term1 sigma1) = (subst term2 sigma1);;
subst term1 sigma2 = subst term2 sigma2;; *)