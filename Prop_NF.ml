type prop = T
 | F
 | Letter of string
 | Not of prop
 | And of prop * prop
 | Or of prop * prop
 | Impl of prop * prop
 | Iff of prop * prop
;;

let rec nnfpos p = match p with
 T -> p
 | F -> p
 | Letter s -> p
 | Not p1 -> nnfneg p1
 | And (p1, p2) -> And (nnfpos p1, nnfpos p2)
 | Or (p1, p2) -> Or (nnfpos p1, nnfpos p2)
 | Impl (p1, p2) -> Or (nnfneg p1, nnfpos p2)
 | Iff (p1, p2) -> Or( And(nnfpos p1, nnfpos p2), And(nnfneg p1, nnfneg p2))
and
 nnfneg p = match p with
 T -> F
 | F -> T
 | Letter s -> Not( Letter s)
 | Not p1 -> nnfpos p1
 | And (p1, p2) -> Or (nnfneg p1, nnfneg p2)
 | Or (p1, p2) -> And (nnfneg p1, nnfneg p2)
 | Impl (p1, p2) -> And (nnfpos p1, nnfneg p2)
 | Iff (p1, p2) -> Or( And(nnfneg p1, nnfpos p2), And(nnfpos p1, nnfneg p2))
;;

let rec member x s =
	match s with
	[] -> false
	| h::t -> if(x = h) then true else member x t;;
	
let rec union s1 s2 =
	match s1 with
	[]-> s2
	| x::xs -> if(member x s2 = false) then union xs (x::s2) else union xs s2;;

let rec subset s1 s2 =
	match s1 with 
	[] -> true
	|h::t -> if (member h s2 = true) then subset t s2 else false;;   
	

let equal s1 s2 =
	if (subset s1 s2 && subset s2 s1) then true else false;;

(* Vars Of Preposition *)
(* Time complexity : O(n^2) , Space Complexity : O(n), n is size of variables *)
let rec vars p = match p with
 T -> []
 | F -> []
 | Letter s -> [s]
 | Not p1 -> vars p1
 | And (p1, p2) -> union(vars p1) (vars p2)
 | Or (p1, p2) -> union(vars p1) (vars p2)
 | Impl (p1, p2) -> union(vars p1) (vars p2)
 | Iff (p1, p2) -> union(vars p1) (vars p2)
;;

(* CNF Of Preposition *)
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)
let rec distributeOr p = 
  match p with
  | T -> T
  | F -> F
  | Letter s -> Letter s
  | And(p1,p2) -> And(distributeOr p1, distributeOr p2)
  | Or(And(p1,p2),p3) -> And(Or(distributeOr p1, distributeOr p3),Or(distributeOr p2, distributeOr p3))
  | Or(p1,And(p2,p3)) -> And(Or(distributeOr p1, distributeOr p2),Or(distributeOr p1, distributeOr p3))	
  | Not p1 -> Not (distributeOr p1)
  | Or(p1,p2) -> 
      let q1 = distributeOr(p1) and q2 = distributeOr(p2) in
      if p1=q1 && p2=q2 then Or(p1,p2) else distributeOr(Or(q1,q2))
  | _ -> p
;;

let rec cnf_list p = 
	let rec aux acc p = 
    match p with
	| And(p1,p2) -> aux acc p1 @ aux acc p2
	| Or(p1,p2) -> [List.concat ((aux acc p1)@ aux acc p2)]
    | T -> [T::acc]
    | F -> [F::acc]
    | Letter s -> [Letter s::acc]
    | Not p -> [Not p::acc] 
	| _ -> [p::acc] in
  aux [] p 
;;

let cnf p = cnf_list (distributeOr (nnfpos p));;

(* DNF Of Preposition *)
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)
let rec distributeAnd p = 
  match p with
  | T -> T
  | F -> F
  | Letter s -> Letter s
  | Or(p1,p2) -> Or(distributeAnd p1, distributeAnd p2)
  | And(Or(p1,p2),p3) -> Or(And(distributeAnd p1, distributeAnd p3),And(distributeAnd p2, distributeAnd p3))
  | And(p1,Or(p2,p3)) -> Or(And(distributeAnd p1, distributeAnd p2),And(distributeAnd p1, distributeAnd p3))	
  | Not p1 -> Not (distributeAnd p1)
  | And(p1,p2) -> 
      let q1 = distributeAnd(p1) and q2 = distributeAnd(p2) in
      if p1=q1 && p2=q2 then And(p1,p2) else distributeAnd(And(q1,q2))
  | _ -> p
;;


let rec dnf_list p = 
	let rec aux acc p = 
    match p with
	| Or(p1,p2) -> aux acc p1 @ aux acc p2
	| And(p1,p2) -> [List.concat ((aux acc p1)@ aux acc p2)]
    | T -> [T::acc]
    | F -> [F::acc]
    | Letter s -> [Letter s::acc]
    | Not p -> [Not p::acc] 
	| _ -> [p :: acc] in
  aux [] p 
;;

let dnf p = dnf_list (distributeAnd (nnfpos p));;

(* PART - 2 *)

let rec lookup x l =
 match l with
 [] -> F
 | (k,v) :: kvs -> if x=k then v else lookup x kvs
;;

let rec istrue p l = match p with
	T -> true
 | F -> false
 | Letter s -> if (lookup s l == F) then false else true
 | Not p1 -> not (istrue p1 l)
 | And (p1, p2) -> (istrue p1 l) && (istrue p2 l)
 | Or (p1, p2) -> (istrue p1 l) || (istrue p2 l)
 | Impl (p1, p2) -> (not (istrue p1 l)) || (istrue p2 l)
 | Iff (p1, p2) -> ((istrue p1 l) && (istrue p2 l)) ||
 (( not (istrue p1 l)) && (not (istrue p2 l)))
;;


let rec valuate p v l h =
	match v with 
	[] -> if istrue p l then Hashtbl.add h "true" l else Hashtbl.add h "false" l;
	| x :: xs -> valuate p xs ((x,T)::l) h;valuate p xs ((x,F)::l) h;;

let rec isequalValuation p1 p2 v l = 
	match v with
	[] -> if(istrue p1 l == istrue p2 l) then true else false
	| x :: xs -> isequalValuation p1 p2 xs ((x,T)::l) && isequalValuation p1 p2 xs ((x,F)::l);;
	
(* iSatisfiable p*)	
(* Time complexity : worst case : O(2^n) ,Space Complexity : O(2^n) where n is size of vars *)
let isSatisfiable p = 
	let my_hash = Hashtbl.create 10000 in
	Hashtbl.add my_hash "" [];
	valuate p (vars p) [] my_hash;
	Hashtbl.mem my_hash "true";;

(* satisfier p*)	
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)
let satisfier p =
	let my_hash = Hashtbl.create 10000 in
	Hashtbl.add my_hash "" [];
	valuate p (vars p) [] my_hash;
	Hashtbl.find my_hash "true";;

(* isFalsifiable p*)	
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)	
let isFalsifiable p = 
	let my_hash = Hashtbl.create 10000 in
	Hashtbl.add my_hash "" [];
	valuate p (vars p) [] my_hash;
	Hashtbl.mem my_hash "false";;

(* falsifier p*)	
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)	
let falsifier p =
	let my_hash = Hashtbl.create 10000 in
	Hashtbl.add my_hash "" [];
	valuate p (vars p) [] my_hash;
	Hashtbl.find my_hash "false";;

(* models p*)	
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)	
let models p = 
	let my_hash = Hashtbl.create 10000 in
	Hashtbl.add my_hash "" [];
	valuate p (vars p) [] my_hash;
	Hashtbl.find_all my_hash "true";;

(* isTautology p*)	
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)	
let isTautology p = 
	let my_hash = Hashtbl.create 10000 in
	Hashtbl.add my_hash "" [];
	valuate p (vars p) [] my_hash;
	if(Hashtbl.find my_hash "false" = []) then false else true;;

(* isContradiction p*)	
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)	
let isContradiction p =
	let my_hash = Hashtbl.create 10000 in
	Hashtbl.add my_hash "" [];
	valuate p (vars p) [] my_hash;
	if(Hashtbl.find my_hash "true" = []) then false else true;;

(* isContradiction p*)	
(* Time complexity : O(n^2) , Space Complexity : O(1) where n is size of vars *)	
let isEquivalent p1 p2 =
	if(equal (vars p1) (vars p2) = false) then false else isequalValuation p1 p2 (vars p1) [];;

let rec map2 f p l=
 match l with
 [] -> []
 | h :: xs -> if(f p h) then true :: map2 f p xs else false :: map2 f p xs;;

(* entails p*)	
(* Time complexity : O(2^n) , Space Complexity : O(2^n) where n is size of vars *)	
let rec entails pset p =
	let premise = List.fold_left(fun x y -> And (x,y)) T pset in
	let allsatisfy = models premise in
	let r = map2 istrue p allsatisfy in
	List.fold_left (fun x y -> x && y) true r;;