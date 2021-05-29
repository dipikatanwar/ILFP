type clause =
 Fact of string
| Rule of string * string list
;;

type program = clause list
;;

let c1 = Fact "B";;
let c2 = Fact "E";;
let c3 = Fact "H";;
let c4 = Rule ("A",["B";"C"]);;
let c5 = Rule ("A",["D"]);;
let c6 = Rule ("C",["E";"F"]);;
let c7 = Rule ("C",["G"]);;
let c8 = Rule ("D",["E";"G"]);;
let c9 = Rule ("G",["H";"B"]);;

let testprogram = [c1;c2;c3;c4;c5;c6;c7;c8;c9];;

let goal = ["A";"B";"C";"D"];;

let rec member x s =
match s with
[] -> false
| h::t -> if(x = h) then true else member x t;;

let rec union s1 s2 =
match s1 with
[]-> s2
| x::xs -> if(member x s2 = false) then union xs (x::s2) else union xs s2;;

let rec append l1 l2 = match l1 with
	[] -> l2
	| x::xs -> x :: (append xs l2);;
	
let rec isExist p h =
match p with
[] -> false
| Fact s :: xs -> if s == h then true else isExist xs h
| Rule (s,l) :: xs -> if s == h then true else isExist xs h
;;

let rec replace p h t=
match p with
[] -> h :: t
| Fact s :: xs -> if s == h then t else replace xs h t
| Rule (s,l) :: xs -> if s == h then union l t else replace xs h t
;;

let rec remove x l1 l2=
 match l1 with 
 [] -> l2
 |  h :: t -> if h != x then h::l2 else remove x t l2;; 
 
let removehead l =
	match l with
	[] -> l
	| h:: t -> t;;
 
let rec removelist l1 l2 =
	match l1 with
	[] -> l2
	| h :: t -> (removelist t (remove h l2 []))
	;;
 
let rec reversereplaced p g h =
	match p with
	[] -> []
	| Fact s :: xs -> if s == h then g else reversereplaced xs g h
	| Rule (s,l) :: xs -> if s == h then (removelist l g) else reversereplaced xs g h
	;;
 
 
let backtrack p g ex = 
	match ex with
	[] -> g
	| h :: t -> (reversereplaced p g h);;

let rec length l = match l with 
 [] -> 0
 | x :: xs -> 1+ length xs;;

let rec resolution p g explored count =
	match g with
	[] -> true
	| h :: t -> if( isExist p h) then resolution p (replace p h t) (h :: explored) count
			else  
			begin 
				if count >= length g then false else resolution p (backtrack p g explored) (removehead explored) (count+1)
			end
	;;
	
resolution testprogram goal [] 0;;