module Ls =
struct
let emptyset = [];;
	
let rec member x s =
	match s with
	[] -> false
	| h::t -> if(x = h) then true else member x t;;

let rec subset s1 s2 =
	match s1 with 
	[] -> true
	|h::t -> if (member h s2 = true) then subset t s2 else false;; 

let equal s1 s2 =
	if (subset s1 s2 && subset s2 s1) then true else false;;
	
let rec union s1 s2 =
	match s1 with
	[]-> s2
	| x::xs -> if(member x s2 = false) then union xs (x::s2) else union xs s2;;
	
let rec intersection s1 s2 =
	match s1 with 
	[] -> s1
	| x :: xs -> if (member x s2 == true) then (x::(intersection xs s2)) else intersection xs s2;;
		

let rec difference s1 s2 =
	match s1 with
	[] -> s1
	| x::xs -> if (member x s2 == true) then difference xs s2 else x::difference xs s2;;

let rec pair x s2 =
	match s2 with
	[] -> []
	| h :: t -> (x,h) :: pair x t;;
	
let rec product s1 s2 = 
	match s1 with
	[] -> []
	| h :: t ->  union (pair h s2) (product t s2);;	

let rec map f l =
 match l with
 [] -> []
 | x :: xs -> f x :: map f xs
;;

let rec power l = 
	match l with
	[]->[[]]
	|x::xs -> 
	let a = power xs in
	let b = map (fun (elem) -> x::elem) a in
	a @ b;;

end

module Cf =
struct
let emptyset x = false;;

let member f x = if f x then true else false;; 

let union f g x = if f x then true else g x;;

let intersection f g x = if f x then (g x) else false;;

let difference f g x = if f x then not (g x) else false;;

let product f g x = (f x , g x);;

(*

let subset f g x = if f x and g x return true else false 

let equal f g x = if( f x == g x) return true else false;

power f x =  ?? 

*)
end

	
	
	











	
	







	

	
