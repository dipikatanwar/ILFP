(* Define Exceptions *)
exception NOT_UNIFIABLE;;
exception NOT_EQUAL;;
exception NOT_FOUND;;

(* Declaration of data type *)
type symbol = string;;
type variable = string;;
type constant = string;;
type term = V of variable | C of constant | Node of symbol*(term list);;
type formula = Node of symbol*(term list);;
type head = formula;;
type body = formula list;;
type clause =
    Fact of head
  | Rule of head * body
;;

type goal = formula;;
type program = clause list
;;


(*utility functions *)

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

let rec foldl f e l = match l with
    [ ] -> e
  | x :: xs -> foldl f (f e x) xs
;;

let rec length l = match l with
    [] -> 0
  |x::xs -> 1 + length xs
;;

let rec foldl2 f e l1 l2 = match l1,l2 with
    [ ], [] -> e
  | x :: xs, y::ys -> foldl2 f (f e x y) xs ys
  |_,_ ->raise NOT_EQUAL
;;

let rec find x l = match l with
    [] -> false
  |h::t -> if h=x then true else find x t
;;

let rec map f l = match l with
    [] -> []
  | x::xs -> (f x)::map f xs
;;

let rec findKey k l = match l with
    [] -> raise NOT_FOUND
  |(x,y)::rest -> if x=k then (true,y) else findKey k rest
;;

let rec varFromTerm t = match t with
    V x -> x::[]
  | C x -> []
  | Node (a,l) -> foldl (fun x y -> union x (varFromTerm y)) [] l;;

let rec vars pt = match pt with
    [] -> []
  | Node (a,l) :: rem_g -> union (foldl (fun x y -> union x (varFromTerm y)) [] l) (vars rem_g)
;;

let rec occurs x t =
  match t with
  | V v -> v = x
  | C _ -> false
  | Node (_, trm_lst) -> List.exists (occurs x) trm_lst
;;

let rec termMGU p1 p2 = match (p1, p2) with
  | (C a, C b) -> if a = b then [] else raise NOT_UNIFIABLE
  | (V v, C a) | (C a, V v) ->      [(v, C a)]
  | (C _, Node _) | (Node _, C _) -> raise NOT_UNIFIABLE
  | (V v, V w) -> if v <> w then [(v, V w)] else []
  | (V v, (Node _ as t)) | ( Node _ as t, V v) -> if not (occurs v t) then [(v, t)] else raise NOT_UNIFIABLE
  | (Node (f, trm_lst_1), Node (g, trm_lst_2)) ->
      if (f = g) && (length trm_lst_1 = length trm_lst_2)
      then foldl2 (fun acc c1 c2 -> union acc (termMGU c1 c2)) [] trm_lst_1 trm_lst_2
      else raise NOT_UNIFIABLE
;;

let mgu f1 f2 = match (f1, f2) with
  | (Node (f, trm_lst_1), Node (g, trm_lst_2)) ->
      if (f = g) && (length trm_lst_1 = length trm_lst_2)
      then foldl2 (fun acc c1 c2 -> union acc (termMGU c1 c2)) [] trm_lst_1 trm_lst_2
      else raise NOT_UNIFIABLE
  | _ -> raise NOT_UNIFIABLE
;;

let toString t =
  match t with
  | C c -> c
  | V v -> v
  | _   -> ""
;;

let rec find_to_print_from_sub s gvars = match s with
  | [] -> ""
  | (v, t) :: rest ->
      if find v gvars then
        (Printf.sprintf "\n%s = %s" v (toString t))^(find_to_print_from_sub rest gvars)
      else (find_to_print_from_sub rest gvars)
;;


let separateHeadBody clause = match clause with
  | Fact head -> (head, [])
  | Rule (head, body) -> (head, body)
;;

let rec replaceTermWith trm sub =
  match trm with
  | C c -> C c
  | V v  ->
      (
        try
          let _, t = findKey v sub in t
        with
          NOT_FOUND -> V v
      )
  | Node (pred, trm_lst) -> Node (pred, map (fun t -> replaceTermWith t sub) trm_lst)
;;

let substitute body sub =
  let substituteFormula f sub = match f with
    | Node (pred, trm_lst) -> Node (pred, map (fun t -> replaceTermWith t sub) trm_lst) in
  foldl (fun acc f -> union acc [(substituteFormula f sub)]) [] body
;;

let rec compose s1 s2 =
  let rec compose_with_acc s1 s2 acc =
    (
      match (s1, s2) with

      | ([], []) -> acc
      | (l, []) | ([], l) -> union acc l
      | (((v1, t1) :: rest1), ((v2, t2) :: rest2)) ->

          if (occurs v2 t1) then
            compose_with_acc rest1 rest2
              (union acc [(v1, replaceTermWith t1 [(v2, t2)]); (v2, t2)])
          else
            compose_with_acc rest1 rest2 (union [(v1, t1); (v2, t2)] acc)
    )
  in
  compose_with_acc s1 s2 []
;;

let rec plist l = match l with
  | [] -> ""
  | x::xs -> print_string x; print_string " LL"; plist xs
;;

let rec getVarMapping prologProgram goalList ans_acc gvars =
  match goalList with
  | [] ->
      let to_print = find_to_print_from_sub ans_acc gvars in
      if ans_acc <> [] && gvars <> [] then
        let _ = print_string to_print in
        let _ = print_string " " in
        true
      else true

  | goal::remainingGoalList ->
      let rec resolve p g =
        match p with
        | [] -> false
        | first::rest ->
            try
              let headOf, bodyOf = separateHeadBody first in
              let subst = mgu g headOf in
              let new_goals = substitute (union remainingGoalList bodyOf) subst in
              let ret1 = getVarMapping prologProgram new_goals (compose ans_acc subst) gvars in
              let ret2 =resolve rest g in
              ret1 || ret2
            with
              NOT_UNIFIABLE -> resolve rest g in
      resolve prologProgram goal
;;


let interpreter p g =
  let gvars = vars g in
  getVarMapping p g [] gvars;;
;;


let f1 = Fact( Node("male",[ C "shantanu"]));;
let f2 = Fact( Node("female",[ C "ganga"]));;
let f3 = Fact( Node("female",[ C "satyavati"]));;
let f4 = Fact( Node("male",[ C "bheeshm"]));;
let f5 = Fact( Node("married",[C "shantanu" ; C "satyavati"]));;
let f6 = Fact( Node("married",[C "shantanu"; C "ganga"]));;
let f7 = Fact( Node("child",[C "bheeshm"; C "shantanu"]));;
let f8 = Fact( Node("child",[C "bheeshm" ;C "ganga"]));;
let f9 = Fact( Node("male",[ C "chitrangad"]));;
let f10 = Fact( Node("child",[C "chitrangad"; C "shantanu"]));;
let f11= Fact( Node("child",[C "chitrangad" ;C "satyavati"]));;

let f12 = Fact( Node("male",[ C "vichitravirya"]));;
let f13 = Fact( Node("child",[C "vichitravirya"; C "shantanu"]));;
let f14= Fact( Node("child",[C "vichitravirya" ;C "satyavati"]));;
let f15 = Fact( Node("female",[ C "ambika"]));;
let f16 = Fact( Node("female",[ C "ambalika"]));;
let f17= Fact( Node("married",[C "vichitravirya" ;C "ambika"]));;
let f18= Fact( Node("married",[C "vichitravirya" ;C "ambalika"]));;

let f19 = Fact( Node("male",[ C "dhritrashtra"]));;
let f20 = Fact( Node("male",[ C "pandu"]));;

let f21 = Fact( Node("child",[C "dhritrashtra"; C "vichitravirya"]));;
let f22= Fact( Node("child",[C "dhritrashtra" ;C "ambika"]));;
let f23 = Fact( Node("child",[C "pandu"; C "vichitravirya"]));;
let f24= Fact( Node("child",[C "pandu" ;C "ambalika"]));;
let f25 = Fact( Node("female",[ C "kunti"]));;
let f26 = Fact( Node("female",[ C "madri"]));;
let f27= Fact( Node("married",[C "pandu" ;C "kunti"]));;
let f28= Fact( Node("married",[C "pandu" ;C "madri"]));;

let f29 = Fact( Node("male",[ C "yudhishter"]));;
let f30 = Fact( Node("male",[ C "bheem"]));;
let f31 = Fact( Node("male",[ C "arjun"]));;
let f32 = Fact( Node("male",[ C "nakul"]));;
let f33 = Fact( Node("male",[ C "sehdev"]));;

let f34 = Fact( Node("child",[C "yudhishter"; C "pandu"]));;
let f35= Fact( Node("child",[C "yudhishter" ;C "kunti"]));; 
let f36 = Fact( Node("child",[C "bheem"; C "pandu"]));;
let f37= Fact( Node("child",[C "bheem" ;C "kunti"]));;
let f38 = Fact( Node("child",[C "arjun"; C "pandu"]));;
let f39= Fact( Node("child",[C "arjun" ;C "kunti"]));;
let f40 = Fact( Node("child",[C "nakul"; C "pandu"]));;
let f41= Fact( Node("child",[C "nakul" ;C "madri"]));;
let f42 = Fact( Node("child",[C "sehdev"; C "pandu"]));;
let f43= Fact( Node("child",[C "sehdev" ;C "kunti"]));;

let r1 = Rule(
    Node("father",[V "X"; V "Y"]),[
      Node("male",[V "X"]);
      Node("child",[V "Y"; V "X"])
    ]);;

let r2 = Rule(
    Node("mother",[V "X"; V "Y"]),[
      Node("female",[V "X"]);
      Node("child",[V "Y"; V "X"])
    ]);;

let r3 = Rule(
    Node("husband",[V "X"; V "Y"]),[
      Node("male",[V "X"]);
      Node("female",[V "Y"]);
      Node("married",[V "X"; V "Y"])
    ]);;
let r3 = Rule(
    Node("wife",[V "X"; V "Y"]),[
      Node("female",[V "X"]);
      Node("male",[V "Y"]);
      Node("married",[V "Y"; V "X"])
    ]);;

let r4 = Rule(
    Node("son",[V "X"; V "Y"]),[
      Node("male",[V "X"]);
      Node("child",[V "X"; V "Y"])
    ]);;


let r5 = Rule(
    Node("brother",[V "X"; V "Y"]),[
      Node("male",[V "X"]);
      Node("child",[V "X"; V "Z"]);
      Node("child",[V "Y"; V "Z"]);
    ]);;

let r6 = Rule(
    Node("father_in_law",[V "X"; V "Y"]),[
      Node("male",[V "X"]);
      Node("child",[V "Z"; V "X"]);
      Node("married",[V "Z";V "Y"])
    ]);;

let program = [f1;f2;f3;f4;f5;f6;f7;f8;f9;f10;f11;f12;f13;f14;f15;f16;f17;f18;f19;f20;f21;f22;f23;f24;f25;f26;f27;f28;f29;f30;f31;f32;f33;f34;f35;f36;f37;f38;f39;f40;f41;f42;f43;r1;r2;r3;r4;r5;r6];;

let g1 = [Node("wife", [C "ganga"; C "shantanu"])];;
let g2 = [Node("wife", [V "X"; C "vichitravirya"])];;

let g3 = [Node("father", [C "shantanu"; C "bheeshm"])];; 
let g4 = [Node("father",[C "shantanu"; V "Y"])];;
let g5 = [Node("father_in_law",[C "shantanu"; V "Y"])];;

let g6 = [Node("brother", [V "X"; C "pandu"])];; 
let g7 = [Node("brother", [C "dhritrashtra"; C "pandu"])];;

let g8 = [Node("male", [C "shantanu"])];;

(*
  interpreter program g1;;
  interpreter program g2;;
  interpreter program g3;;
  interpreter program g4;;
  interpreter program g5;;
  interpreter program g6;;
  interpreter program g7;;
  interpreter program g8;; 
  *)
