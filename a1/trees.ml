(* Compile with: ocamlc -g -o trees trees.ml *)

type inttree = Empty | Node of int * inttree * inttree

(* use this function in fromList *)
let rec insert t i =
  match t with
      Empty -> Node(i,Empty,Empty)
    | Node(j,l,r) -> 
      if i=j 
      then t 
      else if i < j 
      then Node(j,insert l i,r)
      else Node(j,l,insert r i)

(* no need for this function; it is just an example *)
let rec member t i =
  match t with
      Empty -> false
    | Node(j,l,r) -> i=j || (i < j && member l i) || member r i

(* put fromList, sum1, prod1, avg1, map and negateAll here *)

(* Q1 *)
let rec fromList l = 
    match l with
        [] -> Empty
    |   head :: tail -> insert (fromList tail) head
;;

(* Q2 *)
let rec sum1 tree =
    match tree with
        Empty -> 0
    |   Node(i,j,k) -> i + (sum1 j + sum1 k)
;;
let rec prod1 tree =
    match tree with
        Empty -> 1
    |   Node(i,j,k) -> i * (prod1 j * prod1 k)
;;
let rec length tree =
    match tree with
        Empty -> 0
    |   Node(i,j,k) -> 1 + (length j + length k)
;;
let rec avg1 tree =
    (sum1 tree) / (length tree)
;;

(* Q3 *)
(*
let rec map =
;;
*)

(* Q4 *)
(*
let rec negateAll =
;;
*)

let rec fold f a t =
  match t with
      Empty -> a
    | Node(j,l,r) -> fold f (fold f (f a j) l) r

(* put sum2, prod2, and avg2 here *)
(* Q4 *)
(*
let rec sum2 tree =
;;
*)
(*
let rec prod2 tree =
;;
*)
(*
let rec avg2 tree =
;;
*)


type 'a iterator = Nomore | More of 'a * (unit -> 'a iterator)

let rec iter t =
  let rec f t k =
    match t with
	Empty -> k ()
      | Node(j,l,r) -> More(j, fun () -> f l (fun () -> f r k))
  in f t (fun () -> Nomore)

(* Q6 *)
(*
let rec sum3 tree =
;;
*)
(*
let rec prod3 tree =
*)
(*
let rec avg3 tree =
*)

     

(* challenge problem: put optionToException and exceptionToOption here *)

(* a little testing -- commented out since the functions do not exist yet *)

(*
let tr = fromList [0;1;2;3;4;5;6;7;8;9;9;9;1] (* repeats get removed *)
let print_ans f t = print_string (string_of_int (f t)); print_string "\n"
let _ = print_ans sum1 tr
let _ = print_ans prod1 tr
let _ = print_ans avg1 tr
let _ = print_ans sum2 tr
let _ = print_ans prod2 tr
let _ = print_ans avg2 tr
let _ = print_ans sum3 tr
let _ = print_ans prod3 tr
let _ = print_ans avg3 tr
*)
