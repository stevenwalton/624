(* CPS Homework: A6 *)

(* Problem 1: Low-level functions in CPS *)
let rec addk (a:int) (b:int) k =
    k (a + b)
;;

let rec subk (a:int) (b:int) k =
    k (a - b)
;;

let timesk (a:int) (b:int) k =
    k (a * b)
;;

let plusk (a:float) (b:float) k =
    k (a +. b)
;;

let take_awayk (a:float) (b:float) k =
    k (a -. b)
;;

let multk (a:float) (b:float) k =
    k (a *. b)
;;

let catk (a:string) (b:string) k =
    k (a ^ b)
;;

let consk a lst k =
    k (lst @ [a])
;;

let lessk a b k =
    k (a < b)
;;

let eqk a b k =
    k (a == b)
;;

let report x =
    print_string "Result: ";
    print_int x;
    print_newline();;

addk 3 4 report;;

(* Problem 2: Nesting continuations *)
