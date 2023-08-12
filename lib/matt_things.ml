(* problem 1 *)
let rec last l =
  match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last tail
;;

let%test "should return last element" = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test "should handle empty list" = last [] = None

(*problem 2*)
let rec last_two l =
  match l with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

let%test "should return last two elements" =
  last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")
;;

let%test "should handle single element list" = last_two [ "a" ] = None
let%test "should handle empty list" = last_two [] = None

(*problem 3*)
exception Failed of string

let rec nth (l : 'a list) (n : int) : 'a =
  match l with
  | [] -> raise (Failed "nth")
  | head :: tail -> if n = 0 then head else nth tail (n - 1)
;;

let%test "should return nth element of int list" = nth [ 0; 1; 2; 3; 4 ] 2 = 2
let%test "should return nth element of string list" = nth [ "a"; "b"; "c"; "d" ] 2 = "c"
let%test "should return 0th element" = nth [ 'a'; 'b' ] 0 = 'a'

(*problem 4*)
let rec length l = 
  match l with
    | [] -> 0
    | _ :: tail -> 1 + length tail
;;

let%test "should return length of list" = length [0; 1; 2] = 3
let%test "should return length of empty list" = length [] = 0

