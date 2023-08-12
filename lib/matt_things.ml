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

let%test "should return length of list" = length [ 0; 1; 2 ] = 3
let%test "should return length of empty list" = length [] = 0

(*problem 4 tail recursive*)
let length_tail_rec l =
  let rec go ls acc =
    match ls with
    | [] -> acc
    | _ :: tail -> go tail (acc + 1)
  in
  go l 0
;;

let%test "should return length of list" = length_tail_rec [ 0; 1; 2 ] = 3
let%test "should return length of empty list" = length_tail_rec [] = 0

(*problem 5*)
let rev l =
  let rec go ls acc =
    match ls with
    | [] -> acc
    | x :: tail -> go tail (x :: acc)
  in
  go l []
;;

let%test "should reverse int list" = rev [ 1; 2; 3; 4 ] = [ 4; 3; 2; 1 ]
let%test "should reverse char list" = rev [ 'a'; 'b'; 'c' ] = [ 'c'; 'b'; 'a' ]
let%test "should handle empty string" = rev [] = []

(*problem 5 using function pattern matching syntax*)
let rev2 l =
  let rec go acc = function
    | [] -> acc
    | x :: tail -> go (x :: acc) tail
  in
  go [] l
;;

let%test "should reverse int list" = rev2 [ 1; 2; 3; 4 ] = [ 4; 3; 2; 1 ]
let%test "should reverse char list" = rev2 [ 'a'; 'b'; 'c' ] = [ 'c'; 'b'; 'a' ]
let%test "should handle empty string" = rev2 [] = []

(*problem 6*)
let is_palindrome list =
  let list_rev = rev list in
  let rec lists_equal list1 list2 =
    match list1, list2 with
    | [], [] -> true
    | _, [] | [], _ -> false
    | h1 :: tail1, h2 :: tail2 -> if h1 = h2 then lists_equal tail1 tail2 else false
  in
  lists_equal list list_rev
;;

let%test "should be true for odd length palindrome" =
  is_palindrome [ 1; 2; 3; 4; 3; 2; 1 ]
;;

let%test "should be true for even length palindrom" = is_palindrome [ 1; 2; 3; 3; 2; 1 ]
let%test "should be true for empty" = is_palindrome []

let%test "should be false for odd length non-palindrome" =
  not (is_palindrome [ 1; 2; 3; 1; 2; 3; 4 ])
;;

let%test "should be false for even length non-palindrome" =
  not (is_palindrome [ 1; 2; 1; 2 ])
;;
