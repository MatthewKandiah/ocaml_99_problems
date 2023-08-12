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

(*problem 6 - without overcomplicating it!*)
let is_palindrome_simple list = list = rev list

let%test "should be true for odd length palindrome" =
  is_palindrome_simple [ 1; 2; 3; 4; 3; 2; 1 ]
;;

let%test "should be true for even length palindrom" =
  is_palindrome_simple [ 1; 2; 3; 3; 2; 1 ]
;;

let%test "should be true for empty" = is_palindrome_simple []

let%test "should be false for odd length non-palindrome" =
  not (is_palindrome_simple [ 1; 2; 3; 1; 2; 3; 4 ])
;;

let%test "should be false for even length non-palindrome" =
  not (is_palindrome_simple [ 1; 2; 1; 2 ])
;;

(*problem 7 - flatten a list*)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten (l : 'a node list) =
  let rec aux list acc =
    match list with
    | [] -> rev acc
    | One head :: tail -> aux tail (head :: acc)
    | Many head_list :: tail ->
      let flattened_head_list = flatten head_list in
      aux tail (rev flattened_head_list @ acc)
  in
  aux l []
;;

let%test "should return trivial flat list" =
  flatten [ One "a"; One "b"; One "c" ] = [ "a"; "b"; "c" ]
;;

let%test "should return flattened list" =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]
;;

(*problem 7 without needing two recursive functions*)
(*neater way of doing the same thing I was doing, the inner aux call on the head list will generate our reverse-order flattened list, avoiding the repeated reverses my method required!*)
let flatten_better list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  rev (aux [] list)
;;

let%test "should return trivial flat list" =
  flatten_better [ One "a"; One "b"; One "c" ] = [ "a"; "b"; "c" ]
;;

let%test "should return flattened list" =
  flatten_better [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]
;;

(*problem 8 - eliminate duplicates*)
let compress list =
  let rec aux (cmp : 'a option) (acc : 'a list) (l : 'a list) =
    match l with
    | [] -> acc
    | head :: tail ->
      (match cmp with
       | None -> aux (Some head) (head :: acc) tail
       | Some x ->
         if head != x then aux (Some head) (head :: acc) tail else aux cmp acc tail)
  in
  rev (aux None [] list)
;;

let%test "should return list without consecutive duplicates" =
  compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

(*problem 8 - nice syntax to do the same thing*)
(*neat, you can destructure the head off multiple times, then use `as` to still refer to the whole tail*)
(*no need to explicitly pass in the comparison element, we just inspect elements in pairs*)
let rec compress2 = function
  | a :: (b :: _ as t) -> if a = b then compress2 t else a :: compress t
  | smaller -> smaller
;;

let%test "should return list without consecutive duplicates" =
  compress2 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

(*problem 9 - pack consecutive duplicates*)
let pack list =
  let rec aux (cmp : 'a option) (tmp : 'a list) (acc : 'a list list) (ls : 'a list) =
    match ls with
    | [] -> if tmp = [] then acc else acc @ [ tmp ]
    | head :: tail ->
      (match cmp with
       | None -> aux (Some head) [ head ] acc tail
       | Some x ->
         if head = x
         then aux cmp (head :: tmp) acc tail
         else aux (Some head) [ head ] (acc @ [ tmp ]) tail)
  in
  aux None [] [] list
;;

let%test "should pack consecutive values into sublists" =
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

let%test "should handle trivial case" =
  pack [ 1; 2; 3; 4; 5 ] = [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 5 ] ]
;;

let%test "should handle empty list" = pack [] = []

(*problem 9 - their solution*)
(*very neat, same trick destructuring out two head values while still getting the whole tail*)
(*think I need some practice thinking about adding lists to lists of lists in the same way as adding a value to a list*)
let pack2 list =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux (a :: current) acc t else aux [] ((a :: current) :: acc) t
  in
  rev (aux [] [] list)
;;

let%test "should pack consecutive values into sublists" =
  pack2 [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

let%test "should handle trivial case" =
  pack2 [ 1; 2; 3; 4; 5 ] = [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 5 ] ]
;;
