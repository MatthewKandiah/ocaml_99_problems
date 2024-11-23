(*
  Problem 1 - Tail of a list
*)

let rec last lst = 
  match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest;;

assert (last [1; 2; 3] = Some 3);;
assert (last ['a'] = Some 'a');;
assert (last [] == None);;

(*
  Problem 2 - Last two elements of a list
*)

let rec last_two lst =
  match lst with
  | [] -> None
  | [x] -> None
  | [x; y] -> Some (x , y)
  | _ :: rest -> last_two rest;;

assert (last_two ["a"; "b"; "c"; "d"] = Some ("c", "d"));;
assert (last_two [1] = None);;
assert (last_two [] = None);;

(*
  Problem 3 - N'th element of a list
*)

let rec nth lst n =
  match (lst, n) with
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: rest, m -> nth rest (m-1);;

assert (nth ["a"; "b"; "c"; "d"; "e"] 0 = Some "a");;
assert (nth ["a"; "b"; "c"; "d"; "e"] 1 = Some "b");;
assert (nth ["a"; "b"; "c"; "d"; "e"] 2 = Some "c");;
assert (nth ["a"; "b"; "c"; "d"; "e"] 3 = Some "d");;
assert (nth ["a"; "b"; "c"; "d"; "e"] 4 = Some "e");;
assert (nth ["a"; "b"; "c"; "d"; "e"] 5 = None);;
assert (nth [] 0 = None);;
assert (nth [] 5 = None);;

(*
  Problem 4 - Length of a list
*)

let length lst =
  let rec length_aux l n = match l with
    | [] -> n
    | _ :: rest -> length_aux rest (n + 1)
  in
  length_aux lst 0;;

assert (length [] = 0);;
assert (length [1] = 1);;
assert (length [1; 2; 3] = 3)

(*
  Problem 5 - Reverse a list
*)

let rev lst =
  let rec rev_aux l r = match l with
    | [] -> r
    | head :: rest -> rev_aux rest (head :: r)
  in
  rev_aux lst [];;

(*
  l = [1 2 3 4]; r = [] START
  l = [2 3 4];  r = [1]
  l = [3 4];  r = [2 1]
  l = [4];  r = [3 2 1]
  l = []; r = [4 3 2 1] DONE
*)

assert (rev [] = []);;
assert (rev [1] = [1]);;
assert (rev [1; 2] = [2; 1]);;
assert (rev [1; 2; 3; 4] = [4; 3; 2; 1]);;

(*
  Problem 6 - Palindrome
*)

let is_palindrome lst = 
  lst = (rev lst);;

assert (is_palindrome ["x"; "a"; "m"; "a"; "x"] = true);;
assert (is_palindrome ["x"; "a"; "a"; "x"] = true);;
assert (is_palindrome [1; 2] = false);;

(*
  Problem 7 - Flatten a list
*)

type 'a node = 
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec flatten_aux i o = 
  match i with
    | [] -> o
    | One x :: rest -> flatten_aux rest (x :: o)
    | Many inner_list :: rest -> flatten_aux rest ((flatten_aux inner_list o))
  in rev (flatten_aux lst []);;

assert (flatten [] = []);;
assert (flatten [One 1; One 2; One 3] = [1; 2; 3]);;
assert (flatten [One 1; Many [One 1; One 2; One 3]] = [1; 1; 2; 3]);;
assert (flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"]);;

(*
  Problem 8 - Eliminate Duplicates
*)

let compress lst = 
  let rec compress_aux acc = function
  | [] -> acc
  | [x] -> x :: acc
  | h :: (th :: _ as t) -> if h = th then compress_aux acc t else compress_aux (h :: acc) t
  in rev (compress_aux [] lst);;

assert (compress [] = []);;
assert (compress [1] = [1]);;
assert (compress [1; 1; 1;] = [1]);;
assert (compress ["a"; "a"; "b"] = ["a"; "b"]);;
assert (compress ["a"; "a"; "a"; "b"; "c"; "c"; "c"; "c"; "c"; "b"; "b"; "e"; "e"; "e"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "b"; "e"]);;

(*
  Problem 10 - Run-Length Encoding
*)

let encode lst =
  let rec encode_aux l acc = match l, acc with
  | [], _ -> acc
  | h :: t, [] -> encode_aux t [(1, h)]
  | h :: t, (cnt_h, char_h) :: acc_t -> if h = char_h then encode_aux t ((cnt_h + 1, char_h) :: acc_t) else encode_aux t ((1, h) :: acc)
  in rev (encode_aux lst []);;
  
assert (encode [] = []);;
assert (encode ["a"] = [(1, "a")]);;
assert (encode ["a"; "a"] = [(2, "a")]);;
assert (encode ["a"; "b"] = [(1, "a"); (1, "b")]);;
assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;

(*
	Problem 9 - Pack consecutive duplicates
*)

let times count x =
  let rec times_aux n acc = match n with
    | 0 -> acc
    | m -> times_aux (m-1) (x :: acc)
  in times_aux count [];;

assert (times 0 "a" = []);;
assert (times 1 "a" = ["a"]);;
assert (times 5 "a" = ["a"; "a"; "a"; "a"; "a" ]);;

let pack lst =
	let rec encoded_to_packed l = match l with
		| [] -> []
		| (h_cnt, h_char) :: t -> (times h_cnt h_char) :: (encoded_to_packed t)
	in encoded_to_packed (encode lst);;

assert (pack [] = []);;
assert (pack ["a"] = [["a"]]);;
assert (pack ["a"; "b"] = [["a"]; ["b"]]);;
assert (pack ["a"; "a"] = [["a"; "a"]]);;
assert (pack ["a"; "a"; "b"; "c"; "c"; "c"] = [["a"; "a"]; ["b"]; ["c"; "c"; "c"]]);;
assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];["e"; "e"; "e"; "e"]]);;

