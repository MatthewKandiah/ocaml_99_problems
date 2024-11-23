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

