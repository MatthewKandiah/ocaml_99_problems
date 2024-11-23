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
