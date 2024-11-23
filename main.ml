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
