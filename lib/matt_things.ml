(* problem 1 *)
let rec last l = match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last tail
;;

let%test "should return last element" = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test "should handle empty list" = last [] = None

let rec last_two l = match l with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some(x, y)
  | _ :: tail -> last_two tail
  ;;

let%test "should return last two elements" = last_two ["a"; "b"; "c"; "d"] = Some ("c", "d")
let%test "should handle single element list" = last_two ["a"] = None
let%test "should handle empty list" = last_two [] = None

