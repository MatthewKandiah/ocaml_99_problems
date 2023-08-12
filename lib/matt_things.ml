(* problem 1 *)
let rec last l = match l with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tail -> last tail
;;

let%test "should return last element" = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test "should handle empty list" = last [] = None
