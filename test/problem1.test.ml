(* # last ["a" ; "b" ; "c" ; "d"];; *)
(* - : string option = Some "d" *)
(* # last [];; *)
(* - : 'a option = None *)
let%test "should pass" = true
let%test "should fail" = false
