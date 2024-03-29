(* Test code *)

(* factorial *)
let%test _ = Hw2.factorial 0 = 1
let%test _ = Hw2.factorial (-5) = (-1)
let%test _ = Hw2.factorial 3 = 6

(* fib *)
let%test _ = Hw2.fib 0 = 1
let%test _ = Hw2.fib 1 = 1
let%test _ = Hw2.fib 2 = 2
let%test _ = Hw2.fib 3 = 3
let%test _ = Hw2.fib 4 = 5
let%test _ = Hw2.fib 5 = 8

(* acc *)
let%test _ = Hw2.acc 4 = 10
let%test _ = Hw2.acc (-4) = (-10)
let%test _ = Hw2.acc 0 = 0

(* pow *)
let%test _ = Hw2.pow 2 3 = 8
let%test _ = Hw2.pow 3 5 = 243
let%test _ = Hw2.pow 5 4 = 625
let%test _ = Hw2.pow (-3) 4 = 0