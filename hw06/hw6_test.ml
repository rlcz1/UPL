(* Test code *)
let%test _ = Hw6.interp (ParserMain.parse "1 + 2 - 3") = Value.NumV 0
let%test _ = Hw6.interp (ParserMain.parse "1 + 2 - 4") = Value.NumV (-1)
let%test _ = Hw6.interp (ParserMain.parse "1 + 2") = Value.NumV 3