(* Test code *)
let%test _ = Hw7.interp (ParserMain.parse "let x = 1 in let y = 3 in x + y") Store.empty = Store.NumV 4
let%test _ = Hw7.interp (ParserMain.parse "x") [("x", NumV 1)] = Store.NumV 1
let%test _ = Hw7.interp (ParserMain.parse "let x = 1 in let y = 3 in x + y") [("x", NumV 3)] = Store.NumV 4