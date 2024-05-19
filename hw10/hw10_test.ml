(* Test code *)
let%test _ = Hw10.interp (ParserMain.parse "(fun x y -> x + y) 1 3") Store.empty = Store.NumV 4
let%test _ = Hw10.interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") Store.empty = Store.NumV 4
let%test _ = Hw10.interp (ParserMain.parse "let f = (fun x y -> x - y) in f 1 3") Store.empty = Store.NumV (-2)