(* Test code *)
let%test _ = Hw11.interp (ParserMain.parse "if 3 < 1 then 2 else 3") Store.empty = Store.NumV 3
let%test _ = Hw11.interp (ParserMain.parse "if 1 < 3 then 2 else 3") Store.empty = Store.NumV 2
let%test _ = Hw11.interp (ParserMain.parse "if (1 - 1) < (0 + 1) then 0 + 7 else 3") Store.empty = Store.NumV 7
