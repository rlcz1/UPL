(* Test code *)
let prog = "let rec sum = (fun x -> if 0 < x then x + (sum (x - 1)) else 0) in sum 10" 
let%test _ = Hw12.interp (ParserMain.parse prog) Store.empty = Store.NumV 55
let prog = "let rec f = (fun x -> if 0 < x then f (x - 1) else 0) in f 10"
let%test _ = Hw12.interp (ParserMain.parse prog) Store.empty = Store.NumV 0
let prog = "let rec sub = (fun x -> if 0 < x then sub (x - 1) else 0) in sub 10"
let%test _ = Hw12.interp (ParserMain.parse prog) Store.empty = Store.NumV 0