(* Test code *)

(* let%test _ = Fstore.find "f" [("f", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] = (["x"], Ast.Add (Ast.Num 1, Ast.Id "x"))
let%test _ = Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) Fstore.empty = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]
let%test _ = Fstore.add "f" ["x"; "y"] (Ast.Add (Ast.Id "x", Ast.Id "y")) [("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] = 
[("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))); ("f2", (["x"], Ast.Add (Ast.Num 1, Ast.Id "x")))] *)
let%test _ = Hw9.interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y endef f1(3, 4) + f2(4, 7)") = Store.NumV 4
let%test _ = Hw9.interp_def (Ast.FunDef ("f", ["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y"))) Fstore.empty = [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))]
let%test _ = Hw9.interp_expr (Ast.Add (Ast.Call ("f", [Ast.Num 3; Ast.Num 2]), Ast.Id "x")) [("f", (["x"; "y"], Ast.Add (Ast.Id "x", Ast.Id "y")))] [("x", Store.NumV 10)] = Store.NumV 15