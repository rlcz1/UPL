(* 
   (ParserMain.parse “true”) = Ast.Lambda (“x”, Ast.Lambda (“y”, Ast.Id “x”))
   (ParserMain.parse “false”) = Ast.Lambda (“x”, Ast.Lambda (“y”, Ast.Id “y”))
   (ParserMain.parse “if x < 1 then 2 else 3”) = Ast.App (Ast.App (Ast.LessThan (Ast.Id “x”, Ast.Num 1), Ast.Num2), Ast.Num 3)
*)

let rec interp (e : Ast.expr) (s : Store.t) : Store.value  =
   match e with
   | Num n -> Store.NumV n
   | Id x -> Store.find x s
   | Add (e1, e2) -> (
      match interp e1 s, interp e2 s with
      | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2) 
      | _ -> failwith (Format.asprintf "[Error] Not a number: %a + %a" Ast.pp e1 Ast.pp e2)
   )
   | Sub (e1, e2) -> (
      match interp e1 s, interp e2 s with
      | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2)
      | _ -> failwith (Format.asprintf "[Error] Not a number: %a - %a" Ast.pp e1 Ast.pp e2)
   )
   | LetIn (x, e1, e2) -> 
      let v = interp e1 s in
      interp e2 (Store.add x v s)
   | App (e1, e2) -> (
      match interp e1 s with
      | Store.ClosureV (arg, e, s') -> (
         let e2v = interp e2 s in
         interp e (Store.add arg e2v s')
      )
      | _ -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp e1)
   )
   | Lambda (arg, e) -> (
      Store.ClosureV (arg, e, s)
   )
   | LessThan (e1, e2) -> (
      match interp e1 s, interp e2 s with
      | Store.NumV n1, Store.NumV n2 -> (
         (* true false는 lambda로 표현 *)
         if n1 < n2 then interp (Lambda ("x", Lambda ("y", Id "x"))) s
         else interp (Lambda ("x", Lambda ("y", Id "y"))) s
      )
      | _ -> failwith (Format.asprintf "[Error] Not a number: %a < %a" Ast.pp e1 Ast.pp e2)
   )