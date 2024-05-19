
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

      (* match interp e1 s, interp e2 s with
      | Store.ClosureV (x, e, s'), v -> interp e (Store.add x v s') *)
   )
   | Lambda (arg, e) -> (
      Store.ClosureV (arg, e, s)
   )