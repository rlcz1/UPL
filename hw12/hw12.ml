
let rec interp (e : Ast.expr) (s : Store.t) : Store.value  =
   match e with
   | Num n -> Store.NumV n
   | Id x -> (
      (* Store.find x s *)
      match Store.find x s with
      | Store.NumV n -> Store.NumV n
      | Store.ClosureV (arg, e, s') -> Store.ClosureV (arg, e, s')
      | Store.FreezedV (e', s') -> interp e' s'
   )
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
   | RLetIn (x, e1, e2) -> (
         match interp e1 s with
         | ClosureV (x', e, s') ->
            let rec s'' = (x, Store.ClosureV (x', e, s'')) :: s' in
            interp e2 s''
         | _ -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp e1)
   )
   | App (e1, e2) -> (
      match interp e1 s with
      | Store.ClosureV (arg, e, s') -> (
         let freez = Store.FreezedV (e2, s) in
         interp e (Store.add arg freez s')
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