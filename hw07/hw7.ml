
let rec interp (e : Ast.expr) (s : Store.t) : Store.value  =
   match e with
   | Num n -> Store.NumV n
   | Id x -> Store.find x s
   | Add (e1, e2) -> (
      match interp e1 s, interp e2 s with
      | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2)
   )
   | Sub (e1, e2) -> (
      match interp e1 s, interp e2 s with
      | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2)
   )
   | LetIn (x, e1, e2) -> 
      let v = interp e1 s in
      interp e2 (Store.add x v s)