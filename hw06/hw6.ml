 type expr =
 | Num of int
 | Add of expr * expr
 | Sub of expr * expr

type t =
 | NumV of int

let rec interp (e : Ast.expr) : Value.t  =
 match e with
 | Num n -> NumV n
 | Add (e1, e2) -> (
    match interp e1, interp e2 with
    | NumV n1, NumV n2 -> NumV (n1 + n2)
 )
 | Sub (e1, e2) -> (
    match interp e1, interp e2 with
    | NumV n1, NumV n2 -> NumV (n1 - n2)
 )
