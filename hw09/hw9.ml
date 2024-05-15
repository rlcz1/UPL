(*
• interp_prog: Ast.prog -> Store.value
• interp_def: Ast.fundef -> Fstore.t -> Fstore.t
• interp_expr: Ast.expr -> Fstore.t -> Store.t -> Store.value *)

(* type expr = Num of int
  | Id of string
  | Add of expr * expr
  | Sub of expr * expr
  | LetIn of string * expr * expr
  | Call of string * expr list
type fundef = FunDef of string * string list * expr
type prog = Prog of fundef list * expr *)


let interp_def (d : Ast.fundef) (ft : Fstore.t) : Fstore.t =
   match d with
   | FunDef (name, args, expr) -> Fstore.add name args expr ft

let rec interp_expr (e: Ast.expr) (ft: Fstore.t) (t:Store.t) : Store.value =
   match e with
   | Num n -> Store.NumV n
   | Id x -> Store.find x t
   | Add (e1, e2) -> (
      match (interp_expr e1 ft t, interp_expr e2 ft t) with
      | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 + n2)
   )
   | Sub (e1, e2) -> (
      match (interp_expr e1 ft t, interp_expr e2 ft t) with
      | Store.NumV n1, Store.NumV n2 -> Store.NumV (n1 - n2)
   )
   | LetIn (x, e1, e2) ->
      let e1v = interp_expr e1 ft t in
      interp_expr e2 ft (Store.add x e1v t)
   | Call (name, args) ->
      (* 2 각arg의 value list *)
      let argV_list = List.map (fun arg -> interp_expr arg ft t) args in 
      let (arg_names, body) = Fstore.find name ft in
      let c_store = 
         List.fold_left2 (fun acc name value -> Store.add name value acc) t arg_names argV_list in
      interp_expr body ft c_store

let interp_prog (prog : Ast.prog) : Store.value =
   match prog with
   | Prog (defs, expr) ->
      let ft = 
         List.fold_left (fun acc def -> interp_def def acc) Fstore.empty defs in
      interp_expr expr ft Store.empty
