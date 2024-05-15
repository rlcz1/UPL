type t = (string * (string list * Ast.expr)) list
let empty: t = []

(* find: string -> t -> string list * Ast.expr *)
let rec find name t =
  match t with
  | [] -> failwith ("Unbound function: " ^ name)
  | (key, value) :: tl -> if key = name then value else find name tl

let add name args expr t =
  let add_filter = 
    List.filter (fun (key, _) -> key <> name) t in
  (name, (args, expr)) :: add_filter