type value = NumV of int
type t = (string * value) list
let empty = [] 

let rec find s t =
  match t with
  | [] -> failwith ("Free identifier: " ^ s)
  | (s', v) :: tl -> (
    if s = s' then v 
    else find s tl
  )

let rec add s v t =
  match t with
  | [] -> [(s, v)]
  | (s', v') :: tl -> (
    if s = s' then (s, v) :: tl
    else (s', v') :: add s v tl
  )


(* 
Store.add "x" (NumV 5) [("y", NumV 1); ("x", NumV 3)];;
*)
