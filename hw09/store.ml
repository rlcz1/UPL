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

  let add s v t =
    let add_filter = 
      List.filter (fun (key, _) -> key <> s) t in
    (s, v) :: add_filter
  