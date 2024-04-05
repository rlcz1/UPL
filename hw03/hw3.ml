let map f lst =
  let rec map' f lst acc =
    match lst with
    | [] -> List.rev acc
    | head :: tail -> map' f tail ((f head) :: acc)
  in
  map' f lst []

let rec fold_left f init lst =
  match lst with
  | [] -> init
  | head :: tail -> fold_left f (f init head) tail
