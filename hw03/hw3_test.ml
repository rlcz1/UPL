(* Test code *)

(* map *)
let%test _ = Hw3.map (fun x -> x + 1) [1;2;3] = [2;3;4]
let%test _ = Hw3.map (fun x -> x * 2) [1;2;3] = [2;4;6]
let%test _ = Hw3.map (fun x -> (string_of_int x) ^ "_string") [1;2]
= ["1_string"; "2_string"]

(* fold_left *)
let%test _ = Hw3.fold_left (fun acc x -> acc + x) 0 [1;2;3] = 6
let%test _ = Hw3.fold_left (fun i x -> x + i) 0 [] = 0
let%test _ = Hw3.fold_left (fun i x -> x + i) 0 [1] = 1