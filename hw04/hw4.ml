type token = TI of int | TO of char
type state = Q0 | Q1 | Q2

let lex s =
  let rec lex' s state b1 b2 =
    match s with
    | [] -> (
        match state with
        | Q1 -> b2 @ [TI (int_of_string (String.of_seq (List.to_seq (List.rev b1))))]
        | Q2 -> b2 @ [TO (List.hd b1)]
        | _ -> failwith "Failed in Lexing"
      )
    | h::t -> (
      match h with
      | ' ' -> lex' t state b1 b2
      | '+' | '-' | '*' | '/' -> (
        match state with
        | Q1 -> lex' t Q2 [h] (b2 @ [TI (int_of_string (String.of_seq (List.to_seq (List.rev b1))))])
        | _ -> failwith "Failed in Lexing"
      )
      | '0' .. '9' -> (
        match state with
        | Q0 -> lex' t Q1 [h] b2
        | Q1 -> lex' t Q1 (h::b1) b2
        | Q2 -> lex' t Q1 [h] (b2 @ [TO (List.hd b1)])
      )
      | _ -> failwith "Failed in Lexing"
    )
  in lex' (List.of_seq (String.to_seq s)) Q0 [] []