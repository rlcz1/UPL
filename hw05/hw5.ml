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

(* parse *)

type op = ADD | SUB | MUL | DIV
type expr = E of op * expr * expr | Num of int
type stack_elem = Token of token | Expr of expr | Op of op

let parse lst =
  let rec parse' lst stack =
    match lst with
    | [] -> (
      match stack with
      | [Expr expr] -> expr
      | [Expr e1; Op op; Expr e2] -> E (op, e2, e1)
      | _ -> failwith "Failed in Parsing1"
    )
    | TI n :: t -> (
      let stack' = Expr (Num n) :: stack in
      match stack' with
      | Expr e1 :: Op op :: Expr e2 :: tl -> (
        match op with
        | MUL | DIV -> parse' t (Expr (E (op, e2, e1)) :: tl)
        | ADD | SUB -> (
          match t with
          | TO c :: _ -> (
            match c with
            | '+' | '-' -> parse' t (Expr (E (op, e2, e1)) :: tl)
            | '*' | '/' -> parse' t stack'
            | _ -> failwith "Failed in Parsing"
          )
          | [] -> (
            match stack' with
            | [Expr e1; Op op; Expr e2] -> E (op, e2, e1)
            | _ -> failwith "Failed in Parsing"
          )
          | _ -> failwith "Failed in Parsing"
        )
      )
      | _ -> parse' t stack'
    )
    | TO c :: t -> (
      match c with
      | '+' -> parse' t (Op ADD :: stack)
      | '-' -> parse' t (Op SUB :: stack)
      | '*' -> parse' t (Op MUL :: stack)
      | '/' -> parse' t (Op DIV :: stack)
      | _ -> failwith "Failed in Parsing3"
    )
  in parse' lst []
