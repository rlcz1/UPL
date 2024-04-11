(* Test code *)
let%test _ = Hw5.parse(Hw5.lex "1+2*3") = E (ADD, Num 1, E (MUL, Num 2, Num 3))
let%test _ = Hw5.parse(Hw5.lex "1*2+3") = E (ADD, E (MUL, Num 1, Num 2), Num 3)
let%test _ = Hw5.parse(Hw5.lex "1+2+3") = E (ADD, E (ADD, Num 1, Num 2), Num 3)
let%test _ = Hw5.parse(Hw5.lex "1+2") = E (ADD, Num 1, Num 2)
let%test _ = Hw5.parse(Hw5.lex "2/1") = E (DIV, Num 2, Num 1)
