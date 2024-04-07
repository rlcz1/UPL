(* Test code *)
let%test _ = Hw4.lex "1 * 361 - 89 + 2 / 56" = [TI 1; TO '*'; TI 361; TO '-'; TI 89; TO '+'; TI 2; TO '/'; TI 56]
let%test _ = Hw4.lex "1 + 2" = [TI 1; TO '+'; TI 2]
let%test _ = Hw4.lex "1 + 22 * " = [TI 1; TO '+'; TI 22; TO '*']