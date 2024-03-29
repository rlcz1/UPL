(* 하나의 정수를 받아 피보나치 수를 계산하여 반환하는 함수 *)
let rec fib n =
  match n with
  | 0 -> 1
  | 1 -> 1
  | i -> fib (i - 1) + fib (i - 2) 

(* 하나의 정수를 받아 팩토리얼을 계산하여 반환 *)
let rec factorial n =
  if n < 0 then -1
  else if n = 0 then 1
  else n * factorial (n - 1) 


(* 하나의 정수를 0부터 해당 정수까지의 모든 정수의 합을 반환 *)
let rec acc n =
  if n < 0 then acc (n + 1) + n
  else if n = 0 then 0
  else n + acc (n - 1) 

(* 정수 2개를 받아 첫 번째 정수를 두 번째 정수만큼 곱한 값을 반환 *)
let rec pow x n =
  if x < 0 then 0
  else if n < 0 then 0
  else if n = 0 then 1
  else x * pow x (n - 1)