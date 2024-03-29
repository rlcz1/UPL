(* +, -, *, / 연산 지원하는 calc 함수 구현
함수 이름은 calc
이외의 연산자가 들어오면 failwith “Unsupported operation”
calc : char -> int -> int -> int
예시) calc ‘+’ 1 3 = 4
예시) calc ‘%’ 1 3 = Failure (“Unsupported operation”) *)
let calc x y z =
  if x = '+' then y + z
  else if x = '-' then y - z
  else if x = '*' then y * z
  else if x = '/' then y / z
  else failwith "Unsupported operation"