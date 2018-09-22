#use "past_to_ast.ml"

let rec fact x =
  let rec fact_acc x acc =
    if x <= 1.0 then acc else fact_acc (x -. 1.0) (acc *. x)
in
  fact_acc x 1.0

let rec eval = function
  | Sum (a, b) -> eval a +. eval b
  | Diff (a, b) -> eval a -. eval b
  | Prod (a, b) -> eval a *. eval b
  | Cos (a) -> cos (eval a)
  | Fact (a) -> fact (eval a)
  | Float (a) -> a