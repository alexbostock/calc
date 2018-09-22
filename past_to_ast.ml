#use "parser.ml"

exception AstException

type ast =
  | Sum of ast * ast
  | Diff of ast * ast
  | Prod of ast * ast
  | Cos of ast
  | Fact of ast
  | Float of float

let rec past_to_ast = function
  | U (Statement, expr) -> past_to_ast expr
  | B (Expr, d, e2) -> combine (fun (x, y) -> Sum (x, y)) (collect_e2s e2 [past_to_ast d])
  | B (Diff, p, d2) -> combine (fun (x, y) -> Prod (x, y)) (collect_d2s d2 [past_to_ast p])
  | B (Prod, t, N Prod2) -> past_to_ast t
  | B (Prod, t, U (Prod2, p)) -> Prod (past_to_ast t, past_to_ast p)
  | U (Trig, t) -> Cos (past_to_ast t)
  | U (Trig2, f) -> past_to_ast f
  | B (Fact, F f, f2) -> collect_facts f2 f
  | _ -> raise AstException

and collect_e2s e l = match e, l with
  | N Expr2, l -> l
  | B (Expr2, d, e2), l -> collect_e2s e2 (past_to_ast d :: l)
  | _, _ -> raise AstException

and collect_d2s d l = match d, l with
  | N Diff2, l -> l
  | B (Diff2, p, d2), l -> collect_d2s d2 (past_to_ast p :: l)
  | _, _ -> raise AstException

and combine f = function
  | (x::[]) -> x
  | (x::xs) -> f (combine f xs, x)
  | _ -> raise AstException

and collect_facts f2 n = match f2 with
  | N Fact2 -> Float n
  | U (Fact2, f) -> Fact (collect_facts f n)
  | _ -> raise AstException