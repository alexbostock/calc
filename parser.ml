#use "lexer.ml"

exception ParseError

type nullary =
  | Expr2
  | Diff2
  | Prod2
  | Trig
  | Fact2

type unary =
  | Statement
  | Prod2
  | Trig
  | Trig2
  | Fact2

type binary =
  | Expr
  | Expr2
  | Diff
  | Diff2
  | Fact
  | Prod

type past =
  | N of nullary
  | U of unary * past
  | B of binary * past * past
  | F of float

let accept t = match !tokens with
  | Cons (x, xs) when x = t -> tokens := xs ()
  | _ -> raise ParseError

let accept_float () = match !tokens with
  | Cons (Float f, xs) -> tokens := xs (); f
  | _ -> raise ParseError

let next () = match !tokens with
  | Cons (x, xs) -> x
  | End -> raise ParseError

let rec s () =
  let expr = e ()
in
  accept Eof;
  U (Statement, expr)

and e () =
  let diff = d () in
  let expr2 = e2 () in
  B (Expr, diff, expr2)

and e2 () = match next () with
  | Plus ->
    accept Plus;
    let diff = d () in
    let expr2 = e2 () in
    B (Expr2, diff, expr2)
  | Eof -> N Expr2
  | _ -> raise ParseError

and d () =
  let prod = p () in
  let diff2 = d2 () in
  B (Diff, prod, diff2)

and d2 () = match next () with
  | Minus ->
    let prod = p () in
    let diff2 = d2 () in
    accept Minus; B (Diff2, prod, diff2)
  | Plus
  | Eof -> N Diff2
  | _ -> raise ParseError

and p () =
  let trig = t () in
  let prod2 = p2 () in
  B (Prod, trig, prod2)

and p2 () = match next () with
  | Times -> accept Times; U (Prod2, p ())
  | Plus
  | Minus
  | Eof -> N Prod2
  | _ -> raise ParseError

and t () = match next () with
  | Cos -> accept Cos; U (Trig, t ())
  | Float _ -> U (Trig2, f ())
  | _ -> raise ParseError

and f () =
  let flt = accept_float () in
  let fact2 = f2 () in
  B (Fact, F flt, fact2)

and f2 () = match next () with
  | Bang -> accept Bang; U (Fact2, f2 ())
  | Plus
  | Minus
  | Times
  | Eof -> N Fact2
  | _ -> raise ParseError

let past = s ()