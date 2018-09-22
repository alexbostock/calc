exception LexError

type token =
  | Eof
  | Plus
  | Minus
  | Times
  | Cos
  | Bang
  | Float of float

type 'a stream =
  | End
  | Cons of 'a * (unit -> 'a stream)

let file = "in.calc"
let ic = open_in file

let rec read_char () = try
  Cons (input_char ic, read_char)
with End_of_file -> close_in ic; End

let chars = ref (read_char ())

let rec lex () =
  let rec lex_cos t = match t () with
    | Cons ('o', tl) -> (match tl () with
        | Cons ('s', tail) -> chars := tail (); Cons (Cos, lex)
        | _ -> raise LexError)
    | _ -> raise LexError
  and is_digit c = Char.code c >= 48 && Char.code c < 58
  and change_state buf next_state = match !chars with
    | Cons (c, cs) ->
      chars := cs ();
      Buffer.add_char buf c;
      lex_float buf next_state
    | End -> raise LexError (* Should never occur *)
  and float_from_buf buf = Float (float_of_string (Buffer.contents buf))
  and lex_float buf state = match state, !chars with
    | 0, Cons (c, cs) when is_digit c -> change_state buf 2
    | 0, Cons ('~', cs) -> change_state buf 1
    | 0, _ -> raise LexError

    | 1, Cons (c, cs) when is_digit c -> change_state buf 2
    | 1, _ -> raise LexError

    | 2, Cons (c, cs) when is_digit c -> change_state buf 2
    | 2, Cons ('.', cs) -> change_state buf 3
    | 2, Cons ('e', cs) -> change_state buf 5
    | 2, _ -> float_from_buf buf

    | 3, Cons (c, cs) when is_digit c -> change_state buf 4
    | 3, _ -> raise LexError

    | 4, Cons (c, cs) when is_digit c -> change_state buf 4
    | 4, Cons ('e', cs) -> change_state buf 5
    | 4, _ -> float_from_buf buf

    | 5, Cons (c, cs) when is_digit c -> change_state buf 7
    | 5, Cons ('+', cs) -> change_state buf 6
    | 5, Cons ('-', cs) -> change_state buf 6
    | 5, Cons ('~', cs) -> change_state buf 6
    | 5, _ -> raise LexError

    | 6, Cons (c, cs) when is_digit c -> change_state buf 7
    | 6, _ -> raise LexError

    | 7, Cons (c, cs) when is_digit c -> change_state buf 7
    | 7, _ -> float_from_buf buf
    | _, _ -> raise LexError
in
  match !chars with
  | End -> Cons (Eof, fun () -> End)
  | Cons ('+', tl) -> chars := tl (); Cons (Plus, lex)
  | Cons ('-', tl) -> chars := tl (); Cons (Minus, lex)
  | Cons ('*', tl) -> chars := tl (); Cons (Times, lex)
  | Cons ('!', tl) -> chars := tl (); Cons (Bang, lex)
  | Cons ('c', tl) -> lex_cos tl
  | Cons (c, tl) ->
    if c = ' ' || c = '\012' || c = '\n' || c = '\r' || c = '\t' then (
      chars := tl ();
      lex ()
    ) else
      Cons (lex_float (Buffer.create 100) 0, lex)

let tokens = ref (lex ())