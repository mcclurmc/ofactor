(* Test cases to be parsed by ofactor *)

let f a b =
  let g c d =
    match c with
    | true -> a + d
    | false -> b + d in
  let rec h e f =
    e + f + b in
  g true (h a b)

let string_of_pos (pos : Lexing.position) =
  Printf.sprintf "l%dc%d"
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
