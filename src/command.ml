open Board

type position = Board.position

type object_phrase = position list

type command =
  | Move of object_phrase
  | InvalidMove

let is_not_on_board str =
  let r = String.get str 0 in
  let c = String.get str 1 in
  if
    Char.code (Char.uppercase_ascii r) > 64
    && Char.code (Char.uppercase_ascii r) < 73
    && Char.code c > 48
    && Char.code c < 57
  then false
  else true

let create_command str1 str2 =
  if
    String.length str1 != 2
    || String.length str2 != 2
    || is_not_on_board str1 || is_not_on_board str2
  then InvalidMove
  else Move [ parse_str str1; parse_str str2 ]

let move_it cmd board (color : p_color) =
  match cmd with
  | InvalidMove -> ((board, to_piece "none"), "invalid")
  | Move [ pos1; pos2 ] ->
      let mvd_spaces = move_spaces board pos1 pos2 color in
      if snd mvd_spaces = "in checkmate" then
        (fst mvd_spaces, "in checkmate")
      else if snd mvd_spaces = "in check" then
        (fst mvd_spaces, "in check")
      else if snd mvd_spaces = "put into check" then
        (fst mvd_spaces, "put into check")
      else if snd mvd_spaces = "not turn" then
        (fst mvd_spaces, "wrong turn")
      else if snd mvd_spaces = "clean" then (fst mvd_spaces, "clean")
      else if snd mvd_spaces = "same team" then
        (fst mvd_spaces, "same team")
      else (fst mvd_spaces, "invalid move")
  | _ -> ((board, to_piece "none"), "invalid")
