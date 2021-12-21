type object_phrase
(** List of tuples representing moves. *)

type command
(** Type representing a move with object phrase or invalid move *)

val create_command : string -> string -> command
(** Creates command type based on two input strings*)

val move_it :
  command ->
  Board.board ->
  Board.p_color ->
  (Board.board * Board.piece) * string
(** Returns the movement for a move from Board state 1 and completes it
    and sends an updated board back*)