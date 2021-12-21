(** state.mli*)

type state
(** The type for the current state of the board *)

val current_board : state -> Board.board
(** [current_board st ] takes in a state and returns the corresponding
    board for the state. *)

val create_state :
  Board.board ->
  Board.piece list ->
  Board.piece list ->
  Board.p_color ->
  state
(** [create_state bd l1 l2 color] takes in a board, set of white pieces
    captured, set of black pieces captured, and current player's turn
    and stores that in a record that is the state of the game
    representing all of that.*)

val white_taken : state -> Board.piece list
(** [white_taken st ] takes in a state and returns the corresponding
    list of white pieces captured in a piece list. *)

val black_taken : state -> Board.piece list
(** [black_taken st ] takes in a state and returns the corresponding
    list of black pieces captured in a piece list. *)

val cur_turn : state -> Board.p_color
(** [cur_turn st ] takes in a state and returns the corresponding color
    for the players turn it is. *)

type result =
  | Legal of state
  | Illegal  (** The type for whether a state position is legal or not*)
