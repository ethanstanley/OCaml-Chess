exception Malformed

exception DoesNotExist

exception InvalidMove

exception MoveUnfeasible

type p_color
(** The type of color associated with a piece such as Black or White. *)

type piece
(** A representation of a piece such as King, Queen, Pawn, Knight, Rook,
    and Bishop and their corresponding color. *)

type position
(** A representation of a specific space on the board. For example,
    ('A', 1) is the bottom left space. *)

type board
(** A representation of the entire chess board consisting of 64 spaces
    and any pieces currently present and their respective positions. *)

type space
(** A representation of one of the 64 spaces on a chess board. Contains
    information about the space's position, whether it is a free space,
    and what piece is on the space if it is not free. *)

val to_position : string -> position
(** [to_position str ] takes in a string and returns the corresponding
    position type. Ex: "A, 1" -> ('A', 1) *)

val to_color : string -> p_color
(** [to_color str ] takes in a string and returns the corresponding
    color type. Ex: "White" -> White *)

val to_piece : string -> piece
(** [to_piece str ] takes in a string and returns the corresponding
    piece type. Ex: "Pawn Black" -> Pawn Black *)

val space_to_position_fst : space -> char

val space_to_position_snd : space -> int

val to_space : string -> bool -> string -> string -> space
(** [to_space] returns a space value with the given input components. *)

val spaces_of_json : Yojson.Basic.t -> space
(** [spaces_of_json j] takes in a json representation of a space and
    returns a space type. *)

val rows_of_json : Yojson.Basic.t list -> space list
(** [rows_of_json j] takes in a json representation of a list of spaces
    and returns an actual list of corresponding space types.*)

val from_json : Yojson.Basic.t -> board
(** [from_json j] returns the board of its corresponding json
    representation j. *)

val start_board : board
(** A representation of the initial board at the start of the game with
    all pieces on their rightful space and position. *)

val print_piece : piece -> string
(** [print_piece p] takes in a piece type p and returns its string
    representation. Example: Pawn White -> "♟︎" *)

val parse_str : string -> position
(** [parse_str] returns a position tuple or exception based on a string
    input ex) e4 -> ('E', 4).*)

val find_space : position -> board -> space
(** [find_space p b] returns the corresponding space of the given
    position and board. *)

val valid_space : position -> bool
(** [valid_space pos] returns true if the given space is a valid space
    on the board and false if not. *)

val is_valid_move_king : piece -> board -> position -> position -> bool
(** [is_valid_move_king p pos_1 pos_2] returns true if the given move
    for a king piece is valid and false otherwise. *)

val is_valid_move_knight :
  piece -> board -> position -> position -> bool
(** [is_valid_move_knight p pos_1 pos_2] returns true if the given move
    for a knight piece is valid and false otherwise. *)

val is_valid_move_bishop :
  piece -> board -> position -> position -> bool
(** [is_valid_move_bishop p pos_1 pos_2] returns true if the given move
    for a bishop piece is valid and false otherwise. *)

val is_valid_move_rook : piece -> board -> position -> position -> bool
(** [is_valid_move_rook p pos_1 pos_2] returns true if the given move
    for a rook piece is valid and false otherwise. *)

val is_valid_move_pawn : piece -> board -> position -> position -> bool
(** [is_valid_move_pawn p pos_1 pos_2] returns true if the given move
    for a pawn piece is valid and false otherwise. *)

val is_valid_move_queen : piece -> board -> position -> position -> bool
(** [is_valid_move_queen p pos_1 pos_2] returns true if the given move
    for a queen piece is valid and false otherwise. *)

val is_valid_move : board -> position -> position -> piece -> bool
(** [is_valid_move] returns a boolean determining whether a move from
    [pos_1] to [pos_2] is possible where str1 and str2 are string
    representations of squares. ex) e4 e5 -> true if the move is valid. *)

val parse_row : space list -> string
(** [parse_row r] returns the string reprsentation of the given space
    list. *)

val parse_board : board -> string list -> string list
(** [parse_board b l] returns the string representation of the entire
    board with the rows labeled with numbers 1 through 8.*)

val stringify : string list -> unit
(** [stringify l] adds a string of letters A through H to label the
    columns of the board. *)

val print_board : board -> unit
(** [print_board b] is the board in string format*)

val translate_to_spaces : board -> position -> position -> space * space

val same_team : space -> space -> bool
(** [same_team s1 s2] returns true if space 1 and space 2 contain pieces
    of the same color and false otherwise.*)

val move_away : space -> space
(** [move_away s] returns the updated original space without the piece
    that was on it.*)

val numlist : string list
(** A list containing the string numerical labels for each row of the
    board.*)

val find_king : board -> string -> space

val move_spaces :
  board -> position -> position -> p_color -> (board * piece) * string
(** Returns a board with position 1 moved into position 2*)

val in_check : board -> board -> string -> space -> bool * position

val find_king_moves : board -> p_color -> space -> space list