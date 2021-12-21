open OUnit2
open Chess
open Board
open Command
open State

let start_board =
  from_json (Yojson.Basic.from_file "./boards/start_board.json")

let test_board =
  from_json (Yojson.Basic.from_file "./boards/test_board.json")

let space_A_1 = to_space "A, 1" false "Rook White" "White"

let space_A_2 = to_space "A, 2" false "Pawn White" "White"

let space_A_3 = to_space "A, 3" true "None" "None"

let space_B_2 = to_space "B, 2" false "Pawn White" "White"

let open_space_A_2 = to_space "A, 2" true "None" "None"

let space_H_8 = to_space "H, 8" false "Rook Black" "Black"

let space_E_5 = to_space "E, 5" true "None" "None"

let space_E_1 = to_space "E, 1" false "King White" "White"

let space_E_8 = to_space "E, 8" false "King Black" "Black"

let valid_parse_str_test
    (name : string)
    (str : string)
    (expected_output : position) =
  name >:: fun _ -> assert_equal expected_output (parse_str str)

let invalid_parse_str_test
    (name : string)
    (str : string)
    (expected_output : exn) =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> parse_str str)

let valid_find_space_test
    (name : string)
    (pos : position)
    (board : board)
    (expected_output : space) =
  name >:: fun _ -> assert_equal expected_output (find_space pos board)

let invalid_find_space_test
    (name : string)
    (pos : position)
    (board : board)
    (expected_output : exn) =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> find_space pos board)

let valid_space_test
    (name : string)
    (pos : position)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (valid_space pos)

let is_valid_move_king_test
    (name : string)
    (piece : piece)
    (board : board)
    (start_space : position)
    (end_space : position)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_valid_move_king piece board start_space end_space)

let is_valid_move_knight_test
    (name : string)
    (piece : piece)
    (board : board)
    (start_space : position)
    (end_space : position)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_valid_move_knight piece board start_space end_space)

let is_valid_move_bishop_test
    (name : string)
    (piece : piece)
    (board : board)
    (start_space : position)
    (end_space : position)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_valid_move_bishop piece board start_space end_space)

let is_valid_move_pawn_test
    (name : string)
    (piece : piece)
    (board : board)
    (start_space : position)
    (end_space : position)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_valid_move_pawn piece board start_space end_space)

let is_valid_move_rook_test
    (name : string)
    (piece : piece)
    (board : board)
    (start_space : position)
    (end_space : position)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_valid_move_rook piece board start_space end_space)

let is_valid_move_queen_test
    (name : string)
    (piece : piece)
    (board : board)
    (start_space : position)
    (end_space : position)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_valid_move_queen piece board start_space end_space)

let is_valid_move_test
    (name : string)
    (board : board)
    (pos_1 : position)
    (pos_2 : position)
    (piece : piece)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (is_valid_move board pos_1 pos_2 piece)

let translate_to_spaces_test
    (name : string)
    (board : board)
    (pos_1 : position)
    (pos_2 : position)
    (expected_output : space * space) =
  name >:: fun _ ->
  assert_equal expected_output (translate_to_spaces board pos_1 pos_2)

let same_team_test
    (name : string)
    (space_1 : space)
    (space_2 : space)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (same_team space_1 space_2)

let move_away_test
    (name : string)
    (space : space)
    (expected_output : space) =
  name >:: fun _ -> assert_equal expected_output (move_away space)

let find_king_test
    (name : string)
    (board : board)
    (color : string)
    (expected_output : space) : test =
  name >:: fun _ -> assert_equal expected_output (find_king board color)

let in_check_test
    (name : string)
    (board : board)
    (search_board : board)
    (color : string)
    (king_space : space)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (fst (in_check board search_board color king_space))

let board_test =
  [
    valid_parse_str_test "Space (A, 1) entered with lowercase a" "a1"
      (to_position "A, 1");
    valid_parse_str_test "Space (A, 1) entered with uppercase A" "A1"
      (to_position "A, 1");
    valid_parse_str_test "Space (H, 8) entered with lowercase h" "h8"
      (to_position "H, 8");
    valid_parse_str_test "Space (H, 8) entered with uppercase H" "H8"
      (to_position "H, 8");
    invalid_parse_str_test "Invalid string with a length greater than 2"
      "a10" Malformed;
    invalid_parse_str_test "Invalid string with a length less than 2"
      "a" Malformed;
    invalid_parse_str_test "Invalid empty string" "" Malformed;
    valid_find_space_test "Valid space (A, 1)" (to_position "A, 1")
      start_board space_A_1;
    valid_find_space_test "Valid space (H, 8)" (to_position "H, 8")
      start_board space_H_8;
    valid_find_space_test "Valid space (E, 5)" (to_position "E, 5")
      start_board space_E_5;
    invalid_find_space_test
      "Invalid space with letter out of bounds of A-H"
      (to_position "J, 1") start_board DoesNotExist;
    invalid_find_space_test "Invalid space with number less than 1-8"
      (to_position "A, 0") start_board DoesNotExist;
    invalid_find_space_test "Invalid space with number greater than 8"
      (to_position "A, 9") start_board DoesNotExist;
    invalid_find_space_test
      "Invalid space with letter out of bounds of A-H and number not \
       in range 1-8"
      (to_position "M, 9") start_board DoesNotExist;
    valid_space_test "Invalid space with number less than 1."
      (to_position "A, 0") false;
    valid_space_test "Invalid space with number greater than 8"
      (to_position "A, 9") false;
    valid_space_test "Invalid space with letter greater than H"
      (to_position "I, 1") false;
    valid_space_test "Invalid space with non letter character"
      (to_position "!, 1") false;
    valid_space_test "Valid corner case space A 1" (to_position "A, 1")
      true;
    valid_space_test "Valid corner case space H 8" (to_position "H, 8")
      true;
    valid_space_test "Valid space in middle of board"
      (to_position "E, 4") true;
    is_valid_move_king_test
      "A king moving one open space forward is valid."
      (to_piece "King Black") start_board (to_position "A, 1")
      (to_position "A, 2") true;
    is_valid_move_king_test
      "A king moving one open space to the right is valid."
      (to_piece "King Black") start_board (to_position "A, 1")
      (to_position "B, 1") true;
    is_valid_move_king_test
      "A king moving one open space diagonally up and to the right is \
       valid."
      (to_piece "King Black") start_board (to_position "A, 1")
      (to_position "B, 2") true;
    is_valid_move_king_test
      "A king moving one open space to the left is valid."
      (to_piece "King Black") start_board (to_position "H, 8")
      (to_position "G, 8") true;
    is_valid_move_king_test
      "A king moving one open space backwards is valid."
      (to_piece "King Black") start_board (to_position "H, 8")
      (to_position "H, 7") true;
    is_valid_move_king_test
      "A king moving one open space diagonally back and to the left is \
       valid."
      (to_piece "King Black") start_board (to_position "H, 8")
      (to_position "G, 7") true;
    is_valid_move_king_test
      "A king moving one open space diagonally up and to the left is \
       valid."
      (to_piece "King Black") start_board (to_position "H, 1")
      (to_position "G, 2") true;
    is_valid_move_king_test
      "A king moving one open space diagonally back and to the right \
       is valid."
      (to_piece "King White") start_board (to_position "A, 8")
      (to_position "B, 7") true;
    is_valid_move_king_test "A king can't move two spaces"
      (to_piece "King Black") start_board (to_position "A, 1")
      (to_position "A, 3") false;
    is_valid_move_knight_test
      "Valid: Moving knight up 1 and to the left 2"
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "C, 5") true;
    is_valid_move_knight_test
      "Valid: Moving knight up 2 and to the left 1"
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "D, 6") true;
    is_valid_move_knight_test
      "Valid: Moving knight down 1 and to the left 2"
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "C, 3") true;
    is_valid_move_knight_test
      "Valid: Moving knight down 2 and to the left 1"
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "D, 2") true;
    is_valid_move_knight_test
      "Valid: Moving knight down 2 and to the right 1 "
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "F, 2") true;
    is_valid_move_knight_test
      "Valid: Moving knight up 2 and to the right 1"
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "F, 6") true;
    is_valid_move_knight_test
      "Valid: Moving knight down 1 and to the right 2 "
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "G, 3") true;
    is_valid_move_knight_test
      "Valid: Moving knight up 1 and to the right 2"
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "G, 5") true;
    is_valid_move_knight_test "Knight cannot move one space"
      (to_piece "Knight Black")
      start_board (to_position "E, 4") (to_position "E, 5") false;
    is_valid_move_bishop_test
      "Valid: Moving Bishop diagonally up and to the left."
      (to_piece "Bishop White")
      start_board (to_position "C, 4") (to_position "A, 6") true;
    is_valid_move_bishop_test
      "Valid: Moving Bishop diagonally down and to the left."
      (to_piece "Bishop White")
      start_board (to_position "C, 4") (to_position "A, 2") true;
    is_valid_move_bishop_test
      "Valid: Moving Bishop diagonally down and to the right."
      (to_piece "Bishop White")
      start_board (to_position "C, 6") (to_position "F, 3") true;
    is_valid_move_bishop_test
      "Valid: Moving Bishop diagonally up and to the right."
      (to_piece "Bishop White")
      start_board (to_position "A, 3") (to_position "D, 6") true;
    is_valid_move_bishop_test "Invalid: Bishop cannot move vertically."
      (to_piece "Bishop White")
      start_board (to_position "C, 4") (to_position "C, 5") false;
    is_valid_move_bishop_test
      "Invalid: Bishop cannot move horizontally."
      (to_piece "Bishop White")
      start_board (to_position "C, 4") (to_position "D, 4") false;
    is_valid_move_pawn_test
      "A Black Pawn at starting position can move two spaces down."
      (to_piece "Pawn Black") start_board (to_position "A, 7")
      (to_position "A, 5") true;
    is_valid_move_pawn_test
      "A Black Pawn at starting position can move one space down."
      (to_piece "Pawn Black") start_board (to_position "A, 7")
      (to_position "A, 6") true;
    is_valid_move_pawn_test
      "A Black Pawn at starting position cannot move three spaces down."
      (to_piece "Pawn Black") start_board (to_position "A, 7")
      (to_position "A, 4") false;
    is_valid_move_pawn_test
      "A Black Pawn not at starting position cannot move two spaces \
       down."
      (to_piece "Pawn Black") start_board (to_position "B, 6")
      (to_position "B, 4") false;
    is_valid_move_pawn_test "A Black Pawn cannot move horizontally."
      (to_piece "Pawn Black") start_board (to_position "B, 6")
      (to_position "C, 6") false;
    is_valid_move_pawn_test "A Black Pawn cannot move backwards."
      (to_piece "Pawn Black") start_board (to_position "B, 6")
      (to_position "B, 7") false;
    is_valid_move_pawn_test
      "A White Pawn at starting position can move two spaces up."
      (to_piece "Pawn White") start_board (to_position "A, 2")
      (to_position "A, 4") true;
    is_valid_move_pawn_test
      "A White Pawn at starting position can move one space up."
      (to_piece "Pawn White") start_board (to_position "A, 2")
      (to_position "A, 3") true;
    is_valid_move_pawn_test
      "A White Pawn at starting position cannot move three spaces up."
      (to_piece "Pawn White") start_board (to_position "A, 2")
      (to_position "A, 5") false;
    is_valid_move_pawn_test
      "A White Pawn not at starting position cannot move two spaces up."
      (to_piece "Pawn White") start_board (to_position "B, 3")
      (to_position "B, 5") false;
    is_valid_move_pawn_test "A White Pawn cannot move horizontally."
      (to_piece "Pawn White") start_board (to_position "B, 3")
      (to_position "A, 3") false;
    is_valid_move_pawn_test "A White Pawn cannot move backwards."
      (to_piece "Pawn White") start_board (to_position "B, 3")
      (to_position "B, 2") false;
    is_valid_move_pawn_test
      "A white pawn can capture black pawn diagonally right"
      (to_piece "Pawn White") test_board (to_position "D, 4")
      (to_position "E, 5") true;
    is_valid_move_pawn_test
      "A white pawn can capture black pawn diagonally left"
      (to_piece "Pawn White") test_board (to_position "D, 4")
      (to_position "C, 5") true;
    is_valid_move_pawn_test
      "A black pawn can capture white pawn diagonally left"
      (to_piece "Pawn Black") test_board (to_position "E, 5")
      (to_position "D, 4") true;
    is_valid_move_pawn_test
      "A black pawn can capture white pawn diagonally right"
      (to_piece "Pawn Black") test_board (to_position "C, 5")
      (to_position "D, 4") true;
    is_valid_move_rook_test "Valid: Moving a rook to the left."
      (to_piece "Rook White") start_board (to_position "E, 4")
      (to_position "A, 4") true;
    is_valid_move_rook_test "Valid: Moving Rook to the right."
      (to_piece "Rook White") start_board (to_position "E, 4")
      (to_position "H, 4") true;
    is_valid_move_rook_test "Valid: Moving Rook forward."
      (to_piece "Rook White") start_board (to_position "A, 3")
      (to_position "A, 6") true;
    is_valid_move_rook_test "Valid: Moving Rook backwards."
      (to_piece "Rook White") start_board (to_position "A, 6")
      (to_position "A, 3") true;
    is_valid_move_rook_test
      "Invalid: Rook cannot move diagonally up and to the right."
      (to_piece "Rook White") start_board (to_position "E, 4")
      (to_position "F, 5") false;
    is_valid_move_rook_test
      "Invalid: Rook cannot move diagonally up and to the left."
      (to_piece "Rook White") start_board (to_position "E, 4")
      (to_position "D, 5") false;
    is_valid_move_rook_test
      "Invalid: Rook cannot move diagonally down and to the right."
      (to_piece "Rook White") start_board (to_position "E, 4")
      (to_position "F, 3") false;
    is_valid_move_rook_test
      "Invalid: Rook cannot move diagonally down and to the left."
      (to_piece "Rook White") start_board (to_position "E, 4")
      (to_position "D, 3") false;
    is_valid_move_queen_test
      "Valid: Moving Queen to the left horizontally."
      (to_piece "Queen Black")
      start_board (to_position "E, 4") (to_position "A, 4") true;
    is_valid_move_queen_test
      "Valid: Moving Queen to the right horizontally."
      (to_piece "Queen Black")
      start_board (to_position "E, 4") (to_position "H, 4") true;
    is_valid_move_queen_test "Valid: Moving Queen forward."
      (to_piece "Queen Black")
      start_board (to_position "E, 3") (to_position "E, 6") true;
    is_valid_move_queen_test "Valid: Moving Queen backwards."
      (to_piece "Queen Black")
      start_board (to_position "E, 6") (to_position "E, 3") true;
    is_valid_move_queen_test
      "Valid: Moving Queen diagonally up and to the left."
      (to_piece "Queen Black")
      start_board (to_position "E, 4") (to_position "C, 6") true;
    is_valid_move_queen_test
      "Valid: Moving Queen diagonally up and to the right."
      (to_piece "Queen Black")
      start_board (to_position "E, 4") (to_position "G, 6") true;
    is_valid_move_queen_test
      "Valid: Moving Queen diagonally down and to the left."
      (to_piece "Queen Black")
      start_board (to_position "F, 6") (to_position "C, 3") true;
    is_valid_move_queen_test
      "Valid: Moving Queen diagonally down and to the right."
      (to_piece "Queen Black")
      start_board (to_position "B, 6") (to_position "E, 3") true;
    is_valid_move_test
      "A White Rook at starting state cannot move\n\
      \      if\n\
      \  its path is blocked." start_board (to_position "A, 1")
      (to_position "A, 3") (to_piece "Rook White") false;
    is_valid_move_test
      "A Black Rook at starting state cannot move\n\
      \      if\n\
      \  its path is blocked." start_board (to_position "A, 8")
      (to_position "A, 6") (to_piece "Rook Black") false;
    is_valid_move_test
      "A White Bishop at starting state cannot\n\
      \      move\n\
      \  if its path is blocked." start_board (to_position "C, 1")
      (to_position "A, 3")
      (to_piece "Bishop White")
      false;
    is_valid_move_test
      "A White Bishop at starting state cannot\n\
      \      move\n\
      \  if its path is blocked." start_board (to_position "C, 1")
      (to_position "E, 3")
      (to_piece "Bishop White")
      false;
    is_valid_move_test
      "A White Queen at starting state cannot move\n\
      \      if\n\
      \  its path is blocked." start_board (to_position "D, 1")
      (to_position "B, 3")
      (to_piece "Queen White")
      false;
    is_valid_move_test
      "A White Queen at starting state cannot move\n\
      \      if\n\
      \  its path is blocked." start_board (to_position "D, 1")
      (to_position "F, 3")
      (to_piece "Queen White")
      false;
    is_valid_move_test
      "A White Queen at starting state cannot move\n\
      \      if\n\
      \  its path is blocked." start_board (to_position "D, 1")
      (to_position "D, 3")
      (to_piece "Queen White")
      false;
    translate_to_spaces_test
      "Returns the two spaces on the starting board corresponding to \
       positions ('A', 2) and ('A', 3)"
      start_board (to_position "A, 2") (to_position "A, 3")
      (space_A_2, space_A_3);
    same_team_test
      "Spaces (A, 2) and (A, 3) do not occupy pieces of the same color \
       on the start board."
      space_A_2 space_A_3 false;
    same_team_test
      "Spaces (A, 2) and (B, w) occupy pieces of the same color on the \
       start board."
      space_A_2 space_B_2 true;
    move_away_test
      "Space (A, 2) is now open with no piece after the White Pawn \
       moves off it."
      space_A_2 open_space_A_2;
    find_king_test "Find starting white king" start_board "White"
      space_E_1;
    find_king_test "Find starting black king" start_board "Black"
      space_E_8;
  ]

let suite = "test suite for A2" >::: List.flatten [ board_test ]

let _ = run_test_tt_main suite
