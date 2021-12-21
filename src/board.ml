open Array
open Yojson.Basic.Util

exception Malformed

exception DoesNotExist

exception InvalidMove

exception MoveUnfeasible

type p_color =
  | White
  | Black
  | None

type piece =
  | Pawn of p_color
  | King of p_color
  | Queen of p_color
  | Knight of p_color
  | Bishop of p_color
  | Rook of p_color
  | None

type position = char * int

type space = {
  position_id : position;
  is_open : bool;
  piece_id : piece;
  color : p_color;
}

type board = space list list

let to_position str : position =
  let letter = String.get str 0 in
  let num = int_of_string (String.sub str 3 1) in
  (letter, num)

let to_color s =
  match s with
  | "Black" -> Black
  | "White" -> White
  | _ -> None

let color_to_str c =
  match c with
  | Black -> "Black"
  | White -> "White"
  | None -> ""

let to_piece = function
  | "Rook White" -> Rook White
  | "Rook Black" -> Rook Black
  | "Bishop White" -> Bishop White
  | "Bishop Black" -> Bishop Black
  | "Knight White" -> Knight White
  | "Knight Black" -> Knight Black
  | "Queen White" -> Queen White
  | "Queen Black" -> Queen Black
  | "King White" -> King White
  | "King Black" -> King Black
  | "Pawn White" -> Pawn White
  | "Pawn Black" -> Pawn Black
  | _ -> None

let to_space
    (id : string)
    (avail : bool)
    (piece : string)
    (color : string) =
  {
    position_id = to_position id;
    is_open = avail;
    piece_id = to_piece piece;
    color = to_color color;
  }

let defaultempty = to_space "Z, 0" false "none" "none"

let spaces_of_json j =
  {
    position_id = j |> member "position_id" |> to_string |> to_position;
    is_open = j |> member "is_open" |> to_bool;
    piece_id = j |> member "piece_id" |> to_string |> to_piece;
    color = j |> member "color" |> to_string |> to_color;
  }

let rows_of_json j : space list = j |> List.map spaces_of_json

let from_json json =
  json |> member "rows" |> to_list |> List.map to_list
  |> List.map rows_of_json

let start_board : board =
  from_json (Yojson.Basic.from_file "./boards/start_board.json")

let print_piece = function
  | Pawn White -> "♟︎"
  | Pawn Black -> "♙"
  | Pawn None -> " "
  | King White -> "♚"
  | King Black -> "♔"
  | King None -> " "
  | Queen White -> "♛"
  | Queen Black -> "♕"
  | Queen None -> " "
  | Knight White -> "♞"
  | Knight Black -> "♘"
  | Knight None -> " "
  | Bishop White -> "♝"
  | Bishop Black -> "♗"
  | Bishop None -> " "
  | Rook White -> "♜"
  | Rook Black -> "♖"
  | Rook None -> " "
  | None -> " "

let parse_str str =
  if String.length str != 2 then raise Malformed
  else
    (Char.uppercase_ascii str.[0], int_of_string (String.make 1 str.[1]))

let space_to_position_fst sp = fst sp.position_id

let space_to_position_snd sp = snd sp.position_id

let rec find_space (square : position) (board : board) =
  match board with
  | [] -> raise DoesNotExist
  | h :: t -> (
      match List.find_opt (fun x -> x.position_id = square) h with
      | None -> find_space square t
      | Some sp -> sp)

let valid_space (space : position) =
  if
    snd space < 1 || snd space > 8 || fst space < 'A' || fst space > 'H'
  then false
  else true

let is_valid_move_king piece board start_space end_space =
  let sp1 = find_space start_space board in
  let sp2 = find_space end_space board in
  if
    sp1.piece_id == King White
    || (sp1.piece_id == King Black && sp2.piece_id == King White)
    || sp2.piece_id == King Black
  then false
  else if
    Int.abs (Char.code (fst end_space) - Char.code (fst start_space))
    = 1
    && Int.abs (snd end_space - snd start_space) = 1
    || Int.abs (Char.code (fst end_space) - Char.code (fst start_space))
       = 0
       && Int.abs (snd end_space - snd start_space) = 1
    || Int.abs (Char.code (fst end_space) - Char.code (fst start_space))
       = 1
       && Int.abs (snd end_space - snd start_space) = 0
  then true
  else false

let is_valid_move_knight
    piece
    board
    (start_space : position)
    (end_space : position) =
  if
    Char.code (fst end_space) = Char.code (fst start_space) + 2
    && snd end_space = snd start_space + 1
    || Char.code (fst end_space) = Char.code (fst start_space) + 1
       && snd end_space = snd start_space + 2
    || Char.code (fst end_space) = Char.code (fst start_space) - 1
       && snd end_space = snd start_space + 2
    || Char.code (fst end_space) = Char.code (fst start_space) - 2
       && snd end_space = snd start_space + 1
    || Char.code (fst end_space) = Char.code (fst start_space) - 2
       && snd end_space = snd start_space - 1
    || Char.code (fst end_space) = Char.code (fst start_space) - 1
       && snd end_space = snd start_space - 2
    || Char.code (fst end_space) = Char.code (fst start_space) + 1
       && snd end_space = snd start_space - 2
    || Char.code (fst end_space) = Char.code (fst start_space) + 2
       && snd end_space = snd start_space - 1
  then true
  else false

let rec check_right_bishop board position1 position2 =
  if position1 = position2 then true
  else if (find_space position1 board).is_open then
    if snd position1 < snd position2 then
      check_right_bishop board
        (Char.chr (Char.code (fst position1) + 1), snd position1 + 1)
        position2
    else
      check_right_bishop board
        (Char.chr (Char.code (fst position1) + 1), snd position1 - 1)
        position2
  else false

let rec check_left_bishop board position1 position2 =
  if position1 = position2 then true
  else if (find_space position1 board).is_open then
    if snd position1 < snd position2 then
      check_left_bishop board
        (Char.chr (Char.code (fst position1) - 1), snd position1 + 1)
        position2
    else
      check_left_bishop board
        (Char.chr (Char.code (fst position1) - 1), snd position1 - 1)
        position2
  else false

let is_valid_move_bishop
    piece
    board
    (start_space : position)
    (end_space : position) =
  if
    Int.abs (Char.code (fst end_space) - Char.code (fst start_space))
    = Int.abs (snd end_space - snd start_space)
  then
    if Char.code (fst start_space) < Char.code (fst end_space) then
      if snd start_space < snd end_space then
        check_right_bishop board
          ( Char.chr (Char.code (fst start_space) + 1),
            snd start_space + 1 )
          end_space
      else
        check_right_bishop board
          ( Char.chr (Char.code (fst start_space) + 1),
            snd start_space - 1 )
          end_space
    else if snd start_space < snd end_space then
      check_left_bishop board
        (Char.chr (Char.code (fst start_space) - 1), snd start_space + 1)
        end_space
    else
      check_left_bishop board
        (Char.chr (Char.code (fst start_space) - 1), snd start_space - 1)
        end_space
  else false

let pawn_capture (color : string) board start_space end_space =
  if color = "White" then
    let e_space = find_space end_space board in
    (* print_endline (String.make 1 (fst end_space)); print_endline (snd
       end_space); print_endline (String.make 1 (fst start_space)); *)
    abs (int_of_char (fst end_space) - int_of_char (fst start_space))
    = 1
    && snd end_space = snd start_space + 1
    && e_space.is_open = false
    && e_space.color = to_color "Black"
  else if color = "Black" then
    let e_space = find_space end_space board in
    abs (int_of_char (fst end_space) - int_of_char (fst start_space))
    = 1
    && snd end_space = snd start_space - 1
    && e_space.is_open = false
    && e_space.color = to_color "White"
  else false

let is_valid_move_pawn (pawn : piece) board start_space end_space =
  match pawn with
  | Pawn White ->
      if
        pawn_capture "White" board start_space end_space
        || fst start_space = fst end_space
           && (snd end_space = snd start_space + 1
              || (snd start_space = 2 && snd end_space = 4))
           && (find_space end_space board).is_open == true
      then true
      else false
  | Pawn Black ->
      if
        pawn_capture "Black" board start_space end_space
        || fst start_space = fst end_space
           && (snd end_space = snd start_space - 1
              || (snd start_space = 7 && snd end_space = 5))
           && (find_space end_space board).is_open == true
      then true
      else false
  | _ -> false

let rec check_verts_rook board position1 position2 =
  if position1 = position2 then true
  else if (find_space position1 board).is_open then
    if snd position1 < snd position2 then
      check_verts_rook board
        (fst position1, snd position1 + 1)
        position2
    else
      check_verts_rook board
        (fst position1, snd position1 - 1)
        position2
  else false

let rec check_horiz_rook board position1 position2 =
  if position1 = position2 then true
  else if (find_space position1 board).is_open then
    if Char.code (fst position1) < Char.code (fst position2) then
      check_horiz_rook board
        (Char.chr (Char.code (fst position1) + 1), snd position1)
        position2
    else
      check_horiz_rook board
        (Char.chr (Char.code (fst position1) - 1), snd position1)
        position2
  else false

let is_valid_move_rook
    piece
    board
    (start_space : position)
    (end_space : position) =
  if
    Char.code (fst start_space) = Char.code (fst end_space)
    && snd start_space <> snd end_space
    || Char.code (fst start_space) <> Char.code (fst end_space)
       && snd start_space = snd end_space
  then
    if fst start_space = fst end_space then
      if snd start_space < snd end_space then
        check_verts_rook board
          (fst start_space, snd start_space + 1)
          end_space
      else
        check_verts_rook board
          (fst start_space, snd start_space - 1)
          end_space
    else if Char.code (fst start_space) < Char.code (fst end_space) then
      check_horiz_rook board
        (Char.chr (Char.code (fst start_space) + 1), snd start_space)
        end_space
    else
      check_horiz_rook board
        (Char.chr (Char.code (fst start_space) - 1), snd start_space)
        end_space
  else false

let is_valid_move_queen piece board start_space end_space =
  is_valid_move_bishop piece board start_space end_space
  || is_valid_move_rook piece board start_space end_space

let is_valid_move board pos_1 pos_2 piece =
  if valid_space pos_1 = false || valid_space pos_2 = false then false
  else
    match piece with
    | Pawn _ -> is_valid_move_pawn piece board pos_1 pos_2
    | Rook _ -> is_valid_move_rook piece board pos_1 pos_2
    | Bishop _ -> is_valid_move_bishop piece board pos_1 pos_2
    | Knight _ -> is_valid_move_knight piece board pos_1 pos_2
    | Queen _ -> is_valid_move_queen piece board pos_1 pos_2
    | King _ -> is_valid_move_king piece board pos_1 pos_2
    | _ -> false

let rec parse_row row =
  match row with
  | [] -> ""
  | h :: t -> print_piece h.piece_id ^ " " ^ parse_row t

let rec parse_board (board : board) (numlst : string list) =
  match numlst with
  | [] -> []
  | n :: m -> (
      match board with
      | [] -> []
      | h :: t -> (n ^ " " ^ parse_row h) :: parse_board t m)

let rec stringify lst =
  let alphalst = "  A B C D E F G H" in
  List.iter print_endline (alphalst :: lst)

let numlist = [ "8"; "7"; "6"; "5"; "4"; "3"; "2"; "1" ]

let print_board board = stringify (parse_board board numlist)

let translate_to_spaces board pos_1 pos_2 =
  let sp1 = find_space pos_1 board in
  let sp2 = find_space pos_2 board in
  (sp1, sp2)

let same_team space1 space2 =
  if space1.color = space2.color then true else false

let rec find_king board (color : string) =
  match board with
  | [] -> raise DoesNotExist
  | h :: t -> (
      let piece = "King " ^ color in
      match
        List.find_opt
          (fun x ->
            x.color = to_color color && x.piece_id = to_piece piece)
          h
      with
      | None -> find_king t color
      | Some sp -> sp)

let rec verify_check (lst : space list) board king_space color =
  match lst with
  | [] -> (false, ('Z', 10))
  | h :: t ->
      let is_val =
        is_valid_move board h.position_id king_space.position_id
          h.piece_id
      in
      if is_val && h.piece_id != Pawn Black && h.piece_id != Pawn White
      then (true, h.position_id)
      else if is_val then
        let opp_Color = if color == "White" then "Black" else "White" in
        if
          pawn_capture opp_Color board h.position_id
            king_space.position_id
        then (true, h.position_id)
        else verify_check t board king_space color
      else verify_check t board king_space color

let rec in_check
    board
    search_board
    (color : string)
    (king_space : space) =
  let opp =
    if color = "Black" then to_color "White" else to_color "Black"
  in
  match search_board with
  | [] -> (false, ('Z', 10))
  | h :: t ->
      let lst = List.filter (fun x -> x.color = opp) h in
      let vals = verify_check lst board king_space color in
      if fst vals then vals else in_check board t color king_space

let move_away sp1 =
  {
    position_id = sp1.position_id;
    is_open = true;
    piece_id = None;
    color = None;
  }

let move_in sp_moved_to sp_moved_from =
  {
    position_id = sp_moved_to.position_id;
    is_open = false;
    piece_id = sp_moved_from.piece_id;
    color = sp_moved_from.color;
  }

let rec move_space2 (board : board) sp1 sp2 lst1 =
  match lst1 with
  | [] -> []
  | sp_test :: spaces ->
      if sp1 = sp_test then
        move_away sp1 :: move_space2 board sp1 sp2 spaces
      else if sp2 = sp_test then
        move_in sp2 sp1 :: move_space2 board sp1 sp2 spaces
      else sp_test :: move_space2 board sp1 sp2 spaces

let rec move_space (board : board) sp1 sp2 =
  match board with
  | [] -> []
  | lst1 :: t -> move_space2 board sp1 sp2 lst1 :: move_space t sp1 sp2

let rec move_spaces board pos1 pos2 (colorMove : p_color) =
  let opp_Color = if colorMove == White then Black else White in
  let sp1, sp2 = translate_to_spaces board pos1 pos2 in
  if sp1.color != colorMove then
    (([ [ defaultempty ] ], None), "not turn")
  else if same_team sp1 sp2 && sp2.is_open = false then
    (([ [ defaultempty ] ], None), "same team")
  else if is_valid_move board pos1 pos2 sp1.piece_id = false then
    (([ [ defaultempty ] ], None), "invalid move")
  else
    let ck1 =
      in_check board board
        (color_to_str colorMove)
        (find_king board (color_to_str colorMove))
    in
    let mvspace = (move_space board sp1 sp2, sp2.piece_id) in
    if fst ck1 then
      let ck2 =
        in_check (fst mvspace) (fst mvspace)
          (color_to_str colorMove)
          (find_king (fst mvspace) (color_to_str colorMove))
      in
      if fst ck2 then (([ [ defaultempty ] ], None), "in check")
      else if
        in_checkmate (fst mvspace) opp_Color
          (find_king (fst mvspace) (color_to_str opp_Color))
      then ((fst mvspace, None), "in checkmate")
      else (mvspace, "clean")
    else if
      in_checkmate (fst mvspace) opp_Color
        (find_king (fst mvspace) (color_to_str opp_Color))
    then ((fst mvspace, None), "in checkmate")
    else if
      fst
        (in_check (fst mvspace) (fst mvspace)
           (color_to_str colorMove)
           (find_king (fst mvspace) (color_to_str colorMove)))
    then (([ [ defaultempty ] ], None), "put into check")
    else (mvspace, "clean")

and find_king_moves board color king_space =
  (*ANSITerminal.print_string [ ANSITerminal.red ] ("\n King Space" ^
    String.make 1 (fst king_id) ^ " " ^ string_of_int (snd king_id) ^ ",
    ");*)
  let king_col = Char.code (fst king_space.position_id) in
  let king_row = snd king_space.position_id in
  let init_list =
    [
      (Char.chr (king_col - 1), king_row - 1);
      (Char.chr king_col, king_row - 1);
      (Char.chr (king_col + 1), king_row - 1);
      (Char.chr (king_col - 1), king_row);
      (Char.chr (king_col + 1), king_row);
      (Char.chr (king_col - 1), king_row + 1);
      (Char.chr king_col, king_row + 1);
      (Char.chr (king_col + 1), king_row + 1);
    ]
  in
  let valid_ids =
    List.filter
      (fun p_id ->
        fst p_id != '@'
        && fst p_id != 'I'
        && snd p_id != 0
        && snd p_id != 9)
      init_list
  in
  let space_list =
    List.map (fun p_id -> find_space p_id board) valid_ids
  in
  let op_color_list =
    List.filter (fun sp -> sp.color != color) space_list
  in
  let valid_moves =
    List.filter
      (fun sp ->
        let new_board_tuplized =
          move_spaces board king_space.position_id sp.position_id color
        in
        if snd new_board_tuplized != "clean" then false
        else
          let new_board = fst (fst new_board_tuplized) in
          not
            (fst
               (in_check new_board new_board (color_to_str color)
                  (find_king new_board (color_to_str color)))))
      op_color_list
  in
  valid_moves

and find_rook_path attack_id king_id =
  if fst attack_id == fst king_id then
    if snd attack_id == snd king_id then []
    else if snd attack_id > snd king_id then
      [ attack_id ]
      @ find_rook_path (fst attack_id, snd attack_id - 1) king_id
    else
      [ attack_id ]
      @ find_rook_path (fst attack_id, snd attack_id + 1) king_id
  else if fst attack_id == fst king_id then []
  else if fst attack_id > fst king_id then
    [ attack_id ]
    @ find_rook_path
        (Char.chr (Char.code (fst attack_id) - 1), snd attack_id)
        king_id
  else
    [ attack_id ]
    @ find_rook_path
        (Char.chr (Char.code (fst attack_id) + 1), snd attack_id)
        king_id

and find_bishop_path attack_id king_id =
  (*ANSITerminal.print_string [ ANSITerminal.red ] ("\n King Space" ^
    String.make 1 (fst king_id) ^ " " ^ string_of_int (snd king_id) ^ ",
    ");*)
  if fst attack_id == fst king_id && snd attack_id == snd king_id then
    []
  else if fst attack_id < fst king_id then
    if
      (*Left of king*)
      (*Below king*)
      snd attack_id < snd king_id
    then
      [ attack_id ]
      @ find_bishop_path
          (Char.chr (Char.code (fst attack_id) + 1), snd attack_id + 1)
          king_id (*Above*)
    else
      [ attack_id ]
      @ find_bishop_path
          (Char.chr (Char.code (fst attack_id) + 1), snd attack_id - 1)
          king_id
  else if
    (*Right of King*)
    (*Below*)
    snd attack_id < snd king_id
  then
    [ attack_id ]
    @ find_bishop_path
        (Char.chr (Char.code (fst attack_id) - 1), snd attack_id + 1)
        king_id (*Above*)
  else
    [ attack_id ]
    @ find_bishop_path
        (Char.chr (Char.code (fst attack_id) - 1), snd attack_id - 1)
        king_id

and find_path attack_id king_id piece_t =
  match piece_t with
  | Pawn _ -> [ attack_id ]
  | Rook _ -> find_rook_path attack_id king_id
  | Bishop _ -> find_bishop_path attack_id king_id
  | Knight _ -> [ attack_id ]
  | Queen _ ->
      if fst attack_id == fst king_id || snd attack_id == snd king_id
      then find_rook_path attack_id king_id
      else find_bishop_path attack_id king_id
  | _ -> []

and find_pieces_same_color board intended_color =
  match board with
  | h :: t ->
      List.map
        (fun space -> (space.position_id, space.piece_id))
        (List.filter (fun sp -> sp.color == intended_color) h)
      @ find_pieces_same_color t intended_color
  | [] -> []

and check_defenses board color piece_id defense_ops =
  match defense_ops with
  | (p_id, p_type) :: t ->
      if is_valid_move board p_id piece_id p_type then
        let new_board_all = move_spaces board p_id piece_id color in
        if
          snd new_board_all == "in check"
          || snd new_board_all == "put into check"
        then check_defenses board color piece_id t
        else if
          let new_board = fst (fst new_board_all) in
          fst
            (in_check new_board new_board (color_to_str color)
               (find_king new_board (color_to_str color)))
        then check_defenses board color piece_id t
        else true
      else check_defenses board color piece_id t
  | [] -> false

(*ANSITerminal.print_string [ ANSITerminal.red ] ("\n Piece defense
  Location: \n" ^ String.make 1 (fst p_id) ^ " " ^ string_of_int (snd
  p_id) ^ ",\n ");*)
and eval_path board piece_path defense_ops king_space =
  let color = king_space.color in
  match piece_path with
  | piece_id :: t ->
      if check_defenses board color piece_id defense_ops then
        (*ANSITerminal.print_string [ ANSITerminal.red ] ("\n Attack
          spot : \n" ^ String.make 1 (fst piece_id) ^ " " ^
          string_of_int (snd piece_id) ^ ",\n ");*)
        true
      else eval_path board t defense_ops king_space
  | [] -> false

and block_or_capture board attack_space king_space =
  let piece_path =
    find_path attack_space.position_id king_space.position_id
      attack_space.piece_id
  in
  if
    eval_path board piece_path
      (find_pieces_same_color board king_space.color)
      king_space
  then true
  else false

and in_checkmate board color (king_space : space) =
  (* First check if King is actually in check *)
  let vals = in_check board board (color_to_str color) king_space in
  if not (fst vals) then false
  else if List.length (find_king_moves board color king_space) != 0 then
    false
  else if
    block_or_capture board (find_space (snd vals) board) king_space
  then false
  else true
