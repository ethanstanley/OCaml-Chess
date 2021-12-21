open Chess
open Command
open State
include Board

type object_phrase = (char * int) list

let data_dir_prefix = "data" ^ Filename.dir_sep

(**Recursive function that represents the game. Every time it recurses,
   it is the next state of the game [move st] represents the move from
   the current position to the new one that is inputed*)
let rec move st err =
  if not err then
    if cur_turn st = to_color "White" then
      ANSITerminal.print_string [ ANSITerminal.red ] "White's Turn!\n"
    else
      ANSITerminal.print_string [ ANSITerminal.red ] "Black's Turn!\n"
  else if cur_turn st = to_color "White" then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "White, please move again!\n"
  else
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Black, please move again !\n";
  stringify (parse_board (current_board st) numlist);
  print_endline
    "\nPlease enter a board location you intend to move\n  from.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | piece1 -> (
      print_endline
        "Please enter a board\n  location you intend to move to\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | piece2 ->
          let move_package =
            move_it
              (create_command piece1 piece2)
              (current_board st) (cur_turn st)
          in
          if snd move_package = "clean" then
            let new_stuff = fst move_package in
            if cur_turn st == to_color "White" then
              if snd new_stuff = to_piece "None" then
                let new_state =
                  create_state (fst new_stuff) (white_taken st)
                    (black_taken st) (to_color "Black")
                in
                (*let king_space = find_king (current_board new_state)
                  "White" in List.iter (ANSITerminal.print_string [
                  ANSITerminal.red ]) (List.map (fun sp -> String.make 1
                  (space_to_position_fst sp) ^ " " ^ string_of_int
                  (space_to_position_snd sp)) (find_king_moves
                  (current_board new_state) (to_color "White")
                  king_space));*)
                move new_state false
              else
                let new_state =
                  create_state (fst new_stuff) (white_taken st)
                    (snd new_stuff :: black_taken st)
                    (to_color "Black")
                in
                (*let king_space = find_king (current_board new_state)
                  "White" in List.iter (ANSITerminal.print_string [
                  ANSITerminal.red ]) (List.map (fun sp -> String.make 1
                  (space_to_position_fst sp) ^ " " ^ string_of_int
                  (space_to_position_snd sp)) (find_king_moves
                  (current_board new_state) (to_color "White")
                  king_space));*)
                move new_state false
            else if snd new_stuff = to_piece "None" then
              let new_state =
                create_state (fst new_stuff) (white_taken st)
                  (black_taken st) (to_color "White")
              in
              (*let king_space = find_king (current_board new_state)
                "White" in List.iter (ANSITerminal.print_string [
                ANSITerminal.red ]) (List.map (fun sp -> String.make 1
                (space_to_position_fst sp) ^ " " ^ string_of_int
                (space_to_position_snd sp)) (find_king_moves
                (current_board new_state) (to_color "White")
                king_space));*)
              move new_state false
            else
              let new_state =
                create_state (fst new_stuff)
                  (snd new_stuff :: white_taken st)
                  (black_taken st) (to_color "White")
              in
              (*let king_space = find_king (current_board new_state)
                "White" in List.iter (ANSITerminal.print_string [
                ANSITerminal.red ]) (List.map (fun sp -> String.make 1
                (space_to_position_fst sp) ^ " " ^ string_of_int
                (space_to_position_snd sp)) (find_king_moves
                (current_board new_state) (to_color "White")
                king_space));*)
              move new_state false
          else if snd move_package = "in checkmate" then (
            stringify (parse_board (fst (fst move_package)) numlist);
            if cur_turn st == to_color "White" then
              ANSITerminal.print_string [ ANSITerminal.red ]
                "\nGame over. Checkmate. White Wins.\n"
            else
              ANSITerminal.print_string [ ANSITerminal.red ]
                "\nGame over. Checkmate. Black Wins.\n")
          else if snd move_package = "in check" then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYou are in check. Try again...\n";
            move st true)
          else if snd move_package = "put into check" then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\n\
               Don't be silly. That's moving into check. Try again...\n";
            move st true)
          else if snd move_package = "invalid" then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nPlease enter two valid spaces. Try again...\n";
            move st true)
          else if snd move_package = "wrong turn" then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nIt's not your turn... Try again...\n";
            move st true)
          else if snd move_package = "same team" then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYou can't capture your own piece! Try again...\n";
            move st true)
          else if snd move_package = "invalid move" then (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nThat's not a valid move!\n";
            move st true)
          else (
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nPlease use the proper rules. Try again ...\n";
            move st true))

(** [main()] begins the game by calling [move st] with st equal to the
    state of the inital start board of chess*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Chess festivites!\n\n";
  let st = create_state start_board [] [] (to_color "White") in
  move st false

let () = main ()