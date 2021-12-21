open Board

type state = {
  cur_board : board;
  w_taken : piece list;
  b_taken : piece list;
  cur_turn : p_color;
}

let create_state bd w b cur =
  { cur_board = bd; w_taken = w; b_taken = b; cur_turn = cur }

let current_board st = st.cur_board

let white_taken st = st.w_taken

let black_taken st = st.b_taken

let cur_turn st = st.cur_turn

type result =
  | Legal of state
  | Illegal
