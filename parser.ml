open Core

let main () =
  let word_list =
    match In_channel.input_line In_channel.stdin with
    | Some str -> String.split_on_chars ~on:[' '] str
    | None -> [] in
  match word_list with
  | [] -> print_string "No Input\n"
  | hd :: tl -> print_string hd ;;

let _ = main () ;;
