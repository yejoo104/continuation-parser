open Core
open Lexicon

let main () =
  let word_list =
    match In_channel.input_line In_channel.stdin with
    | Some str -> String.split_on_chars ~on:[' '] str
    | None -> [] in
  let token_type_list =
    List.map word_list ~f:(fun str ->
      let tok = str_to_token str in
      let tok_type = token_to_type tok in
      (tok, tok_type)) in
  match token_type_list with
  | [] -> print_string "No Input\n"
  | (tok, _) :: _ -> print_string (token_to_str tok) ;;

let _ = main () ;;
