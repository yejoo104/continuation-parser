open Core ;;
open Lexicon ;;
open Rules ;;

let main () =
  let word_list =
    match In_channel.input_line In_channel.stdin with
    | Some str -> String.split_on_chars ~on:[' '] str
    | None -> [] in
  let token_type_list =
    List.map word_list ~f:(fun str ->
      let tok = str_to_token str in
      let tok_type = token_to_type tok in
      (Single tok, tok_type)) in
  let bound_token_type_list = bind [] [] token_type_list in
  let results = build_all_trees [] bound_token_type_list in
  List.iter ~f:(fun (toks, _) -> print_string (tokens_to_str toks); Out_channel.newline stdout) results

let _ = main () ;;
