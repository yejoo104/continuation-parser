open Core ;;
open Lexicon ;;
open Rules ;;
open Lambda ;;

let main () =
  let word_list =
    match In_channel.input_line In_channel.stdin with
    | Some str -> String.split_on_chars ~on:[' '] str
    | None -> [] in
  let continuations = build_continuation word_list in
  List.iter ~f:(fun toks ->
      print_string (Tokens.to_string toks ^ "\n");
      print_string (((Tokens.to_lambda toks) |> Lambda.to_string) ^ "\n"))
    continuations

let _ = main () ;;
