(*
                    Parser that takes Sentence as Input
                       Builds a Continuation Sentence
                Generates a Fully Reduced Lambda Expression
*)
open Core ;;
open Lexicon ;;
open Rules ;;
open Lambda ;;

(* main function takes a sentence as input and prints both a continuation semantics sentence and a lambda expression *)
let main () =
  let word_list =
    match In_channel.input_line In_channel.stdin with
    | Some str -> String.split_on_chars ~on:[' '] str
    | None -> [] in
  let continuations = build_continuation word_list in
  List.iter ~f:(fun toks ->
      print_string (Phrase.to_string toks ^ "\n");
      print_string (((Phrase.to_lambda toks) |> Lambda.to_string) ^ "\n"))
    continuations

let _ = main () ;;
