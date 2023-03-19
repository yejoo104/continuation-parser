open Core ;;
open Lexicon ;;
open Rules ;;
open Lterm ;;

let main () =
  (* let word_list = *)
  (*   match In_channel.input_line In_channel.stdin with *)
  (*   | Some str -> String.split_on_chars ~on:[' '] str *)
  (*   | None -> [] in *)
  (* let token_type_list = *)
  (*   List.map word_list ~f:(fun str -> *)
  (*     let tok = Token.str_to_token str in *)
  (*     let tok_type = Token.token_to_type tok in *)
  (*     (Tokens.Single tok, tok_type)) in *)
  (* let bound_token_type_list = bind [] [] token_type_list in *)
  (* let results = build_all_trees [] bound_token_type_list in *)
  (* let remove_dups_results = List.dedup_and_sort ~compare:(fun (tok1, _) (tok2, _) -> Stdlib.compare tok1 tok2) results in *)
  (* List.iter ~f:(fun (toks, _) -> print_string (Tokens.tokens_to_str toks); Out_channel.newline stdout) remove_dups_results; *)
  let meta = Construct.construct_to_lterm META in
  let forward = Construct.construct_to_lterm FORWARD in
  let res1 = Lambda.normal_form_print (LApp(meta, forward)) in
  let _res2 = Lambda.normal_form_print (LApp(meta, res1)) in
  ()

let _ = main () ;;
