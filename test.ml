(*
            Continuation Builder and Lambda Interpreter -- Testing
*)

open Core ;;
open Lexicon ;;
open Rules ;;

(* unit test for building continuations *)
let build_unit_test (input : string list) (expected_output : Phrase.t list) (test_name : string) : unit =
  let tokens = build_continuation input in
  let sorted_expected_output = List.sort ~compare:Stdlib.compare expected_output in
  match (List.equal (Stdlib.(=)) tokens sorted_expected_output) with
  | true -> Printf.printf "passed %s\n" test_name
  | false -> Printf.printf "FAILED %s\n" test_name

(* unit test for lambda interpreter *)
let interpret_unit_test (input : Phrase.t) (expected_output : Lterm.t) (test_name : string) : unit =
  let lambda_exp = Phrase.to_lambda input in
  match (Stdlib.(=) lambda_exp expected_output) with
  | true -> Printf.printf "passed %s\n" test_name
  | false -> Printf.printf "FAILED %s\n" test_name

(* all tests for continuation builder *)
let build_tests () =
  print_string "Running Build Tests...\n";
  (* Test 1 : Ye Joo left *)
  let input1 = ["yejoo"; "left"] in
  let output1 = [Phrase.List [Single (Construct BACKWARD); Single Yejoo; Single Left]] in
  build_unit_test input1 output1 "yejoo left";
  (* Test 2 : Ye Joo saw Whansung *)
  let input2 = ["yejoo"; "saw"; "whansung"] in
  let output2 = [Phrase.List [Single (Construct BACKWARD); Single Yejoo; List [Single (Construct FORWARD); Single Saw; Single Whansung]]] in
  build_unit_test input2 output2 "yejoo saw whansung";
  (* Test 3 : Ye Joo's mother left *)
  let input3 = ["yejoo's"; "mother"; "left"] in
  let output3 = [Phrase.List [Single (Construct BACKWARD); List [Single (Construct BACKWARD); Single Yejoos; Single Mother]; Single Left]] in
  build_unit_test input3 output3 "yejoo's mother left";
  (* Test 4 : Ye Joo thought Whansung left *)
  let input4 = ["yejoo"; "thought"; "whansung"; "left"] in
  let output4 = [Phrase.List [Single (Construct BACKWARD); Single Yejoo; List [Single (Construct FORWARD); Single Thought; List [Single (Construct BACKWARD); Single Whansung; Single Left]]]] in
  build_unit_test input4 output4 "yejoo thought whansung left";
  (* Test 5 : everyone left *)
  let input5 = ["everyone"; "left"] in
  let output5 = [Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single Everyone; List [Single (Construct LIFT); Single Left]]]] in
  build_unit_test input5 output5 "everyone left";
  (* Test 6 : someone saw everyone *)
  let input6 = ["someone"; "saw"; "everyone"] in
  let interp61 = Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single Someone; List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); Single Saw]; Single Everyone]]] in
  let interp62 = Phrase.List [Single (Construct EVAL); List [Single (Construct SKIP); Single (Construct EVAL); List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); Single Someone]; List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); Single Saw]]; List [Single (Construct SKIP); Single (Construct LIFT); Single Everyone]]]]] in
  let output6 = [interp61; interp62] in
  build_unit_test input6 output6 "someone saw everyone";
  (* Test 7 : she left *)
  let input7 = ["she"; "left"] in
  let output7 = [Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single She; List [Single (Construct LIFT); Single Left]]]] in
  build_unit_test input7 output7 "she left";
  (* Test 8 : everyone loves her mother *)
  let input8 = ["everyone"; "loves"; "her"; "mother"] in
  let interp81 = Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); List [Single (Construct BIND); Single Everyone]; List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); Single Loves]; List [Single (Construct META); Single (Construct BACKWARD); Single Her; List [Single (Construct LIFT); Single Mother]]]]] in
  let interp82 = Phrase.List [Single (Construct EVAL); List [Single (Construct SKIP); Single (Construct EVAL); List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); Single Everyone]; List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); Single Loves]]; List [Single (Construct SKIP); Single (Construct LIFT); List [Single (Construct META); Single (Construct BACKWARD); Single Her; List [Single (Construct LIFT); Single Mother]]]]]]] in
  let output8 = [interp81; interp82] in
  build_unit_test input8 output8 "everyone loves her mother";
  print_string "\n"

(* all tests for lambda interpreter *)
let interpret_tests () =
  print_string "Running Interpret Tests...\n";
  (* Test 1 : Ye Joo left *)
  let input1 = Phrase.List [Single (Construct BACKWARD); Single Yejoo; Single Left] in
  let output1 = Lterm.LApp (LWord "left", LWord "yejoo") in
  interpret_unit_test input1 output1 "yejoo left";
  (* Test 2 : Ye Joo saw Whansung *)
  let input2 = Phrase.List [Single (Construct BACKWARD); Single Yejoo; List [Single (Construct FORWARD); Single Saw; Single Whansung]] in
  let output2 = Lterm.LApp (LApp (LWord "saw", LWord "whansung"), LWord "yejoo") in
  interpret_unit_test input2 output2 "yejoo saw whansung";
  (* Test 3 : Ye Joo's mother left *)
  let input3 = Phrase.List [Single (Construct BACKWARD); List [Single (Construct BACKWARD); Single Yejoos; Single Mother]; Single Left] in
  let output3 = Lterm.LApp (LWord "left", LApp (LWord "mother", LWord "yejoo")) in
  interpret_unit_test input3 output3 "yejoo's mother left";
  (* Test 4 : Ye Joo thought Whansung left *)
  let input4 = Phrase.List [Single (Construct BACKWARD); Single Yejoo; List [Single (Construct FORWARD); Single Thought; List [Single (Construct BACKWARD); Single Whansung; Single Left]]] in
  let output4 = Lterm.LApp (LApp (LWord "thought", LApp (LWord "left", LWord "whansung")), LWord "yejoo") in
  interpret_unit_test input4 output4 "yejoo thought whansung left";
  (* Test 5 : everyone left *)
  let input5 = Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single Everyone; List [Single (Construct LIFT); Single Left]]] in
  let output5 = Lterm.LForall ("x", LApp (LWord "left", LId "x")) in
  interpret_unit_test input5 output5 "everyone left";
  (* Test 6 : someone saw everyone regular scope *)
  let input6 = Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single Someone; List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); Single Saw]; Single Everyone]]] in
  let output6 = Lterm.LExists ("x", LForall ("x'", LApp (LApp (LWord "saw", LId "x'"), LId "x"))) in
  interpret_unit_test input6 output6 "someone saw everyone regular scope";
  (* Test 7 : someone saw everyone inverse scope *)
  let input7 = Phrase.List [Single (Construct EVAL); List [Single (Construct SKIP); Single (Construct EVAL); List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); Single Someone]; List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); Single Saw]]; List [Single (Construct SKIP); Single (Construct LIFT); Single Everyone]]]]] in
  let output7 = Lterm.LForall ("x", LExists ("x'", LApp (LApp (LWord "saw", LId "x"), LId "x'"))) in
  interpret_unit_test input7 output7 "someone saw everyone inverse scope";
  (* Test 8 : she left *)
  let input8 = Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single She; List [Single (Construct LIFT); Single Left]]] in
  let output8 = Lterm.LLam ("l", LApp (LWord "left", LId "l")) in
  interpret_unit_test input8 output8 "she left";
  (* Test 9 : everyone loves her mother with binding *)
  let input9 = Phrase.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); List [Single (Construct BIND); Single Everyone]; List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); Single Loves]; List [Single (Construct META); Single (Construct BACKWARD); Single Her; List [Single (Construct LIFT); Single Mother]]]]] in
  let output9 = Lterm.LForall ("x", LApp (LApp (LWord "love", LApp (LWord "mother", LId "x")), LId "x")) in
  interpret_unit_test input9 output9 "everyone loves her mother with binding";
  (* Test 10 : everyone loves her mother without binding *)
  let input10 = Phrase.List [Single (Construct EVAL); List [Single (Construct SKIP); Single (Construct EVAL); List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); Single Everyone]; List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); Single Loves]]; List [Single (Construct SKIP); Single (Construct LIFT); List [Single (Construct META); Single (Construct BACKWARD); Single Her; List [Single (Construct LIFT); Single Mother]]]]]]] in
  let output10 = Lterm.LLam ("l", LForall ("x", LApp (LApp (LWord "love", LApp (LWord "mother", LId "l")), LId "x"))) in
  interpret_unit_test input10 output10 "everyone loves her mother without binding";
  print_string "\n"

let run_all_tests () =
  build_tests ();
  interpret_tests()

let _ = run_all_tests () ;;
