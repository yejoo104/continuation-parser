open Core ;;
open Lexicon ;;
open Rules ;;

let build_unit_test (input : string list) (expected_output : Tokens.t list) (test_name : string) : unit =
  let tokens = build_continuation input in
  let sorted_expected_output = List.sort ~compare:Stdlib.compare expected_output in
  match (List.equal (Stdlib.(=)) tokens sorted_expected_output) with
  | true -> Printf.printf "%s passed\n" test_name
  | false -> Printf.printf "%s FAILED\n" test_name

let build_tests () =
  (* Test 1 : Ye Joo left *)
  let input1 = ["yejoo"; "left"] in
  let output1 = [Tokens.List [Single (Construct BACKWARD); Single Yejoo; Single Left]] in
  build_unit_test input1 output1 "yejoo left";
  (* Test 2 : Ye Joo saw Whansung *)
  let input2 = ["yejoo"; "saw"; "whansung"] in
  let output2 = [Tokens.List [Single (Construct BACKWARD); Single Yejoo; List [Single (Construct FORWARD); Single Saw; Single Whansung]]] in
  build_unit_test input2 output2 "yejoo saw whansung";
  (* Test 3 : Ye Joo's mother left *)
  let input3 = ["yejoo's"; "mother"; "left"] in
  let output3 = [Tokens.List [Single (Construct BACKWARD); List [Single (Construct BACKWARD); Single Yejoos; Single Mother]; Single Left]] in
  build_unit_test input3 output3 "yejoo's mother left";
  (* Test 4 : Ye Joo thought Whansung left *)
  let input4 = ["yejoo"; "thought"; "whansung"; "left"] in
  let output4 = [Tokens.List [Single (Construct BACKWARD); Single Yejoo; List [Single (Construct FORWARD); Single Thought; List [Single (Construct BACKWARD); Single Whansung; Single Left]]]] in
  build_unit_test input4 output4 "yejoo thought whansung left";
  (* Test 5 : everyone left *)
  let input5 = ["everyone"; "left"] in
  let output5 = [Tokens.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single Everyone; List [Single (Construct LIFT); Single Left]]]] in
  build_unit_test input5 output5 "everyone left";
  (* Test 6 : someone saw everyone *)
  let input6 = ["someone"; "saw"; "everyone"] in
  let interp61 = Tokens.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single Someone; List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); Single Saw]; Single Everyone]]] in
  let interp62 = Tokens.List [Single (Construct EVAL); List [Single (Construct SKIP); Single (Construct EVAL); List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); Single Someone]; List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); Single Saw]]; List [Single (Construct SKIP); Single (Construct LIFT); Single Everyone]]]]] in
  let output6 = [interp61; interp62] in
  build_unit_test input6 output6 "someone saw everyone";
  (* Test 7 : she left *)
  let input7 = ["she"; "left"] in
  let output7 = [Tokens.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); Single She; List [Single (Construct LIFT); Single Left]]]] in
  build_unit_test input7 output7 "she left";
  (* Test 8 : everyone loves her mother *)
  let input8 = ["everyone"; "loves"; "her"; "mother"] in
  let interp81 = Tokens.List [Single (Construct EVAL); List [Single (Construct META); Single (Construct BACKWARD); List [Single (Construct BIND); Single Everyone]; List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); Single Loves]; List [Single (Construct META); Single (Construct BACKWARD); Single Her; List [Single (Construct LIFT); Single Mother]]]]] in
  let interp82 = Tokens.List [Single (Construct EVAL); List [Single (Construct SKIP); Single (Construct EVAL); List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); Single Everyone]; List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); Single Loves]]; List [Single (Construct SKIP); Single (Construct LIFT); List [Single (Construct META); Single (Construct BACKWARD); Single Her; List [Single (Construct LIFT); Single Mother]]]]]]] in
  let output8 = [interp81; interp82] in
  build_unit_test input8 output8 "everyone loves her mother"

let run_all_tests () =
  build_tests ()

let _ = run_all_tests () ;;
