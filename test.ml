(*
            Continuation Builder and Lambda Interpreter -- Testing
*)

open Core ;;
open Lexicon ;;
open Rules ;;

let forward = Phrase.Single (Construct FORWARD) ;;
let backward = Phrase.Single (Construct BACKWARD) ;;
let lift = Phrase.Single (Construct LIFT) ;;
let eval = Phrase.Single (Construct EVAL) ;;
let meta = Phrase.Single (Construct META) ;;
let skip = Phrase.Single (Construct SKIP) ;;
let bind = Phrase.Single (Construct BIND) ;;

let yejoo = Phrase.Single Yejoo ;;
let yejoos = Phrase.Single Yejoos ;;
let whansung = Phrase.Single Whansung ;;
let left = Phrase.Single Left ;;
let saw = Phrase.Single Saw ;;
let loves = Phrase.Single Loves ;;
let thought = Phrase.Single Thought ;;
let mother = Phrase.Single Mother ;;
let mothers = Phrase.Single Mothers ;;
let friend = Phrase.Single Friend ;;
let someone = Phrase.Single Someone ;;
let everyone = Phrase.Single Everyone ;;
let everyones = Phrase.Single Everyones ;;
let she = Phrase.Single She ;;
let her = Phrase.Single Her ;;

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
  let output1 = [Phrase.List [backward; yejoo; left]] in
  build_unit_test input1 output1 "yejoo left";
  (* Test 2 : Ye Joo saw Whansung *)
  let input2 = ["yejoo"; "saw"; "whansung"] in
  let output2 = [Phrase.List [backward; yejoo; List [forward; saw; whansung]]] in
  build_unit_test input2 output2 "yejoo saw whansung";
  (* Test 3 : Ye Joo's mother left *)
  let input3 = ["yejoo's"; "mother"; "left"] in
  let output3 = [Phrase.List [backward; List [backward; yejoos; mother]; left]] in
  build_unit_test input3 output3 "yejoo's mother left";
  (* Test 4 : Ye Joo thought Whansung left *)
  let input4 = ["yejoo"; "thought"; "whansung"; "left"] in
  let output4 = [Phrase.List [backward; yejoo; List [forward; thought; List [backward; whansung; left]]]] in
  build_unit_test input4 output4 "yejoo thought whansung left";
  (* Test 5 : everyone left *)
  let input5 = ["everyone"; "left"] in
  let output5 = [Phrase.List [eval; List [meta; backward; everyone; List [lift; left]]]] in
  build_unit_test input5 output5 "everyone left";
  (* Test 6 : someone saw everyone *)
  let input6 = ["someone"; "saw"; "everyone"] in
  let interp61 = Phrase.List [eval; List [meta; backward; someone; List [meta; forward; List [lift; saw]; everyone]]] in
  let interp62 = Phrase.List [eval; List [skip; eval; List [meta; List [meta; backward]; List [lift; someone]; List [meta; List [meta; forward]; List [lift; List [lift; saw]]; List [skip; lift; everyone]]]]] in
  let output6 = [interp61; interp62] in
  build_unit_test input6 output6 "someone saw everyone";
  (* Test 7 : she left *)
  let input7 = ["she"; "left"] in
  let output7 = [Phrase.List [eval; List [meta; backward; she; List [lift; left]]]] in
  build_unit_test input7 output7 "she left";
  (* Test 8 : everyone loves her mother *)
  let input8 = ["everyone"; "loves"; "her"; "mother"] in
  let interp81 = Phrase.List [eval; List [meta; backward; List [bind; everyone]; List [meta; forward; List [lift; loves]; List [meta; backward; her; List [lift; mother]]]]] in
  let interp82 = Phrase.List [eval; List [skip; eval; List [meta; List [meta; backward]; List [lift; everyone]; List [meta; List [meta; forward]; List [lift; List [lift; loves]]; List [skip; lift; List [meta; backward; her; List [lift; mother]]]]]]] in
  let output8 = [interp81; interp82] in
  build_unit_test input8 output8 "everyone loves her mother";
  (* Test 9 : yejoo saw someone *)
  let input9 = ["yejoo"; "saw"; "someone"] in
  let output9 = [Phrase.List [eval; List [meta; backward; List [lift; yejoo]; List [meta; forward; List [lift; saw]; someone]]]] in
  build_unit_test input9 output9 "yejoo saw someone";
  (* Test 10 : everyone's mother left *)
  let input10 = ["everyone's"; "mother"; "left"] in
  let output10 = [Phrase.List [eval; List [meta; backward; List [meta; backward; everyones; List [lift; mother]]; List [lift; left]]]] in
  build_unit_test input10 output10 "everyone's mother left";
  (* Test 11 : everyone's mother's friend left *)
  let input11 = ["everyone's"; "mother's"; "friend"; "left"] in
  let output11 = [Phrase.List [eval; List [meta; backward; List [meta; backward; List [meta; backward; everyones; List [lift; mothers]]; List [lift; friend]]; List [lift; left]]]] in
  build_unit_test input11 output11 "everyone's mother's friend left";
  (* Test 12 : yejoo saw her *)
  let input12 = ["yejoo"; "saw"; "her"] in
  let output12 = [Phrase.List [eval; List [meta; backward; List [lift; yejoo]; List [meta; forward; List [lift; saw]; her]]]] in
  build_unit_test input12 output12 "yejoo saw her";
  (* Test 13 : yejoo thought she left *)
  let input13 = ["yejoo"; "thought"; "she"; "left"] in
  let output13 = [Phrase.List [eval; List [meta; backward; List [lift; yejoo]; List [meta; forward; List [lift; thought]; List [meta; backward; she; List [lift; left]]]]]] in
  build_unit_test input13 output13 "yejoo thought she left";
  print_string "\n"

(* all tests for lambda interpreter *)
let interpret_tests () =
  print_string "Running Interpret Tests...\n";
  (* Test 1 : Ye Joo left *)
  let input1 = Phrase.List [backward; yejoo; left] in
  let output1 = Lterm.LApp (LWord "left", LWord "yejoo") in
  interpret_unit_test input1 output1 "yejoo left";
  (* Test 2 : Ye Joo saw Whansung *)
  let input2 = Phrase.List [backward; yejoo; List [forward; saw; whansung]] in
  let output2 = Lterm.LApp (LApp (LWord "saw", LWord "whansung"), LWord "yejoo") in
  interpret_unit_test input2 output2 "yejoo saw whansung";
  (* Test 3 : Ye Joo's mother left *)
  let input3 = Phrase.List [backward; List [backward; yejoos; mother]; left] in
  let output3 = Lterm.LApp (LWord "left", LApp (LWord "mother", LWord "yejoo")) in
  interpret_unit_test input3 output3 "yejoo's mother left";
  (* Test 4 : Ye Joo thought Whansung left *)
  let input4 = Phrase.List [backward; yejoo; List [forward; thought; List [backward; whansung; left]]] in
  let output4 = Lterm.LApp (LApp (LWord "thought", LApp (LWord "left", LWord "whansung")), LWord "yejoo") in
  interpret_unit_test input4 output4 "yejoo thought whansung left";
  (* Test 5 : everyone left *)
  let input5 = Phrase.List [eval; List [meta; backward; everyone; List [lift; left]]] in
  let output5 = Lterm.LForall ("x", LApp (LWord "left", LId "x")) in
  interpret_unit_test input5 output5 "everyone left";
  (* Test 6 : someone saw everyone regular scope *)
  let input6 = Phrase.List [eval; List [meta; backward; someone; List [meta; forward; List [lift; saw]; everyone]]] in
  let output6 = Lterm.LExists ("x", LForall ("x'", LApp (LApp (LWord "saw", LId "x'"), LId "x"))) in
  interpret_unit_test input6 output6 "someone saw everyone regular scope";
  (* Test 7 : someone saw everyone inverse scope *)
  let input7 = Phrase.List [eval; List [skip; eval; List [meta; List [meta; backward]; List [lift; someone]; List [meta; List [meta; forward]; List [lift; List [lift; saw]]; List [skip; lift; everyone]]]]] in
  let output7 = Lterm.LForall ("x", LExists ("x'", LApp (LApp (LWord "saw", LId "x"), LId "x'"))) in
  interpret_unit_test input7 output7 "someone saw everyone inverse scope";
  (* Test 8 : she left *)
  let input8 = Phrase.List [eval; List [meta; backward; she; List [lift; left]]] in
  let output8 = Lterm.LLam ("l", LApp (LWord "left", LId "l")) in
  interpret_unit_test input8 output8 "she left";
  (* Test 9 : everyone loves her mother with binding *)
  let input9 = Phrase.List [eval; List [meta; backward; List [bind; everyone]; List [meta; forward; List [lift; loves]; List [meta; backward; her; List [lift; mother]]]]] in
  let output9 = Lterm.LForall ("x", LApp (LApp (LWord "love", LApp (LWord "mother", LId "x")), LId "x")) in
  interpret_unit_test input9 output9 "everyone loves her mother with binding";
  (* Test 10 : everyone loves her mother without binding *)
  let input10 = Phrase.List [eval; List [skip; eval; List [meta; List [meta; backward]; List [lift; everyone]; List [meta; List [meta; forward]; List [lift; List [lift; loves]]; List [skip; lift; List [meta; backward; her; List [lift; mother]]]]]]] in
  let output10 = Lterm.LLam ("l", LForall ("x", LApp (LApp (LWord "love", LApp (LWord "mother", LId "l")), LId "x"))) in
  interpret_unit_test input10 output10 "everyone loves her mother without binding";
  (* Test 11 : yejoo saw someone *)
  let input11 = Phrase.List [eval; List [meta; backward; List [lift; yejoo]; List [meta; forward; List [lift; saw]; someone]]] in
  let output11 = Lterm.LExists ("x", LApp (LApp (LWord "saw", LId "x"), LWord "yejoo")) in
  interpret_unit_test input11 output11 "yejoo saw someone";
  (* Test 12 : everyone's mother left *)
  let input12 = Phrase.List [eval; List [meta; backward; List [meta; backward; everyones; List [lift; mother]]; List [lift; left]]] in
  let output12 = Lterm.LForall("x", LApp (LWord "left", LApp (LWord "mother", LId "x"))) in
  interpret_unit_test input12 output12 "everyone's mother left";
  (* Test 13 : everyone's mother's friend left *)
  let input13 = Phrase.List [eval; List [meta; backward; List [meta; backward; List [meta; backward; everyones; List [lift; mothers]]; List [lift; friend]]; List [lift; left]]] in
  let output13 = Lterm.LForall ("x", LApp (LWord "left", LApp (LWord "friend", LApp (LWord "mother", LId "x")))) in
  interpret_unit_test input13 output13 "everyone's mother's friend left";
  (* Test 14 : yejoo saw her *)
  let input14 = Phrase.List [eval; List [meta; backward; List [lift; yejoo]; List [meta; forward; List [lift; saw]; her]]] in
  let output14 = Lterm.LLam ("r", LApp (LApp (LWord "saw", LId "r"), LWord "yejoo")) in
  interpret_unit_test input14 output14 "yejoo saw her";
  (* Test 15 : yejoo thought she left *)
  let input15 = Phrase.List [eval; List [meta; backward; List [lift; yejoo]; List [meta; forward; List [lift; thought]; List [meta; backward; she; List [lift; left]]]]] in
  let output15 = Lterm.LLam ("l", LApp (LApp (LWord "thought", LApp (LWord "left", LId "l")), LWord "yejoo")) in
  interpret_unit_test input15 output15 "yejoo thought she left";
  print_string "\n"

let run_all_tests () =
  build_tests ();
  interpret_tests()

let _ = run_all_tests () ;;
