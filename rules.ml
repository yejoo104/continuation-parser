(*
                  Rules of Continuation Based Semantics
*)
open Core ;;
open Type ;;
open Lexicon ;;

(* Applies Rule 1 if applicable. Returns None otherwise *)
let rule1 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | t1, Backward(t2, t3) ->
    (match (equivalent_type t1 t2) with
     | None -> None
     | Some t1 ->
       let t3 = update_type t1 t3 in
       (match is_mismatch t3 with
        | true -> None
        | false -> Some (List [Single (Construct BACKWARD); tok1; tok2], t3)))
  | _ -> None

(* Applies Rule 2 if applicable. Returns None otherwise *)
let rule2 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Forward(t1, t2), t3 ->
    (match (equivalent_type t1 t3) with
     | None -> None
     | Some t1 ->
       let t2 = update_type t1 t2 in
       (match is_mismatch t2 with
        | true -> None
        | false -> Some (List [Single (Construct FORWARD); tok1; tok2], t2)))
  | _ -> None

(* Applies Rule 3 if applicable. Returns None otherwise *)
let rule3 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(t1, t2), t3), Backward(t4, t5) ->
    (match (equivalent_type t1 t4) with
     | None -> None
     | Some t1 ->
       let t5, t2, t3 =
         update_type t1 t5,
         update_type t1 t2,
         update_type t1 t3 in
       (match List.exists ~f:is_mismatch [t5;t2;t3] with
        | true -> None
        | false -> Some (List [Single (Construct META); Single (Construct BACKWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t5, t2), t3))))
  | _ -> None

(* Applies Rule 4 if applicable. Returns None otherwise *)
let rule4 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Forward(t1, t2), Function(Continuation(t3, t4), t5) ->
    (match (equivalent_type t1 t3) with
     | None -> None
     | Some t1 ->
       let t2, t4, t5 =
         update_type t1 t2,
         update_type t1 t4,
         update_type t1 t5 in
       (match List.exists ~f:is_mismatch [t2;t4;t5] with
        | true -> None
        | false -> Some (List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t2, t4), t5))))
  | _ -> None

(* Applies Rule 5 if applicable. Returns None otherwise *)
let rule5 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | t1, Function(Continuation(Backward(t2, t3), t4), t5) ->
    (match (equivalent_type t1 t2) with
     | None -> None
     | Some t1 ->
       let t3, t4, t5 =
         update_type t1 t3,
         update_type t1 t4,
         update_type t1 t5 in
       (match List.exists ~f:is_mismatch [t3;t4;t5] with
        | true -> None
        | false -> Some (List [Single (Construct META); Single (Construct BACKWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t3, t4), t5))))
  | _ -> None

(* Applies Rule 6 if applicable. Returns None otherwise *)
let rule6 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(Forward(t1, t2), t3), t4), t5 ->
    (match (equivalent_type t1 t5) with
     | None -> None
     | Some t1 ->
       let t2, t3, t4 =
         update_type t1 t2,
         update_type t1 t3,
         update_type t1 t4 in
       (match List.exists ~f:is_mismatch [t2;t3;t4] with
        | true -> None
        | false -> Some (List [Single (Construct META); Single (Construct FORWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t2, t3), t4))))
  | _ -> None

(* Applies Rule 7 if applicable. Returns None otherwise *)
let rule7 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(t1, t2), t3), Function(Continuation(Backward(t4, t5), t6), t7) ->
    (match (equivalent_type t1 t4), (equivalent_type t2 t7) with
     | None, _ | _, None -> None
     | Some t1, Some t2 ->
       let t5, t6, t3 =
         update_type t1 t5,
         update_type t1 t6,
         update_type t1 t3 in
       let t5, t6, t3 =
         update_type t2 t5,
         update_type t2 t6,
         update_type t2 t3 in
       (match List.exists ~f:is_mismatch [t5;t6;t3] with
        | true -> None
        | false -> Some (List [Single (Construct META); Single (Construct BACKWARD); tok1; tok2], Function(Continuation(t5, t6), t3))))
  | _ -> None

(* Applies Rule 8 if applicable. Returns None otherwise *)
let rule8 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(Forward(t1, t2), t3), t4), Function(Continuation(t5, t6), t7) ->
    (match (equivalent_type t1 t5), (equivalent_type t3 t7) with
     | None, _ | _, None -> None
     | Some t1, Some t3 ->
       let t2, t6, t4 =
         update_type t1 t2,
         update_type t1 t6,
         update_type t1 t4 in
       let t2, t6, t4 =
         update_type t3 t2,
         update_type t3 t6,
         update_type t3 t4 in
       (match List.exists ~f:is_mismatch [t2;t6;t4] with
        | true -> None
        | false -> Some (List [Single (Construct META); Single (Construct FORWARD); tok1; tok2], Function(Continuation(t2, t6), t4))))
  | _ -> None

(* Applies Rule 9 if applicable. Returns None otherwise *)
let rule9 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Forward(t1, t2), Function(Continuation(t3, t4), t5) ->
    (match (equivalent_type t1 t3) with
     | None -> None
     | Some t1 ->
       let t2, t4, t5 =
         update_type t1 t2,
         update_type t1 t4,
         update_type t1 t5 in
       (match List.exists ~f:is_mismatch [t2;t4;t5] with
        | true -> None
        | false -> Some (List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); tok1]]; List [Single (Construct SKIP); Single (Construct LIFT); tok2]], Function(Continuation(Function(Continuation(t2, Empty "2"), Empty "2"), t4), t5))))
  | _ -> None

(* Applies Rule 10 if applicable. Returns None otherwise *)
let rule10 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(t1, t2), t3), Backward(t4, t5) ->
    (match (equivalent_type t1 t4) with
     | None -> None
     | Some t1 ->
       let t5, t2, t3 =
         update_type t1 t5,
         update_type t1 t2,
         update_type t1 t3 in
       (match List.exists ~f:is_mismatch [t5;t2;t3] with
        | true -> None
        | false -> Some (List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct SKIP); Single (Construct LIFT); tok1]; List [Single (Construct LIFT); List [Single (Construct LIFT); tok2]]], Function(Continuation(Function(Continuation(t5, Empty "1"), Empty "1"), t2), t3))))
  | _ -> None

(* Applies Rule 11 if applicable. Returns None otherwise *)
let rule11 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(t1, t2), t3), Function(Continuation(Function(Continuation(Backward(t4, t5), t6), t7), t8), t9) ->
    (match (equivalent_type t1 t4), (equivalent_type t2 t7) with
     | None, _ | _, None -> None
     | Some t1, Some t2 ->
       let t5, t6, t3, t8, t9 =
         update_type t1 t5,
         update_type t1 t6,
         update_type t1 t3,
         update_type t1 t8,
         update_type t1 t9 in
       let t5, t6, t3, t8, t9 =
         update_type t2 t5,
         update_type t2 t6,
         update_type t2 t3,
         update_type t2 t8,
         update_type t2 t9 in
       (match List.exists ~f:is_mismatch [t5;t6;t3;t8;t9] with
        | true -> None
        | false -> Some (List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(Function(Continuation(t5, t6), t3), t8), t9))))
  | _ -> None

(* Applies Rule 12 if applicable. Returns None otherwise *)
let rule12 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(Function(Continuation(t1, t2), t3), t4), t5), Function(Continuation(Backward(t6, t7), t8), t9) ->
    (match (equivalent_type t1 t6), (equivalent_type t2 t9) with
     | None, _ | _, None -> None
     | Some t1, Some t2 ->
       let t7, t8, t3, t4, t5 =
         update_type t1 t7,
         update_type t1 t8,
         update_type t1 t3,
         update_type t1 t4,
         update_type t1 t5 in
       let t7, t8, t3, t4, t5 =
         update_type t2 t7,
         update_type t2 t8,
         update_type t2 t3,
         update_type t2 t4,
         update_type t2 t5 in
       (match List.exists ~f:is_mismatch [t7;t8;t3;t4;t5] with
        | true -> None
        | false -> Some (List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(Function(Continuation(t7, t8), t3), t4), t5))))
  | _ -> None

(* Applies Rule 13 if applicable. Returns None otherwise *)
let rule13 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(Forward(t1, t2), t3), t4), Function(Continuation(Function(Continuation(t5, t6), t7), t8), t9) ->
    (match (equivalent_type t1 t5), (equivalent_type t3 t7) with
     | None, _ | _, None -> None
     | Some t1, Some t3 ->
       let t2, t6, t4, t8, t9 =
         update_type t1 t2,
         update_type t1 t6,
         update_type t1 t4,
         update_type t1 t8,
         update_type t1 t9 in
       let t2, t6, t4, t8, t9 =
         update_type t3 t2,
         update_type t3 t6,
         update_type t3 t4,
         update_type t3 t8,
         update_type t3 t9 in
       (match List.exists ~f:is_mismatch [t2;t6;t4;t8;t9] with
        | true -> None
        | false -> Some (List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(Function(Continuation(t2, t6), t4), t8), t9))))
  | _ -> None

(* Applies Rule 14 if applicable. Returns None otherwise *)
let rule14 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Function(Continuation(Function(Continuation(Forward(t1, t2), t3), t4), t5), t6), Function(Continuation(t7, t8), t9) ->
    (match (equivalent_type t1 t7), (equivalent_type t3 t9) with
     | None, _ | _, None -> None
     | Some t1, Some t3 ->
       let t2, t8, t4, t5, t6 =
         update_type t1 t2,
         update_type t1 t8,
         update_type t1 t4,
         update_type t1 t5,
         update_type t1 t6 in
       let t2, t8, t4, t5, t6 =
         update_type t3 t2,
         update_type t3 t8,
         update_type t3 t4,
         update_type t3 t5,
         update_type t3 t6 in
       (match List.exists ~f:is_mismatch [t2;t8;t4;t5;t6] with
        | true -> None
        | false -> Some (List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(Function(Continuation(t2, t8), t4), t5), t6))))
  | _ -> None

(* Applies Rule 15 if applicable. Returns None otherwise *)
let rule15 (tok1 : Tokens.t) (tok2 : Tokens.t) (type1 : Type.t) (type2 : Type.t) : (Tokens.t * Type.t) option =
  match type1, type2 with
  | Bind, Function(Continuation(Element, t1), t2) ->
    Some (List [Single (Construct BIND); tok2], Function(Continuation(Element, Pronoun(Element, t1)), t2))
  | _ -> None

(* Given two phrases and their types, applies all the possible rules and returns all possible combinations *)
let apply_all_rules ((tok1, type1): Tokens.t * Type.t) ((tok2, type2): Tokens.t * Type.t) : (Tokens.t * Type.t) list =
  [rule1 tok1 tok2 type1 type2;
   rule2 tok1 tok2 type1 type2;
   rule3 tok1 tok2 type1 type2;
   rule4 tok1 tok2 type1 type2;
   rule5 tok1 tok2 type1 type2;
   rule6 tok1 tok2 type1 type2;
   rule7 tok1 tok2 type1 type2;
   rule8 tok1 tok2 type1 type2;
   rule9 tok1 tok2 type1 type2;
   rule10 tok1 tok2 type1 type2;
   rule11 tok1 tok2 type1 type2;
   rule12 tok1 tok2 type1 type2;
   rule13 tok1 tok2 type1 type2;
   rule14 tok1 tok2 type1 type2;
   rule15 tok1 tok2 type1 type2]
  |> List.filter_map ~f:(fun x -> x)

(* Iterates through a list of tokens and finds all rules that could combine two consecutive tokens *)
let rec find_all_rules (prev : (Tokens.t * Type.t) list) (lst : (Tokens.t * Type.t) list) : (Tokens.t * Type.t) list list =
  match lst with
  | e1 :: e2 :: tl ->
    (match apply_all_rules e1 e2 with
     | [] ->
       find_all_rules (prev @ [e1]) (e2 :: tl)
     | new_tok_lst ->
       let full_tok_lst = Core.List.map ~f:(fun (new_toks, new_type) ->
         prev @ ((new_toks, new_type) :: tl)) new_tok_lst in
       full_tok_lst @ (find_all_rules (prev @ [e1]) (e2 :: tl)))
  | _ -> [] ;;

(* Evaluates the type of a token and returns None if it does not evaluate to a truth value, pronoun value, or something that will eventually become a truth value or pronoun value*)
let rec evaluate ((toks, semantic_type) : Tokens.t * Type.t) : (Tokens.t * Type.t) option =
  match semantic_type with
  | Pronoun(Element, Truth) -> Some (toks, semantic_type)
  | Truth -> Some (toks, semantic_type)
  | Function(Continuation(Truth, Truth), t1) ->
    evaluate (List [Single (Construct EVAL); toks], t1)
  | Function(Continuation(Function(Continuation(Truth, Truth), t1), t2), t3) ->
    evaluate (List [Single (Construct SKIP); Single (Construct EVAL); toks], Function(Continuation(t1, t2), t3))
  | _ -> None

(* Iterates through a queue and recursively builds all possible parses *)
let rec build_all_trees (completed : (Tokens.t * Type.t) list) (lst : (Tokens.t * Type.t) list list) : (Tokens.t * Type.t) list =
  match lst with
  | [] -> completed
  | [parse] :: tl ->
    (match (evaluate parse) with
    | Some eval -> build_all_trees (eval :: completed) tl
    | None -> build_all_trees completed tl)
  | to_find :: tl ->
    let new_rules =  find_all_rules [] to_find in
    build_all_trees completed (new_rules @ tl)

(* Iterates through a list of tokens and adds BIND operator if applicable *)
let rec bind (result : (Tokens.t * Type.t) list list) (prev : (Tokens.t * Type.t) list) (lst : (Tokens.t * Type.t) list) : (Tokens.t * Type.t) list list =
  match lst with
  | [] -> prev :: result
  | (tok, semantic_type) :: tl ->
    (match semantic_type with
     | Function(Continuation(Element, t1), t2) ->
       let new_result = prev @ ((Single (Construct BIND), Bind) :: lst) in
       bind (new_result :: result) (prev @ [(tok, semantic_type)]) tl
     | _ -> bind result (prev @ [(tok, semantic_type)]) tl
    )

(* Builds all continuation-based readings given a sentence *)
let build_continuation (word_list : string list) : Tokens.t list =
  let token_type_list =
    List.map word_list ~f:(fun str ->
        let tok = Token.of_string str in
        let tok_type = Token.to_type tok in
        (Tokens.Single tok, tok_type)) in
  let add_binds_list = bind [] [] token_type_list in
  let continuations = build_all_trees [] add_binds_list in
  continuations
  |> List.dedup_and_sort ~compare:(fun (tok1, _) (tok2, _) -> Stdlib.compare tok1 tok2)
  |> List.map ~f:(fun (tok, _) -> tok)
