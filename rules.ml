open Core ;;
open Type ;;
open Lexicon ;;

let apply_all_rules ((tok1, type1): Tokens.t * Type.t) ((tok2, type2): Tokens.t * Type.t) : (Tokens.t * Type.t) list option =
  match type1, type2 with
  (* Putting this at front for ease of pattern-matching. Clean up a little bit afterwards *)
  (* TODO: DEFINITELY CLEAN THIS SHIT UP *)
  (* TODO: resolve Empty type in a consistent way *)
  (* Rule 11 *)
  | Function(Continuation(t1, t2), t3), Function(Continuation(Function(Continuation(Backward(t4, t5), t6), t7), t8), t9) ->
    (match (equivalent_type t1 t4), (equivalent_type t2 t7) with
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
       if List.exists ~f:is_mismatch [t5;t6;t3;t8;t9] then None else
       Some [List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(Function(Continuation(t5, t6), t3), t8), t9)]
     | None, _ | _, None -> None)
  (* Rule 12 *)
  | Function(Continuation(Function(Continuation(t1, t2), t3), t4), t5), Function(Continuation(Backward(t6, t7), t8), t9) ->
    (match (equivalent_type t1 t6), (equivalent_type t2 t9) with
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
       if List.exists ~f:is_mismatch [t7;t8;t3;t4;t5] then None else
       Some [List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(Function(Continuation(t7, t8), t3), t4), t5)]
     | None, _ | _, None -> None)
  (* Rule 13 *)
  | Function(Continuation(Forward(t1, t2), t3), t4), Function(Continuation(Function(Continuation(t5, t6), t7), t8), t9) ->
    (match (equivalent_type t1 t5), (equivalent_type t3 t7) with
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
       if List.exists ~f:is_mismatch [t2;t6;t4;t8;t9] then None else
       Some [List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(Function(Continuation(t2, t6), t4), t8), t9)]
     | None, _ | _, None -> None
    )
  (* Rule 14 *)
  | Function(Continuation(Function(Continuation(Forward(t1, t2), t3), t4), t5), t6), Function(Continuation(t7, t8), t9) ->
    (match (equivalent_type t1 t7), (equivalent_type t3 t9) with
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
       if List.exists ~f:is_mismatch [t2;t8;t4;t5;t6] then None else
       Some [List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(Function(Continuation(t2, t8), t4), t5), t6)]
     | None, _ | _, None -> None
    )
  (* Rule 7 *)
  | Function(Continuation(t1, t2), t3), Function(Continuation(Backward(t4, t5), t6), t7) ->
    (match (equivalent_type t1 t4), (equivalent_type t2 t7) with
     | Some t1, Some t2 ->
       let t5, t6, t3 =
         update_type t1 t5,
         update_type t1 t6,
         update_type t1 t3 in
       let t5, t6, t3 =
         update_type t2 t5,
         update_type t2 t6,
         update_type t2 t3 in
       if List.exists ~f:is_mismatch [t5;t6;t3] then None else
       Some [List [Single (Construct META); Single (Construct BACKWARD); tok1; tok2], Function(Continuation(t5, t6), t3)]
     | None, _ | _, None -> None)
  (* Rule 8 *)
  | Function(Continuation(Forward(t1, t2), t3), t4), Function(Continuation(t5, t6), t7) ->
    (match (equivalent_type t1 t5), (equivalent_type t3 t7) with
     | Some t1, Some t3 ->
       let t2, t6, t4 =
         update_type t1 t2,
         update_type t1 t6,
         update_type t1 t4 in
       let t2, t6, t4 =
         update_type t3 t2,
         update_type t3 t6,
         update_type t3 t4 in
       if List.exists ~f:is_mismatch [t2;t6;t4] then None else
       Some [List [Single (Construct META); Single (Construct FORWARD); tok1; tok2], Function(Continuation(t2, t6), t4)]
     | None, _ | _, None -> None)
  (* Rule 5 *)
  | t1, Function(Continuation(Backward(t2, t3), t4), t5) ->
    (match (equivalent_type t1 t2) with
     | Some t1 ->
       let t3, t4, t5 =
         update_type t1 t3,
         update_type t1 t4,
         update_type t1 t5 in
       if List.exists ~f:is_mismatch [t3;t4;t5] then None else
       Some [List [Single (Construct META); Single (Construct BACKWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t3, t4), t5)]
     | None -> None)
  (* Rule 6 *)
  | Function(Continuation(Forward(t1, t2), t3), t4), t5 ->
    (match (equivalent_type t1 t5) with
     | Some t1 ->
       let t2, t3, t4 =
         update_type t1 t2,
         update_type t1 t3,
         update_type t1 t4 in
       if List.exists ~f:is_mismatch [t2;t3;t4] then None else
       Some [List [Single (Construct META); Single (Construct FORWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t2, t3), t4)]
     | None -> None)
  (* Rule 3, 10 *)
  | Function(Continuation(t1, t2), t3), Backward(t4, t5) ->
    (match (equivalent_type t1 t4) with
     | Some t1 ->
       let t5, t2, t3 =
         update_type t1 t5,
         update_type t1 t2,
         update_type t1 t3 in
       if List.exists ~f:is_mismatch [t5;t2;t3] then None else
       Some [List [Single (Construct META); Single (Construct BACKWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t5, t2), t3);
             List [Single (Construct META); List [Single (Construct META); Single (Construct BACKWARD)]; List [Single (Construct SKIP); Single (Construct LIFT); tok1]; List [Single (Construct LIFT); List [Single (Construct LIFT); tok2]]], Function(Continuation(Function(Continuation(t5, Empty "1"), Empty "1"), t2), t3)]
     | None -> None)
  (* Rule 4, 9 *)
  | Forward(t1, t2), Function(Continuation(t3, t4), t5) ->
    (match (equivalent_type t1 t3) with
     | Some t1 ->
       let t2, t4, t5 =
         update_type t1 t2,
         update_type t1 t4,
         update_type t1 t5 in
       if List.exists ~f:is_mismatch [t2;t4;t5] then None else
       Some [List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t2, t4), t5);
             List [Single (Construct META); List [Single (Construct META); Single (Construct FORWARD)]; List [Single (Construct LIFT); List [Single (Construct LIFT); tok1]]; List [Single (Construct SKIP); Single (Construct LIFT); tok2]], Function(Continuation(Function(Continuation(t2, Empty "2"), Empty "2"), t4), t5)]
     | None -> None)
  (* Rule 1 *)
  | t1, Backward(t2, t3) ->
    (match (equivalent_type t1 t2) with
     | Some t1 ->
       let t3 = update_type t1 t3 in
       if is_mismatch t3 then None else
       Some [List [Single (Construct BACKWARD); tok1; tok2], t3]
     | None -> None)
  (* Rule 2 *)
  | Forward(t1, t2), t3 ->
    (match (equivalent_type t1 t3) with
     | Some t1 ->
       let t2 = update_type t1 t2 in
       if is_mismatch t2 then None else
       Some [List [Single (Construct FORWARD); tok1; tok2], t2]
     | None -> None)
  (* Rule 15 Note: other half of rule found in bind function below *)
  | Bind, Function(Continuation(Element, t1), t2) ->
    Some [List [Single (Construct BIND); tok2], Function(Continuation(Element, Pronoun(Element, t1)), t2)]
  | _ -> None

let rec find_all_rules (prev : (Tokens.t * Type.t) list) (lst : (Tokens.t * Type.t) list) : (Tokens.t * Type.t) list list =
  match lst with
  | e1 :: e2 :: tl ->
    (match apply_all_rules e1 e2 with
     | Some new_tok_lst ->
       let full_tok_lst = Core.List.map ~f:(fun (new_toks, new_type) ->
         prev @ ((new_toks, new_type) :: tl)) new_tok_lst in
       full_tok_lst @ (find_all_rules (prev @ [e1]) (e2 :: tl))
     | None ->
       find_all_rules (prev @ [e1]) (e2 :: tl))
  | _ -> [] ;;

let rec evaluate ((toks, semantic_type) : Tokens.t * Type.t) : (Tokens.t * Type.t) option =
  match semantic_type with
  | Pronoun(Element, Truth) -> Some (toks, semantic_type)
  | Truth -> Some (toks, semantic_type)
  | Function(Continuation(Truth, Truth), t1) ->
    evaluate (List [Single (Construct EVAL); toks], t1)
  | Function(Continuation(Function(Continuation(Truth, Truth), t1), t2), t3) ->
    evaluate (List [Single (Construct SKIP); Single (Construct EVAL); toks], Function(Continuation(t1, t2), t3))
  | _ -> None

let rec build_all_trees (completed : (Tokens.t * Type.t) list) (lst : (Tokens.t * Type.t) list list) : (Tokens.t * Type.t) list =
  (* print_int (List.length completed); *)
  (* print_int (List.length lst); *)
  match lst with
  | [] -> completed
  | [parse] :: tl ->
    (* let toks, semantic_type = parse in *)
    (* print_string (Tokens.tokens_to_str toks); *)
    (* Out_channel.newline stdout; *)
    (* print_string (type_to_str semantic_type); *)
    (* Out_channel.newline stdout; *)
    (match (evaluate parse) with
    | Some eval -> build_all_trees (eval :: completed) tl
    | None -> build_all_trees completed tl)
  | to_find :: tl ->
    (* List.iter ~f:(fun (toks, _) -> print_string (tokens_to_str toks); Out_channel.newline stdout) to_find; *)
    (* print_string "current run done\n"; *)
    let new_rules =  find_all_rules [] to_find in
    build_all_trees completed (new_rules @ tl)

(* Rule 15 (one part of it lol) *)
(* TODO: Perhaps we want a separate name for adding BIND*)
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
