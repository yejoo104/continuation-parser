open Types ;;
open Lexicon ;;

let apply_rule ((tok1, type1): tokens * semantic_type) ((tok2, type2): tokens * semantic_type) : (tokens * semantic_type) option =
  match type1, type2 with
  (* Putting this at front for ease of pattern-matching. Clean up a little bit afterwards *)
  (* TODO: DEFINITELY CLEAN THIS SHIT UP *)
  | Function(Continuation(t1, t2), t3), Function(Continuation(Backward(t4, t5), t6), t7) ->
    if t1 = t4 && t2 = t7 then Some(List [Single (Construct META); Single (Construct BACKWARD); tok1; tok2], Function(Continuation(t5, t6), t3))
    else None
  | Function(Continuation(Forward(t1, t2), t3), t4), Function(Continuation(t5, t6), t7) ->
    if t1 = t5 && t3 = t7 then Some(List [Single (Construct META); Single (Construct FORWARD); tok1; tok2], Function(Continuation(t2, t6), t4))
    else None
  | t1, Function(Continuation(Backward(t2, t3), t4), t5) ->
    if t1 = t2 then Some(List [Single (Construct META); Single (Construct BACKWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t3, t4), t5))
    else None
  | Function(Continuation(Forward(t1, t2), t3), t4), t5 ->
    if t1 = t5 then Some(List [Single (Construct META); Single (Construct FORWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t2, t3), t4))
    else None
  | Function(Continuation(t1, t2), t3), Backward(t4, t5) ->
    if t1 = t4 then Some (List [Single (Construct META); Single (Construct BACKWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t5, t2), t3))
    else None
  | Forward(t1, t2), Function(Continuation(t3, t4), t5) ->
    if t1 = t3 then Some (List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t2, t4), t5))
    else None
  | t1, Backward(t2, t3) ->
    if t1 = t2 then Some (List [Single (Construct BACKWARD); tok1; tok2], t3)
    else None
  | Forward(t1, t2), t3 ->
    if t1 = t3 then Some (List [Single (Construct FORWARD); tok1; tok2], t2)
    else None
  | _ -> None ;;

let rec find_rule (prev : (tokens * semantic_type) list) (lst : (tokens * semantic_type) list) : (tokens * semantic_type) list option =
  match lst with
  | e1 :: e2 :: tl ->
    (match apply_rule e1 e2 with
    | Some (new_toks, new_type) ->
      Some (prev @ ((new_toks, new_type) :: tl))
    | None ->
      find_rule (prev @ [e1]) (e2 :: tl))
  | _ -> None ;;

let rec build_tree (lst : (tokens * semantic_type) list) : tokens * semantic_type =
  match lst with
  | [] -> failwith "Empty List in build_tree\n"
  | [(toks, semantic_type)] -> toks, semantic_type
  | _ :: _ ->
    match find_rule [] lst with
    | None -> failwith "Implement Backtracking\n"
    | Some new_lst ->
      build_tree new_lst

let apply_all_rules ((tok1, type1): tokens * semantic_type) ((tok2, type2): tokens * semantic_type) : (tokens * semantic_type) list option =
  match type1, type2 with
  (* Putting this at front for ease of pattern-matching. Clean up a little bit afterwards *)
  (* TODO: DEFINITELY CLEAN THIS SHIT UP *)
  | Function(Continuation(t1, t2), t3), Function(Continuation(Backward(t4, t5), t6), t7) ->
    if t1 = t4 && t2 = t7 then Some [List [Single (Construct META); Single (Construct BACKWARD); tok1; tok2], Function(Continuation(t5, t6), t3)]
    else None
  | Function(Continuation(Forward(t1, t2), t3), t4), Function(Continuation(t5, t6), t7) ->
    if t1 = t5 && t3 = t7 then Some [List [Single (Construct META); Single (Construct FORWARD); tok1; tok2], Function(Continuation(t2, t6), t4)]
    else None
  | t1, Function(Continuation(Backward(t2, t3), t4), t5) ->
    if t1 = t2 then Some [List [Single (Construct META); Single (Construct BACKWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t3, t4), t5)]
    else None
  | Function(Continuation(Forward(t1, t2), t3), t4), t5 ->
    if t1 = t5 then Some [List [Single (Construct META); Single (Construct FORWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t2, t3), t4)]
    else None
  | Function(Continuation(t1, t2), t3), Backward(t4, t5) ->
    if t1 = t4 then Some [List [Single (Construct META); Single (Construct BACKWARD); tok1; List [Single (Construct LIFT); tok2]], Function(Continuation(t5, t2), t3)]
    else None
  | Forward(t1, t2), Function(Continuation(t3, t4), t5) ->
    if t1 = t3 then Some [List [Single (Construct META); Single (Construct FORWARD); List [Single (Construct LIFT); tok1]; tok2], Function(Continuation(t2, t4), t5)]
    else None
  | t1, Backward(t2, t3) ->
    if t1 = t2 then Some [List [Single (Construct BACKWARD); tok1; tok2], t3]
    else None
  | Forward(t1, t2), t3 ->
    if t1 = t3 then Some [List [Single (Construct FORWARD); tok1; tok2], t2]
    else None
  | _ -> None

let rec find_all_rules (prev : (tokens * semantic_type) list) (lst : (tokens * semantic_type) list) : (tokens * semantic_type) list list option =
  match lst with
  | e1 :: e2 :: tl ->
    (match apply_all_rules e1 e2 with
     | Some new_tok_lst ->
       let full_tok_lst = Core.List.map ~f:(fun (new_toks, new_type) ->
         prev @ ((new_toks, new_type) :: tl)) new_tok_lst in
       Some full_tok_lst
     | None ->
       find_all_rules (prev @ [e1]) (e2 :: tl))
  | _ -> None ;;

let rec build_all_trees (completed : (tokens * semantic_type) list) (lst : (tokens * semantic_type) list list) : (tokens * semantic_type) list =
  match lst with
  | [] -> completed
  | [(toks, semantic_type)] :: tl ->
    build_all_trees ((toks, semantic_type) :: completed) tl
  | to_find :: tl ->
    match find_all_rules [] to_find with
    | None -> build_all_trees completed tl
    | Some new_rules ->
      build_all_trees completed (new_rules @ tl)
