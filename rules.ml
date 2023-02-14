open Types ;;
open Lexicon ;;

let apply_rule ((tok1, type1): tokens * semantic_type) ((tok2, type2): tokens * semantic_type) : (tokens * semantic_type) option =
  match type1, type2 with
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
      build_tree new_lst ;;
