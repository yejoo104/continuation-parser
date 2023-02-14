open Core ;;
open Types ;;

type construct =
  | FORWARD
  | BACKWARD

let construct_to_str (construct : construct) : string =
  match construct with
  | FORWARD -> "F"
  | BACKWARD -> "B"

type token =
  | Construct of construct
  | Whansung
  | Yejoo
  | Yejoos
  | Mother
  | Left
  | Saw
  | Thought

type tokens =
  | Single of token
  | List of tokens list

let str_to_token (str : string) : token =
  match str with
  | "whansung" -> Whansung
  | "yejoo" -> Yejoo
  | "yejoo's" -> Yejoos
  | "mother" -> Mother
  | "left" -> Left
  | "saw" -> Saw
  | "thought" -> Thought
  | _ -> failwith "couldn't match string to token"

let token_to_str (tok : token) : string =
  match tok with
  | Construct construct -> construct_to_str construct
  | Whansung -> "whansung"
  | Yejoo -> "yejoo"
  | Yejoos -> "yejoo's"
  | Mother -> "mother"
  | Left -> "left"
  | Saw -> "saw"
  | Thought -> "thought"

let token_to_type (tok : token) : semantic_type =
  match tok with
  | Construct _ -> Construct
  | Whansung -> Element
  | Yejoo -> Element
  | Yejoos -> Element
  | Mother -> Backward(Element, Element)
  | Left -> Backward(Element, Truth)
  | Saw -> Forward(Element, Backward(Element, Truth))
  | Thought -> Forward(Truth, Backward(Element, Truth))

let rec tokens_to_str (toks : tokens) : string =
  match toks with
  | Single tok -> token_to_str tok
  | List toks ->
    "(" ^
    let strs = List.map ~f:(fun toks -> tokens_to_str toks) toks in
    List.fold_left ~init:(List.hd_exn strs) ~f:(fun str1 str2 -> str1 ^ " " ^ str2) (List.tl_exn strs)
    ^ ")"
