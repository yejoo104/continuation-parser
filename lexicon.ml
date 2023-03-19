open Core ;;
open Types ;;
open Lterm ;;

module Construct = struct
  type t =
    | FORWARD
    | BACKWARD
    | LIFT
    | EVAL
    | SKIP
    | META
    | BIND

  let construct_to_str (construct : t) : string =
    match construct with
    | FORWARD -> "F"
    | BACKWARD -> "B"
    | LIFT -> "L"
    | EVAL -> "E"
    | SKIP -> "S"
    | META -> "M"
    | BIND -> "BIND"

  let construct_to_lterm (construct : t) : Lambda.t =
    match construct with
    | FORWARD -> LLam("f", LLam("x", LApp(LId "f", LId "x")))
    | BACKWARD -> LLam("x", LLam("f", LApp(LId "f", LId "x")))
    | LIFT -> LLam("x", LLam("c", LApp(LId "c", LId "x")))
    | EVAL -> LLam("X", LApp(LId "X", LLam("x", LId "x")))
    | SKIP -> LLam("X", LLam("Y", LLam("c", LApp(LId "Y", LLam("y", LApp(LId "c", LApp(LId "X", LId "y")))))))
    | META -> LLam("X", LLam("L", LLam("R", LLam("c", LApp(LId "L", LLam("l", LApp(LId "R", LLam("r", LApp(LId "c", LApp(LApp(LId "X", LId "l"), LId "r"))))))))))
    | BIND -> LLam("X", LLam("c", LApp(LId "X", LLam("x", LApp(LApp(LId "c", LId "x"), LId "x")))))
end

module Token = struct
  type t =
    | Construct of Construct.t
    | Her
    | Everyone
    | Someone
    | Whansung
    | Yejoo
    | Yejoos
    | Mother
    | Left
    | Saw
    | Thought
    | Loves

  let str_to_token (str : string) : t =
    match str with
    | "her" -> Her
    | "everyone" -> Everyone
    | "someone" -> Someone
    | "whansung" -> Whansung
    | "yejoo" -> Yejoo
    | "yejoo's" -> Yejoos
    | "mother" -> Mother
    | "left" -> Left
    | "saw" -> Saw
    | "thought" -> Thought
    | "loves" -> Loves
    | _ -> failwith "couldn't match string to token"

  let token_to_str (tok : t) : string =
    match tok with
    | Construct construct -> Construct.construct_to_str construct
    | Her -> "her"
    | Everyone -> "everyone"
    | Someone -> "someone"
    | Whansung -> "whansung"
    | Yejoo -> "yejoo"
    | Yejoos -> "yejoo's"
    | Mother -> "mother"
    | Left -> "left"
    | Saw -> "saw"
    | Thought -> "thought"
    | Loves -> "loves"

  let token_to_type (tok : t) : semantic_type =
    match tok with
    | Construct _ -> Construct
    | Her -> Function(Continuation(Element, Truth), Pronoun(Element, Truth)) (* TODO: actual pronoun type is (e ~ A) -> (e |> a). we assume a = t *)
    | Everyone -> Function(Continuation(Element, Truth), Truth)
    | Someone -> Function(Continuation(Element, Truth), Truth)
    | Whansung -> Element
    | Yejoo -> Element
    | Yejoos -> Element
    | Mother -> Backward(Element, Element)
    | Left -> Backward(Element, Truth)
    | Saw -> Forward(Element, Backward(Element, Truth))
    | Thought -> Forward(Truth, Backward(Element, Truth))
    | Loves -> Forward(Element, Backward(Element, Truth))

  let token_to_lterm (tok : t) : Lambda.t =
    match tok with
    | Construct c -> Construct.construct_to_lterm c
end

module Tokens = struct
  type t =
    | Single of Token.t
    | List of t list

  let rec tokens_to_str (toks : t) : string =
    match toks with
    | Single tok -> Token.token_to_str tok
    | List toks ->
      "(" ^
      let strs = List.map ~f:(fun toks -> tokens_to_str toks) toks in
      List.fold_left ~init:(List.hd_exn strs) ~f:(fun str1 str2 -> str1 ^ " " ^ str2) (List.tl_exn strs)
      ^ ")"

end





