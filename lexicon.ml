(*
                       Lexicon for a Simple Language
*)
open Core ;;
open Type ;;
open Lambda ;;

(* module of special constructs in continuation semantics *)
module Construct = struct
  type t =
    | FORWARD
    | BACKWARD
    | LIFT
    | EVAL
    | SKIP
    | META
    | BIND

  let to_string (construct : t) : string =
    match construct with
    | FORWARD -> "F"
    | BACKWARD -> "B"
    | LIFT -> "L"
    | EVAL -> "E"
    | SKIP -> "S"
    | META -> "M"
    | BIND -> "BIND"

  let to_lambda (construct : t) : Lambda.t =
    match construct with
    | FORWARD -> LLam("f", LLam("x", LApp(LId "f", LId "x")))
    | BACKWARD -> LLam("x", LLam("f", LApp(LId "f", LId "x")))
    | LIFT -> LLam("x", LLam("c", LApp(LId "c", LId "x")))
    | EVAL -> LLam("X", LApp(LId "X", LLam("x", LId "x")))
    | SKIP -> LLam("X", LLam("Y", LLam("c", LApp(LId "Y", LLam("y", LApp(LId "c", LApp(LId "X", LId "y")))))))
    | META -> LLam("X", LLam("L", LLam("R", LLam("c", LApp(LId "L", LLam("l", LApp(LId "R", LLam("r", LApp(LId "c", LApp(LApp(LId "X", LId "l"), LId "r"))))))))))
    | BIND -> LLam("X", LLam("c", LApp(LId "X", LLam("x", LApp(LApp(LId "c", LId "x"), LId "x")))))
end

(* module of tokens that we are using in our language *)
module Token = struct
  type t =
    | Construct of Construct.t
    | She
    | Her
    | Everyone
    | Everyones
    | Someone
    | Whansung
    | Yejoo
    | Yejoos
    | Mother
    | Mothers
    | Friend
    | Left
    | Saw
    | Thought
    | Loves

  let of_string (str : string) : t =
    match str with
    | "she" -> She
    | "her" -> Her
    | "everyone" -> Everyone
    | "everyone's" -> Everyones
    | "someone" -> Someone
    | "whansung" -> Whansung
    | "yejoo" -> Yejoo
    | "yejoo's" -> Yejoos
    | "mother" -> Mother
    | "mother's" -> Mothers
    | "friend" -> Friend
    | "left" -> Left
    | "saw" -> Saw
    | "thought" -> Thought
    | "loves" -> Loves
    | _ -> failwith "couldn't match string to token"

  let to_string (tok : t) : string =
    match tok with
    | Construct construct -> Construct.to_string construct
    | She -> "she"
    | Her -> "her"
    | Everyone -> "everyone"
    | Everyones -> "everyone's"
    | Someone -> "someone"
    | Whansung -> "whansung"
    | Yejoo -> "yejoo"
    | Yejoos -> "yejoo's"
    | Mother -> "mother"
    | Mothers -> "mother's"
    | Friend -> "friend"
    | Left -> "left"
    | Saw -> "saw"
    | Thought -> "thought"
    | Loves -> "loves"

  let to_type (tok : t) : Type.t =
    match tok with
    | Construct _ -> Construct
    (* Note: actual pronoun type is (e ~ A) -> (e |> a). we assume a = t *)
    | She | Her -> Function(Continuation(Element, Truth), Pronoun(Element, Truth))
    | Everyone | Everyones -> Function(Continuation(Element, Truth), Truth)
    | Someone -> Function(Continuation(Element, Truth), Truth)
    | Whansung -> Element
    | Yejoo | Yejoos -> Element
    | Mother | Mothers -> Backward(Element, Element)
    | Friend -> Backward(Element, Element)
    | Left -> Backward(Element, Truth)
    | Saw -> Forward(Element, Backward(Element, Truth))
    | Thought -> Forward(Truth, Backward(Element, Truth))
    | Loves -> Forward(Element, Backward(Element, Truth))

  let to_lambda (tok : t) : Lambda.t =
    match tok with
    | Construct c -> Construct.to_lambda c
    | She | Her -> LLam ("c", LId "c")
    | Everyone | Everyones -> LLam ("c", LForall ("x", LApp (LId "c", LId "x")))
    | Someone -> LLam ("c", LExists ("x", LApp (LId "c", LId "x")))
    | Whansung -> LWord "whansung"
    | Yejoo | Yejoos -> LWord "yejoo"
    | Mother | Mothers -> LWord "mother"
    | Friend -> LWord "friend"
    | Left -> LWord "left"
    | Saw -> LWord "saw"
    | Thought -> LWord "thought"
    | Loves -> LWord "love"
end

(* module of how phrases combine in our language *)
module Phrase = struct
  type t =
    | Single of Token.t
    | List of t list

  let rec to_string (toks : t) : string =
    match toks with
    | Single tok -> Token.to_string tok
    | List toks ->
      "(" ^
      let strs = List.map ~f:(fun toks -> to_string toks) toks in
      List.fold_left ~init:(List.hd_exn strs) ~f:(fun str1 str2 -> str1 ^ " " ^ str2) (List.tl_exn strs)
      ^ ")"

  let rec to_lambda (toks : t) : Lambda.t =
    match toks with
    | Single tok -> Token.to_lambda tok
    | List toks ->
      let lambdas = List.map ~f:(fun toks -> to_lambda toks) toks in
      List.fold_left ~init:(List.hd_exn lambdas) ~f:(fun l1 l2 -> Lambda.apply l1 l2) (List.tl_exn lambdas)
end
