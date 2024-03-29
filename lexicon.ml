(*
                       Lexicon for a Simple Language
*)
open Core ;;
open Type ;;
open Lterm ;;

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

  let to_lambda (construct : t) : Lterm.t =
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
    | He
    | They
    | Her
    | His
    | Their
    | Him
    | Them
    | Everyone
    | Everyones
    | Someone
    | If
    | A
    | Is
    | On
    | InFrontOf
    | Whansung
    | Yejoo
    | Yejoos
    | Mother
    | Mothers
    | Friend
    | Birthday
    | Lawyer
    | Woman
    | Pretty
    | Bankrupt
    | Poor
    | Left
    | Saw
    | Thought
    | Loves
    | Called
    | Interviewed

  let of_string (str : string) : t =
    match str with
    | "she" -> She
    | "he" -> He
    | "they" -> They
    | "her" -> Her
    | "his" -> His
    | "their" -> Their
    | "him" -> Him
    | "them" -> Them
    | "everyone" -> Everyone
    | "everyone's" -> Everyones
    | "someone" -> Someone
    | "if" -> If
    | "a" -> A
    | "is" -> Is
    | "on" -> On
    | "in-front-of" -> InFrontOf
    | "whansung" -> Whansung
    | "yejoo" -> Yejoo
    | "yejoo's" -> Yejoos
    | "mother" -> Mother
    | "mother's" -> Mothers
    | "friend" -> Friend
    | "birthday" -> Birthday
    | "lawyer" -> Lawyer
    | "woman" -> Woman
    | "pretty" -> Pretty
    | "bankrupt" -> Bankrupt
    | "poor" -> Poor
    | "left" -> Left
    | "saw" -> Saw
    | "thought" -> Thought
    | "loves" -> Loves
    | "interviewed" -> Interviewed
    | "called" -> Called
    | _ -> failwith "couldn't match string to token"

  let to_string (tok : t) : string =
    match tok with
    | Construct construct -> Construct.to_string construct
    | She -> "she"
    | He -> "he"
    | They -> "they"
    | Her -> "her"
    | His -> "his"
    | Their -> "their"
    | Him -> "him"
    | Them -> "them"
    | Everyone -> "everyone"
    | Everyones -> "everyone's"
    | Someone -> "someone"
    | If -> "if"
    | A -> "a"
    | Is -> "is"
    | On -> "on"
    | InFrontOf -> "in front of"
    | Whansung -> "whansung"
    | Yejoo -> "yejoo"
    | Yejoos -> "yejoo's"
    | Mother -> "mother"
    | Mothers -> "mother's"
    | Friend -> "friend"
    | Birthday -> "birthday"
    | Lawyer -> "lawyer"
    | Woman -> "woman"
    | Pretty -> "pretty"
    | Bankrupt -> "bankrupt"
    | Poor -> "poor"
    | Left -> "left"
    | Saw -> "saw"
    | Thought -> "thought"
    | Loves -> "loves"
    | Called -> "called"
    | Interviewed -> "interviewed"

  let to_type (tok : t) : Type.t =
    match tok with
    | Construct _ -> Construct
    (* Note: actual pronoun type is (e ~ A) -> (e |> a). we assume a = t *)
    | She | Her | He | His | Him | They | Their | Them -> Function(Continuation(Element, Truth), Pronoun(Element, Truth))
    | Everyone | Everyones -> Function(Continuation(Element, Truth), Truth)
    | Someone -> Function(Continuation(Element, Truth), Truth)
    | If -> Forward(Truth, Forward(Truth, Truth))
    | A -> Forward(Backward(Element, Truth), Function(Continuation(Element, Truth), Truth))
    | Is -> Forward(Backward(Element, Truth), Backward(Element, Truth))
    | On -> Forward(Element, Backward(Backward (Element, Truth), Backward(Element, Truth)))
    | InFrontOf -> Forward(Element, Backward(Backward (Element, Truth), Backward(Element, Truth)))
    | Whansung -> Element
    | Yejoo | Yejoos -> Element
    | Mother | Mothers -> Backward(Element, Element)
    | Friend -> Backward(Element, Element)
    | Birthday -> Backward(Element, Element)
    | Lawyer -> Backward(Element, Element)
    | Woman -> Backward(Element, Truth)
    | Pretty -> Backward(Element, Truth)
    | Bankrupt -> Backward(Element, Truth)
    | Poor -> Backward(Element, Truth)
    | Left -> Backward(Element, Truth)
    | Saw -> Forward(Element, Backward(Element, Truth))
    | Thought -> Forward(Truth, Backward(Element, Truth))
    | Loves -> Forward(Element, Backward(Element, Truth))
    | Called -> Forward(Element, Backward(Element, Truth))
    | Interviewed -> Forward(Element, Backward(Element, Truth))

  let to_lambda (tok : t) : Lterm.t =
    match tok with
    | Construct c -> Construct.to_lambda c
    | She | Her | He | His | Him | They | Their | Them -> LLam ("c", LId "c")
    | Everyone | Everyones -> LLam ("c", LForall ("x", LApp (LId "c", LId "x")))
    | Someone -> LLam ("c", LExists ("x", LApp (LId "c", LId "x")))
    | If -> LLam("P", LLam ("Q", LImplies (LId "P", LId "Q")))
    | A -> LLam("P", LLam("c", LExists ("x", LAnd (LApp (LId "P", LId "x"), LApp (LId "c", LId "x")))))
    | Is -> LLam ("x", LId "x")
    | On -> LWord "on"
    | InFrontOf -> LWord "in-front-of"
    | Whansung -> LWord "whansung"
    | Yejoo | Yejoos -> LWord "yejoo"
    | Mother | Mothers -> LWord "mother"
    | Friend -> LWord "friend"
    | Birthday -> LWord "birthday"
    | Lawyer -> LWord "lawyer"
    | Woman -> LWord "woman"
    | Pretty -> LWord "pretty"
    | Bankrupt -> LWord "bankrupt"
    | Poor -> LWord "poor"
    | Left -> LWord "left"
    | Saw -> LWord "saw"
    | Thought -> LWord "thought"
    | Loves -> LWord "love"
    | Called -> LWord "call"
    | Interviewed -> LWord "interview"
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

  let rec to_lambda (toks : t) : Lterm.t =
    match toks with
    | Single tok -> Token.to_lambda tok
    | List toks ->
      let lambdas = List.map ~f:(fun toks -> to_lambda toks) toks in
      List.fold_left ~init:(List.hd_exn lambdas) ~f:(fun l1 l2 -> Lterm.apply l1 l2) (List.tl_exn lambdas)
end
