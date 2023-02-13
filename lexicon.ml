open Types ;;

type token =
  | Yejoo
  | Left

let str_to_token (str : string) : token =
  match str with
  | "yejoo" -> Yejoo
  | "left" -> Left
  | _ -> failwith "couldn't match string to token" ;;

let token_to_str (tok : token) : string =
  match tok with
  | Yejoo -> "yejoo"
  | Left -> "left" ;;

let token_to_type (tok : token) : semantic_type =
  match tok with
  | Yejoo -> Element
  | Left -> Backward(Element, Truth)
