(*
                   Lambda Construct for a Simple Language
*)

(* Lambda Expression *)
type t =
  | LWord of string
  | LId of string
  | LLam of string * t
  | LForall of string * t
  | LExists of string * t
  | LApp of t * t

(* function that takes a lambda expression as input and returns a corresponding string *)
val to_string : t -> string

(* function that applies one lambda expression to another and reduces it *)
val apply : t -> t -> t
