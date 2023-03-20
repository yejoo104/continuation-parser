type t =
  | LWord of string
  | LId of string
  | LLam of string * t
  | LForall of string * t
  | LExists of string * t
  | LApp of t * t

val to_string : t -> string

val apply : t -> t -> t
