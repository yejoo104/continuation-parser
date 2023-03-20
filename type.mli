type t =
  | Construct (* This is for convenience because we can't represent polymorphism yet *)
  | Bind (* This represents a bind *)
  | Empty of string (* undetermined type *)
  | Element
  | Truth
  | Type_Mismatch (* indicates that type was not resolved *)
  | Forward of t * t (* / operator *)
  | Backward of t * t (* \ operator *)
  | Continuation of t * t (* ~ operator *)
  | Function of t * t (* -> operator *)
  | Pronoun of t * t (* |> operator *)

val to_string : t -> string

val is_mismatch : t -> bool
val equivalent_type : t -> t -> (string option * t) option
val update_type : (string option * t) -> t -> t

