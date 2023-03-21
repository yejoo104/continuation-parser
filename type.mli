(*
                  Types of Continuation Based Semantics
*)

(* Types *)
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

(* Returns True if there is a type mismatch *)
val is_mismatch : t -> bool
(* Given two types, returns whether they are or could be equivalent *)
val equivalent_type : t -> t -> (string option * t) option
(* Given a new type and an old type, either reconciles them or creates a type mismatch if irreconcilable *)
val update_type : (string option * t) -> t -> t

