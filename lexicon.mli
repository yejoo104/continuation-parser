open Lambda ;;
open Type ;;

module Construct : sig
  type t =
    | FORWARD
    | BACKWARD
    | LIFT
    | EVAL
    | SKIP
    | META
    | BIND

  val to_string : t -> string
  val to_lambda : t -> Lambda.t
end

module Token : sig
  type t =
    | Construct of Construct.t
    | She
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

  val of_string : string -> t
  val to_string : t -> string
  val to_type : t -> Type.t
  val to_lambda : t -> Lambda.t
end

module Tokens : sig
  type t =
    | Single of Token.t
    | List of t list

  val to_string : t -> string
  val to_lambda : t -> Lambda.t
end
