(*
                       Lexicon for a Simple Language
*)
open Lterm ;;
open Type ;;

(* module of special constructs in continuation semantics *)
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
  val to_lambda : t -> Lterm.t
end

(* module of tokens that we are using in our language *)
module Token : sig
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
    | On
    | Whansung
    | Yejoo
    | Yejoos
    | Mother
    | Mothers
    | Friend
    | Birthday
    | Left
    | Saw
    | Thought
    | Loves
    | Called

  val of_string : string -> t
  val to_string : t -> string
  val to_type : t -> Type.t
  val to_lambda : t -> Lterm.t
end

(* module of how phrases combine in our language *)
module Phrase : sig
  type t =
    | Single of Token.t
    | List of t list

  val to_string : t -> string
  val to_lambda : t -> Lterm.t
end
