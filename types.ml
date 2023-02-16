type semantic_type =
  | Construct (* This is for convenience because we can't represent polymorphism yet *)
  | Bind (* This represents a bind *)
  | Empty (* undetermined type *)
  | Element
  | Truth
  | Forward of semantic_type * semantic_type (* / operator *)
  | Backward of semantic_type * semantic_type (* \ operator *)
  | Continuation of semantic_type * semantic_type (* ~ operator *)
  | Function of semantic_type * semantic_type (* -> operator *)
  | Pronoun of semantic_type * semantic_type (* |> operator *)

let equivalent_type (t1 : semantic_type) (t2 : semantic_type) : semantic_type option =
  match t1, t2 with
  | Empty, t
  | t, Empty -> Some t
  | t1, t2 -> if t1 = t2 then Some t1 else None
