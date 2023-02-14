type semantic_type =
  | Construct (* This is for convenience because we can't represent polymorphism yet *)
  | Element
  | Truth
  | Forward of semantic_type * semantic_type (* / operator *)
  | Backward of semantic_type * semantic_type (* \ operator *)
  | Continuation of semantic_type * semantic_type (* ~ operator *)
  | Function of semantic_type * semantic_type (* -> operator *)
