type semantic_type =
  | Construct (* This is for convenience because we can't represent polymorphism yet *)
  | Element
  | Truth
  | Forward of semantic_type * semantic_type
  | Backward of semantic_type * semantic_type
