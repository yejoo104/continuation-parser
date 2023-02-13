type semantic_type =
  | Element
  | Truth
  | Forward of semantic_type * semantic_type
  | Backward of semantic_type * semantic_type
