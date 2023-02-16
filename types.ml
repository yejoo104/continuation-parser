type semantic_type =
  | Construct (* This is for convenience because we can't represent polymorphism yet *)
  | Bind (* This represents a bind *)
  | Empty of string (* undetermined type *)
  | Element
  | Truth
  | Type_Mismatch (* indicates that type was not resolved *)
  | Forward of semantic_type * semantic_type (* / operator *)
  | Backward of semantic_type * semantic_type (* \ operator *)
  | Continuation of semantic_type * semantic_type (* ~ operator *)
  | Function of semantic_type * semantic_type (* -> operator *)
  | Pronoun of semantic_type * semantic_type (* |> operator *)

let fail_fast_mismatch (t : semantic_type) : bool =
  match t with
  | Type_Mismatch -> true
  | _ -> false

let equivalent_type (t1 : semantic_type) (t2 : semantic_type) : (string option * semantic_type) option =
  match t1, t2 with
  | Empty id1, Empty id2 ->
    (* TODO: the else case is mishandled *)
    if id1 = id2 then Some (None, Empty id1) else Some (None, Empty (id1 ^ id2))
  | Empty id, t
  | t, Empty id -> Some (Some id, t)
  | t1, t2 -> if t1 = t2 then Some (None, t1) else None

let update_type ((new_id, new_type) : (string option * semantic_type)) (old_type : semantic_type) : semantic_type =
  match new_id, old_type with
  | Some id1, Empty id2 ->
    if id1 = id2 then new_type else Type_Mismatch
  | _ -> old_type


let rec type_to_str (semantic_type : semantic_type) : string =
  match semantic_type with
  | Construct -> "construct"
  | Bind -> "bind"
  | Empty str -> "empty" ^ str
  | Element -> "e"
  | Truth -> "t"
  | Type_Mismatch -> "mismatch"
  | Forward(t1, t2) -> "(" ^ (type_to_str t1) ^ "/" ^ (type_to_str t2) ^ ")"
  | Backward(t1, t2) -> "(" ^ (type_to_str t1) ^ "\\" ^ (type_to_str t2) ^ ")"
  | Continuation(t1, t2) -> "(" ^ (type_to_str t1) ^ "~" ^ (type_to_str t2) ^ ")"
  | Function(t1, t2) -> "(" ^ (type_to_str t1) ^ "->" ^ (type_to_str t2) ^ ")"
  | Pronoun(t1, t2) -> "(" ^ (type_to_str t1) ^ "|>" ^ (type_to_str t2) ^ ")"
