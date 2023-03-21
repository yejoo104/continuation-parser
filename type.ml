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

let is_mismatch (t : t) : bool =
  match t with
  | Type_Mismatch -> true
  | _ -> false

let equivalent_type (t1 : t) (t2 : t) : (string option * t) option =
  match t1, t2 with
  | Empty id1, Empty id2 ->
    if id1 = id2 then Some (None, Empty id1) else Some (None, Type_Mismatch)
  | Empty id, t
  | t, Empty id -> Some (Some id, t)
  | t1, t2 -> if t1 = t2 then Some (None, t1) else None

let update_type ((new_id, new_type) : (string option * t)) (old_type : t) : t =
  match new_id, old_type with
  | Some id1, Empty id2 ->
    if id1 = id2 then new_type else Type_Mismatch
  | _ -> old_type

let rec to_string (t : t) : string =
  match t with
  | Construct -> "construct"
  | Bind -> "bind"
  | Empty str -> "empty" ^ str
  | Element -> "e"
  | Truth -> "t"
  | Type_Mismatch -> "mismatch"
  | Forward(t1, t2) -> "(" ^ (to_string t1) ^ "/" ^ (to_string t2) ^ ")"
  | Backward(t1, t2) -> "(" ^ (to_string t1) ^ "\\" ^ (to_string t2) ^ ")"
  | Continuation(t1, t2) -> "(" ^ (to_string t1) ^ "~" ^ (to_string t2) ^ ")"
  | Function(t1, t2) -> "(" ^ (to_string t1) ^ "->" ^ (to_string t2) ^ ")"
  | Pronoun(t1, t2) -> "(" ^ (to_string t1) ^ "|>" ^ (to_string t2) ^ ")"
