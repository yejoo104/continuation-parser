open Core ;;

module Lambda = struct
  type t =
    | LId of string
    | LLam of string * t
    | LApp of t * t

  let rec lambda_to_string (lambda : t) : string =
    match lambda with
    | LId id -> id
    | LLam (var, exp) -> "/" ^ var ^ "." ^ (lambda_to_string exp)
    | LApp (exp1, exp2) ->
      (match exp1, exp2 with
       | LLam _, LLam _ ->
         "(" ^ (lambda_to_string exp1) ^ ") (" ^ (lambda_to_string exp2) ^ ")"
       | LLam _, _ ->
         "(" ^ (lambda_to_string exp1) ^ ")" ^ (lambda_to_string exp2)
       | _, LLam _ ->
         (lambda_to_string exp1) ^ " (" ^ (lambda_to_string exp2) ^ ")"
       | _, _ -> (lambda_to_string exp1) ^ " " ^ (lambda_to_string exp2))
end


