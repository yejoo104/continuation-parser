module SS = Set.Make(String) ;;

module Lambda = struct
  type t =
    | LWord of string
    | LId of string
    | LLam of string * t
    | LForall of string * t
    | LExists of string * t
    | LApp of t * t

  let rec lambda_to_string (lambda : t) : string =
    match lambda with
    | LWord word -> word
    | LId id -> id
    | LLam (var, exp) -> "λ" ^ var ^ "." ^ (lambda_to_string exp)
    | LForall (var, exp) -> "∀" ^ var ^ "." ^ (lambda_to_string exp)
    | LExists (var, exp) -> "∃" ^ var ^ "." ^ (lambda_to_string exp)
    | LApp (exp1, exp2) ->
      (match exp1, exp2 with
       | LLam _, LLam _ ->
         "(" ^ (lambda_to_string exp1) ^ ") (" ^ (lambda_to_string exp2) ^ ")"
       | LLam _, _ ->
         "(" ^ (lambda_to_string exp1) ^ ") " ^ (lambda_to_string exp2)
       | _, LLam _ ->
         (lambda_to_string exp1) ^ " (" ^ (lambda_to_string exp2) ^ ")"
       | _, _ -> (lambda_to_string exp1) ^ " " ^ (lambda_to_string exp2))

  let rec fv (lambda : t) : SS.t =
    match lambda with
    | LWord _ -> SS.empty
    | LId x -> SS.singleton x
    | LLam (x, exp) | LForall (x, exp) | LExists (x, exp) ->
      SS.remove x (fv exp)
    | LApp (exp1, exp2) -> SS.union (fv exp1) (fv exp2)

  let rec fresh_var (v : string) (fvs : SS.t) =
    let new_var = v ^ "'" in
    if (SS.mem new_var fvs) then fresh_var new_var fvs
    else new_var

  (* substitute all instances of v in e1 with e2 *)
  let rec substitute (e1 : t) (v : string) (e2 : t) =
    match e1 with
    | LWord _ -> e1
    | LId x -> if x = v then e2 else e1
    (* TODO: unpretty / redundant code *)
    | LLam (x, exp) ->
      if x = v then e1
      else let fvs = fv e2 in
        if not (SS.mem x fvs) then
          LLam (x, substitute exp v e2)
        else let y = fresh_var x (SS.union fvs (fv exp)) in
          LLam (y, substitute (substitute exp x (LId y)) v e2)
    | LForall (x, exp) ->
      if x = v then e1
      else let fvs = fv e2 in
        if not (SS.mem x fvs) then
          LForall (x, substitute exp v e2)
        else let y = fresh_var x (SS.union fvs (fv exp)) in
          LForall (y, substitute (substitute exp x (LId y)) v e2)
    | LExists (x, exp) ->
      if x = v then e1
      else let fvs = fv e2 in
        if not (SS.mem x fvs) then
          LExists (x, substitute exp v e2)
        else let y = fresh_var x (SS.union fvs (fv exp)) in
          LExists (y, substitute (substitute exp x (LId y)) v e2)
    | LApp (e3, e4) -> LApp (substitute e3 v e2, substitute e4 v e2)

  let rec reduce (lambda : t) : t option =
    match lambda with
    | LWord _ | LId _ -> None
    (* TODO: unpretty / redundant code *)
    | LLam (x, exp) ->
      (match reduce exp with
       | None -> None
       | Some new_exp -> Some (LLam (x, new_exp)))
    | LForall (x, exp) ->
      (match reduce exp with
       | None -> None
       | Some new_exp -> Some (LForall (x, new_exp)))
    | LExists (x, exp) ->
      (match reduce exp with
       | None -> None
       | Some new_exp -> Some (LExists (x, new_exp)))
    | LApp (LLam (x, e1), e2) -> Some (substitute e1 x e2)
    | LApp (exp1, exp2) ->
      (match reduce exp1 with
       | None -> (match reduce exp2 with
                  | None -> None
                  | Some new_exp2 -> Some (LApp (exp1, new_exp2)))
       | Some new_exp1 -> Some (LApp (new_exp1, exp2)))

  let normal_form_print (lambda : t) : t =
    let rec helper t =
      print_endline(" = " ^ lambda_to_string t);
      match reduce t with
        None -> t
      | Some t' -> helper t'
    in
    match reduce lambda with
      None -> (print_endline("Term already in normal form"); lambda)
    | Some t' -> (print_endline("   " ^ lambda_to_string lambda); helper t')

  let rec normal_form (lambda : t) : t =
    match reduce lambda with
    | None -> lambda
    | Some new_lambda -> normal_form new_lambda

  let apply (lambda1 : t) (lambda2 : t) : t =
    let rec bound_vars (lambda : t) : SS.t =
      match lambda with
      | LLam (var, exp) -> SS.add var (bound_vars exp)
      | _ -> SS.empty in
    let rec replace_vars (lambda : t) (to_replace : SS.t) : t =
      match lambda with
      | LLam (var, exp) ->
        if (SS.mem var to_replace) then
          let new_var = fresh_var var to_replace in
          LLam (new_var, replace_vars (substitute exp var (LId new_var)) to_replace)
        else LLam (var, replace_vars exp to_replace)
      | _ -> lambda in
    let l1_bound_vars = bound_vars lambda1 in
    let new_l2 = replace_vars lambda2 l1_bound_vars in
    normal_form (LApp (lambda1, new_l2))
end


