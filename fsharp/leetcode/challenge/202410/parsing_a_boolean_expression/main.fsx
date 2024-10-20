let parseBoolExpr (expression: string) : bool =
    let rec parseBoolExpr' cs =
        match cs with
        | [] -> true, []
        | 't' :: t -> true, t
        | 'f' :: t -> false, t
        | '!' :: '(' :: t ->
            let v, cs = parseBoolExpr' t
            not v, List.tail cs
        | '&' :: '(' :: t ->
            let exprs, t = parseExprs t []
            List.reduce (&&) exprs, t
        | '|' :: '(' :: t ->
            let exprs, t = parseExprs t []
            List.reduce (||) exprs, t
        | _ -> failwith "never reach here"

    and parseExprs cs acc =
        match cs with
        | [] -> failwith ") is not found"
        | ')' :: t -> acc, t
        | ',' :: t -> parseExprs t acc
        | _ ->
            let v, t = parseBoolExpr' cs
            parseExprs t (v :: acc)

    parseBoolExpr' (Seq.toList expression) |> fst

// false
parseBoolExpr "&(|(f))"

// true
parseBoolExpr "|(f,f,f,t)"

// true
parseBoolExpr "!(&(f,t))"

// true
parseBoolExpr "!(&(&(!(&(f)),&(t),|(f,f,t)),&(t),&(t,t,f)))"
