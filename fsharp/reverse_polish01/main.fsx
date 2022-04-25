let calculateReversePolish (exp: string) : float =
    let rec f (cs: char list) (stack: double list) : float =
        match cs with
        | [] -> List.head stack
        | h :: t ->
            match h with
            | c when h >= '0' && h <= '9' ->
                let h' = (int h) - (int '0') |> double
                f t (h' :: stack)
            | c ->
                match stack with
                | b :: a :: rest ->
                    match c with
                    | '+' -> f t ((a + b) :: rest)
                    | '-' -> f t ((a - b) :: rest)
                    | '*' -> f t ((a * b) :: rest)
                    | '/' -> f t ((a / b) :: rest)
                    | op -> failwith $"unsupported operator '{op}'"
                | _ -> failwith $"input expression is wrong: {exp}"

    f (exp |> Seq.toList) []

// 10.0
calculateReversePolish "612+*8-"
