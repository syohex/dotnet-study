let scoreOfParentheses (s: string) : int =
    let rec scoreOfParentheses' cs stack =
        match cs with
        | [] ->
            match stack with
            | [] -> failwith "never happen"
            | v :: _ -> v
        | h :: t ->
            if h = '(' then
                scoreOfParentheses' t (0 :: stack)
            else
                match stack with
                | [] -> failwith "never happen"
                | v :: [] ->
                    let score = if v = 0 then 1 else v * 2
                    scoreOfParentheses' t [ score ]
                | v1 :: v2 :: rest ->
                    let score = if v1 = 0 then v2 + 1 else v2 + v1 * 2
                    scoreOfParentheses' t (score :: rest)

    scoreOfParentheses' (s |> Seq.toList) []

// 1
scoreOfParentheses "()"

// 2
scoreOfParentheses "(())"

// 4
scoreOfParentheses "((()))"

// 3
scoreOfParentheses "()()()"

// 6
scoreOfParentheses "(()(()))"
