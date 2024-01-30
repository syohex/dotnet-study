let evalRPN (tokens: string list) : int =
    let rec evalRPN' tokens stack =
        match tokens with
        | [] -> List.head stack
        | h :: t ->
            match h with
            | "+" ->
                let num1, num2 = List.head (List.tail stack), List.head stack
                evalRPN' t ((num1 + num2) :: stack)
            | "-" ->
                let num1, num2 = List.head (List.tail stack), List.head stack
                evalRPN' t ((num1 - num2) :: stack)
            | "*" ->
                let num1, num2 = List.head (List.tail stack), List.head stack
                evalRPN' t ((num1 * num2) :: stack)
            | "/" ->
                let num1, num2 = List.head (List.tail stack), List.head stack
                evalRPN' t ((num1 / num2) :: stack)
            | _ -> evalRPN' t ((int h) :: stack)

    evalRPN' tokens []

// 9
evalRPN [ "2"; "1"; "+"; "3"; "*" ]

// 6
evalRPN [ "4"; "13"; "5"; "/"; "+" ]

// 22
evalRPN [ "10"; "6"; "9"; "3"; "+"; "-11"; "*"; "/"; "*"; "17"; "+"; "5"; "+" ]
