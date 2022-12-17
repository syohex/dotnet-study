let evalRPN (tokens: string list) : int =
    let rec evalRPN' tokens stack =
        match tokens with
        | [] -> stack |> List.head |> int
        | h :: t ->
            match h with
            | "+" ->
                let n =
                    (List.head stack)
                    + (stack |> List.tail |> List.head)

                evalRPN' t (n :: (stack |> List.skip 2))
            | "-" ->
                let n =
                    (stack |> List.tail |> List.head)
                    - (List.head stack)

                evalRPN' t (n :: (stack |> List.skip 2))
            | "*" ->
                let n =
                    (stack |> List.tail |> List.head)
                    * (List.head stack)

                evalRPN' t (n :: (stack |> List.skip 2))
            | "/" ->
                let n =
                    (stack |> List.tail |> List.head)
                    / (List.head stack)

                evalRPN' t (n :: (stack |> List.skip 2))
            | _ -> evalRPN' t ((h |> int64) :: stack)

    evalRPN' tokens []

// 9
evalRPN [ "2"; "1"; "+"; "3"; "*" ]

// 6
evalRPN [ "4"; "13"; "5"; "/"; "+" ]

// 22
evalRPN [ "10"
          "6"
          "9"
          "3"
          "+"
          "-11"
          "*"
          "/"
          "*"
          "17"
          "+"
          "5"
          "+" ]
