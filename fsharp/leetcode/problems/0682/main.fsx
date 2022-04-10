let calPoints (ops: string list) : int =
    let rec calPoints' ops stack =
        match ops with
        | [] -> stack |> List.sum
        | h :: t ->
            match h with
            | "+" ->
                let n1 = List.head stack
                let n2 = List.tail stack |> List.head
                calPoints' t ((n1 + n2) :: stack)
            | "D" ->
                let n = List.head stack
                calPoints' t ((n * 2) :: stack)
            | "C" -> calPoints' t (List.tail stack)
            | s -> calPoints' t ((int s) :: stack)

    calPoints' ops []

// 30
calPoints [ "5"; "2"; "C"; "D"; "+" ]

// 27
calPoints [ "5"
            "-2"
            "4"
            "C"
            "D"
            "9"
            "+"
            "+" ]
