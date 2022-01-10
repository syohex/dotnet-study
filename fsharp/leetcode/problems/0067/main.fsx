let addBinary (a: string) (b: string) : string =
    let rec addBinary' (a: int list) (b: int list) (carry: int) (acc: int list) : string =
        match (a, b) with
        | ([], []) ->
            if carry = 1 then
                (carry :: acc) |> System.String.Concat
            else
                acc |> System.String.Concat
        | (n1 :: t1, []) ->
            let (v, c) =
                if n1 + carry = 2 then
                    (0, 1)
                else
                    (n1 + carry, 0)

            addBinary' t1 b c (v :: acc)
        | ([], n2 :: t2) ->
            let (v, c) =
                if n2 + carry = 2 then
                    (0, 1)
                else
                    (n2 + carry, 0)

            addBinary' a t2 c (v :: acc)
        | (n1 :: t1, n2 :: t2) ->
            let sum = n1 + n2 + carry

            let (v, c) =
                if sum >= 2 then
                    (sum % 2, 1)
                else
                    (sum, 0)

            addBinary' t1 t2 c (v :: acc)

    let al =
        a
        |> Seq.toList
        |> List.map (fun c -> int c - int '0')
        |> List.rev

    let bl =
        b
        |> Seq.toList
        |> List.map (fun c -> int c - int '0')
        |> List.rev

    addBinary' al bl 0 []

addBinary "11" "1"
addBinary "1010" "1011"
addBinary "1111" "1"
