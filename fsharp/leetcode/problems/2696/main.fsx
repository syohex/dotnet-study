let minLength (s: string) : int =
    let rec minLength' cs ok acc =
        match cs with
        | [] ->
            if ok then
                List.length acc
            else
                minLength' (List.rev acc) true []
        | c :: [] -> minLength' [] ok (c :: acc)
        | c1 :: c2 :: t ->
            if (c1 = 'A' && c2 = 'B') || (c1 = 'C' && c2 = 'D') then
                minLength' t false acc
            else
                minLength' (c2 :: t) ok (c1 :: acc)

    minLength' (s |> Seq.toList) true []

// 2
minLength "ABFCACDB"

// 5
minLength "ACBBD"
