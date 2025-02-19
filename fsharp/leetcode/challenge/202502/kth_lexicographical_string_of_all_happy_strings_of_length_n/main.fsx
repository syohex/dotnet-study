let getHappyString (n: int) (k: int) : string =
    let rec f pos prev (acc: char list) cands =
        if pos = n then
            (acc |> List.rev |> System.String.Concat) :: cands
        else
            "abc"
            |> Seq.fold (fun cands c -> if c = prev then cands else f (pos + 1) c (c :: acc) cands) cands

    f 0 '?' [] [] |> List.rev |> List.tryItem (k - 1) |> Option.defaultValue ""

// "c"
getHappyString 1 3

// ""
getHappyString 1 4

// "cab"
getHappyString 3 9

getHappyString 10 100
