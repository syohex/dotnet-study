let minimumSteps (s: string) : int64 =
    s
    |> Seq.fold (fun (acc, blacks) c -> if c = '0' then acc + blacks, blacks else acc, blacks + 1) (0, 0)
    |> fst
    |> int64

// 1
minimumSteps "101"

// 2
minimumSteps "100"

// 0
minimumSteps "0111"

// 3
minimumSteps "1010"
