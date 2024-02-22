let findJudge (n: int) (trust: (int * int) list) : int =
    let trustees, trusters =
        trust
        |> List.fold
            (fun (trustees: int[], trusters) (truster, trustee) ->
                trustees.[trustee] <- trustees.[trustee] + 1
                trustees, Set.add truster trusters)
            (Array.zeroCreate (n + 1), Set.empty)

    seq { 1..n }
    |> Seq.tryFind (fun i -> trustees.[i] = n - 1 && not <| Set.contains i trusters)
    |> Option.defaultValue -1

// 2
findJudge 2 [ (1, 2) ]

// 3
findJudge 3 [ (1, 3); (2, 3) ]

// -1
findJudge 3 [ (1, 3); (2, 3); (3, 1) ]

// 1
findJudge 1 []

// -1
findJudge 3 [ (1, 2); (2, 3) ]
