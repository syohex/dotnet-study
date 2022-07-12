let makeSquare (matchSticks: int list) : bool =
    let rec makeSquare' pos (edges: int []) len (matchSticks: int []) =
        if pos = matchSticks.Length then
            edges.[0] = edges.[1]
            && edges.[1] = edges.[2]
            && edges.[2] = edges.[3]
        else
            let ret =
                seq { 0 .. 3 }
                |> Seq.fold
                    (fun ret i ->
                        if ret then
                            true
                        else if edges.[i] + matchSticks.[pos] <= len then
                            edges.[i] <- edges.[i] + matchSticks.[pos]

                            let ret' =
                                makeSquare' (pos + 1) edges len matchSticks

                            edges.[i] <- edges.[i] - matchSticks.[pos]
                            ret'
                        else
                            false)
                    false

            ret

    let sum = matchSticks |> List.sum

    if sum % 4 <> 0 then
        false
    else
        let edges = Array.zeroCreate 4

        let matchSticks' =
            matchSticks
            |> List.sort
            |> List.rev
            |> List.toArray

        makeSquare' 0 edges (sum / 4) matchSticks'

// true
makeSquare [ 1; 1; 2; 2; 2 ]

// false
makeSquare [ 3; 3; 3; 3; 4 ]

// false
makeSquare [ 19
             6
             13
             3
             3
             2
             18
             17
             20
             20
             14
             10
             3
             18
             9 ]

// false
makeSquare [ 4
             13
             1
             1
             14
             15
             1
             3
             13
             1
             3
             5
             2
             8
             12 ]
