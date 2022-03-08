let cellsInRange (s: string) : string list =
    let cellsInRange' startRow endRow startCol endCol =
        seq {
            for i in startCol..endCol do
                for j in startRow..endRow do
                    yield sprintf "%c%c" i j
        }
        |> Seq.toList

    let ca = s |> Seq.toArray
    cellsInRange' ca.[1] ca.[4] ca.[0] ca.[3]

// ["K1","K2","L1","L2"]
cellsInRange "K1:L2"

// ["A1","B1","C1","D1","E1","F1"]
cellsInRange "A1:F1"
