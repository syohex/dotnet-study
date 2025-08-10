let reorderedPowerOf2 (n: int) : bool =
    let toSortedBits n = n |> string |> Seq.sort |> Seq.toList

    let nums = seq { 0..31 } |> Seq.map (fun n -> toSortedBits (2 <<< n)) |> Set.ofSeq
    Set.contains (toSortedBits n) nums

// true
reorderedPowerOf2 1

// false
reorderedPowerOf2 10

// true
reorderedPowerOf2 652
