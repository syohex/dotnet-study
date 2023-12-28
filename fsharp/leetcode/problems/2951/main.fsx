let findPeaks (mountain: int list) : int list =
    let mountain = Array.ofList mountain

    seq { 1 .. (Array.length mountain - 2) }
    |> Seq.filter (fun i -> mountain.[i - 1] < mountain.[i] && mountain.[i] > mountain.[i + 1])
    |> Seq.toList

// []
findPeaks [ 2; 4; 4 ]

// [1;3]
findPeaks [ 1; 3; 2; 8; 5 ]
