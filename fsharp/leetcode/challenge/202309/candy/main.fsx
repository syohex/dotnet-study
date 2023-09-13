open System

let candy (ratings: int list) : int =
    let v = List.toArray ratings
    let len = v.Length

    if len = 1 then
        1
    else
        let lefts =
            seq { 1 .. (len - 1) }
            |> Seq.fold
                (fun acc i ->
                    if v.[i - 1] < v.[i] then
                        (List.head acc + 1) :: acc
                    else
                        1 :: acc)
                [ 1 ]
            |> List.rev

        let rights =
            seq { 0 .. (len - 2) }
            |> Seq.rev
            |> Seq.fold
                (fun acc i ->
                    if v.[i] > v.[i + 1] then
                        (List.head acc + 1) :: acc
                    else
                        1 :: acc)
                [ 1 ]

        List.zip lefts rights
        |> List.map (fun (left, right) -> Math.Max(left, right))
        |> List.sum

// 5
candy [ 1; 0; 2 ]

// 4
candy [ 1; 2; 2 ]
