open System

let trap (height: int list) : int =
    let removeHeadAndLast = List.tail >> List.rev >> List.tail >> List.rev

    let leftHeight =
        height
        |> List.fold
            (fun (acc, prev) n ->
                let max = Math.Max(n, prev)
                max :: acc, max)
            ([], List.head height)
        |> fst
        |> List.rev

    let revHeight = List.rev height

    let rightHeight =
        revHeight
        |> List.fold
            (fun (acc, prev) n ->
                let max = Math.Max(n, prev)
                max :: acc, max)
            ([], List.head revHeight)
        |> fst

    let height' = removeHeadAndLast height
    let left = removeHeadAndLast leftHeight
    let right = removeHeadAndLast rightHeight

    List.zip3 height' left right
    |> List.fold (fun acc (h, l, r) -> acc + Math.Min(l, r) - h) 0

// 6
trap [ 0; 1; 0; 2; 1; 0; 1; 3; 2; 1; 2; 1 ]

// 9
trap [ 4; 2; 0; 3; 2; 5 ]
