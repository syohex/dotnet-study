let candy (ratings: int list) : int =
    let rec candy' ratings prev acc =
        match ratings with
        | [] -> acc |> List.rev
        | h :: t ->
            if prev < h then
                candy' t h ((acc.Head + 1) :: acc)
            else
                candy' t h (1 :: acc)

    let lefts = candy' ratings.Tail ratings.Head [ 1 ]
    let rev = ratings |> List.rev
    let rights = candy' rev.Tail rev.Head [ 1 ]

    lefts
    |> List.zip (rights |> List.rev)
    |> List.fold (fun acc (l, r) -> acc + System.Math.Max(l, r)) 0

// 5
candy [ 1; 0; 2 ]

// 4
candy [ 1; 2; 2 ]

// 1
candy [ 1 ]

// 2
candy [ 0; 0 ]

// 3
candy [ 0; 1 ]
