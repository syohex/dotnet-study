let candy (ratings: int list) : int =
    let rec minCandies ratings prevRating prevCandy acc =
        match ratings with
        | [] -> acc
        | h :: t ->
            if h > prevRating then
                minCandies t h (prevCandy + 1) ((prevCandy + 1) :: acc)
            else
                minCandies t h 1 (1 :: acc)

    let lefts = minCandies (List.tail ratings) (List.head ratings) 1 [ 1 ] |> List.rev
    let rev = List.rev ratings
    let rights = minCandies (List.tail rev) (List.head rev) 1 [ 1 ]

    List.zip lefts rights
    |> List.fold (fun acc (left, right) -> acc + max left right) 0

// 5
candy [ 1; 0; 2 ]

// 4
candy [ 1; 2; 2 ]
