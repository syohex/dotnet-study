let maximumLength (nums: int list) : int =
    let odds = nums |> List.filter (fun n -> n % 2 = 1) |> List.length
    let evens = nums |> List.filter (fun n -> n % 2 = 0) |> List.length

    let isEven = (List.head nums) % 2 = 0

    let oddEven =
        List.tail nums
        |> List.fold
            (fun (acc, isEven) n ->
                let m = n % 2 = 0
                if m <> isEven then acc + 1, not isEven else acc, isEven)
            (1, isEven)
        |> fst

    max odds (max evens oddEven)

// 4
maximumLength [ 1; 2; 3; 4 ]

// 6
maximumLength [ 1; 2; 1; 1; 2; 1; 2 ]

// 2
maximumLength [ 1; 3 ]
