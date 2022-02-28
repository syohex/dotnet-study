let toStr (startNum: int) (endNum: int) : string =
    if startNum = endNum then
        sprintf $"{startNum}"
    else
        sprintf $"{startNum}->{endNum}"

let summaryRanges (nums: int list) : string list =
    let rec summaryRanges' nums start prev acc =
        match nums with
        | [] -> ((toStr start prev) :: acc) |> List.rev
        | head :: tail ->
            if prev + 1 = head then
                summaryRanges' tail start head acc
            else
                summaryRanges' tail head head ((toStr start prev) :: acc)

    match nums with
    | [] -> []
    | head :: tail -> summaryRanges' tail head head []

// ["0->2", "4->5", "7"]
summaryRanges [ 0; 1; 2; 4; 5; 7 ]

// ["0", "2->4", "6", "8-9"]
summaryRanges [ 0; 2; 3; 4; 6; 8; 9 ]

// []
summaryRanges []

// ["1"]
summaryRanges [ 1 ]
