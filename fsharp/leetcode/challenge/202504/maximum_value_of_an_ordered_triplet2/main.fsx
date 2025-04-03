let maximumTripletValue (nums: int list) : int64 =
    let rec getPrefixes nums (acc: int64[]) =
        match nums with
        | [] -> acc
        | (i, h) :: t ->
            acc.[i] <- max h acc.[i - 1]
            getPrefixes t acc

    let rec getPostfixes nums (acc: int64[]) =
        match nums with
        | [] -> acc
        | (i, h) :: t ->
            acc.[i] <- max h acc.[i + 1]
            getPostfixes t acc

    let nums = List.map int64 nums |> List.indexed
    let len = List.length nums

    let prefixes: int64[] = Array.zeroCreate len
    prefixes.[0] <- List.head nums |> snd

    let prefixes = getPrefixes (List.tail nums) prefixes

    let revs = List.rev nums
    let postfixes: int64[] = Array.zeroCreate len
    postfixes.[len - 1] <- List.head revs |> snd

    let postfixes = getPostfixes (List.tail revs) postfixes

    nums
    |> List.tail
    |> List.take (len - 2)
    |> List.fold (fun acc (i, n) -> max acc ((prefixes.[i - 1] - n) * postfixes.[i + 1])) 0L

// 77
maximumTripletValue [ 12; 6; 1; 2; 7 ]

// 133
maximumTripletValue [ 1; 10; 3; 4; 19 ]

// 0
maximumTripletValue [ 1; 2; 3 ]
