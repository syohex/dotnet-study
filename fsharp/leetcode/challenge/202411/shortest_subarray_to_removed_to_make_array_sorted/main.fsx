let findLengthOfShortestSubarray (arr: int[]) : int =
    let rec rightPos i =
        if i <= 0 then 0
        else if arr.[i - 1] <= arr.[i] then rightPos (i - 1)
        else i

    let rec leftPos i =
        if i >= arr.Length - 1 then arr.Length - 1
        else if arr.[i] <= arr.[i + 1] then leftPos (i + 1)
        else i

    let rec findLengthOfShortestSubarray' i j left acc =
        if i > left || j >= arr.Length then
            acc
        else if arr.[i] <= arr.[j] then
            findLengthOfShortestSubarray' (i + 1) j left (min acc (j - i - 1))
        else
            findLengthOfShortestSubarray' i (j + 1) left acc

    let left = leftPos 0

    if left = arr.Length - 1 then
        0
    else
        let right = rightPos (arr.Length - 1)
        findLengthOfShortestSubarray' 0 right left (min (arr.Length - (left + 1)) right)

// 3
findLengthOfShortestSubarray [| 1; 2; 3; 10; 4; 2; 3; 5 |]

// 4
findLengthOfShortestSubarray [| 5; 4; 3; 2; 1 |]

// 0
findLengthOfShortestSubarray [| 1; 2; 3 |]

// 3
findLengthOfShortestSubarray [| 2; 2; 2; 1; 1; 1 |]

// 7
findLengthOfShortestSubarray [| 10; 13; 17; 21; 15; 15; 9; 17; 22; 22; 13 |]
