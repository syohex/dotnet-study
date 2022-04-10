let toDigit (num: int) : int [] =
    let rec toDigit' num acc =
        if num = 0 then
            acc |> List.toArray
        else
            toDigit' (num / 10) ((num % 10) :: acc)

    if num = 0 then
        [| 0 |]
    else
        toDigit' num []

let largestInteger (num: int) : int =
    let rec largestInteger' (nums: int []) i =
        if i = nums.Length then
            nums |> Array.fold (fun acc n -> acc * 10 + n) 0
        else
            let isOdd = nums.[i] % 2 <> 0

            let (_, maxIndex) =
                seq { i + 1 .. nums.Length - 1 }
                |> Seq.fold
                    (fun (max, idx) j ->
                        let n = nums.[j]

                        if (isOdd && n % 2 = 0) || ((not isOdd) && n % 2 <> 0) then
                            max, idx
                        else if max < n then
                            n, j
                        else
                            max, idx)
                    (nums.[i], i)

            let tmp = nums.[i]
            nums.[i] <- nums.[maxIndex]
            nums.[maxIndex] <- tmp
            largestInteger' nums (i + 1)

    largestInteger' (num |> toDigit) 0

// 3412
largestInteger 1234

// 87655
largestInteger 65875

// 251
largestInteger 251
