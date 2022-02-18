let findRemoveIndex (nums: int list) : int =
    let rec findRemoveIndex' prev idx nums =
        match nums with
        | [] -> idx
        | x :: xs ->
            if prev > x then
                idx
            else
                findRemoveIndex' x (idx + 1) xs

    findRemoveIndex' (List.head nums) 0 (List.tail nums)

let removeNthElement (n: int) (nums: int list) : int list =
    (List.take n nums) @ (List.skip (n + 1) nums)

let removeKDigits (num: string) (k: int) : string =
    let rec removeKDigits' (nums: int list) (k: int) =
        if k = 0 then
            nums
        else
            let idx = findRemoveIndex nums
            removeKDigits' (removeNthElement idx nums) (k - 1)

    let nums =
        num
        |> Seq.toList
        |> List.map (fun c -> int c - int '0')

    let digits =
        removeKDigits' nums k |> List.skipWhile ((=) 0)

    if List.isEmpty digits then
        "0"
    else
        digits |> List.toArray |> System.String.Concat


// "1219"
removeKDigits "1432219" 3

// "200"
removeKDigits "10200" 1

// "0"
removeKDigits "10" 2

// "1"
removeKDigits "00001" 0
