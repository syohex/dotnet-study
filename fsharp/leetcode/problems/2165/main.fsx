let toDigits (num: int64) : int64 list =
    let rec toDigits' (num: int64) acc =
        if num = 0 then
            acc
        else
            toDigits' (num / 10L) ((num % 10L) :: acc)

    if num = 0 then
        [ 0 ]
    else
        toDigits' num []

let smallestNumber (num: int64) : int64 =
    if num < 0 then
        toDigits -num
        |> List.sortDescending
        |> List.fold (fun acc n -> acc * 10L + n) 0L
        |> ((*) -1L)
    else
        let digitsWithIndex =
            toDigits num
            |> List.sort
            |> List.mapi (fun i n -> i, n)

        match digitsWithIndex
              |> List.tryFind (fun (_, n) -> n <> 0)
            with
        | None -> 0
        | Some (index, init) ->
            digitsWithIndex
            |> List.fold (fun acc (i, n) -> if i = index then acc else acc * 10L + n) init

// 103
smallestNumber 310

// -7650
smallestNumber -7605

// 0
smallestNumber 0

// -1
smallestNumber -1
