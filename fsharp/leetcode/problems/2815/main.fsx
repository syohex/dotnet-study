open System

let maxSum (nums: int list) : int =
    let rec maxDigit num (acc: int) =
        if num = 0 then
            acc
        else
            maxDigit (num / 10) (Math.Max(acc, num % 10))

    nums
    |> List.fold
        (fun acc n ->
            let digit = maxDigit n 0

            match Map.tryFind digit acc with
            | Some(v) -> Map.add digit (n :: v) acc
            | None -> Map.add digit [ n ] acc)
        Map.empty
    |> Map.fold
        (fun acc _ v ->
            match List.sort v |> List.rev with
            | []
            | _ :: [] -> acc
            | h1 :: h2 :: _ -> Math.Max(acc, h1 + h2))
        -1

// 88
maxSum [ 51; 71; 17; 24; 42 ]

// -1
maxSum [ 1; 2; 3; 4 ]
