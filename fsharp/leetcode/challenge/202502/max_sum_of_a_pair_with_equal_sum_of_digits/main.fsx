let maximumSum (nums: int list) : int =
    let rec toDigit n acc =
        if n = 0 then acc else toDigit (n / 10) (acc + n % 10)

    nums
    |> List.fold
        (fun acc n ->
            let digit = toDigit n 0
            let v = Map.tryFind digit acc |> Option.defaultValue []
            Map.add digit (n :: v) acc)
        Map.empty
    |> Map.values
    |> Seq.fold
        (fun acc v ->
            match List.sort v |> List.rev with
            | []
            | _ :: [] -> acc
            | a :: b :: _ -> max acc (a + b))
        -1

// 54
maximumSum [ 18; 43; 36; 13; 7 ]

// -1
maximumSum [ 10; 12; 19; 14 ]
