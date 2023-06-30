open System

let countBeautifulPairs (nums: int list) : int =
    let rec gcd a b = if a % b = 0 then b else gcd b (a % b)

    let rec countBeautifulPairs' pairs acc =
        match pairs with
        | []
        | _ :: [] -> acc
        | (first, _) :: t ->
            let count = t |> List.filter (fun (_, last) -> gcd first last = 1) |> List.length
            countBeautifulPairs' t (acc + count)

    let charToInt = Char.GetNumericValue >> int

    let pairs =
        nums
        |> List.map string
        |> List.map (fun s -> Seq.head s |> charToInt, Seq.last s |> charToInt)

    countBeautifulPairs' pairs 0

// 5
countBeautifulPairs [ 2; 5; 1; 4 ]

// 2
countBeautifulPairs [ 11; 21; 12 ]

// 7
countBeautifulPairs [ 31; 25; 72; 79; 74 ]
