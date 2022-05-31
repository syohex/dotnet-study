open System

let hasAllCodes (s: string) (k: int) : bool =
    if s.Length < k then
        false
    else
        let sets =
            seq { 0 .. (s.Length - k) }
            |> Seq.map (fun i -> s.Substring(i, k))
            |> Seq.fold (fun acc s -> Set.add s acc) Set.empty

        sets.Count = (Math.Pow(2, k) |> int)

// true
hasAllCodes "00110110" 2

// true
hasAllCodes "0110" 1

// false
hasAllCodes "0110" 2

// false
hasAllCodes "10" 99
