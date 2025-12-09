let toFreq (nums: int list) : Map<int, int64> =
    let rec toFreq' nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            match Map.tryFind h acc with
            | Some v -> toFreq' t <| Map.add h (v + 1L) acc
            | None -> toFreq' t <| Map.add h 1L acc

    toFreq' nums Map.empty

let specialTriplets (nums: int list) : int =
    let modulo = 1_000_000_007L

    let rec specialTriplets' nums (visited: Map<int, int64>) (freq: Map<int, int64>) acc =
        match nums with
        | [] -> acc |> int
        | h :: t ->
            let key = h * 2

            let left = Map.tryFind key visited |> Option.defaultValue 0
            let count = Map.tryFind h visited |> Option.defaultValue 0
            let visited = Map.add h (count + 1L) visited
            let left' = Map.tryFind key visited |> Option.defaultValue 0
            let right = Map.tryFind key freq |> Option.defaultValue 0

            let acc = (acc + left * (right - left') % modulo) % modulo
            specialTriplets' t visited freq acc

    let freq = toFreq nums
    specialTriplets' nums Map.empty freq 0

// 1
specialTriplets [ 6; 3; 6 ]

// 1
specialTriplets [ 0; 1; 0; 0 ]

// 2
specialTriplets [ 8; 4; 2; 8; 4 ]
