open System

let divisorsSum (n: int) : int =
    let limit = (Math.Sqrt n) |> int
    [ 2 .. limit ]
    |> List.fold
        (fun acc m ->
            if n % m <> 0 then acc
            else if n / m = m then acc + m
            else acc + m + (n / m)) 1

let amicableNumbers (n: int) : (int * int) list =
    let numToSum =
        [ 1 .. n ]
        |> List.fold (fun acc n -> Map.add n (divisorsSum n) acc) Map.empty

    [ 1 .. n ]
    |> List.map (fun m ->
        let sum = Map.find m numToSum

        match Map.tryFind sum numToSum with
        | None -> None
        | Some (v) ->
            if m = v && m < sum then
                Some(m, sum)
            else
                None)
    |> List.filter Option.isSome
    |> List.map (fun v -> defaultArg v (-1, -1))

amicableNumbers 1_000_000