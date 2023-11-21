let rev (n: int) : int =
    let rec rev' n acc =
        if n <= 0 then acc else rev' (n / 10) (acc * 10 + (n % 10))

    rev' n 0

let countNicePairs (nums: int list) : int =
    let rec countNicePairs' diffs dp acc =
        match diffs with
        | [] -> acc
        | h :: t ->
            match Map.tryFind h dp with
            | None -> countNicePairs' t (Map.add h 1 dp) acc
            | Some(v) -> countNicePairs' t (Map.add h (v + 1) dp) (acc + v)

    let diffs = nums |> List.map (fun n -> n - rev n)
    countNicePairs' diffs Map.empty 0

// 2
countNicePairs [ 42; 11; 1; 97 ]

// 4
countNicePairs [ 13; 10; 35; 24; 76 ]
