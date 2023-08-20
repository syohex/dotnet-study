let countPairs (nums: int list) (target: int) : int =
    let rec countPairs' nums target acc =
        match nums with
        | []
        | _ :: [] -> acc
        | h :: t ->
            let n =
                t
                |> List.filter (fun m -> h + m < target)
                |> List.length

            countPairs' t target (acc + n)

    countPairs' nums target 0

// 3
countPairs [ -1; 1; 2; 3; 1 ] 2

// 10
countPairs [ -6; 2; 5; -2; -7; -1; 3 ] -2
