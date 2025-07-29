let smallestSubArray (nums: int list) : int list =
    let bits = Array.init 32 (fun _ -> System.Int32.MaxValue)

    nums
    |> List.indexed
    |> List.rev
    |> List.map (fun (i, n) ->
        let max_bit =
            seq { 0..31 }
            |> Seq.fold
                (fun acc j ->
                    let b = n &&& (1 <<< j)

                    if b <> 0 then
                        bits.[j] <- i
                        acc
                    elif bits.[j] <> System.Int32.MaxValue then
                        max acc bits.[j]
                    else
                        acc)
                i

        max_bit - i + 1)
    |> List.rev

// [3,3,2,2,1]
smallestSubArray [ 1; 0; 2; 1; 3 ]

// [2,1]
smallestSubArray [ 1; 2 ]
