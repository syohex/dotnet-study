let findKOr (nums: int list) (k: int) : int =
    seq { 0..31 }
    |> Seq.fold
        (fun acc i ->
            let bit = 1 <<< i
            let count = nums |> List.filter (fun n -> n &&& bit <> 0) |> List.length
            if count >= k then acc + bit else acc)
        0

// 9
findKOr [ 7; 12; 9; 8; 9; 15 ] 4

// 0
findKOr [ 2; 12; 1; 11; 4; 5 ] 6

// 15
findKOr [ 10; 8; 5; 9; 11; 6; 8 ] 1
