open System

let minimumSize (nums: int list) (maxOperations: int) : int =
    let isPossible balls =
        nums
        |> List.fold (fun acc n -> acc + (Math.Ceiling(double n / double balls) |> int) - 1) 0
        |> fun n -> n <= maxOperations

    let rec minimumSize' left right =
        if left >= right then
            left
        else
            let mid = left + (right - left) / 2

            if isPossible mid then
                minimumSize' left mid
            else
                minimumSize' (mid + 1) right

    minimumSize' 1 (List.max nums)

// 3
minimumSize [ 9 ] 2

// 2
minimumSize [ 2; 4; 8; 2 ] 4
