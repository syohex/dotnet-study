open System

let maxScore (cardPoints: int list) (k: int) : int =
    let rec maxScore' cards revCards sum (ret: int) =
        match cards, revCards with
        | [], [] -> ret
        | [], _
        | _, [] -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 ->
            let sum' = sum + h1 - h2
            maxScore' t1 t2 sum' (Math.Max(ret, sum'))

    let cards = cardPoints |> List.take k
    let revCards = cardPoints |> List.rev |> List.take k |> List.rev
    let sum = revCards |> List.sum
    maxScore' cards revCards sum sum

// 12
maxScore [ 1; 2; 3; 4; 5; 6; 1 ] 3

// 4
maxScore [ 2; 2; 2 ] 2

// 55
maxScore [ 9; 7; 7; 9; 7; 7; 9 ] 7

// 248
maxScore [ 100; 40; 17; 9; 73; 75 ] 3
