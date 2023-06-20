let getAverage (nums: int[]) (k: int) : int[] =
    let len = Array.length nums
    let windowSize = 2 * k + 1

    if windowSize > len then
        Array.init len (fun _ -> -1)
    else
        let sum = Array.take windowSize nums |> Array.sum |> int64
        let windowSize' = int64 windowSize
        let ret = Array.init len (fun _ -> -1)
        ret.[windowSize - k - 1] <- (sum / windowSize') |> int

        seq { windowSize .. (len - 1) }
        |> Seq.fold
            (fun sum i ->
                let sum' = sum + (int64 nums.[i]) - (int64 nums.[i - windowSize])
                ret.[i - k] <- (sum' / windowSize') |> int
                sum')
            sum
        |> ignore

        ret

// [-1,-1,-1,5,4,4,-1,-1,-1]
getAverage [| 7; 4; 3; 9; 1; 8; 5; 2; 6 |] 3

// [100000]
getAverage [| 100000 |] 0

// [-1]
getAverage [| 100000 |] 1

// [-1]
getAverage [| 8 |] 10000
