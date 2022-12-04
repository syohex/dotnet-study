let minimumAverageDifference (nums: int list) : int =
    let rec accumulate nums sum acc =
        match nums with
        | [] -> acc |> List.rev
        | h :: t -> accumulate t (sum + h) ((sum + h) :: acc)

    let rec minimumAverageDifference' (accs: int64 list) (n: int64) (len: int64) (sum:int64) (min: int64) (ret:int64) =
        match accs with
        | [] -> ret |> int
        | h :: [] ->
            let a = h / n
            if a < min then
                minimumAverageDifference' [] (n + 1L) len sum a (n - 1L)
            else
                minimumAverageDifference' [] (n + 1L) len sum min ret
        | h :: t ->
            let a = h / n
            let b = (sum - h) / (len - n)
            let diff = System.Math.Abs(a - b)
            if diff < min then
                minimumAverageDifference' t (n + 1L) len sum diff (n - 1L)
            else
                minimumAverageDifference' t (n + 1L) len sum min ret

    let sum = nums |> List.sum |> int64
    let len = nums |> List.length |> int64
    let accs = accumulate nums 0 [] |> List.map int64
    minimumAverageDifference' accs 1 len sum (System.Int64.MaxValue) -1

// 3
minimumAverageDifference [2;5;3;9;5;3]

// 0
minimumAverageDifference [0]
