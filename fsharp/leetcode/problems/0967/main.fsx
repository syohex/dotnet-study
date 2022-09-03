let numsSameConsecDiff (n: int) (k: int) : int list =
    let rec numsSameConsecDiff' num i n k acc =
        if i = n then
            num :: acc
        else
            let digit = num % 10

            let acc' =
                if digit - k >= 0 then
                    numsSameConsecDiff' ((num * 10) + (digit - k)) (i + 1) n k acc
                else
                    acc

            if k <> 0 && digit + k <= 9 then
                numsSameConsecDiff' ((num * 10) + (digit + k)) (i + 1) n k acc'
            else
                acc'

    seq { 1 .. 9 }
    |> Seq.fold (fun acc i -> numsSameConsecDiff' i 1 n k acc) []
    |> List.sort

// [181,292,707,818,929]
numsSameConsecDiff 3 7

// [10,12,21,23,32,34,43,45,54,56,65,67,76,78,87,89,98]
numsSameConsecDiff 2 1
