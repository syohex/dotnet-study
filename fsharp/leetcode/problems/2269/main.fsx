let divisorSubstrings (num: int) (k: int) : int =
    let rec toList num acc =
        if num = 0 then
            acc
        else
            toList (num / 10) ((num % 10) :: acc)

    let rec divisorSubstrings' len v num k ret =
        if len < k then
            ret
        else
            let sum = v |> List.take k |> List.fold (fun acc n -> acc * 10 + n) 0

            if sum <> 0 && num % sum = 0 then
                divisorSubstrings' (len - 1) (List.tail v) num k (ret + 1)
            else
                divisorSubstrings' (len - 1) (List.tail v) num k ret

    let v = toList num []
    divisorSubstrings' v.Length v num k 0

// 2
divisorSubstrings 240 2

// 2
divisorSubstrings 430043 2
