let maxScore (s: string) : int =
    let rec maxScore' cs zeros ones acc =
        match cs with
        | [] -> failwith "never reach here"
        | _ :: [] -> acc
        | h :: t ->
            if h = '0' then
                maxScore' t (zeros + 1) ones (max acc (zeros + 1 + ones))
            else
                maxScore' t zeros (ones - 1) (max acc (zeros + ones - 1))

    let cs = s |> Seq.toList
    let ones = cs |> List.filter ((=) '1') |> List.length
    maxScore' cs 0 ones 0

// 5
maxScore "011101"

// 5
maxScore "00111"

// 3
maxScore "1111"
