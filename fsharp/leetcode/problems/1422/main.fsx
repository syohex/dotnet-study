open System

let maxScore (s: string) : int =
    let rec maxScore' cs left right acc =
        match cs with
        | [] -> failwith "never reach here"
        | _ :: [] -> acc
        | h :: t ->
            let left', right' = if h = '1' then left, right - 1 else left + 1, right
            maxScore' t left' right' (Math.Max(acc, left' + right'))

    let cs = Seq.toList s
    let left = if List.head cs = '0' then 1 else 0
    let right = List.tail cs |> List.filter ((=) '1') |> List.length
    maxScore' (List.tail cs) left right (left + right)

// 5
maxScore "011101"

// 5
maxScore "00111"

// 3
maxScore "1111"
