let countHomogenous (s: string) : int =
    let modulo = 1_000_000_007

    let rec countHomonogenous' cs prev count acc =
        match cs with
        | [] -> acc
        | h :: t ->
            if h = prev then
                let count' = count + 1
                countHomonogenous' t h count' ((acc + count') % modulo)
            else
                countHomonogenous' t h 1 ((acc + 1) % modulo)

    let cs = Seq.toList s
    countHomonogenous' (List.tail cs) (List.head cs) 1 1

// 13
countHomogenous "abbcccaa"

// 2
countHomogenous "xy"

// 15
countHomogenous "zzzzz"
