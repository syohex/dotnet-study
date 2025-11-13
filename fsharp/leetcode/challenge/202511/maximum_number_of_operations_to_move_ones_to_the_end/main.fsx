let maxOperations (s: string) : int =
    let rec maxOperations' cs ones isZero acc =
        match cs with
        | [] -> acc
        | h :: t ->
            let acc = if isZero && h = '1' then acc + ones else acc
            let ones = if h = '1' then ones + 1 else ones
            maxOperations' t ones (h = '0') acc

    maxOperations' (Seq.toList s) 0 false 0

// 4
maxOperations "1001101"

// 0
maxOperations "00111"
