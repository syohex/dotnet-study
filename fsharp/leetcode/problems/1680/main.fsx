let concatenatedBinary (n: int) : int =
    let rec concat n acc =
        if n = 0 then
            acc
        else
            concat (n / 2) ((n % 2) :: acc)

    let rec concatNum i n acc =
        if i > n then
            acc
        else
            let bits = concat i []
            concatNum (i + 1) n (acc @ bits)

    let rec concatenatedBinary' bits (acc: int64) =
        match bits with
        | [] -> acc % 1_000_000_007L
        | h :: t ->
            let acc' = (acc * 2L + (int64 h)) % 1_000_000_007L
            concatenatedBinary' t acc'

    let bits = concatNum 1 n []
    concatenatedBinary' bits 0L |> int

// 1
concatenatedBinary 1

// 27
concatenatedBinary 3

// 505379714
concatenatedBinary 12
