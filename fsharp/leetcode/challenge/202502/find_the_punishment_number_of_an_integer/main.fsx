let punishmentNumber (n: int) : int =
    let rec isPunishment n rest =
        if rest < 0 || n < rest then
            false
        elif n = rest then
            true
        else
            isPunishment (n / 10) (rest - n % 10)
            || isPunishment (n / 100) (rest - n % 100)
            || isPunishment (n / 1000) (rest - n % 1000)

    seq { 1..n }
    |> Seq.fold (fun acc n -> if isPunishment (n * n) n then acc + n * n else acc) 0

// 182
punishmentNumber 10

// 1478
punishmentNumber 37
