let romanToInt (s: string) : int =
    let getValue c prev =
        match c with
        | 'M' -> if prev = 'C' then 800 else 1000
        | 'D' -> if prev = 'C' then 300 else 500
        | 'C' -> if prev = 'X' then 80 else 100
        | 'L' -> if prev = 'X' then 30 else 50
        | 'X' -> if prev = 'I' then 8 else 10
        | 'V' -> if prev = 'I' then 3 else 5
        | 'I' -> 1
        | _ -> failwith "never reach here"

    let rec romanToInt' cs prev acc =
        match cs with
        | [] -> acc
        | c :: rest -> romanToInt' rest c (acc + (getValue c prev))

    romanToInt' (s |> Seq.toList) ' ' 0

// 3
romanToInt "III"

// 58
romanToInt "LVIII"

// 1994
romanToInt "MCMXCIV"
