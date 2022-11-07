let maximum69Number (num: int) : int =
    let rec toDigits n acc =
        if n <= 0 then
            acc
        else
            toDigits (n / 10) ((n % 10) :: acc)

    toDigits num []
    |> List.fold
        (fun (sum, ok) n ->
            if ok || n = 9 then
                sum * 10 + n, ok
            else
                sum * 10 + 9, true)
        (0, false)
    |> fst

// 9969
maximum69Number 9669

// 9999
maximum69Number 9996

// 9999
maximum69Number 9999

// 9
maximum69Number 6
