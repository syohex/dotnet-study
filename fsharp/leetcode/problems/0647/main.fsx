let countSubstrings (s: string) : int =
    let len = s.Length
    let cs = s |> Seq.toArray
    let dp = Array2D.init len len (fun _ _ -> false)
    let mutable ret = 0

    for i in 0 .. (len - 1) do
        dp.[i, i] <- true
        ret <- ret + 1

    for i in 0 .. (len - 2) do
        dp.[i, i + 1] <- cs.[i] = cs.[i + 1]
        if dp.[i, i + 1] then ret <- ret + 1

    for i in 3..len do
        for j in 0 .. (len - i) do
            let k = j + i - 1
            dp.[j, k] <- dp.[j + 1, k - 1] && cs.[j] = cs.[k]
            if dp.[j, k] then ret <- ret + 1

    ret

// 3
countSubstrings "abc"

// 6
countSubstrings "aaa"
