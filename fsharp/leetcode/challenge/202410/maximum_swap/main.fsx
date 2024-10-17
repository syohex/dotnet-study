let maximumSwap (num: int) : int =
    let rec maximumSwap' i (s: (int * char)[]) =
        if i >= s.Length then
            num
        else
            let digit = snd s.[i]

            let _, pos =
                Array.skip i s
                |> Array.fold
                    (fun (maxChar, pos) (j, c) ->
                        if maxChar < c || (pos <> s.Length && maxChar <= c) then
                            c, j
                        else
                            maxChar, pos)
                    (digit, s.Length)

            if pos <> s.Length then
                let cs = s |> Array.map snd
                let tmp = cs.[i]
                cs.[i] <- cs.[pos]
                cs.[pos] <- tmp
                cs |> Array.fold (fun acc c -> 10 * acc + int c - int '0') 0
            else
                maximumSwap' (i + 1) s


    let s = string num |> fun s -> s.ToCharArray() |> Array.indexed
    maximumSwap' 0 s

// 7236
maximumSwap 2736

// 9973
maximumSwap 9973

// 9913
maximumSwap 1993

// 52341342
maximumSwap 22341345

// 98863
maximumSwap 98368
