let bestClosingTime (customers: string) : int =
    let rec bestClosingTime' i cs count min ret =
        match cs with
        | [] -> ret
        | h :: t ->
            let count' = if h = 'Y' then count - 1 else count + 1

            if count' < min then
                bestClosingTime' (i + 1) t count' count' i
            else
                bestClosingTime' (i + 1) t count' min ret

    let cs = Seq.toList customers
    let count = cs |> List.filter (fun c -> c = 'Y') |> List.length
    bestClosingTime' 1 cs count count 0

// 2
bestClosingTime "YYNY"

// 0
bestClosingTime "NNNNN"

// 4
bestClosingTime "YYYY"
