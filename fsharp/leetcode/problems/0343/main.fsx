open System

let integerBreak (n: int) : int =
    let rec integerBreak' m n acc cache =
        if m <= 1 then
            acc, cache
        else
            match Map.tryFind m cache with
            | Some(v) -> acc * v, cache
            | None ->
                let limit = if m = n then m - 1 else m

                let ret, cache' =
                    seq { 1..limit }
                    |> Seq.fold
                        (fun (ret, cache) i ->
                            let ret', cache' = integerBreak' (m - i) n (acc * i) cache
                            Math.Max(ret, ret'), cache')
                        (1, cache)

                ret, Map.add m ret cache'

    integerBreak' n n 1 Map.empty |> fst

// 1
integerBreak 2

// 4
integerBreak 4

// 36
integerBreak 10

integerBreak 58
