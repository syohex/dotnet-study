let fib (n: int) : int =
    let rec fib' n cache =
        match n with
        | 0 -> 0, cache
        | 1 -> 1, cache
        | _ ->
            match Map.tryFind n cache with
            | Some (v) -> v, cache
            | None ->
                let ret', cache' = fib' (n - 1) cache
                let ret'', cache'' = fib' (n - 2) cache'
                let ret = ret' + ret''
                ret, Map.add n ret cache''

    fib' n Map.empty |> fst

// 0
fib 0

// 1
fib 0

// 1
fib 2

// 2
fib 3

// 3
fib 4

// 4181
fib 19

// 6765
fib 20