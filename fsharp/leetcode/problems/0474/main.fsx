open System

let findMaxForm (strs: string list) (m: int) (n: int) : int =
    let countBits (s: string) : (int * int) =
        s
        |> Seq.fold
            (fun (zeros, ones) c ->
                if c = '0' then
                    zeros + 1, ones
                else
                    zeros, ones + 1)
            (0, 0)

    let rec findMaxForm' i bits m n cache =
        if i = 0 then
            0, cache
        else
            match Map.tryFind (i, m, n) cache with
            | Some (v) -> v, cache
            | None ->
                match bits with
                | [] -> failwith "never reach here"
                | (zeros, ones) :: rest ->
                    let ret1, cache1 = findMaxForm' (i - 1) rest m n cache

                    if m - zeros >= 0 && n - ones >= 0 then
                        let ret2, cache2 =
                            findMaxForm' (i - 1) rest (m - zeros) (n - ones) cache1

                        let ret = Math.Max(ret1, ret2 + 1)
                        ret, Map.add (i, m, n) ret cache2
                    else
                        ret1, Map.add (i, m, n) ret1 cache

    let bits = strs |> List.map countBits
    findMaxForm' strs.Length bits m n Map.empty |> fst

// 4
findMaxForm [ "10"; "0001"; "111001"; "1"; "0" ] 5 3

// 2
findMaxForm [ "10"; "0"; "1" ] 1 1
