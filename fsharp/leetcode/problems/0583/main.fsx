let minDistance (word1: string) (word2: string) : int =
    let rec maxCommonLength' cs1 cs2 pos1 pos2 cache =
        match cs1, cs2 with
        | [], []
        | [], _
        | _, [] -> 0, cache
        | h1 :: t1, h2 :: t2 ->
            let key = pos1, pos2

            match Map.tryFind key cache with
            | Some (v) -> v, cache
            | None ->
                if h1 = h2 then
                    let ret, cache' =
                        maxCommonLength' t1 t2 (pos1 - 1) (pos2 - 1) cache

                    let ret' = ret + 1
                    ret', Map.add key ret' cache'
                else
                    let ret1, cache' =
                        maxCommonLength' t1 cs2 (pos1 - 1) pos2 cache

                    let ret2, cache'' =
                        maxCommonLength' cs1 t2 pos1 (pos2 - 1) cache'

                    let ret = System.Math.Max(ret1, ret2)
                    ret, Map.add key ret cache''

    let cs1 = word1 |> Seq.toList |> List.rev
    let cs2 = word2 |> Seq.toList |> List.rev

    let commonLen, _ =
        maxCommonLength' cs1 cs2 (word1.Length - 1) (word2.Length - 1) Map.empty

    word1.Length + word2.Length - (2 * commonLen)

// 2
minDistance "sea" "eat"

// 4
minDistance "leetcode" "etco"
