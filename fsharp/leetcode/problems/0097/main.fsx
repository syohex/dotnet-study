let isInterleave (s1: string) (s2: string) (s3: string) : bool =
    let rec isInterleave' (p1, cs1) (p2, cs2) (p3, cs3) cache =
        match cs3 with
        | [] -> List.isEmpty cs1 && List.isEmpty cs2, cache
        | h3 :: t3 ->
            if Set.contains (p1, p2, p3) cache then
                true, cache
            else
                let ret', cache' =
                    match cs1 with
                    | [] -> false, cache
                    | h1 :: t1 ->
                        if h1 = h3 then
                            isInterleave' (p1 + 1, t1) (p2, cs2) (p3 + 1, t3) cache
                        else
                            false, cache

                if ret' then
                    true, Set.add (p1, p2, p3) cache'
                else
                    let ret'', cache'' =
                        match cs2 with
                        | [] -> false, cache'
                        | h2 :: t2 ->
                            if h2 = h3 then
                                isInterleave' (p1, cs1) (p2 + 1, t2) (p3 + 1, t3) cache
                            else
                                false, cache

                    if ret'' then
                        true, Set.add (p1, p2, p3) cache''
                    else
                        false, cache''

    isInterleave' (0, s1 |> Seq.toList) (0, s2 |> Seq.toList) (0, s3 |> Seq.toList) Set.empty
    |> fst


// true
isInterleave "aabcc" "dbbca" "aadbbcbcac"

// false
isInterleave "aabcc" "dbbca" "aadbbbaccc"

// true
isInterleave "" "" ""

// false
isInterleave "a" "b" "a"

// false
isInterleave "" "" "a"
