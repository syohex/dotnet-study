let isInterleave (s1: string) (s2: string) (s3: string) : bool =
    let rec isInterleave' (i1, cs1) (i2, cs2) (i3, cs3) cache =
        match cs3 with
        | [] -> List.isEmpty cs1 && List.isEmpty cs2, cache
        | h3 :: t3 ->
            if Set.contains (i1, i2, i3) cache then
                false, cache
            else
                match cs1, cs2 with
                | [], [] -> false, cache
                | h1 :: t1, [] ->
                    if h1 = h3 then
                        isInterleave' (i1 + 1, t1) (i2, cs2) (i3 + 1, t3) cache
                    else
                        false, Set.add (i1, i2, i3) cache
                | [], h2 :: t2 ->
                    if h2 = h3 then
                        isInterleave' (i1, cs1) (i2 + 1, t2) (i3 + 1, t3) cache
                    else
                        false, Set.add (i1, i2, i3) cache
                | h1 :: t1, h2 :: t2 ->
                    let ret1, cache1 =
                        if h1 = h3 then
                            isInterleave' (i1 + 1, t1) (i2, cs2) (i3 + 1, t3) cache
                        else
                            false, cache

                    if ret1 then
                        true, cache1
                    else if h2 = h3 then
                        isInterleave' (i1, cs1) (i2 + 1, t2) (i3 + 1, t3) cache1
                    else
                        false, Set.add (i1, i2, i3) cache1


    let cs1, cs2, cs3 = Seq.toList s1, Seq.toList s2, Seq.toList s3
    isInterleave' (0, cs1) (0, cs2) (0, cs3) Set.empty |> fst

// true
isInterleave "aabcc" "dbbca" "aadbbcbcac"

// false
isInterleave "aabcc" "dbbca" "aadbbbaccc"

// true
isInterleave "" "" ""

// false
isInterleave "a" "b" "a"
