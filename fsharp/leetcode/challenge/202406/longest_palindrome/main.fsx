let longestPalindrome (s: string) : int =
    let rec longestPalindrome' counts useOdd acc =
        match counts with
        | [] -> acc
        | h :: t ->
            if h % 2 = 0 then longestPalindrome' t useOdd (acc + h)
            elif useOdd then longestPalindrome' t useOdd (acc + h - 1)
            else longestPalindrome' t true (acc + h)

    let counts =
        s
        |> Seq.fold
            (fun acc c ->
                match Map.tryFind c acc with
                | Some(v) -> Map.add c (v + 1) acc
                | None -> Map.add c 1 acc)
            Map.empty
        |> Map.values
        |> Seq.toList

    longestPalindrome' counts false 0

// 7
longestPalindrome "abccccdd"

// 1
longestPalindrome "a"
