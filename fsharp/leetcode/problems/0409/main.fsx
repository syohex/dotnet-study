let longestPalindrome (s: string) : int =
    let rec toFreq chars m =
        match chars with
        | [] -> m
        | c :: cs ->
            match Map.tryFind c m with
            | Some (v) -> toFreq cs (Map.add c (v + 1) m)
            | None -> toFreq cs (Map.add c 1 m)

    let rec longestPalindrome' keys freq ret hasOdd =
        match keys with
        | [] -> ret + (if hasOdd then 1 else 0)
        | h :: t ->
            let count = Map.find h freq

            if count % 2 = 0 then
                longestPalindrome' t freq (ret + count) hasOdd
            else
                longestPalindrome' t freq (ret + (count - 1)) true

    let freq = toFreq (s |> Seq.toList) Map.empty
    longestPalindrome' (freq |> Map.keys |> Seq.toList) freq 0 false

// 7
longestPalindrome "abccccdd"

// 1
longestPalindrome "a"

// 2
longestPalindrome "bb"
