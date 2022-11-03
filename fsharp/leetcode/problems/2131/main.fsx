let longestPalindrome (words: string list) : int =
    let freq =
        words
        |> List.fold
            (fun acc w ->
                match Map.tryFind w acc with
                | Some (v) -> Map.add w (v + 1) acc
                | None -> Map.add w 1 acc)
            Map.empty

    let ret, hasDup, _ =
        freq
        |> Map.fold
            (fun (ret, dup, visited) word count ->
                if Set.contains word visited then
                    ret, dup, visited
                else
                    let visited' = Set.add word visited
                    let rev = word |> Seq.rev |> System.String.Concat

                    if word = rev then
                        if count % 2 = 0 then
                            ret + (2 * count), dup, visited'
                        else
                            ret + (2 * (count - 1)), true, visited'
                    else
                        match Map.tryFind rev freq with
                        | Some (v) -> ret + (4 * System.Math.Min(count, v)), dup, Set.add rev visited'
                        | None -> ret, dup, visited')
            (0, false, Set.empty)

    if hasDup then ret + 2 else ret

// 6
longestPalindrome [ "lc"; "cl"; "gg" ]

// 8
longestPalindrome [ "ab"
                    "ty"
                    "yt"
                    "lc"
                    "cl"
                    "ab" ]

// 2
longestPalindrome [ "cc"; "ll"; "xx" ]

// 14
longestPalindrome [ "em"
                    "pe"
                    "mp"
                    "ee"
                    "pp"
                    "me"
                    "ep"
                    "em"
                    "em"
                    "me" ]

// 26
longestPalindrome [ "ll"
                    "lb"
                    "bb"
                    "bx"
                    "xx"
                    "lx"
                    "xx"
                    "lx"
                    "ll"
                    "xb"
                    "bx"
                    "lb"
                    "bb"
                    "lb"
                    "bl"
                    "bb"
                    "bx"
                    "xl"
                    "lb"
                    "xx" ]
