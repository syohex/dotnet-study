open System

let lengthOfLongestSubstring (s: string) : int =
    let rec lengthOfLongestSubstring' cs len (start: int) pos ret =
        match cs with
        | [] -> Math.Max(ret, len - start)
        | (i, c) :: rest ->
            match Map.tryFind c pos with
            | None -> lengthOfLongestSubstring' rest len start (Map.add c i pos) ret
            | Some (j) ->
                let start' = j + 1
                let ret' = Math.Max(ret, i - start)
                lengthOfLongestSubstring' rest len start' (Map.add c i pos) ret'

    let cs =
        s |> Seq.mapi (fun i c -> i, c) |> Seq.toList

    lengthOfLongestSubstring' cs s.Length 0 Map.empty 0

// 3
lengthOfLongestSubstring "abcabcbb"

// 1
lengthOfLongestSubstring "bbbbb"

// 3
lengthOfLongestSubstring "pwwkew"

// 1
lengthOfLongestSubstring " "

// 3
lengthOfLongestSubstring "dvdf"
