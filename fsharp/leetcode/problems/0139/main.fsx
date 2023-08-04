let toSubstrings (s: string) (start: int) : (int * string) list =
    seq { (start + 1) .. s.Length }
    |> Seq.map (fun i -> i, s.Substring(start, i - start))
    |> Seq.toList

let wordBreak (s: string) (wordDict: string list) : bool =
    let rec wordBreak' q (s: string) visited words =
        match q with
        | [] -> false
        | _ ->
            if List.exists (fun pos -> pos = s.Length) q then
                true
            else
                let q', visited' =
                    q
                    |> List.fold
                        (fun (acc, visited) pos ->
                            toSubstrings s pos
                            |> List.fold
                                (fun (acc, visited) (i, str) ->
                                    if Set.contains str words then
                                        i :: acc, Set.add i visited
                                    else
                                        acc, visited)
                                (acc, visited))
                        ([], visited)

                wordBreak' (List.rev q') s visited' words

    let words = Set.ofList wordDict
    wordBreak' [ 0 ] s Set.empty words

// true
wordBreak "leetcode" [ "leet"; "code" ]
// true
wordBreak "applepenapple" [ "apple"; "pen" ]
// false
wordBreak "catsandog" [ "cats"; "dog"; "sand"; "and"; "cat" ]
