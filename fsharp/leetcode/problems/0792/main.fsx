let numMatchingSubseq (s: string) (words: string list) : int =
    let rec findPosition positions pos prev =
        match positions with
        | [] -> None
        | h :: t ->
            if prev <= pos && pos <= h then
                Some(h)
            else
                findPosition t pos h

    let rec isSubstring (cs: char list) pos (charPositions: int list []) : bool =
        match cs with
        | [] -> true
        | h :: t ->
            let index = int h - int 'a'
            let positions = charPositions.[index]

            match findPosition positions pos -1 with
            | Some (v) -> isSubstring t (v + 1) charPositions
            | None -> false

    let rec numMatchingSubseq' words charPositions ret =
        match words with
        | [] -> ret
        | h :: t ->
            if isSubstring (h |> Seq.toList) -1 charPositions then
                numMatchingSubseq' t charPositions (ret + 1)
            else
                numMatchingSubseq' t charPositions ret

    let charPositions =
        s
        |> Seq.mapi (fun i c -> i, int c - int 'a')
        |> Seq.fold
            (fun (acc: int list []) (i, index) ->
                acc.[index] <- acc.[index] @ [ i ]
                acc)
            (Array.init 26 (fun _ -> []))

    numMatchingSubseq' words charPositions 0

// 3
numMatchingSubseq "abcde" [ "a"; "bb"; "acd"; "ace" ]

numMatchingSubseq
    "dsahjpjauf"
    [ "ahjpjau"
      "ja"
      "ahbwzgqnuk"
      "tnmlanowax" ]
