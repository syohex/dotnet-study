let countCharacters (words: string list) (chars: string) : int =
    let toCount (s: string) : int[] =
        s
        |> Seq.fold
            (fun (acc: int[]) c ->
                let idx = int c - int 'a'
                acc.[idx] <- acc.[idx] + 1
                acc)
            (Array.zeroCreate 26)

    let canCreate word (table: int[]) =
        let t = toCount word
        seq { 0..25 } |> Seq.forall (fun i -> t.[i] <= table.[i])

    let rec countCharacters' words table acc =
        match words with
        | [] -> acc
        | h :: t ->
            if canCreate h table then
                countCharacters' t table (acc + h.Length)
            else
                countCharacters' t table acc

    let table = toCount chars
    countCharacters' words table 0

// 6
countCharacters [ "cat"; "bt"; "hat"; "tree" ] "atach"

// 10
countCharacters [ "hello"; "world"; "leetcode" ] "welldonehoneyr"
