let removeDuplicateLetters (s: string) : string =
    let rec popBiggerChars c acc (freq: int[]) used =
        match acc with
        | [] -> acc, used
        | h :: t ->
            if c < h && freq.[int h - int 'a'] > 0 then
                popBiggerChars c t freq (Set.remove h used)
            else
                acc, used


    let rec removeDuplicateLetters' cs (freq: int[]) used (acc: char list) =
        match cs with
        | [] -> acc |> List.rev |> System.String.Concat
        | h :: t ->
            let index = int h - int 'a'
            freq.[index] <- freq.[index] - 1
            if Set.contains h used then
                removeDuplicateLetters' t freq used acc
            else
                let acc', used' = popBiggerChars h acc freq used
                removeDuplicateLetters' t freq (Set.add h used') (h :: acc')


    let freq =
        s
        |> Seq.fold
            (fun (acc: int[]) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    removeDuplicateLetters' (Seq.toList s) freq Set.empty []

// abc
removeDuplicateLetters "cbabc"

// acdb
removeDuplicateLetters "cbacdcbc"

// bac
removeDuplicateLetters "bbcaac"

// eacb
removeDuplicateLetters "ecbacba"

// abacb
removeDuplicateLetters "abacb"
