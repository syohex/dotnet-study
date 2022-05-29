let rearrangeCharacters (s: string) (target: string) : int =
    let toFreq (s: string) =
        s
        |> Seq.fold
            (fun (acc: int []) c ->
                let key = int c - int 'a'
                acc.[key] <- acc.[key] + 1
                acc)
            (Array.zeroCreate 26)

    let rec rearrangeCharacters' (sFreq: int []) (tFreq: int []) ret =
        seq { 0..25 }
        |> Seq.iter (fun i -> sFreq.[i] <- sFreq.[i] - tFreq.[i])

        if sFreq |> Array.forall (fun n -> n >= 0) then
            rearrangeCharacters' sFreq tFreq (ret + 1)
        else
            ret

    let sFreq = toFreq s
    let tFreq = toFreq target
    rearrangeCharacters' sFreq tFreq 0

// 2
rearrangeCharacters "ilovecodingonleetcode" "code"

// 1
rearrangeCharacters "abcba" "abc"

// 1
rearrangeCharacters "abbaccaddaeea" "aaaaa"
