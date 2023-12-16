let isAnagram (s: string) (t: string) : bool =
    let toFreq (s: string) : int[] =
        s
        |> Seq.fold
            (fun (acc: int[]) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    toFreq s = toFreq t

// true
isAnagram "anagram" "nagaram"

// false
isAnagram "rat" "car"
