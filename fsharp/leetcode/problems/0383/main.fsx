let canConstruct (ransomNote: string) (magazine: string) : bool =
    let f (s: string) : int [] =
        s
        |> Seq.fold
            (fun (acc: int []) c ->
                let index = int c - int 'a'
                acc.[index] <- acc.[index] + 1
                acc)
            (Array.zeroCreate 26)

    Array.zip (f ransomNote) (f magazine)
    |> Array.forall (fun (a, b) -> a <= b)

// false
canConstruct "a" "b"

// false
canConstruct "aa" "ab"

// true
canConstruct "aa" "aab"
