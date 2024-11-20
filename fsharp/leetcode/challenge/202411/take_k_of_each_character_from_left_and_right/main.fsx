let takeCharacters (s: string) (k: int) : int =
    let rec countChars i (cs: char[]) (acc: int[]) =
        if i >= cs.Length then
            acc
        else
            let index = int cs.[i] - int 'a'
            acc.[index] <- acc.[index] + 1
            countChars (i + 1) cs acc

    let isValid (count: int[]) (total: int[]) : bool =
        Array.zip total count |> Array.forall (fun (t, c) -> t - c >= k)

    let rec adjustWindow (cs: char[]) left right (count: int[]) (total: int[]) =
        if left > right || isValid count total then
            left
        else
            let index = int cs.[left] - int 'a'
            count.[index] <- count.[index] - 1
            adjustWindow cs (left + 1) right count total

    let rec takeCharacters' (cs: char[]) left right (count: int[]) (total: int[]) acc =
        if right >= cs.Length then
            cs.Length - acc
        else
            let index = int cs.[right] - int 'a'
            count.[index] <- count.[index] + 1

            let left = adjustWindow cs left right count total
            let acc = max acc (right - left + 1)
            takeCharacters' cs left (right + 1) count total acc

    let cs = Seq.toArray s
    let total = countChars 0 cs (Array.zeroCreate 3)

    if Array.exists (fun n -> n < k) total then
        -1
    else
        takeCharacters' cs 0 0 (Array.zeroCreate 3) total 0

// 8
takeCharacters "aabaaaacaabc" 2

// -1
takeCharacters "a" 1

// 0
takeCharacters "abc" 0

// 3
takeCharacters "abc" 1
