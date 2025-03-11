let numberOfSubstrings (s: string) : int =
    let rec adjustLeft left right (abc: int[]) acc =
        if Array.forall (fun v -> v >= 1) abc then
            let acc = acc + s.Length - right
            let index = int s.[left] - int 'a'
            abc.[index] <- abc.[index] - 1
            adjustLeft (left + 1) right abc acc
        else
            left, abc, acc

    let rec numberOfSubstrings' left right (abc: int[]) acc =
        if right >= s.Length then
            acc
        else
            let index = int s.[right] - int 'a'
            abc.[index] <- abc.[index] + 1
            let left, abc, acc = adjustLeft left right abc acc
            numberOfSubstrings' left (right + 1) abc acc

    numberOfSubstrings' 0 0 (Array.zeroCreate 3) 0

// 10
numberOfSubstrings "abcabc"

// 3
numberOfSubstrings "aaacb"

// 1
numberOfSubstrings "abc"
        
