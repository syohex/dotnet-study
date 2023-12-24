open System

let minOperations (s: string) : int =
    let rec minOperations' i cs zeroStart (oneStart: int) =
        match cs with
        | [] -> Math.Min(zeroStart, oneStart)
        | h :: t ->
            if i % 2 = 0 then
                if h = '0' then
                    minOperations' (i + 1) t zeroStart (oneStart + 1)
                else
                    minOperations' (i + 1) t (zeroStart + 1) oneStart
            else if h = '0' then
                minOperations' (i + 1) t (zeroStart + 1) oneStart
            else
                minOperations' (i + 1) t zeroStart (oneStart + 1)

    minOperations' 0 (Seq.toList s) 0 0

// 1
minOperations "0100"

// 0
minOperations "10"

// 2
minOperations "1111"
