let strToTable (s: string) : int array =
    let rec strToTable' cs (arr: int []) =
        match cs with
        | [] -> arr
        | x :: xs ->
            let index = int x - int 'a'
            arr.[index] <- arr.[index] + 1
            strToTable' xs arr

    strToTable' (s |> Seq.toList) (Array.zeroCreate 26)

let minSteps (s: string) (t: string) : int =
    let sTable = s |> strToTable
    let tTable = t |> strToTable

    seq {0..25}
    |> Seq.fold (fun acc i -> acc + System.Math.Abs(sTable.[i] - tTable.[i])) 0

// 7
minSteps "leetcode" "coats"

// 0
minSteps "night" "thing"