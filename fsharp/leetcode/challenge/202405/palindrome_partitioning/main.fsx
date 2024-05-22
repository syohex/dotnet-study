open System

let partition (s: string) : string list list =
    let isPalindrome s =
        let q = Seq.toList s
        q = List.rev q

    let rec partion' pos acc ret =
        if pos >= s.Length then
            (List.rev acc) :: ret
        else
            seq { pos .. (s.Length - 1) }
            |> Seq.fold
                (fun ret i ->
                    let substr = s[pos..i]

                    if isPalindrome substr then
                        partion' (i + 1) (substr :: acc) ret
                    else
                        ret)
                ret

    partion' 0 [] [] |> List.rev

// [["a","a","b"],["aa","b"]]
partition "aab"

// [["a"]]
partition "a"
