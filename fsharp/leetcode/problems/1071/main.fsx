open System

let commonDivisors (n1: int) (n2: int) : int list =
    let rec commonDivisors' i limit n1 n2 acc =
        if i > limit then
            List.rev acc
        else if n1 % i = 0 && n2 % i = 0 then
            commonDivisors' (i + 1) limit n1 n2 (i :: acc)
        else
            commonDivisors' (i + 1) limit n1 n2 acc

    commonDivisors' 1 (Math.Min(n1, n2)) n1 n2 []

let gcdOfStrings (str1: string) (str2: string) : string =
    let rec gcdOfStrings' divisors (cs1: char list) (cs2: char list) =
        match divisors with
        | [] -> ""
        | h :: t ->
            let c1 = List.take h cs1
            let c2 = List.take h cs2

            if c1 <> c2 then
                gcdOfStrings' t cs1 cs2
            else
                let ok1 = cs1 |> List.chunkBySize h |> List.forall (fun n -> c1 = n)
                let ok2 = cs2 |> List.chunkBySize h |> List.forall (fun n -> c2 = n)

                if ok1 && ok2 then
                    c1 |> String.Concat
                else
                    gcdOfStrings' t cs1 cs2

    let divisors = commonDivisors str1.Length str2.Length
    let cs1, cs2 = Seq.toList str1, Seq.toList str2

    gcdOfStrings' (divisors |> List.rev) cs1 cs2

// "ABC"
gcdOfStrings "ABCABC" "ABC"

// "AB"
gcdOfStrings "ABABAB" "ABAB"

// ""
gcdOfStrings "LEET" "CODE"

// ""
gcdOfStrings "AAAAAAAAA" "AAACCC"

// "TAUXX"
gcdOfStrings "TAUXXTAUXXTAUXXTAUXXTAUXX" "TAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXX"
