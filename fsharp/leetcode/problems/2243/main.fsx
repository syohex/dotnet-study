let chunks (s: string) (k: int) : char list list =
    let rec chunks' cs len k acc =
        if len = 0 then
            acc |> List.rev
        elif len <= k then
            (cs :: acc) |> List.rev
        else
            let chunk = List.take k cs
            let rest = List.skip k cs
            chunks' rest (len - k) k (chunk :: acc)

    chunks' (s |> Seq.toList) s.Length k []

let replace (cs: char list) : string =
    cs
    |> List.map (fun c -> int c - int '0')
    |> List.fold (fun acc n -> acc + n) 0
    |> System.String.Concat

let rec digitSum (s: string) (k: int) : string =
    if s.Length <= k then
        s
    else
        let s' =
            chunks s k
            |> List.map replace
            |> System.String.Concat

        digitSum s' k

// "135"
digitSum "11111222223" 3

// "000"
digitSum "00000000" 3

// "4169"
digitSum "71818186138735364590516322993378229838446988388364431324753408563431136824898916288399" 85
