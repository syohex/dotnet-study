let compareVersion (version1: string) (version2: string) : int =
    let toNumber s =
        let rec toNumber' cs acc =
            match cs with
            | [] -> acc
            | h :: t -> toNumber' t (acc * 10 + int h - int '0')

        toNumber' (Seq.toList s) 0

    let rec compareVersion' v1 v2 =
        match v1, v2 with
        | [], [] -> 0
        | h1 :: t1, [] ->
            let n1 = toNumber h1
            if n1 = 0 then compareVersion' t1 [] else compare n1 0
        | [], h2 :: t2 ->
            let n2 = toNumber h2
            if n2 = 0 then compareVersion' t2 [] else compare 0 n2
        | h1 :: t1, h2 :: t2 ->
            let n1, n2 = toNumber h1, toNumber h2
            if n1 = n2 then compareVersion' t1 t2 else compare n1 n2

    let v1 = version1.Split('.') |> Array.toList
    let v2 = version2.Split('.') |> Array.toList
    compareVersion' v1 v2

// -1
compareVersion "1.2" "1.10"

// 0
compareVersion "1.01" "1.001"

// 0
compareVersion "1.0" "1.0.0.0"

// 1
compareVersion "1.0.1" "1"
