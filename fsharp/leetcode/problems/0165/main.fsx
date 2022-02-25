let strToInt (s: string) : int =
    s |> Seq.fold (fun acc c -> acc * 10 + (int c - int '0')) 0

let splitVersion (ver: string) : int list =
    ver.Split('.')
    |> Array.map strToInt
    |> Array.toList

let compareVersion (version1: string) (version2: string) : int =
    let rec compareVersion' v1 v2 =
        match v1, v2 with
        | [], [] -> 0
        | h1 :: t1, [] ->
            if h1 = 0 then
                compareVersion' t1 []
            else
                1
        | [], h2 :: t2 ->
            if h2 = 0 then
                compareVersion' [] t2
            else
                -1
        | h1 :: t1, h2 :: t2 ->
            if h1 > h2 then 1
            elif h1 < h2 then -1
            else compareVersion' t1 t2

    compareVersion' (splitVersion version1) (splitVersion version2)

// 0
compareVersion "1.01" "1.001"

// 1
compareVersion "2.0.0.0.01" "1.001"

// 0
compareVersion "1.0" "1.0.0"

// -1
compareVersion "0.1" "1.1"
