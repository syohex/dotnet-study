open System

let generateNumbers (n: int) : int list =
    let rec generateNumbers' n (r: Random) acc =
        if n = 0 then
            acc
        else
            generateNumbers' (n - 1) r (r.Next(0, 100) :: acc)

    generateNumbers' n (new Random()) []

let merge a b =
    let rec merge' a b acc =
        match a, b with
        | [], [] -> acc |> List.rev
        | h :: t, [] -> merge' t b (h :: acc)
        | [], h :: t -> merge' a t (h :: acc)
        | h1 :: t1, h2 :: t2 ->
            if h1 < h2 then
                merge' t1 b (h1 :: acc)
            else
                merge' a t2 (h2 :: acc)

    merge' a b []

let rec mergeSort (nums: int list) =
    let len = nums.Length

    if len <= 1 then
        nums
    else
        let half = len / 2
        let a = nums |> List.take half |> mergeSort
        let b = nums |> List.skip half |> mergeSort

        merge a b

let nums = generateNumbers 100
let got = mergeSort nums
let expected = List.sort nums

// true
got |> List.zip expected |> List.forall (fun (a, b) -> a = b)
