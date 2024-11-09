let toBits (n: int) : int list =
    let rec toBits' n acc =
        if n = 0 then acc else toBits' (n / 2) ((n % 2) :: acc)

    toBits' n []

let minEnd (n: int) (x: int) : int64 =
    let rec minEnd' xBits nBits acc =
        match xBits, nBits with
        | [], [] ->
            acc
            |> List.rev
            |> List.skipWhile ((=) 0)
            |> List.fold (fun acc n -> acc * 2L + int64 n) 0L
        | _, [] -> failwith "never reach here"
        | [], h :: t -> minEnd' [] t (h :: acc)
        | h1 :: t1, h2 :: t2 ->
            if h1 = 1 then
                minEnd' t1 nBits (1 :: acc)
            else
                minEnd' t1 t2 (h2 :: acc)

    let xBits = toBits x
    let nBits = toBits (n - 1)
    minEnd' xBits nBits []

// 6
minEnd 3 4

// 15
minEnd 2 7
