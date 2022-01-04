let bitwiseComplement (n: int) : int =
    let rec toBinary (n: int) (acc: int list) : int list =
        if n = 0 then
            acc
        else
            toBinary (n / 2) ((n % 2) :: acc)

    let bs = if n = 0 then [ 0 ] else toBinary n []

    (0, bs)
    ||> List.fold (fun acc n -> if n = 0 then acc * 2 + 1 else acc * 2)

bitwiseComplement 5
bitwiseComplement 7
bitwiseComplement 10
