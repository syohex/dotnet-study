let rec toComplementBinary (n: int) : int list =
    let rec inner (ns: int list) (n: int) : int list =
        match n with
        | 0 -> ns
        | m when m % 2 = 0 -> inner (1 :: ns) (m / 2)
        | m when m % 2 = 1 -> inner (0 :: ns) (m / 2)
        | _ -> failwith "never reach here"

    inner [] n

let toNumber (ns: int list) : int =
    let rec inner (acc: int) (ns: int list) : int =
        match ns with
        | [] -> acc
        | head :: tail -> inner (acc * 2 + head) tail

    match ns with
    | [] -> 1
    | _ -> inner 0 ns

let findComplement (num: int) : int = num |> toComplementBinary |> toNumber
