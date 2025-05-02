let pushDominoes (dominoes: string) : string =
    let len = dominoes.Length

    let rec pushRight i (dominoes: char list) (forces: int[]) force =
        match dominoes with
        | [] -> forces
        | h :: t ->
            match h with
            | 'L' ->
                forces.[i] <- 0
                pushRight (i + 1) t forces 0
            | 'R' ->
                forces.[i] <- len
                pushRight (i + 1) t forces len
            | _ ->
                let v = max 0 (force - 1)
                forces.[i] <- v
                pushRight (i + 1) t forces v

    let rec pushLeft i (dominoes: char list) (forces: int[]) force =
        match dominoes with
        | [] -> forces
        | h :: t ->
            match h with
            | 'L' ->
                forces.[i] <- forces.[i] - len
                pushLeft (i - 1) t forces len
            | 'R' ->
                pushLeft (i - 1) t forces 0
            | _ ->
                let v = max 0 (force - 1)
                forces.[i] <- forces.[i] - v
                pushLeft (i - 1) t forces v

    let dominoes = Seq.toList dominoes
    let forces = Array.zeroCreate len
    let forces = pushRight 0 dominoes forces 0
    let forces = pushLeft (len - 1) (List.rev dominoes) forces 0

    forces
    |> Array.fold
        (fun acc c ->
            if c > 0 then 'R' :: acc
            elif c < 0 then 'L' :: acc
            else '.' :: acc)
        []
    |> List.rev
    |> System.String.Concat

// "RR.L"
pushDominoes "RR.L"

// "LL.RR.LLRRLL.."
pushDominoes ".L.R...LR..L.."
