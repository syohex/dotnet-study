open System

let removeDuplicates (s: string) (k: int) : string =
    let rec removeDuplicates' (cs: char list) k (stack: (char * int) list) =
        match cs with
        | [] ->
            stack
            |> List.fold (fun acc (c, n) -> (List.replicate n c) @ acc) []
            |> String.Concat
        | h :: t ->
            match stack with
            | [] -> removeDuplicates' t k ((h, 1) :: stack)
            | (prev, count) :: t2 ->
                if h = prev then
                    if count = k - 1 then
                        removeDuplicates' t k t2
                    else
                        removeDuplicates' t k ((prev, count + 1) :: t2)
                else
                    removeDuplicates' t k ((h, 1) :: stack)

    removeDuplicates' (s |> Seq.toList) k []

// "abcd"
removeDuplicates "abcd" 2

// "aa"
removeDuplicates "deeedbbcccbdaa" 3

// "ps"
removeDuplicates "pbbcggttciiippooaais" 2

// "ybth"
removeDuplicates "yfttttfbbbbnnnnffbgffffgbbbbgssssgthyyyy" 4
