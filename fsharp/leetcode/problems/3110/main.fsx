let scoreOfString (s: string) : int =
    let rec scoreOfString' cs prev acc =
        match cs with
        | [] -> acc
        | h :: t -> scoreOfString' t h (acc + abs (prev - h))

    let cs = s |> Seq.map int |> Seq.toList

    match cs with
    | [] -> failwith "never reach here"
    | h :: t -> scoreOfString' t h 0

// 13
scoreOfString "hello"

// 50
scoreOfString "zaz"
