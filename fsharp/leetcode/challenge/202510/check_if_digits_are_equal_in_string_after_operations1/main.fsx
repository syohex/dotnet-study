let hasSameDigits (s: string) : bool =
    let rec hasSameDigits' cs =
        match cs with
        | []
        | _ :: [] -> failwith "never reach here"
        | a :: b :: [] -> a = b
        | h :: t ->
            let v =
                t
                |> List.fold
                    (fun (acc, prev) c ->
                        let v = (prev + c) % 10
                        v :: acc, c)
                    ([], h)
                |> fst
                |> List.rev

            hasSameDigits' v

    let cs = s |> Seq.map (fun c -> int c - int '0') |> Seq.toList
    hasSameDigits' cs

// true
hasSameDigits "3902"

// false
hasSameDigits "34789"
