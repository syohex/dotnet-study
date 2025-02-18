let smallestNumber (pattern: string) : string =
    let rec smallestNumber' pattern i stack (acc: char list) =
        let stack = (char i + char '0') :: stack

        match pattern with
        | [] ->
            stack
            |> List.fold (fun acc n -> n :: acc) acc
            |> List.rev
            |> System.String.Concat
        | h :: t ->
            if h = 'I' then
                let acc = stack |> List.fold (fun acc n -> n :: acc) acc
                smallestNumber' t (i + 1) [] acc
            else
                smallestNumber' t (i + 1) stack acc

    smallestNumber' (Seq.toList pattern) 1 [] []

// "123549876"
smallestNumber "IIIDIDDD"

// "4321"
smallestNumber "DDD"
