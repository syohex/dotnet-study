let isValid (s: string) : bool =
    let isPair openCh endCh =
        match openCh with
        | '(' -> endCh = ')'
        | '{' -> endCh = '}'
        | '[' -> endCh = ']'
        |_ -> failwith "never reach here"

    let rec isValid' cs stack =
        match cs with
        | [] -> List.isEmpty stack
        | head :: tail ->
            match head with
            | '[' | '(' | '{' -> isValid' tail (head :: stack)
            | _ ->
                match stack with
                | [] -> false
                | top :: rest ->
                    if isPair top head then
                        isValid' tail rest
                    else
                        false

    isValid' (s |> Seq.toList) []

// true
isValid "()"

// true
isValid "()[]{}"

// false
isValid "(]"

// false
isValid "]"

// false
isValid "((("
