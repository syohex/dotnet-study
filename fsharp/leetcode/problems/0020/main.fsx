let isValid (s: string) : bool =
    let rec isValid' (cs: char list) (stack: char list): bool =
        match cs with
        | [] -> true
        | c :: tail -> match c with
                        | '(' | '[' | '{' -> isValid' tail (c :: stack)
                        | '}' -> if (List.head stack) = '{' then isValid' tail (List.tail stack)
                                 else false
                        | ']' -> if (List.head stack) = '[' then isValid' tail (List.tail stack)
                                 else false
                        | ')' -> if (List.head stack) = '(' then isValid' tail (List.tail stack)
                                 else false
                        | _ -> failwith "neve reach here"

    isValid' (s |> Seq.toList) []

isValid "()"
isValid "()[]{}"
isValid "(}"
