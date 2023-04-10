let isValid (s: string) : bool =
    let rec isValid' cs acc =
        match cs with
        | [] ->
            match acc with
            | [] -> true
            | _ -> false
        | h :: t ->
            match h with
            | '('
            | '['
            | '{' -> isValid' t (h :: acc)
            | ')' ->
                match List.tryHead acc with
                | Some(v) when v = '(' -> isValid' t (List.tail acc)
                | _ -> false
            | ']' ->
                match List.tryHead acc with
                | Some(v) when v = '[' -> isValid' t (List.tail acc)
                | _ -> false
            | '}' ->
                match List.tryHead acc with
                | Some(v) when v = '{' -> isValid' t (List.tail acc)
                | _ -> false
            | _ -> failwith "never reach here"

    isValid' (Seq.toList s) []

// true
isValid "()"

// true
isValid "(){}[]"

// false
isValid "(]"

// false
isValid "]"
