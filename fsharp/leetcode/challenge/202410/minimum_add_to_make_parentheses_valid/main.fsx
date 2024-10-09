let minAddToMakeValid (s: string) : int =
    let rec minAddToMakeValid' cs opens unbalanced =
        match cs with
        | [] -> opens + unbalanced
        | '(' :: t -> minAddToMakeValid' t (opens + 1) unbalanced
        | _ :: t ->
            if opens > 0 then
                minAddToMakeValid' t (opens - 1) unbalanced
            else
                minAddToMakeValid' t opens (unbalanced + 1)

    minAddToMakeValid' (Seq.toList s) 0 0

// 1
minAddToMakeValid "())"

// 3
minAddToMakeValid "((("

// 0
minAddToMakeValid "((()))"

// 0
minAddToMakeValid "()()()"
