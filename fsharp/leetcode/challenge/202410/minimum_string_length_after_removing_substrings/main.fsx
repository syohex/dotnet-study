let minLength (s: string) : int =
    let rec minLength' cs acc =
        match cs with
        | [] -> List.length acc
        | h :: t ->
            if h = 'B' || h = 'D' then
                match List.tryHead acc with
                | Some(prev) when (h = 'B' && prev = 'A') || (h = 'D' && prev = 'C') -> minLength' t (List.tail acc)
                | _ -> minLength' t (h :: acc)
            else
                minLength' t (h :: acc)

    minLength' (Seq.toList s) []

// 2
minLength "ABFCACDB"

// 5
minLength "ACBBD"
