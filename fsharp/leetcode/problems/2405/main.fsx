let partitonString (s: string) : int =
    let rec partitonString' cs window acc =
        match cs with
        | [] -> acc
        | h :: t ->
            if Set.contains h window then
                partitonString' t (Set.add h Set.empty) (acc + 1)
            else
                partitonString' t (Set.add h window) acc

    partitonString' (Seq.toList s) Set.empty 1

// 4
partitonString "abacaba"

// 6
partitonString "ssssss"
