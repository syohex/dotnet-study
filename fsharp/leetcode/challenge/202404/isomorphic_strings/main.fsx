let isIsomorphic (s: string) (t: string) : bool =
    let rec isIsomorphic' cs m mapped =
        match cs with
        | [] -> true
        | (c1, c2) :: t ->
            match Map.tryFind c1 m with
            | Some(v) -> if v = c2 then isIsomorphic' t m mapped else false
            | None ->
                if Set.contains c2 mapped then
                    false
                else
                    isIsomorphic' t (Map.add c1 c2 m) (Set.add c2 mapped)

    let cs = s |> Seq.zip t |> Seq.toList
    isIsomorphic' cs Map.empty Set.empty

// true
isIsomorphic "egg" "add"

// false
isIsomorphic "foo" "bar"

// true
isIsomorphic "paper" "title"

// false
isIsomorphic "badc" "baba"

// false
isIsomorphic "bbbaaaba" "aaabbbba"
