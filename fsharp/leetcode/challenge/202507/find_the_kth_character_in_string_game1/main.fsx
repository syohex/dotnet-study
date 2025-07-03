let kthCharacter (k: int) : char =
    let rec kthCharacter' cs =
        if List.length cs >= k then
            List.item (k - 1) cs
        else
            let cs' =
                cs
                |> List.fold (fun acc c -> if c = 'z' then 'a' :: acc else (char (int c + 1)) :: acc) (List.rev cs)
                |> List.rev

            kthCharacter' cs'

    kthCharacter' [ 'a' ]

// 'b'
kthCharacter 5

// 'c'
kthCharacter 10
