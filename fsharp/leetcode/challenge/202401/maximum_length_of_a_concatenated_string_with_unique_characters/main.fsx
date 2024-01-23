let maxLength (arr: string list) : int =
    let rec maxLength' arr (candidates: string list) acc =
        match arr with
        | [] -> acc
        | h :: t ->
            let candidates' =
                candidates
                |> List.fold
                    (fun acc s ->
                        let newStr = h + s
                        let chars = Set.ofSeq newStr

                        if newStr.Length = Set.count chars then
                            newStr :: acc
                        else
                            acc)
                    candidates

            let acc' = candidates' |> List.map (fun s -> s.Length) |> List.max
            maxLength' t candidates' acc'

    maxLength' arr [ "" ] 0

// 4
maxLength [ "un"; "iq"; "ue" ]

// 6
maxLength [ "cha"; "r"; "act"; "ers" ]

// 26
maxLength [ "abcdefghijklmnopqrstuvwxyz" ]
