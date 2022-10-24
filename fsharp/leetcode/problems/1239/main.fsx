let maxLength (arr: string list) : int =
    let rec maxLength' (arr: string list) (cands: string list) ret =
        match arr with
        | [] -> ret
        | h :: t ->
            let cands' =
                cands
                |> List.fold
                    (fun acc cand ->
                        let tmp = cand + h
                        let chars = tmp |> Set.ofSeq |> Seq.length

                        if chars = tmp.Length then
                            tmp :: acc
                        else
                            acc)
                    cands

            let ret' =
                cands'
                |> List.fold (fun acc s -> System.Math.Max(acc, s.Length)) 0

            maxLength' t cands' ret'

    maxLength' arr [ "" ] 0

// 4
maxLength [ "un"; "iq"; "ue" ]

// 6
maxLength [ "cha"; "r"; "act"; "ers" ]

// 26
maxLength [ "abcdefghijklmnopqrstuvwxyz" ]
