let findSubSequences (nums: int list) : int list list =
    let isNotEmpty = List.isEmpty >> not

    let rec findSubSequences' nums acc ret =
        match nums with
        | [] ->
            if isNotEmpty acc && List.tail acc |> isNotEmpty then
                Set.add (acc |> List.rev) ret
            else
                ret
        | h :: t ->
            let ret' = findSubSequences' t acc ret

            match acc with
            | [] -> findSubSequences' t (h :: acc) ret'
            | prev :: _ ->
                if prev <= h then
                    findSubSequences' t (h :: acc) ret'
                else
                    ret'

    findSubSequences' nums [] Set.empty |> Seq.toList

// [[4,6],[4,6,7],[4,6,7,7],[4,7],[4,7,7],[6,7],[6,7,7],[7,7]]
findSubSequences [ 4; 6; 7; 7 ]

// [[4,4]]
findSubSequences [ 4; 4; 3; 2; 1 ]
