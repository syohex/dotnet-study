let findKthBit (n: int) (k: int) : char =
    if n = 1 then
        '0'
    else
        seq { 2..n }
        |> Seq.fold
            (fun acc _ ->
                acc
                @ [ '1' ]
                @ (acc |> List.rev |> List.map (fun n -> if n = '1' then '0' else '1')))
            [ '0' ]
        |> List.item (k - 1)

// '0'
findKthBit 3 1

// 1
findKthBit 4 11
