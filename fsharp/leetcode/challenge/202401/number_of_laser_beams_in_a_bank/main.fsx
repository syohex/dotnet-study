let numberOfBeams (bank: string list) : int =
    let rec numberOfBeams' bank prev acc =
        match bank with
        | [] -> acc
        | h :: t ->
            let ones = Seq.filter ((=) '1') h |> Seq.length

            if ones > 0 then
                numberOfBeams' t ones (acc + (prev * ones))
            else
                numberOfBeams' t prev acc

    numberOfBeams' bank 0 0

// 8
numberOfBeams [ "011001"; "000000"; "010100"; "001000" ]

// 0
numberOfBeams [ "000"; "111"; "000" ]
