let bytesToHexStr (bytes: byte list) : string =
    let rec bytesToHexStr' bytes (acc: string list) =
        match bytes with
        | [] -> acc |> List.rev |> System.String.Concat
        | h :: t ->
            let s = sprintf "%02x" h
            bytesToHexStr' t (s :: acc)

    bytesToHexStr' bytes []

bytesToHexStr [ 0xBAuy
                0xADuy
                0xF0uy
                0x0Duy ]

bytesToHexStr [ 1uy
                2uy
                3uy
                4uy
                5uy
                6uy ]
