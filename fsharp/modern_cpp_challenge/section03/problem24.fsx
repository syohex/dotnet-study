let hexCharToInt (c: char) : int =
    if c >= 'a' && c <= 'f' then
        int c - int 'a' + 10
    elif c >= 'A' && c <= 'F' then
        int c - int 'A' + 10
    else
        int c - int '0'

let hexStrToBytes (str: string) : byte list =
    let rec hexStrToBytes' cs acc =
        match cs with
        | [] -> acc |> List.rev
        | a :: b :: t ->
            let b =
                (hexCharToInt a) * 16 + (hexCharToInt b) |> byte

            hexStrToBytes' t (b :: acc)
        | _ -> failwith "never reach here"

    let cs = str |> Seq.toList
    hexStrToBytes' cs []

hexStrToBytes "BAADF00D"
|> List.map (sprintf "%02x")

hexStrToBytes "010203040506"
|> List.map (sprintf "%d")
