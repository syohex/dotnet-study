let decodeMessage (key: string) (message: string) : string =
    let rec createTable key next table =
        match key with
        | [] -> table
        | h :: t ->
            if h >= 'a'
               && h <= 'z'
               && (Map.containsKey h table |> not) then
                createTable t (next + 1) (Map.add h (char next) table)
            else
                createTable t next table

    let table = createTable (key |> Seq.toList) (int 'a') Map.empty

    message
    |> Seq.fold
        (fun acc c ->
            match Map.tryFind c table with
            | Some (v) -> acc + string v
            | None -> acc + string c)
        ""

let key1 = "the quick brown fox jumps over the lazy dog"
let message1 = "vkbs bs t suepuv"

// "this is a secret"
decodeMessage key1 message1

let key2 = "eljuxhpwnyrdgtqkviszcfmabo"
let message2 = "zwx hnfx lqantp mnoeius ycgk vcnjrdb"

// "the five boxing wizards jump quickly"
decodeMessage key2 message2
