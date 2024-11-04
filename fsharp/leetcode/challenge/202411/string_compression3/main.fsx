open System

let compressedString (word: string) : string =
    let numToChar (n: int) = char <| n + int '0'

    let rec compressedString' cs prev count (acc: char list) =
        match cs with
        | [] -> (prev :: numToChar count :: acc) |> List.rev |> String.Concat
        | h :: t ->
            if count = 9 then
                compressedString' t h 1 (prev :: '9' :: acc)
            elif h = prev then
                compressedString' t prev (count + 1) acc
            else
                compressedString' t h 1 (prev :: numToChar count :: acc)

    match Seq.toList word with
    | [] -> failwith "never reach here"
    | h :: t -> compressedString' t h 1 []

// "1a1b1c1d1e"
compressedString "abcde"

// "9a5a2b"
compressedString "aaaaaaaaaaaaaabb"

// "9a1y"
compressedString "aaaaaaaaay"
