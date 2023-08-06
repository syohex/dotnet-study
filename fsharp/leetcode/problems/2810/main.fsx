open System

let finalString (s: string) : string =
    let rec finalString' (cs: char list) (acc: char list) =
        match cs with
        | [] -> List.rev acc |> String.Concat
        | h :: t ->
            if h = 'i' then
                finalString' t (List.rev acc)
            else
                finalString' t (h :: acc)

    finalString' (Seq.toList s) []

// "rtsng"
finalString "string" |> printfn "%A"

// "ponter"
finalString "poiinter" |> printfn "%A"
