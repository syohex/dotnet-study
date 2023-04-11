open System

let removeStars (s: string) : string =
    let rec removeStars' (cs: char list) acc =
        match cs with
        | [] -> acc |> List.rev |> List.toArray |> String
        | h :: t ->
            if h = '*' then
                removeStars' t (List.tail acc)
            else
                removeStars' t (h :: acc)

    removeStars' (Seq.toList s) []

// "lecoe"
removeStars "leet**cod*e"

// ""
removeStars "erase*****"
