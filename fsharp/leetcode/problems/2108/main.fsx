let is_palindrome (str: string) : bool =
    let rec inner (orig: char list) (rev: char list) : bool =
        match orig with
        | [] -> true
        | _ ->
            if (List.head orig) = (List.head rev) then
                inner (List.tail orig) (List.tail rev)
            else
                false

    let reversed = str |> Seq.toList |> Seq.rev |> Seq.toList
    inner (Seq.toList str) reversed

let first_palindrome (words: string list) : string =
    match (words |> List.tryFind is_palindrome) with
    | Some s -> s
    | None -> ""

first_palindrome [ "abc"
                   "car"
                   "ada"
                   "racecar"
                   "cool" ]
