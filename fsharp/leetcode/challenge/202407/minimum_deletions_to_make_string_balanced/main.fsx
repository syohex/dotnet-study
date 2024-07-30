let minimumDeletions (s: string) : int =
    let rec countChars cs c count acc =
        match cs with
        | [] -> acc
        | h :: t -> countChars t c (count + if h = c then 1 else 0) (count :: acc)

    let cs = Seq.toList s
    let countB = countChars cs 'b' 0 [] |> List.rev
    let countA = countChars (List.rev cs) 'a' 0 []

    List.zip countA countB |> List.map (fun (a, b) -> a + b) |> List.min

// 2
minimumDeletions "aababbab"

// 2
minimumDeletions "bbaaaaabb"
