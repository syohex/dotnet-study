let reverseString (s: char list) : char list =
    let rec reverseString' s acc =
        match s with
        | [] -> acc
        | head :: tail -> reverseString' tail (head :: acc)

    reverseString' s []

reverseString ("hello" |> Seq.toList)
reverseString ("Hannah" |> Seq.toList)
