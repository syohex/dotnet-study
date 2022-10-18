let groupBy (s: string) : (int * char) list =
    let rec groupBy' cs prev count acc =
        match cs with
        | [] -> ((count, prev) :: acc) |> List.rev
        | h :: t ->
            if h = prev then
                groupBy' t prev (count + 1) acc
            else
                groupBy' t h 1 ((count, prev) :: acc)

    let cs = s |> Seq.toList
    groupBy' (List.tail cs) (List.head cs) 1 []

let groupToString (groups: (int * char) list) : string =
    groups
    |> List.fold (fun acc (count, c) -> acc + sprintf $"{count}{c}") ""

let countAndSay (n: int) : string =
    let rec countAndSay' i n prev =
        if i = n then
            prev
        else
            countAndSay' (i + 1) n (prev |> groupBy |> groupToString)

    countAndSay' 1 n "1"

// "1"
countAndSay 1

// "1211"
countAndSay 4
