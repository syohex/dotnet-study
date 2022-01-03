let removeDuplicates (s: string) : string =
    let rec removeDuplicates' (cs: char list) (stack: char list) : string =
        match cs with
        | [] -> stack |> List.rev |> Array.ofList |> System.String.Concat
        | head :: tail when List.isEmpty stack -> removeDuplicates' tail (head :: stack)
        | head :: tail when head = (List.head stack) -> removeDuplicates' tail (List.tail stack)
        | head :: tail -> removeDuplicates' tail (head :: stack)

    removeDuplicates' (s |> Seq.toList) []

removeDuplicates "abbaca"
removeDuplicates "azxxzy"
