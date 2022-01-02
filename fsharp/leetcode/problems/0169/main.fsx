let majorityElement (nums: int list) : int =
    nums |> List.countBy id |> List.sortBy (fun (_, count) -> count) |> List.rev |> List.head |> fst

majorityElement [3;2;3]
majorityElement [2;2;1;1;1;2;2]
