let containsDuplicate (nums: int list) : bool =
    let mutable m = Map.empty<int, int>
    nums |> List.iter (fun n ->
        m <- if Map.containsKey n m then Map.add n (m.[n] + 1) m
             else Map.add n 1 m) 
    (m |> Map.filter (fun _ v -> v >= 2) |> Map.toSeq |> Seq.length) <> 0

containsDuplicate [1;2;3;1]
containsDuplicate [1;2;3;4]
containsDuplicate [1;1;1;3;3;4;3;2;4;2]
