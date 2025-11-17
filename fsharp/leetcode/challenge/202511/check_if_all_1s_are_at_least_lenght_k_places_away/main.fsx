let kLengthApart (nums: int list) (k: int) : bool =
    nums
    |> List.indexed
    |> List.filter (snd >> (=) 1)
    |> List.map fst
    |> List.windowed 2
    |> List.map (fun v -> (List.item 1 v) - (List.item 0 v) - 1)
    |> List.forall (fun n -> n >= k)

// true
kLengthApart [ 1; 0; 0; 0; 1; 0; 0; 1 ] 2

// false
kLengthApart [ 1; 0; 0; 1; 0; 1 ] 2
