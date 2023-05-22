let topKFrequent (nums: int list) (k: int) : int list =
    List.countBy id nums
    |> List.sortWith (fun (num1, count1) (num2, count2) ->
        if count1 = count2 then
            compare num1 num2
        else
            compare count2 count1)
    |> List.take k
    |> List.map fst

// [1, 2]
topKFrequent [ 1; 1; 1; 2; 2; 3 ] 2

// [1]
topKFrequent [ 1 ] 1
