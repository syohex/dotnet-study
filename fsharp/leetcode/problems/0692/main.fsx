let topKFrequent (words: string list) (k: int) : string list =
    words
    |> List.fold
        (fun acc word ->
            match Map.tryFind word acc with
            | Some (n) -> Map.add word (n + 1) acc
            | None -> Map.add word 1 acc)
        Map.empty
    |> Map.toList
    |> List.sortWith (fun (word1, count1) (word2, count2) ->
        if count1 = count2 then
            compare word1 word2
        else
            compare count2 count1)
    |> List.take k
    |> List.map fst

let words1 =
    [ "i"
      "love"
      "leetcode"
      "i"
      "love"
      "coding" ]
// ["i","love"]
topKFrequent words1 2

let words2 =
    [ "the"
      "day"
      "is"
      "sunny"
      "the"
      "the"
      "the"
      "sunny"
      "is"
      "is" ]
// ["the","is","sunny","day"]
topKFrequent words2 4
