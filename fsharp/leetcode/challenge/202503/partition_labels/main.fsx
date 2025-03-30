let partitionLabels (s: string) : int list =
    let rec partitionLabels' cs maxPos prev m acc =
        match cs with
        | [] -> List.rev acc
        | (i, h) :: t ->
            let maxPos = max maxPos (Map.find h m)

            if i = maxPos then
                partitionLabels' t maxPos i m ((i - prev) :: acc)
            else
                partitionLabels' t maxPos prev m acc

    let m = s |> Seq.indexed |> Seq.fold (fun acc (i, n) -> Map.add n i acc) Map.empty
    let cs = s |> Seq.indexed |> Seq.toList
    partitionLabels' cs 0 -1 m []

// [9,7,8]
partitionLabels "ababcbacadefegdehijhklij"

// [10]
partitionLabels "eccbbbbdec"
