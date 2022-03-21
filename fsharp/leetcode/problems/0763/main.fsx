let partitionLabels (s: string) : int list =
    let rec lastPos xs m =
        match xs with
        | [] -> m
        | (i, h) :: t -> lastPos t (Map.add h i m)

    let rec partitionLabels' (cs: (int * char) list) (lastPos: Map<char, int>) curMax prev acc =
        match cs with
        | [] -> acc |> List.rev
        | (i, h) :: t ->
            let curMax = System.Math.Max(curMax, Map.find h lastPos)

            if i = curMax then
                let len = i - prev
                partitionLabels' t lastPos curMax i (len :: acc)
            else
                partitionLabels' t lastPos curMax prev acc

    let cs = s |> Seq.toList |> List.mapi (fun i c -> i, c)
    let lastPos = lastPos cs Map.empty
    partitionLabels' cs lastPos -1 -1 []

// [9, 7, 8]
partitionLabels "ababcbacadefegdehijhklij"

// [10]
partitionLabels "eccbbbbdec"
