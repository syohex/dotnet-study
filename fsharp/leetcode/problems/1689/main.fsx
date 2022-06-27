let minPartitions (n: string) : int =
    n
    |> Seq.map (fun m -> (int m) - (int '0'))
    |> Seq.max

// 3
minPartitions "32"

// 8
minPartitions "82734"

// 9
minPartitions "27346209830709182346"
