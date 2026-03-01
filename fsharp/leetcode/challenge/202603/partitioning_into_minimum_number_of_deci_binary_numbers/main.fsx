let minPartitions (n: string) : int =
    n
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.max

// 3
minPartitions "32" 

// 8
minPartitions "83123423"

// 9
minPartitions "1134392849583884"
