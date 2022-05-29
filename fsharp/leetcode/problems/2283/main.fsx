let digitCount (num: string) : bool =
    let freq =
        num
        |> Seq.fold
            (fun (acc: int []) c ->
                let key = int c - int '0'
                acc.[key] <- acc.[key] + 1
                acc)
            (Array.zeroCreate 10)

    num
    |> Seq.mapi (fun i c -> i, c)
    |> Seq.forall (fun (i, c) ->
        let num = int c - int '0'
        num = freq.[i])

// true
digitCount "1210"

// false
digitCount "030"
