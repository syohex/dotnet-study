let maxEnvelopes (envelopes: (int * int) list) : int =
    let envs =
        envelopes
        |> List.sortWith (fun (w1, h1) (w2, h2) ->
            if w1 = w2 then
                compare h2 h1
            else
                compare w1 w2)

    let dp = Array.zeroCreate envs.Length
    let mutable len = 0

    for (_, height) in envs do
        let p =
            System.Array.BinarySearch(dp, 0, len, height)

        let p' = if p >= 0 then p else -1 * (p + 1)
        dp.[p'] <- height
        if p' = len then len <- len + 1

    len

// 3
maxEnvelopes [ (5, 4)
               (6, 4)
               (6, 7)
               (2, 3) ]

// 1
maxEnvelopes [ (1, 1); (1, 1); (1, 1) ]

// 5
maxEnvelopes [ (2, 100)
               (3, 200)
               (4, 300)
               (5, 500)
               (5, 400)
               (5, 250)
               (6, 370)
               (6, 360)
               (7, 380) ]
