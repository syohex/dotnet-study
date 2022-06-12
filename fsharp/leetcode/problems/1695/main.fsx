let maximumUniqueSubarray (nums: int list) : int =
    let rec maximumUniqueSubarray' nums i start (acc: int []) pos ret =
        match nums with
        | [] -> ret
        | h :: t ->
            acc.[i + 1] <- acc.[i] + h

            match Map.tryFind h pos with
            | None ->
                let ret' = acc.[i + 1] - acc.[start]
                maximumUniqueSubarray' t (i + 1) start acc (Map.add h i pos) ret'
            | Some (p) ->
                let start' = System.Math.Max(start, p + 1)
                let ret' = acc.[i + 1] - acc.[start']
                maximumUniqueSubarray' t (i + 1) start' acc (Map.add h i pos) ret'

    let acc = Array.zeroCreate (nums.Length + 1)
    maximumUniqueSubarray' nums 0 0 acc Map.empty 0

// 17
maximumUniqueSubarray [ 4; 2; 4; 5; 6 ]

// 8
maximumUniqueSubarray [ 5
                        2
                        1
                        2
                        5
                        2
                        1
                        2
                        5 ]

// 10
maximumUniqueSubarray [ 10 ]

// 16911
maximumUniqueSubarray [ 187
                        470
                        25
                        436
                        538
                        809
                        441
                        167
                        477
                        110
                        275
                        133
                        666
                        345
                        411
                        459
                        490
                        266
                        987
                        965
                        429
                        166
                        809
                        340
                        467
                        318
                        125
                        165
                        809
                        610
                        31
                        585
                        970
                        306
                        42
                        189
                        169
                        743
                        78
                        810
                        70
                        382
                        367
                        490
                        787
                        670
                        476
                        278
                        775
                        673
                        299
                        19
                        893
                        817
                        971
                        458
                        409
                        886
                        434 ]
