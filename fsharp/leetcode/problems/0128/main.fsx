let longestConsecutive (nums: int list) : int =
    let rec checkConsecutive num s len =
        if Set.contains num s then
            checkConsecutive (num + 1) s (len + 1)
        else
            len

    let rec longestConsecutive' nums s ret =
        match nums with
        | [] -> ret
        | h :: t ->
            let len = checkConsecutive (h + 1) s 1
            longestConsecutive' t s (System.Math.Max(ret, len))

    let s = nums |> Set.ofList
    longestConsecutive' nums s 0

// 4
longestConsecutive [ 100
                     4
                     200
                     1
                     3
                     2 ]

// 9
longestConsecutive [ 0
                     3
                     7
                     2
                     5
                     8
                     4
                     6
                     0
                     1 ]
