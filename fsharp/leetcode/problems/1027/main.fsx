open System

let longestArithSeqLength (nums: int list) : int =
    let rec longestArithSeqLength' (nums: int[]) i (dp: Map<int, int>[]) ret =
        if i >= nums.Length then
            ret
        else
            let dp', acc' =
                seq { 0 .. (i - 1) }
                |> Seq.fold
                    (fun ((dp: Map<int, int>[]), acc) j ->
                        let diff = nums.[i] - nums.[j]

                        let prev =
                            match Map.tryFind diff dp.[j] with
                            | Some(v) -> v
                            | None -> 1

                        dp.[i] <- Map.add diff (prev + 1) dp.[i]
                        let acc' = Math.Max(acc, prev + 1)
                        dp, acc')
                    (dp, ret)

            longestArithSeqLength' nums (i + 1) dp' acc'

    let nums' = List.toArray nums
    let dp = Array.init nums'.Length (fun _ -> Map.empty)
    longestArithSeqLength' nums' 1 dp 0

// 4
longestArithSeqLength [ 3; 6; 9; 12 ]

// 3
longestArithSeqLength [ 9; 4; 7; 2; 10 ]

// 4
longestArithSeqLength [ 20; 1; 15; 3; 10; 5; 8 ]
