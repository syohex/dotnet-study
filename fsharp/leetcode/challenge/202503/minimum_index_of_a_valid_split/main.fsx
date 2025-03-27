let minimumIndex (nums: int list) : int =
    let len = List.length nums

    let rec minimumIndex' nums maxKey leftCount rightCount =
        match nums with
        | [] -> failwith "never reach here"
        | _ :: [] -> -1
        | (i, n) :: t ->
            let leftCount, rightCount =
                if n = maxKey then
                    leftCount + 1, rightCount - 1
                else
                    leftCount, rightCount

            let leftSize = i + 1
            let rightSize = len - (i + 1)

            if leftCount > leftSize / 2 && rightCount > rightSize / 2 then
                i
            else
                minimumIndex' t maxKey leftCount rightCount

    let maxKey, maxVal =
        nums
        |> List.fold
            (fun acc n ->
                let v = Map.tryFind n acc |> Option.defaultValue 0
                Map.add n (v + 1) acc)
            Map.empty
        |> Map.fold (fun (maxK, maxV) k v -> if v > maxV then k, v else maxK, maxV) (0, 0)

    minimumIndex' (List.indexed nums) maxKey 0 maxVal

// 2
minimumIndex [ 1; 2; 2; 2 ]

// 4
minimumIndex [ 2; 1; 3; 1; 1; 1; 7; 1; 2; 1 ]

// -1
minimumIndex [ 3; 3; 3; 3; 7; 2; 2 ]
