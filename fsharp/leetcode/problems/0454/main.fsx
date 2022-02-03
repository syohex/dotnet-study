let sumMap (nums1: int list) (nums2: int list) : Map<int, int> =
    let rec sumMap' n ns (m: Map<int, int>) =
        match ns with
        | [] -> m
        | head :: tail ->
            let key = head + n

            let m =
                match Map.tryFind key m with
                | Some v -> Map.add key (v + 1) m
                | None -> Map.add key 1 m

            sumMap' n tail m

    nums1
    |> List.fold (fun m n -> sumMap' n nums2 m) Map.empty


let fourSumCount (nums1: int list) (nums2: int list) (nums3: int list) (nums4: int list) : int =
    let rec inner n ns (m: Map<int, int>) acc =
        match ns with
        | [] -> acc
        | head :: tail ->
            let key = -(n + head)

            match Map.tryFind key m with
            | Some _ -> inner n tail m (acc + 1)
            | None -> inner n tail m acc

    let m = sumMap nums1 nums2

    nums3
    |> List.map (fun n -> inner n nums4 m 0)
    |> List.sum

// 2
fourSumCount [ 1; 2 ] [ -2; -1 ] [ -1; 2 ] [ 0; 2 ]

// 1
fourSumCount [ 0 ] [ 0 ] [ 0 ] [ 0 ]
