let maxResult (nums: int list) (k: int) : int =
    let updateQueue (q: (int * int) list) i k score =
        let q' = q |> List.filter (fun (pos, _) -> pos > i - k)

        ((i, score) :: q')
        |> List.sortWith (fun (_, score1) (_, score2) -> compare score2 score1)

    let rec maxResult' i nums k q dp =
        match nums with
        | [] -> dp
        | h :: t ->
            let _, score = List.head q
            let dp' = h + score
            let q' = updateQueue q i k dp'
            maxResult' (i + 1) t k q' dp'

    let q = [ (0, nums.Head) ]
    maxResult' 1 nums.Tail k q nums.Head

// 7
maxResult [ 1; -1; -2; 4; -7; 3 ] 2

// 17
maxResult [ 10; -5; -2; 4; 0; 3 ] 3

// 0
maxResult [ 1; -5; -20; 4; -1; 3; -6; -3 ] 2
