let maximumDifference (nums: int list) : int =
    let rec maximumDifference' nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let acc =
                t
                |> List.filter (fun n -> n > h)
                |> List.fold (fun acc n -> max acc (n - h)) acc

            maximumDifference' t acc

    maximumDifference' nums -1

// 4
maximumDifference [ 7; 1; 5; 4 ]

// -1
maximumDifference [ 4; 3; 2; 1 ]

// 9
maximumDifference [ 1; 5; 2; 10 ]
