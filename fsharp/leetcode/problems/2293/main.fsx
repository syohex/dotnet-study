open System

let minMaxGame (nums: int list) : int =
    let rec apply i (nums: int list) acc =
        match nums with
        | [] -> acc |> List.rev
        | a :: b :: t ->
            if i % 2 = 0 then
                apply (i + 1) t (Math.Min(a, b) :: acc)
            else
                apply (i + 1) t (Math.Max(a, b) :: acc)
        | _ -> failwith "never reach here"

    and minMaxGame' nums =
        if nums |> List.tail |> List.isEmpty then
            nums |> List.head
        else
            minMaxGame' (apply 0 nums [])

    minMaxGame' nums

// 1
minMaxGame [ 1; 3; 5; 2; 4; 8; 2; 2 ]

// 3
minMaxGame [ 3 ]
