open System

let findMaxLength (nums: int list) : int =
    let rec findMaxLength' (nums: (int * int) list) sum max (m: Map<int, int>) =
        match nums with
        | [] -> max
        | (i, head) :: tail ->
            let sum = if head = 1 then sum + 1 else sum - 1

            match Map.tryFind sum m with
            | Some idx -> findMaxLength' tail sum (Math.Max(max, i - idx)) m
            | None -> findMaxLength' tail sum max m

    let numsWithIndex = nums |> List.mapi (fun i n -> (i, n))
    let m = Map.empty |> Map.add 0 -1
    findMaxLength' numsWithIndex 0 0 m

// 2
findMaxLength [ 0; 1 ]

// 2
findMaxLength [ 0; 1; 0 ]
