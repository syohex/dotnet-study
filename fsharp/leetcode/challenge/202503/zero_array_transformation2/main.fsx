let minZeroArray (nums: int list) (queries: (int * int * int) list) : int =
    let len = List.length nums

    let f count =
        let rec f' nums steps value =
            match nums, steps with
            | [], _ -> true
            | _, [] -> failwith "never reach here"
            | h1 :: t1, h2 :: t2 ->
                let value = value + h2
                if h1 > value then false else f' t1 t2 value

        let steps =
            queries
            |> List.take count
            |> List.fold
                (fun (acc: int[]) (start, end', value) ->
                    acc.[start] <- acc.[start] + value
                    acc.[end' + 1] <- acc.[end' + 1] - value
                    acc)
                (Array.zeroCreate (len + 1))
            |> List.ofArray

        f' nums steps 0

    let rec minZeroArray' left right =
        if left > right then
            left
        else
            let mid = left + (right - left) / 2

            if f mid then
                minZeroArray' left (mid - 1)
            else
                minZeroArray' (mid + 1) right

    if f (List.length queries) then
        minZeroArray' 0 (len - 1)
    else
        -1

// 0
minZeroArray [ 0 ] [ (0, 0, 1); (0, 0, 2) ]

// 2
minZeroArray [ 2; 0; 2 ] [ (0, 2, 1); (0, 2, 1); (1, 1, 3) ]

// -1
minZeroArray [ 4; 3; 2; 1 ] [ (1, 3, 2); (0, 2, 1) ]
