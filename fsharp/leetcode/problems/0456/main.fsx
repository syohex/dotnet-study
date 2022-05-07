open System

let find132pattern (nums: int list) : bool =
    let rec popSmallerValues num stack =
        match stack with
        | [] -> []
        | h :: t ->
            if num > h then
                popSmallerValues num t
            else
                stack

    let rec find132pattern' nums minVals stack =
        match nums, minVals with
        | [], [] -> false
        | h :: t, currentMin :: restMins ->
            if h = currentMin then
                find132pattern' t (List.tail minVals) (h :: stack)
            else
                match stack with
                | [] -> find132pattern' t restMins [ h ]
                | _ ->
                    let stack' = popSmallerValues currentMin stack

                    match stack' with
                    | [] -> find132pattern' t restMins [ h ]
                    | top :: _ ->
                        if h > top then
                            true
                        else
                            find132pattern' t restMins (h :: stack')
        | _ -> failwith "never reach here"

    if nums.Length < 3 then
        false
    else
        let minVals =
            nums
            |> List.tail
            |> List.fold
                (fun acc n ->
                    let h = List.head acc
                    (Math.Min(n, h)) :: acc)
                [ List.head nums ]
            |> List.rev

        let nums' = nums |> List.tail |> List.rev
        let minVals' = minVals |> List.tail |> List.rev
        find132pattern' nums' minVals' []

// false
find132pattern [ 1; 2; 3; 4 ]

// true
find132pattern [ 3; 1; 4; 2 ]

// true
find132pattern [ -1; 3; 2; 0 ]

// true
find132pattern [ 1; 3; 2 ]

// false
find132pattern [ 4; 3; 2 ]

// false
find132pattern [ 1 ]

// false
find132pattern [ 1; 3 ]
