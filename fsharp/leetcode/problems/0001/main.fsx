let twoSum (nums: int list) (target: int) : (int * int) =
    let ps = nums |> List.mapi (fun i x -> (i, x))

    let rec twoSum' (idx_value: (int * int) list) : (int * int) option =
        match idx_value with
        | [] -> None
        | (index, num) :: tail ->
            match List.tryFind (fun (i, v) -> v = target - num && i <> index) ps with
            | Some (i, _) -> Some(index, i)
            | _ -> twoSum' tail

    match twoSum' ps with
    | Some v -> v
    | _ -> failwith "never reach here"

twoSum [ 2; 7; 11; 15 ] 9
twoSum [ 3; 2; 4 ] 6
twoSum [ 3; 3 ] 6
