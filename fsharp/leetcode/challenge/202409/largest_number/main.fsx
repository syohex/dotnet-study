let largestNumber (nums: int list) : string =
    let v =
        nums |> List.map string |> List.sortWith (fun a b -> compare (b + a) (a + b))

    match v with
    | [] -> failwith "never reach here"
    | h :: _ when h = "0" -> "0"
    | _ -> System.String.Concat v

// 210
largestNumber [ 10; 2 ]

// 9534330
largestNumber [ 3; 30; 34; 5; 9 ]
