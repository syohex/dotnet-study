let largestGoodInteger (num: string) : string =
    let rec largestGoodInteger' (cs: char list) prev count acc =
        match cs with
        | [] ->
            match acc with
            | [] -> ""
            | _ -> acc |> List.sort |> List.rev |> List.head
        | h :: t ->
            if h = prev then
                if count = 2 then
                    let s = new string (h, 3)
                    largestGoodInteger' t ' ' 0 (s :: acc)
                else
                    largestGoodInteger' t h (count + 1) acc
            else
                largestGoodInteger' t h 1 acc

    largestGoodInteger' (num |> Seq.toList) ' ' 0 []

// 777
largestGoodInteger "6777133339"

// 000
largestGoodInteger "2300019"

// ""
largestGoodInteger "42352338"
