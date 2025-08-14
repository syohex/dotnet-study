let largestGoodInteger (num: string) : string =
    let rec f num acc =
        match num with
        | a :: b :: c :: t ->
            if a = b && b = c then
                f t (max acc a)
            else
                f (List.tail num) acc
        | _ -> if acc = -1 then "" else sprintf "%d%d%d" acc acc acc

    let v = num |> Seq.map (fun c -> int c - int '0') |> Seq.toList
    f v -1

// 777
largestGoodInteger "6777133339"

// 000
largestGoodInteger "2300019"

// ""
largestGoodInteger "42352338"
