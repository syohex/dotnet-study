open System

let largestGoodInteger (num: string) : string =
    let rec largestGoodInteger' (cs: char list) acc =
        match cs with
        | [] -> acc
        | a :: b :: c :: t when a = b && b = c ->
            let s = [ a; a; a ] |> String.Concat

            if s > acc then
                largestGoodInteger' t s
            else
                largestGoodInteger' t acc
        | _ :: t -> largestGoodInteger' t acc

    largestGoodInteger' (Seq.toList num) ""

// 777
largestGoodInteger "6777133339"

// 000
largestGoodInteger "2300019"

// ""
largestGoodInteger "42352338"

// 888
largestGoodInteger "333888"

// 888
largestGoodInteger "888333"
