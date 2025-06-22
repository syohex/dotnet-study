open System

let divideString (s: string) (k: int) (fill: char) : string list =
    let rec divideString' (cs: char list) (tmp: string) acc =
        match cs with
        | [] ->
            if tmp.Length = 0 then
                List.rev acc
            else
                let f = string fill
                let count = k - s.Length % k
                let tmp = seq { 1..count } |> Seq.fold (fun acc _ -> acc + f) tmp
                List.rev (tmp :: acc)
        | h :: t ->
            let tmp = tmp + string h
            let acc, tmp = if tmp.Length = k then tmp :: acc, "" else acc, tmp
            divideString' t tmp acc

    divideString' (Seq.toList s) "" []

// ["abc","def","ghi"]
divideString "abcdefghi" 3 'x'
// ["abc","def","ghi","jxx"]
divideString "abcdefghig" 3 'x'
