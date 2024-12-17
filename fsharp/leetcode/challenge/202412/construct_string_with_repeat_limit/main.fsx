#r "nuget:FSharpx.Collections"

open System
open FSharpx.Collections

let repeatLimitedString (s: string) (repeatLimit: int) : string =
    let consChars c count acc =
        seq { 1..count } |> Seq.fold (fun acc _ -> c :: acc) acc

    let toString (cs: char list) = cs |> List.rev |> String.Concat

    let rec repeatLimitedString' q acc =
        match PriorityQueue.tryPop q with
        | None -> toString acc
        | Some(((c, count), q')) ->
            if count <= repeatLimit then
                repeatLimitedString' q' (consChars c count acc)
            else
                let acc = consChars c repeatLimit acc

                match PriorityQueue.tryPop q' with
                | None -> toString acc
                | Some(((c2, count2), q'')) ->
                    let acc = c2 :: acc

                    let q =
                        if count2 > 1 then
                            PriorityQueue.insert (c2, count2 - 1) q''
                        else
                            q''

                    let q = PriorityQueue.insert (c, count - repeatLimit) q
                    repeatLimitedString' q acc

    let q =
        s
        |> Seq.countBy id
        |> Seq.fold (fun acc (c, count) -> PriorityQueue.insert (c, count) acc) (PriorityQueue.empty true)

    repeatLimitedString' q []

// "zzcccac"
repeatLimitedString "cczazcc" 3

// "bbabaa"
repeatLimitedString "aababab" 2
