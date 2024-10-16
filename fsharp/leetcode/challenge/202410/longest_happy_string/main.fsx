#r "nuget:FSharpx.Collections"

open FSharpx.Collections
open System

let longestDiverseString (a: int) (b: int) (c: int) : string =
    let toString (cs: char list) = cs |> List.rev |> String.Concat

    let rec longestDiverseString' q prev1 prev2 (acc: char list) =
        match PriorityQueue.tryPop q with
        | None -> toString acc
        | Some(((count, ch), q1)) ->
            if prev2 = ch && prev1 = ch then
                match PriorityQueue.tryPop q1 with
                | None -> toString acc
                | Some(((count2, ch2), q2)) ->
                    let acc = ch2 :: acc

                    let q' =
                        if count2 >= 2 then
                            PriorityQueue.insert ((count2 - 1), ch2) q2
                        else
                            q2

                    let q' = PriorityQueue.insert (count, ch) q'
                    longestDiverseString' q' ch2 prev1 acc
            else
                let acc = ch :: acc

                let q' =
                    if count >= 2 then
                        PriorityQueue.insert ((count - 1), ch) q1
                    else
                        q1

                longestDiverseString' q' ch prev1 acc

    let q = PriorityQueue.empty true
    let q = if a > 0 then PriorityQueue.insert (a, 'a') q else q
    let q = if b > 0 then PriorityQueue.insert (b, 'b') q else q
    let q = if c > 0 then PriorityQueue.insert (c, 'c') q else q

    longestDiverseString' q ' ' ' ' []

// "ccbccacc"
longestDiverseString 1 1 7

// "aabaa"
longestDiverseString 7 1 0
