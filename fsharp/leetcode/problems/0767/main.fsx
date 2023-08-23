#r "nuget:FSharpx.Collections"

open System
open FSharpx.Collections

let reorganizeString (s: string) : string =
    let rec toFreq (cs: char list) acc =
        match cs with
        | [] -> acc
        | h :: t ->
            match Map.tryFind h acc with
            | Some(n) -> toFreq t (Map.add h (n + 1) acc)
            | None -> toFreq t (Map.add h 1 acc)

    let rec reorganizeString' (q: IPriorityQueue<(int * char)>) (acc: char list) =
        match PriorityQueue.tryPop q with
        | None -> acc |> List.rev |> String.Concat
        | Some((count1, c1), q') ->
            match acc with
            | [] ->
                if count1 - 1 >= 1 then
                    reorganizeString' (PriorityQueue.insert (count1 - 1, c1) q') (c1 :: acc)
                else
                    reorganizeString' q' (c1 :: acc)
            | prev :: _ when prev <> c1 ->
                if count1 - 1 >= 1 then
                    reorganizeString' (PriorityQueue.insert (count1 - 1, c1) q') (c1 :: acc)
                else
                    reorganizeString' q' (c1 :: acc)
            | _ ->
                match PriorityQueue.tryPop q' with
                | None -> ""
                | Some((count2, c2), t2) ->
                    if count2 - 1 >= 1 then
                        let q'' =
                            t2
                            |> PriorityQueue.insert ((count2 - 1), c2)
                            |> PriorityQueue.insert ((count1, c2))

                        reorganizeString' q'' (c2 :: acc)
                    else
                        let q'' = t2 |> PriorityQueue.insert ((count1, c2))
                        reorganizeString' q'' (c2 :: acc)

    let q =
        toFreq (Seq.toList s) Map.empty
        |> Map.fold (fun acc c count -> PriorityQueue.insert (count, c) acc) (PriorityQueue.empty true)

    reorganizeString' q []

// "aba"
reorganizeString "aab"

// ""
reorganizeString "aaab"
