let partition (s: string) : string list list =
    let isPalindrome (s: char list) : bool = s = List.rev s

    let rec partition' (cs: char list) (acc: char list list) (ret: string list list) =
        match cs with
        | [] ->
            let strs =
                acc
                |> List.rev
                |> List.map (fun c -> System.String.Concat(Array.ofList c))

            strs :: ret
        | _ ->
            seq { 1 .. cs.Length }
            |> Seq.map (fun n -> List.take n cs, List.skip n cs)
            |> Seq.filter (fun (head, _) -> isPalindrome head)
            |> Seq.fold (fun ret' (head, rest) -> partition' rest (head :: acc) ret') ret

    partition' (s |> Seq.toList) [] []

// [["a","a","b"],["aa","b"]]
partition "aab"

// [["a"]]
partition "a"
