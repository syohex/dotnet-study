let kthDistinct (arr: string list) (k: int) : string =
    let rec kthDistinct' arr i m k =
        match arr with
        | [] -> ""
        | h :: t ->
            match Map.tryFind h m with
            | Some(v) ->
                if v = 1 then
                    let i' = i + 1
                    if i' = k then h else kthDistinct' t i' m k
                else
                    kthDistinct' t i m k
            | None -> failwith "never reach here"

    let m = arr |> List.countBy id |> Map.ofList
    kthDistinct' arr 0 m k

// "a"
kthDistinct [ "d"; "b"; "c"; "b"; "c"; "a" ] 2

// "aaa"
kthDistinct [ "aaa"; "aa"; "a" ] 1

// ""
kthDistinct [ "a"; "b"; "a" ] 3
