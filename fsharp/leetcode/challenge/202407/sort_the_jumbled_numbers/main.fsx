let convert (n: int) (mapping: int[]) : int =
    let rec convert' n acc v =
        if n = 0 then
            acc
        else
            let acc = acc + v * mapping.[n % 10]
            convert' (n / 10) acc (v * 10)

    if n = 0 then
        mapping.[0]
    else
        convert' n 0 1

let sortJumbled (mapping: int[]) (nums: int list) : int list =
    nums
    |> List.mapi (fun i num -> i, num, convert num mapping)
    |> List.sortWith (fun (i1, _, mapped1) (i2, _, mapped2) ->
                        if mapped1 = mapped2 then
                            compare i1 i2
                        else
                            compare mapped1 mapped2)
    |> List.map (fun (_, num, _) -> num)