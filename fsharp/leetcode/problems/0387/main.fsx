let firstUniqChar (s: string) : int =
    let rec firstUniqChar' cs (acc: int list []) =
        match cs with
        | [] ->
            match Array.tryFind (fun v -> List.length v = 1) acc with
            | None -> -1
            | _ ->
                acc
                |> Array.fold
                    (fun acc v ->
                        if List.length v = 1 then
                            System.Math.Min(acc, List.last v)
                        else
                            acc)
                    System.Int32.MaxValue
        | (i, h) :: t ->
            let index = int h - int 'a'
            acc.[index] <- i :: acc.[index]
            firstUniqChar' t acc

    let cs =
        s |> Seq.toList |> List.mapi (fun i c -> i, c)

    firstUniqChar' cs (Array.create 26 [])


// 0
firstUniqChar "leetcode"

// 2
firstUniqChar "loveleetcode"

// -1
firstUniqChar "aabb"

// -1
firstUniqChar ""
