let findTheWinner (n: int) (k: int) : int =
    let rec findTheWinner' start k (v: int[]) =
        if Array.length v <= 1 then
            v.[0]
        else
            let removed = (start + k - 1) % (Array.length v)

            let v' =
                v |> Array.indexed |> Array.filter (fun (i, _) -> i <> removed) |> Array.map snd

            findTheWinner' removed k v'

    let v = Array.init n (fun i -> i + 1)
    findTheWinner' 0 k v

// 3
findTheWinner 5 2

// 1
findTheWinner 6 5
