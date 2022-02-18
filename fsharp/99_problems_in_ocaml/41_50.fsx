// problem 41
let goldbachList m n =
    seq { m .. n }
    |> Seq.filter (fun a -> a % 2 = 0)
    |> Seq.map (fun a -> (a, goldbach a))
    |> Seq.toList

goldbachList 9 20

let goldbachLimit m n limit =
    goldbachList m n
    |> List.filter (fun (_, (b, c)) -> b > limit && c > limit)

goldbachLimit 1 2000 50
