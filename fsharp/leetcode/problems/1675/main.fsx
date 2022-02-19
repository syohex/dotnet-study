#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let minimumDeviation (nums: int list) : int =
    let rec initQueue nums acc (q: IPriorityQueue<int>) : (IPriorityQueue<int> * int) =
        match nums with
        | [] -> q, (List.min acc)
        | x :: xs ->
            if x % 2 = 0 then
                initQueue xs (x :: acc) (PriorityQueue.insert x q)
            else
                let even = x * 2
                initQueue xs (even :: acc) (PriorityQueue.insert even q)

    let rec minimumDeviation' (q: IPriorityQueue<int>) min acc : int =
        if PriorityQueue.isEmpty q then
            acc |> List.min
        else
            let (head, tail) = PriorityQueue.pop q
            let diff = head - min

            if head % 2 = 1 then
                (diff :: acc) |> List.min
            else
                let v = head / 2
                let newMin = if v < min then v else min
                minimumDeviation' (PriorityQueue.insert v tail) newMin (diff :: acc)

    let q, min =
        initQueue nums [] (PriorityQueue.empty true)

    minimumDeviation' q min []

// 1
minimumDeviation [ 1; 2; 3; 4 ]

// 3
minimumDeviation [ 4; 1; 5; 20; 3 ]

// 3
minimumDeviation [ 2; 10; 8 ]
