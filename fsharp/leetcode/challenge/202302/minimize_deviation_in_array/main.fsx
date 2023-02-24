#r "nuget:FSharpx.Collections"

open System
open FSharpx.Collections

let minimumDeviation (nums: int list) : int =
    let rec initQueue nums (min: int) (q: IPriorityQueue<int>) : (IPriorityQueue<int> * int) =
        match nums with
        | [] -> q, min
        | h :: t ->
            if h % 2 = 0 then
                initQueue t (Math.Min(min, h)) (PriorityQueue.insert h q)
            else
                let even = h * 2
                initQueue t (Math.Min(min, even)) (PriorityQueue.insert even q)

    let rec minimumDeviation' (q: IPriorityQueue<int>) (min: int) (ret: int) : int =
        if PriorityQueue.isEmpty q then
            ret
        else
            let (head, tail) = PriorityQueue.pop q
            let ret' = Math.Min(ret, head - min)

            if head % 2 = 1 then
                ret'
            else
                let v = head / 2
                let min' = Math.Min(min, v)
                minimumDeviation' (PriorityQueue.insert v tail) min' ret'

    let q, min = initQueue nums Int32.MaxValue (PriorityQueue.empty true)
    minimumDeviation' q min Int32.MaxValue

// 1
minimumDeviation [ 1; 2; 3; 4 ]

// 3
minimumDeviation [ 4; 1; 5; 20; 3 ]

// 3
minimumDeviation [ 2; 10; 8 ]
