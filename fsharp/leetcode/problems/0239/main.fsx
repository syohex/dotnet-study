#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let maxSlidingWindow (nums: int list) (k: int) : int list =
    let rec popSmallerValues i (nums: int[]) (q: Deque<int>) =
        match q.TryLast with
        | None -> q
        | Some(j) ->
            if nums.[i] >= nums.[j] then
                popSmallerValues i nums q.Initial
            else
                q

    let rec initWindow i k (nums: int[]) (q: Deque<int>) =
        if i >= k then
            q
        else if Deque.isEmpty q then
            initWindow (i + 1) k nums (q.Conj i)
        else
            let q' = popSmallerValues i nums q
            initWindow (i + 1) k nums (q'.Conj i)

    let rec maxSlidingWindow' i k (nums: int[]) (q: Deque<int>) acc =
        if i >= nums.Length then
            List.rev acc
        else
            let q' =
                match q.TryHead with
                | None -> q
                | Some(v) -> if v = i - k then q.Tail else q

            let q' = popSmallerValues i nums q'
            let q' = q'.Conj i
            maxSlidingWindow' (i + 1) k nums q' (nums.[q'.Head] :: acc)

    let nums' = List.toArray nums
    let q = initWindow 0 k nums' Deque.empty
    maxSlidingWindow' k k nums' q [nums.[q.Head]]

// [3, 3, 5, 5, 6, 7]
maxSlidingWindow [ 1; 3; -1; -3; 5; 3; 6; 7 ] 3

// [1]
maxSlidingWindow [ 1 ] 1
