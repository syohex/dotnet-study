open System

type LinkedList =
    | Leaf
    | Node of int * LinkedList

type Solution =
    { Head: LinkedList
      Rand: Random }

    static member init(list: LinkedList) : Solution = { Head = list; Rand = new Random() }

    member this.getRandom() : int =
        let rec getRandom' list (rand: Random) n acc =
            match list with
            | Leaf -> acc
            | Node(v, next) ->
                let r = rand.NextDouble()
                if r < 1.0 / n then
                    getRandom' next rand (n + 1.0) v
                else
                    getRandom' next rand (n + 1.0) acc

        getRandom' this.Head this.Rand 1.0 0


let list1 = Node(1, Node(2, Node(3, Leaf)))
let s = Solution.init list1
seq {1..10} |> Seq.iter (fun _ -> printfn "%d" (s.getRandom()))
