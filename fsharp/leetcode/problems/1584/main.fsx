#r "nuget:FSharpx.Collections"

open FSharpx.Collections

[<CustomEquality; CustomComparison>]
type Data =
    { Distance: int
      X: int
      Y: int
      Index: int }

    override this.GetHashCode() = hash this

    override this.Equals other =
        match other with
        | :? Data as o ->
            this.Distance = o.Distance
            && this.X = o.X
            && this.Y = o.Y
            && this.Index = o.Index
        | _ -> failwith "cannot compare with other types"

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Data as o -> compare this.Distance o.Distance
            | _ -> failwith "cannot compare with other types"

let minCostConnectPoints (points: (int * int) list) : int =
    let rec minCostConnectPoints' n points (queue: IPriorityQueue<Data>) used acc =
        if n = 0 then
            acc
        else
            let h, rest = PriorityQueue.pop queue

            if Set.contains h.Index used then
                minCostConnectPoints' n points rest used acc
            else
                let q' =
                    points
                    |> List.mapi (fun i p -> i, p)
                    |> List.fold
                        (fun acc (i, p) ->
                            if Set.contains i used then
                                acc
                            else
                                let x = fst p
                                let y = snd p
                                let dist = (abs (h.X - x)) + (abs (h.Y - y))

                                PriorityQueue.insert
                                    { Distance = dist
                                      X = x
                                      Y = y
                                      Index = i }
                                    acc)
                        rest

                let used' = Set.add h.Index used
                minCostConnectPoints' (n - 1) points q' used' (acc + h.Distance)

    let len = List.length points

    match points with
    | [] -> failwith "empty points are not acceptable"
    | (x, y) :: _ ->
        let q =
            PriorityQueue.empty false
            |> PriorityQueue.insert
                { Distance = 0
                  X = x
                  Y = y
                  Index = 0 }

        minCostConnectPoints' len points q Set.empty 0

// 20
minCostConnectPoints [ (0, 0)
                       (2, 2)
                       (3, 10)
                       (5, 2)
                       (7, 0) ]

// 18
minCostConnectPoints [ (3, 12)
                       (-2, 5)
                       (-4, 1) ]
