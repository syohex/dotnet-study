#r "nuget:FSharpx.Collections"

open System
open FSharpx.Collections

[<CustomEquality; CustomComparison>]
type Data =
    { X: int
      Y: int
      Diff: int }

    override this.GetHashCode() = hash this

    override this.Equals other =
        match other with
        | :? Data as o -> this.X = o.X && this.Y = o.Y && this.Diff = o.Diff
        | _ -> failwith "cannot compare with other types"

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Data as o -> compare this.Diff o.Diff
            | _ -> failwith "cannot compare with other types"

let nextPositions (row: int) (col: int) (rows: int) (cols: int) : (int * int) list =
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    steps
    |> List.map (fun (x, y) -> row + x, col + y)
    |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)

let minimumEffortPath (heights: int [,]) : int =
    let rec minimumEffortPath' (q: IPriorityQueue<Data>) (dp: int [,]) (used: bool [,]) (heights: int [,]) rows cols =
        if PriorityQueue.isEmpty q then
            dp.[rows - 1, cols - 1]
        else
            let h, t = PriorityQueue.pop q
            used.[h.X, h.Y] <- true

            let nexts = nextPositions h.X h.Y rows cols

            let q' =
                nexts
                |> List.fold
                    (fun acc (x, y) ->
                        let diff =
                            Math.Abs(heights.[x, y] - heights.[h.X, h.Y])

                        let max = Math.Max(diff, dp.[h.X, h.Y])

                        if max < dp.[x, y] then
                            dp.[x, y] <- max
                            PriorityQueue.insert { X = x; Y = y; Diff = max } acc
                        else
                            acc)
                    t

            minimumEffortPath' q' dp used heights rows cols

    let rows = Array2D.length1 heights
    let cols = Array2D.length2 heights

    let dp =
        Array2D.init rows cols (fun _ _ -> Int32.MaxValue)

    let used =
        Array2D.init rows cols (fun _ _ -> false)

    let q =
        PriorityQueue.empty false
        |> PriorityQueue.insert { X = 0; Y = 0; Diff = 0 }

    dp.[0, 0] <- 0
    used.[0, 0] <- true

    minimumEffortPath' q dp used heights rows cols

let heights1 =
    array2D [ [ 1; 2; 2 ]
              [ 3; 8; 2 ]
              [ 5; 3; 5 ] ]
// 2
minimumEffortPath heights1

let heights2 =
    array2D [ [ 1; 2; 3 ]
              [ 3; 8; 4 ]
              [ 5; 3; 5 ] ]
// 1
minimumEffortPath heights2

let heights3 =
    array2D [ [ 1; 2; 1; 1; 1 ]
              [ 1; 2; 1; 2; 1 ]
              [ 1; 2; 1; 2; 1 ]
              [ 1; 2; 1; 2; 1 ]
              [ 1; 1; 1; 2; 1 ] ]
// 0
minimumEffortPath heights3

let heights4 = array2D [ [ 1; 10; 6; 7; 9; 10; 4; 9 ] ]
// 9
minimumEffortPath heights4

let heights5 = array2D [ [ 3 ] ]
// 0
minimumEffortPath heights5
