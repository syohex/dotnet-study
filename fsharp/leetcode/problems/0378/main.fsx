#r "nuget:FSharpx.Collections"

open FSharpx.Collections

[<CustomEquality; CustomComparison>]
type Data =
    { Value: int
      Row: int
      Col: int }

    override this.GetHashCode() = hash this

    override this.Equals other =
        match other with
        | :? Data as o ->
            this.Value = o.Value
            && this.Row = o.Row
            && this.Col = o.Col
        | _ -> false

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Data as o -> compare this.Value o.Value
            | _ -> failwith "cannot compare with other types"

let kthSmallest (matrix: int [,]) (k: int) : int =
    let rec initPriorityQueye i (matrix: int [,]) (pq: IPriorityQueue<Data>) =
        if i >= (Array2D.length1 matrix) then
            pq
        else
            let pq' =
                PriorityQueue.insert
                    { Value = matrix.[i, 0]
                      Row = i
                      Col = 0 }
                    pq

            initPriorityQueye (i + 1) matrix pq'

    let rec kthSmallest' k (pq: IPriorityQueue<Data>) (matrix: int [,]) =
        if k = 0 then
            let d = PriorityQueue.peek pq
            d.Value
        else
            let d, pq' = PriorityQueue.pop pq

            if d.Col < (Array2D.length2 matrix) - 1 then
                kthSmallest'
                    (k - 1)
                    (PriorityQueue.insert
                        { Value = matrix.[d.Row, d.Col + 1]
                          Row = d.Row
                          Col = d.Col + 1 }
                        pq')
                    matrix
            else
                kthSmallest' (k - 1) pq' matrix

    let pq =
        initPriorityQueye 0 matrix (PriorityQueue.empty false)

    kthSmallest' (k - 1) pq matrix

let matrix1 =
    array2D [ [ 1; 5; 9 ]
              [ 10; 11; 13 ]
              [ 12; 13; 15 ] ]

// 13
kthSmallest matrix1 8

let matrix2 = array2D [ [ -5 ] ]
// -5
kthSmallest matrix2 1
