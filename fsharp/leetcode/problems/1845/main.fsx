#r "nuget: Fsharpx.Collections"

open FSharpx.Collections

type SeatManager =
    { seats: IPriorityQueue<int> }

    static member init(n: int) : SeatManager =
        let q =
            seq { 1..n }
            |> Seq.fold (fun acc i -> PriorityQueue.insert i acc) (PriorityQueue.empty false)

        { seats = q }

    static member reserve(s: SeatManager) : (int * SeatManager) =
        let i, q = PriorityQueue.pop s.seats
        i, { seats = q }

    static member unreserve (seatNumber: int) (s: SeatManager) : SeatManager =
        let q = PriorityQueue.insert seatNumber s.seats
        { seats = q }

let s = SeatManager.init 5
let r1, s1 = SeatManager.reserve s
let r2, s2 = SeatManager.reserve s1

let s3 = SeatManager.unreserve 2 s2
let r3, s4 = SeatManager.reserve s3
let r4, s5 = SeatManager.reserve s4
let r5, s6 = SeatManager.reserve s5
let r6, s7 = SeatManager.reserve s6
let s8 = SeatManager.unreserve 5 s7
let r7, _ = SeatManager.reserve s8

// [1,2,2,3,4,5,5]
[ r1; r2; r3; r4; r5; r6; r7 ]
