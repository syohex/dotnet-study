#r "nuget: FSharpx.Collections"

open FSharpx.Collections

[<CustomEquality; CustomComparison>]
type PersonData =
    { Name: string
      Age: int }

    override this.GetHashCode() = hash this

    override this.Equals other =
        match other with
        | :? PersonData as o -> this.Name = o.Name && this.Age = o.Age
        | _ -> false

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? PersonData as o ->
                if this.Age <> o.Age then
                    compare this.Age o.Age
                else
                    compare this.Name o.Name
            | _ -> failwith "cannot compare with different object"


let q =
    PriorityQueue.empty true
    |> PriorityQueue.insert { Name = "Alice"; Age = 42 }
    |> PriorityQueue.insert { Name = "Bob"; Age = 50 }
    |> PriorityQueue.insert { Name = "Chris"; Age = 18 }
    |> PriorityQueue.insert { Name = "David"; Age = 73 }
    |> PriorityQueue.insert { Name = "Elly"; Age = 33 }
    |> PriorityQueue.insert { Name = "Yoshida"; Age = 73 }

let (a, q1) = PriorityQueue.pop q
printfn "poped=%A" a // Yoshida

let (b, q2) = PriorityQueue.pop q1
printfn "poped=%A" b // David

let (c, q3) = PriorityQueue.pop q2
printfn "poped=%A" c // Bob

let (d, q4) = PriorityQueue.pop q3
printfn "poped=%A" d // Alice

let (e, q5) = PriorityQueue.pop q4
printfn "poped=%A" e // Elly

let (f, _) = PriorityQueue.pop q5
printfn "poped=%A" f // Chris
