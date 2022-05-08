type NestedInteger =
    | Int of int
    | List of NestedInteger list

type NestedIterator =
    { mutable Nums: int list }

    static member init(nestedList: NestedInteger list) : NestedIterator =
        let rec flatten nestedList acc =
            match nestedList with
            | [] -> acc |> List.rev
            | h :: t ->
                match h with
                | Int (n) -> flatten t (n :: acc)
                | List (v) ->
                    let lst = flatten v []
                    let acc' = (lst |> List.rev) @ acc
                    flatten t acc'

        let nums = flatten nestedList []
        { Nums = nums }

    member this.next() : int =
        let ret = List.head this.Nums
        this.Nums <- List.tail this.Nums
        ret

    member this.hasNext() : bool = List.isEmpty this.Nums |> not

let check (ni: NestedIterator) : int list =
    seq {
        while ni.hasNext () do
            yield ni.next ()
    }
    |> Seq.toList

let lst1 =
    [ List([ Int(1); Int(1) ])
      Int(2)
      List([ Int(1); Int(1) ]) ]

let ni = NestedIterator.init lst1
// [1;1;2;1;1]
check ni

let lst2 =
    [ Int(1)
      List([ Int(4); List([ Int(6) ]) ]) ]

let ni2 = NestedIterator.init lst2
// [1;4;6]
check ni2

let lst3 =
    [ List([ Int(1); Int(2) ])
      Int(3)
      List([ Int(4); Int(5) ]) ]

let ni3 = NestedIterator.init lst3
// [1;2;3;4;5]
check ni3
