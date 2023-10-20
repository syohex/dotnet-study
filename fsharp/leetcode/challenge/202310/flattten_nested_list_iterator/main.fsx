type NestedInteger =
    | Integer of int
    | Nested of NestedInteger list

    static member isInteger(ni: NestedInteger) : bool =
        match ni with
        | Integer(_) -> true
        | _ -> false

    static member getInteger(ni: NestedInteger) : int =
        match ni with
        | Integer(n) -> n
        | _ -> failwith "invalid getIntger call"

type NestedIterator =
    { nums: int list }

    static member init(nl: NestedInteger list) : NestedIterator =
        let rec flatten nl acc =
            match nl with
            | [] -> acc
            | h :: t ->
                match h with
                | Integer(n) -> flatten t (n :: acc)
                | Nested(nl') ->
                    let acc' = flatten nl' acc
                    flatten t acc'

        { nums = flatten nl [] |> List.rev }

    static member next(ni: NestedIterator) : (int * NestedIterator) =
        List.head ni.nums, { nums = List.tail ni.nums }

    static member hasNext(ni: NestedIterator) : bool = List.isEmpty ni.nums |> not

let collect (ni: NestedIterator) =
    let rec collect' ni acc =
        if NestedIterator.hasNext ni then
            let v, ni' = NestedIterator.next ni
            collect' ni' (v :: acc)
        else
            List.rev acc

    collect' ni []

let ni1 =
    NestedIterator.init
        [ Nested([ Integer(1); Integer(1) ])
          Integer(2)
          Nested([ Integer(1); Integer(1) ]) ]
// [1;1;2;1;1]
collect ni1

let ni2 =
    NestedIterator.init [ Integer(1); Nested([ Integer(4); Nested([ Integer(6) ]) ]) ]
// [1;4;6]
collect ni2
