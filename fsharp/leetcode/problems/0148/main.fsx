type LinkedList =
    | LinkEnd
    | LinkNode of int * LinkedList

let rec toList (xs: LinkedList) : int list =
    match xs with
    | LinkEnd -> []
    | LinkNode(v, tail) -> v :: toList tail

let listLength (xs: LinkedList) : int =
    let rec listLength' xs acc =
        match xs with
        | LinkEnd -> acc
        | LinkNode (_, tail) -> listLength' tail (acc + 1)

    listLength' xs 0

let splitList (xs: LinkedList) (len: int) : (LinkedList * LinkedList) =
    let rec firstHalf xs n =
        if n = 0 then
            LinkEnd
        else
            match xs with
            | LinkEnd -> LinkEnd
            | LinkNode (v, tail) -> LinkNode(v, firstHalf tail (n - 1))

    let rec secondHalf xs n =
        if n = 0 then
            xs
        else
            match xs with
            | LinkEnd -> LinkEnd
            | LinkNode (_, tail) -> secondHalf tail (n - 1)

    if len = 1 then
        (xs, LinkEnd)
    else
        let half = len / 2
        (firstHalf xs half, secondHalf xs half)

let rec merge (xs: LinkedList) (ys: LinkedList) : LinkedList =
    match xs, ys with
    | LinkEnd, LinkEnd -> LinkEnd
    | LinkNode(v1, tail1), LinkEnd -> LinkNode(v1, merge tail1 LinkEnd)
    | LinkEnd, LinkNode(v2, tail2) -> LinkNode(v2, merge LinkEnd tail2)
    | LinkNode(v1, tail1), LinkNode(v2, tail2) ->
        if v1 <= v2 then
            LinkNode(v1, merge tail1 ys)
        else
            LinkNode(v2, merge xs tail2)

let rec sortList (xs: LinkedList) : LinkedList =
    let len = listLength xs
    if len <= 1 then
        xs
    else
        let (firstHalf, secondHalf) = splitList xs len
        let firstSorted = sortList firstHalf
        let secondSorted = sortList secondHalf
        merge firstSorted secondSorted

// [1,2,3,4]
let data1 =
    LinkNode(4, LinkNode(2, LinkNode(1, LinkNode(3, LinkEnd))))

sortList data1

// [-1,0,3,4,5]
let data2 =
    LinkNode(-1, LinkNode(5, LinkNode(3, LinkNode(4, LinkNode(0, LinkEnd)))))

sortList data2

// []
sortList LinkEnd

// [-1,0,1,2,3,3,4,4,5]
merge data1 data2 |> sortList |> toList