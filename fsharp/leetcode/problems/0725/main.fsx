type ListNode =
    | Leaf
    | Node of int * ListNode

let listLength (head: ListNode) : int =
    let rec listLength' node acc =
        match node with
        | Leaf -> acc
        | Node(_, next) -> listLength' next (acc + 1)

    listLength' head 0

let rec listToListNode ns =
    match ns with
    | [] -> Leaf
    | h :: t -> Node(h, listToListNode t)

let splitListToParts (head: ListNode) (k: int) : ListNode list =
    let rec splitListToParts' node count elements mods acc ret =
        match node with
        | Leaf ->
            match acc with
            | [] -> ret
            | _ -> (listToListNode (List.rev acc)) :: ret
        | Node(i, next) ->
            let acc' = i :: acc

            if count - 1 = 0 then
                let count' = if mods > 0 then elements + 1 else elements
                splitListToParts' next count' elements (mods - 1) [] ((listToListNode (List.rev acc')) :: ret)
            else
                splitListToParts' next (count - 1) elements mods acc' ret

    let len = listLength head
    let elements = len / k
    let mods = if len % k = 0 then 0 else len % k
    let nulls = if len < k then k - len else 0
    let count = if mods > 0 then elements + 1 else elements

    let splited = splitListToParts' head count elements (mods - 1) [] []
    seq { 1..nulls } |> Seq.fold (fun acc _ -> Leaf :: acc) splited |> List.rev

let list1 = Node(1, Node(2, Node(3, Leaf)))
// [[1],[2],[3],[],[]]
splitListToParts list1 5

let list2 =
    Node(1, Node(2, Node(3, Node(4, Node(5, Node(6, Node(7, Node(8, Node(9, Node(10, Leaf))))))))))
// [[1,2,3,4],[5,6,7],[8,9,10]]
splitListToParts list2 3
