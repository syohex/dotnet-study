type MyList =
    | Nil
    | Node of int * MyList

let rec listToMyList (nums: int list) : MyList =
    match nums with
    | [] -> Nil
    | h :: t -> Node(h, listToMyList t)

let myListLength (lst: MyList) : int =
    let rec myListLength' lst acc =
        match lst with
        | Nil -> acc
        | Node(_, next) -> myListLength' next (1 + acc)

    myListLength' lst 0

let splitListToParts (head: MyList) (k: int) : MyList list =
    let rec splitListToParts' node count n m acc ret =
        match node with
        | Nil ->
            let nulls = k - List.length ret
            let ret = seq { 1..nulls } |> Seq.fold (fun acc _ -> Nil :: acc) ret
            List.rev ret
        | Node(v, next) ->
            if m > 0 && count = n + 1 then
                let lst = listToMyList (List.rev (v :: acc))
                splitListToParts' next 1 n (m - 1) [] (lst :: ret)
            elif m = 0 && count = n then
                let lst = listToMyList (List.rev (v :: acc))
                splitListToParts' next 1 n 0 [] (lst :: ret)
            else
                splitListToParts' next (count + 1) n m (v :: acc) ret

    let len = myListLength head
    let n, m = len / k, len % k
    splitListToParts' head 1 n m [] []

let list1 = Node(1, Node(2, Node(3, Nil)))
// [[1],[2],[3],[],[]]
splitListToParts list1 5

let list2 =
    Node(1, Node(2, Node(3, Node(4, Node(5, Node(6, Node(7, Node(8, Node(9, Node(10, Nil))))))))))
// [[1,2,3,4],[5,6,7],[8,9,10]]
splitListToParts list2 3
