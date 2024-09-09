type MyList =
    | Nil
    | Node of int * MyList

let spiralMatrix (m: int) (n: int) (head: MyList) : int[,] =
    let steps = [| (0, 1); (1, 0); (0, -1); (-1, 0) |]

    let rec spiralMatrix' node dir row col (acc: int[,]) =
        match node with
        | Nil -> acc
        | Node(v, next) ->
            acc.[row, col] <- v
            let r, c = row + fst steps.[dir], col + snd steps.[dir]

            if r >= 0 && r < m && c >= 0 && c < n && acc.[r, c] = -1 then
                spiralMatrix' next dir r c acc
            else
                let d = (dir + 1) % 4
                let r, c = row + fst steps.[d], col + snd steps.[d]
                spiralMatrix' next d r c acc

    let acc = Array2D.init m n (fun _ _ -> -1)
    spiralMatrix' head 0 0 0 acc

let rec listToNode (nums: int list) =
    match nums with
    | [] -> Nil
    | h :: t -> Node(h, listToNode t)

let list1 = listToNode [ 3; 0; 2; 6; 8; 1; 7; 9; 4; 2; 5; 5; 0 ]
// [[3,0,2,6,8],[5,0,-1,-1,1],[5,2,4,9,7]]
spiralMatrix 3 5 list1

let list2 = Node(0, Node(1, Node(2, Nil)))
// [[0,1,2,-1]]
spiralMatrix 1 4 list2
