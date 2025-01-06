let minOperations (boxes: string) : int list =
    let rec minOperations' cs rs leftBalls leftMoves rightBalls rightMoves accLeft accRight =
        match cs, rs with
        | [], [] -> (List.rev accLeft, accRight) ||> List.zip |> List.map (fun (a, b) -> a + b)
        | [], _
        | _, [] -> failwith "never reach here"
        | l1 :: t1, r1 :: t2 ->
            let accLeft = leftMoves :: accLeft
            let leftBalls = leftBalls + if l1 = '1' then 1 else 0
            let leftMoves = leftMoves + leftBalls

            let accRight = rightMoves :: accRight
            let rightBalls = rightBalls + if r1 = '1' then 1 else 0
            let rightMoves = rightMoves + rightBalls

            minOperations' t1 t2 leftBalls leftMoves rightBalls rightMoves accLeft accRight

    let cs = Seq.toList boxes
    minOperations' cs (List.rev cs) 0 0 0 0 [] []

// [1,1,3]
minOperations "110"

// [11,8,5,4,3,4]
minOperations "001011"
