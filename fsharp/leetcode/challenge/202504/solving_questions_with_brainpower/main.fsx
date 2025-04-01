let mostPoints (questions: (int * int) list) : int64 =
    let rec mostPoints' questions (dp: int64[]) =
        match questions with
        | [] -> dp.[0]
        | (i, (score, brainpower)) :: t ->
            let point =
                if i + brainpower + 1 < dp.Length then
                    int64 score + dp.[i + brainpower + 1]
                else
                    int64 score

            dp.[i] <- max point dp.[i + 1]
            mostPoints' t dp

    match List.indexed questions |> List.rev with
    | [] -> failwith "never reach here"
    | (_, (score, _)) :: t ->
        let len = List.length questions
        let dp: int64[] = Array.zeroCreate len
        dp.[len - 1] <- score
        mostPoints' t dp

// 5
mostPoints [ (3, 2); (4, 3); (4, 4); (2, 5) ]

// 7
mostPoints [ (1, 1); (2, 2); (3, 3); (4, 4); (5, 5) ]
