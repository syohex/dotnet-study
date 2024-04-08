let countStudents (students: int list) (sandwiches: int list) : int =
    let rec countStudents' sandwiches zeros ones =
        match sandwiches with
        | [] -> 0
        | h :: t ->
            if h = 0 && zeros > 0 then
                countStudents' t (zeros - 1) ones
            elif h = 1 && ones > 0 then
                countStudents' t zeros (ones - 1)
            else
                zeros + ones

    let zeros, ones =
        students
        |> List.fold (fun (zeros, ones) n -> if n = 0 then zeros + 1, ones else zeros, ones + 1) (0, 0)

    countStudents' sandwiches zeros ones

// 0
countStudents [ 1; 1; 0; 0 ] [ 0; 1; 0; 1 ]

// 3
countStudents [ 1; 1; 1; 0; 0; 1 ] [ 1; 0; 0; 0; 1; 1 ]
