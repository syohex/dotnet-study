let divideArray (nums: int list) (k: int) : (int * int * int) list =
    let rec divideArray' nums k acc =
        match nums with
        | [] -> List.rev acc
        | h1 :: h2 :: h3 :: t ->
            if h3 - h1 > k then
                []
            else
                divideArray' t k ((h1, h2, h3) :: acc)
        | _ -> failwith "never reach here"

    divideArray' (List.sort nums) k []

// [[1,1,3],[3,4,5],[7,8,9]]
divideArray [ 1; 3; 4; 8; 7; 9; 3; 5; 1 ] 2

// []
divideArray [ 1; 3; 3; 2; 7; 3 ] 3
