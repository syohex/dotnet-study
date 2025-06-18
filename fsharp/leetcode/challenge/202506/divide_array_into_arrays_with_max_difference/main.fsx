let divideArray (nums: int list) (k: int) : (int * int * int) list =
    let rec divideArray' nums k acc =
        match nums with
        | [] -> List.rev acc
        | _ :: []
        | _ :: _ :: [] -> failwith "never reach here"
        | a :: b :: c :: t ->
            if c - a > k then
                []
            else
                divideArray' t k ((a, b, c) :: acc)

    divideArray' (List.sort nums) k []

// [[1,1,3],[3,4,5],[7,8,9]]
divideArray [ 1; 3; 4; 8; 7; 9; 3; 5; 1 ] 2

// []
divideArray [ 2; 4; 2; 2; 5; 2 ] 2
