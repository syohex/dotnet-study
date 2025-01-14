let findThePrefixCommonArray (a: int list) (b: int list) : int list =
    let rec findThePrefixCommonArray' nums (freq: int[]) commons acc =
        match nums with
        | [] -> List.rev acc
        | (a, b) :: t ->
            freq.[a - 1] <- freq.[a - 1] + 1
            let commons = if freq.[a - 1] = 2 then commons + 1 else commons
            freq.[b - 1] <- freq.[b - 1] + 1
            let commons = if freq.[b - 1] = 2 then commons + 1 else commons
            findThePrefixCommonArray' t freq commons (commons :: acc)

    let freq = Array.zeroCreate (List.length a)
    findThePrefixCommonArray' (List.zip a b) freq 0 []

// [0,2,3,4]
findThePrefixCommonArray [ 1; 3; 2; 4 ] [ 3; 1; 2; 4 ]

// [0,1,3]
findThePrefixCommonArray [ 2; 3; 1 ] [ 3; 1; 2 ]
