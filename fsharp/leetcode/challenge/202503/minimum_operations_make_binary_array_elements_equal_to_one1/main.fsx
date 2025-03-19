let minOperations (nums: int list) : int =
    let rec minOperations' nums acc ret =
        match nums with 
        | [] -> if List.forall ((=) 1) acc then ret else -1
        | h :: [] -> minOperations' [] (h :: acc) ret
        | h1 :: h2 :: [] -> minOperations' [h2] (h1 :: acc) ret
        | h1 :: h2 :: h3 :: t ->
            if h1 = 0 then
               let h2 = if h2 = 1 then 0 else 1
               let h3 = if h3 = 1 then 0 else 1
               minOperations' (h2 :: h3 :: t) (1 :: acc) (ret + 1)
            else
               minOperations' (h2 :: h3 :: t) acc ret

    minOperations' nums [] 0

// 3
let a = minOperations [0;1;1;1;0;0]

// -1
let b = minOperations [0;1;1;1]

printfn "a=%d, b=%d" a b
               
