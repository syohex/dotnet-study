let maxIceCream (costs: int list) (coins: int) : int =
    let rec maxIceCream' i sum costs coins =
        match costs with
        | [] -> i
        | h :: t ->
            let sum' = sum + h

            if sum' > coins then
                i
            else
                maxIceCream' (i + 1) sum' t coins

    maxIceCream' 0 0 (costs |> List.sort) coins

// 4
maxIceCream [ 1; 3; 2; 4; 1 ] 7

// 0
maxIceCream [ 10; 6; 8; 7; 7; 8 ] 5

// 6
maxIceCream [ 1; 6; 3; 1; 2; 5 ] 20

// 9
maxIceCream [ 7; 3; 3; 6; 6; 6; 10; 5; 9; 2 ] 56
