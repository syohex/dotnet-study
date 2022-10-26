let checkSubarrayNum (nums: int list) (k: int) : bool =
    let rec checkSubarrayNum' nums i sum k m =
        match nums with
        | [] -> false
        | h :: t ->
            let sum' = h + sum
            let modulo = sum' % k

            match Map.tryFind modulo m with
            | Some (j) ->
                if j < i then
                    true
                else
                    checkSubarrayNum' t (i + 1) sum' k m
            | None -> checkSubarrayNum' t (i + 1) sum' k (Map.add modulo (i + 1) m)

    checkSubarrayNum' nums 0 0 k (Map.add 0 0 Map.empty)


// true
checkSubarrayNum [ 23; 2; 4; 6; 7 ] 6

// true
checkSubarrayNum [ 23; 2; 6; 4; 7 ] 6

// false
checkSubarrayNum [ 23; 2; 6; 4; 7 ] 13

// true
checkSubarrayNum [ 0; 0 ] 1
