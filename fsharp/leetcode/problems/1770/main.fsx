let maximumScore (nums: int list) (multipliers: int list) : int =
    let rec maximumScore' left right multipliers (nums: int []) cache =
        match multipliers with
        | [] -> 0, cache
        | h :: t ->
            match Map.tryFind (left, right) cache with
            | Some (v) -> v, cache
            | None ->
                let val1, cache' =
                    maximumScore' (left + 1) right t nums cache

                let val2, cache'' =
                    maximumScore' left (right - 1) t nums cache'

                let leftVal = val1 + nums.[left] * h
                let rightVal = val2 + nums.[right] * h

                let ret = System.Math.Max(leftVal, rightVal)
                ret, Map.add (left, right) ret cache''

    let nums' = nums |> List.toArray

    maximumScore' 0 (nums.Length - 1) multipliers nums' Map.empty
    |> fst


// 14
maximumScore [ 1; 2; 3 ] [ 3; 2; 1 ]

let nums2 = [ -5; -3; -3; -2; 7; 1 ]
let multipliers2 = [ -10; -5; 3; 4; 6 ]
// 102
maximumScore nums2 multipliers2
