let maximumSum (nums: int list) : int =
    let rec digitSum num acc =
        if num = 0 then
            acc
        else
            digitSum (num / 10) ((num % 10) + acc)

    let rec splitByDigitsSum nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let sum = digitSum h 0

            match Map.tryFind sum acc with
            | Some ((n1, n2)) ->
                if h > n1 then
                    splitByDigitsSum t (Map.add sum (h, n1) acc)
                elif h > n2 then
                    splitByDigitsSum t (Map.add sum (n1, h) acc)
                else
                    splitByDigitsSum t acc
            | None -> splitByDigitsSum t (Map.add sum (h, -1) acc)

    splitByDigitsSum nums Map.empty
    |> Map.fold
        (fun ret _ (n1, n2) ->
            if n2 = -1 then
                ret
            else
                System.Math.Max(ret, n1 + n2))
        -1

// 54
maximumSum [ 18; 43; 36; 13; 7 ]

// -1
maximumSum [ 10; 12; 19; 14 ]
