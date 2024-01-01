let findContentChildren (g: int list) (s: int list) : int =
    let rec findContentChildren' g s acc =
        match s with
        | [] -> acc
        | h :: t ->
            match g with
            | [] -> acc
            | h' :: t' ->
                if h >= h' then
                    findContentChildren' t' t (acc + 1)
                else
                    findContentChildren' g t acc

    findContentChildren' (List.sort g) (List.sort s) 0

// 1
findContentChildren [ 1; 2; 3 ] [ 1; 1 ]

// 2
findContentChildren [ 1; 2 ] [ 1; 2; 3 ]
