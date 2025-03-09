let numberOfAlternatingGroups (colors: int list) (k: int) : int =
    let rec numberOfAlternatingGroups' left right colors prev origLen acc =
        if left >= origLen then
            acc
        else
            match colors with
            | [] -> failwith "never reach here"
            | h :: t ->
                if h = prev then
                    numberOfAlternatingGroups' right (right + 1) t h origLen acc
                else
                    let right = right + 1

                    if right - left < k then
                        numberOfAlternatingGroups' left right t h origLen acc
                    else
                        numberOfAlternatingGroups' (left + 1) right t h origLen (acc + 1)

    let origLen = List.length colors
    let colors = colors @ (List.take k colors)

    match colors with
    | [] -> failwith "never reach here"
    | h :: t -> numberOfAlternatingGroups' 0 1 t h origLen 0

// 3
numberOfAlternatingGroups [ 0; 1; 0; 1; 0 ] 3

// 2
numberOfAlternatingGroups [ 0; 1; 0; 0; 1; 0; 1 ] 6

// 0
numberOfAlternatingGroups [ 1; 1; 0; 1 ] 4
