let separateSquares (squares: (int * int * int) list) : double =
    let eps = 0.00001

    let totalArea, maxY =
        squares
        |> List.fold
            (fun (totalArea, maxY) (_, y, len) -> totalArea + (double len * double len), max maxY (y + len))
            (0.0, 0)

    let check (limit: double) =
        let sum =
            squares
            |> List.filter (fun (_, y, _) -> double y < limit)
            |> List.fold
                (fun acc (_, y, len) ->
                    let height =
                        if double (y + len) >= limit then
                            limit - double y
                        else
                            double len

                    let area = double len * height
                    acc + area)
                0.0

        sum >= totalArea / 2.0

    let rec f (left: double) (right: double) =
        if (abs (right - left)) <= eps then
            right
        else
            let mid = left + (right - left) / 2.0
            if check mid then f left mid else f mid right

    f 0 (double maxY)

// 1.0
separateSquares [ (0, 0, 1); (2, 2, 1) ]

// 1.166667
separateSquares [ (0, 0, 2); (1, 1, 1) ]
