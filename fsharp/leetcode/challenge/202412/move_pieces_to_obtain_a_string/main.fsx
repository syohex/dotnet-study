let canChange (start: string) (target: string) : bool =
    let rec canChange' cs1 cs2 =
        match cs1, cs2 with
        | [], [] -> true
        | [], _
        | _, [] -> failwith "never reach here"
        | (i1, h1) :: t1, (i2, h2) :: t2 ->
            if h1 <> h2 || (h1 = 'L' && i1 < i2) || (h1 = 'R' && i1 > i2) then
                false
            else
                canChange' t1 t2

    let nonEmptyPositions (s: string) =
        s |> Seq.indexed |> Seq.filter (fun (_, c) -> c <> '_') |> Seq.toList

    let cs1 = nonEmptyPositions start
    let cs2 = nonEmptyPositions target
    canChange' cs1 cs2

// true
canChange "_L__R__R_" "L______RR"

// false
canChange "R_L_" "__LR"

// false
canChange "_R" "R_"
