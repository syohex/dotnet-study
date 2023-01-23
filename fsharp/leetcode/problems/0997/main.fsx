let findJudge (n: int) (trust: (int * int) list) : int =
    let rec trustCount trust acc =
        match trust with
        | [] -> acc
        | (_, trusted) :: t ->
            match Map.tryFind trusted acc with
            | Some(v) -> trustCount t (Map.add trusted (v + 1) acc)
            | None -> trustCount t (Map.add trusted 1 acc)

    let rec hasTrustPeople trust acc =
        match trust with
        | [] -> acc
        | (people, _) :: t -> hasTrustPeople t (Set.add people acc)

    let rec findJudge' count trustPeople ret =
        match count with
        | [] -> ret
        | (trusted, count) :: t ->
            if count = n - 1 && (Set.contains trusted trustPeople |> not) then
                if ret <> -1 then -1 else findJudge' t trustPeople trusted
            else
                findJudge' t trustPeople ret

    let count = trustCount trust Map.empty |> Map.toList
    let trustPeople = hasTrustPeople trust Set.empty

    findJudge' count trustPeople -1

// 2
findJudge 2 [ (1, 2) ]

// 3
findJudge 3 [ (1, 3); (2, 3) ]

// -1
findJudge 3 [ (1, 3); (2, 3); (3, 1) ]

// 3
findJudge 4 [ (1, 3); (1, 4); (2, 3); (2, 4); (4, 3) ]

// -1
findJudge 3 [ (1, 2); (2, 3) ]
