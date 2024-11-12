let maximumBeauty (items: (int * int) list) (queries: int list) : int list =
    let rec binarySearch query left right (items: (int * int)[]) acc =
        if left > right then
            acc
        else
            let mid = left + (right - left) / 2

            if fst items.[mid] <= query then
                binarySearch query (mid + 1) right items (max acc (snd items.[mid]))
            else
                binarySearch query left (mid - 1) items acc


    let items =
        items
        |> List.sortWith (fun (p1, b1) (p2, b2) -> if p1 = p2 then compare b2 b1 else compare p1 p2)

    let items =
        items
        |> List.fold
            (fun (acc, maxVal) (p, b) ->
                let maxVal = max maxVal b
                (p, maxVal) :: acc, maxVal)
            ([], 0)
        |> fst
        |> List.rev
        |> List.toArray

    queries |> List.map (fun q -> binarySearch q 0 (items.Length - 1) items 0)

// [2,4,5,5,6,6]
maximumBeauty [ (1, 2); (3, 2); (2, 4); (5, 6); (3, 5) ] [ 1..6 ]

// [4]
maximumBeauty [ (1, 2); (1, 2); (1, 3); (1, 4) ] [ 1 ]

// [0]
maximumBeauty [ (10, 1000) ] [ 5 ]

// [962,962,962,962,746,962,962,962,946,962,962,919,746,746,962,962,962,919,962]
maximumBeauty
    [ (193, 732)
      (781, 962)
      (864, 954)
      (749, 627)
      (136, 746)
      (478, 548)
      (640, 908)
      (210, 799)
      (567, 715)
      (914, 388)
      (487, 853)
      (533, 554)
      (247, 919)
      (958, 150)
      (193, 523)
      (176, 656)
      (395, 469)
      (763, 821)
      (542, 946)
      (701, 676) ]
    [ 885
      1445
      1580
      1309
      205
      1788
      1214
      1404
      572
      1170
      989
      265
      153
      151
      1479
      1180
      875
      276
      1584 ]
