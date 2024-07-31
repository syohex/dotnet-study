let minHeightShelves (books: (int * int) list) (shelfWidth: int) : int =
    let rec minHeightShelves' i books remainingWidth rowHeight cache =
        match books with
        | [] -> failwith "never reach here"
        | (width, height) :: [] ->
            if width <= remainingWidth then
                (max height rowHeight), cache
            else
                height, cache
        | (width, height) :: t ->
            match Map.tryFind (i, remainingWidth) cache with
            | Some(v) -> v, cache
            | None ->
                let ret1, cache = minHeightShelves' (i + 1) t (shelfWidth - width) height cache
                let ret1 = ret1 + rowHeight

                let rowHeight = max rowHeight height

                let ret2, cache =
                    if width <= remainingWidth then
                        minHeightShelves' (i + 1) t (remainingWidth - width) rowHeight cache
                    else
                        ret1, cache

                let ret = min ret1 ret2
                ret, Map.add (i, remainingWidth) ret cache

    minHeightShelves' 0 books shelfWidth 0 Map.empty |> fst

// 6
minHeightShelves [ (1, 1); (2, 3); (2, 3); (1, 1); (1, 1); (1, 1); (1, 1); (1, 2) ] 4

// 4
minHeightShelves [ (1, 3); (2, 4); (3, 2) ] 6
