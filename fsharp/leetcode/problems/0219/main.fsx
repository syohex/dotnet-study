let containsNearByDuplicate (nums: int list) (k: int) : bool =
    let rec containsNearByDuplicate' nums i k acc =
        match nums with
        | [] -> false
        | h :: t ->
            match Map.tryFind h acc with
            | None -> containsNearByDuplicate' t (i + 1) k (Map.add h i acc)
            | Some (prev) ->
                if i - prev <= k then
                    true
                else
                    containsNearByDuplicate' t (i + 1) k (Map.add h i acc)

    containsNearByDuplicate' nums 0 k Map.empty

// true
containsNearByDuplicate [ 1; 2; 3; 1 ] 3

// true
containsNearByDuplicate [ 1; 0; 1; 1 ] 1

// false
containsNearByDuplicate [ 1; 2; 3; 1; 2; 3 ] 2
