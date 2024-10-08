let minSwaps (s: string) : int =
    let rec minSwaps' cs opens unbalanced =
        match cs with
        | [] ->
            if unbalanced % 2 = 0 then
                unbalanced / 2
            else
                unbalanced / 2 + 1
        | h :: t ->
            if h = '[' then minSwaps' t (opens + 1) unbalanced
            else if opens > 0 then minSwaps' t (opens - 1) unbalanced
            else minSwaps' t opens (unbalanced + 1)

    minSwaps' (Seq.toList s) 0 0

// 1
minSwaps "][]["

// 2
minSwaps "]]][[["

// 0
minSwaps "[]"
