let maxDepth (s: string) : int =
    let rec maxDepth' cs depth ret =
        match cs with
        | [] -> ret
        | h :: t ->
            match h with
            | '(' -> maxDepth' t (depth + 1) (System.Math.Max(ret, depth + 1))
            | ')' -> maxDepth' t (depth - 1) ret
            | _ -> maxDepth' t depth ret

    maxDepth' (Seq.toList s) 0 0

// 3
maxDepth "(1+(2*3)+((8)/4))+1"

// 3
maxDepth "(1)+((2))+(((3)))"
