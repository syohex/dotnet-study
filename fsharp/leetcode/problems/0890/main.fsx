let findAndReplacePattern (words: string list) (pattern: string) : string list =
    let toIndexPattern word =
        let rec toIndexPattern' cs i cache acc =
            match cs with
            | [] -> acc |> List.rev
            | h :: t ->
                match Map.tryFind h cache with
                | Some (pos) -> toIndexPattern' t (i + 1) cache (pos :: acc)
                | None -> toIndexPattern' t (i + 1) (Map.add h i cache) (i :: acc)

        toIndexPattern' (word |> Seq.toList) 0 Map.empty []

    let pp = toIndexPattern pattern

    words
    |> List.filter (fun word ->
        let wp = toIndexPattern word

        List.zip pp wp
        |> List.forall (fun (a, b) -> a = b))

// ["mee", "aqq"]
findAndReplacePattern
    [ "abc"
      "deq"
      "mee"
      "aqq"
      "dkd"
      "ccc" ]
    "abb"

// ["a","b","c"]
findAndReplacePattern [ "a"; "b"; "c" ] "a"

// []
findAndReplacePattern
    [ "ktittgzawn"
      "dgphvfjniv"
      "gceqobzmis"
      "alrztxdlah"
      "jijuevoioe"
      "mawiizpkub"
      "onwpmnujos"
      "zszkptjgzj"
      "zwfvzhrucv"
      "isyaphcszn" ]
    "zdqmjnczma"
