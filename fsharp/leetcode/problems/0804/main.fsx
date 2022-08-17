let uniqueMorseRepresentations (words: string list) : int =
    let codes =
        [| ".-"
           "-..."
           "-.-."
           "-.."
           "."
           "..-."
           "--."
           "...."
           ".."
           ".---"
           "-.-"
           ".-.."
           "--"
           "-."
           "---"
           ".--."
           "--.-"
           ".-."
           "..."
           "-"
           "..-"
           "...-"
           ".--"
           "-..-"
           "-.--"
           "--.." |]

    let rec uniqueMorseRepresentations' words acc =
        match words with
        | [] -> Set.count acc
        | h :: t ->
            let code =
                h
                |> Seq.fold
                    (fun s c ->
                        let index = int c - int 'a'
                        let code = codes.[index]
                        s + code)
                    ""

            uniqueMorseRepresentations' t (Set.add code acc)

    uniqueMorseRepresentations' words Set.empty

// 2
uniqueMorseRepresentations [ "gin"
                             "zen"
                             "gig"
                             "msg" ]

// 1
uniqueMorseRepresentations [ "a" ]
