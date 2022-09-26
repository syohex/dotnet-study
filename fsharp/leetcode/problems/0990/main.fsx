let equationsPossible (equations: string list) : bool =
    let rec createGraph (equations: string list) (graph: Set<int> []) =
        match equations with
        | [] -> graph
        | h :: t ->
            if h.[1] = '=' then
                let a = int h.[0] - int 'a'
                let b = int h.[3] - int 'a'

                graph.[a] <- Set.add b graph.[a]
                graph.[b] <- Set.add a graph.[b]

                createGraph t graph
            else
                createGraph t graph

    let rec colorGraph node color (graph: Set<int> []) (colors: int []) =
        if colors.[node] <> -1 then
            colors
        else
            colors.[node] <- color

            graph.[node]
            |> Set.fold (fun acc next -> colorGraph next color graph acc) colors

    let rec equationsPossible' (equations: string list) (colors: int []) =
        match equations with
        | [] -> true
        | h :: t ->
            if h.[1] = '!' then
                let a = int h.[0] - int 'a'
                let b = int h.[3] - int 'a'

                if colors.[a] = colors.[b] then
                    false
                else
                    equationsPossible' t colors
            else
                equationsPossible' t colors


    let graph = createGraph equations (Array.init 26 (fun _ -> Set.empty))

    let colors =
        seq { 0..25 }
        |> Seq.fold (fun acc i -> colorGraph i i graph acc) (Array.zeroCreate 26)

    equationsPossible' equations colors

// false
equationsPossible [ "a==b"; "b!=a" ]

// true
equationsPossible [ "a==b"; "b==a" ]


// false
equationsPossible [ "a==b"
                    "b!=c"
                    "c==a" ]
