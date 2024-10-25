type Trie =
    { IsTerminated: bool
      Table: Map<string, Trie> }

    static member Empty =
        { IsTerminated = false
          Table = Map.empty }

    static member Insert (dirs: string list) (trie: Trie) : Trie =
        match dirs with
        | [] -> { trie with IsTerminated = true }
        | h :: t ->
            let next = Map.tryFind h trie.Table |> Option.defaultValue Trie.Empty

            { trie with
                Table = Map.add h (Trie.Insert t next) trie.Table }

    static member IsSubfolder (dirs: string list) (trie: Trie) : bool =
        match dirs with
        | [] -> false
        | h :: t ->
            if trie.IsTerminated then
                true
            else
                Trie.IsSubfolder t (Map.find h trie.Table)

let toDirectories (path: string) : string list =
    path.Split('/') |> Array.filter ((<>) "") |> Array.toList

let removeSubfolders (folder: string list) : string list =
    let dirsList = folder |> List.map toDirectories
    let trie = dirsList |> List.fold (fun acc dirs -> Trie.Insert dirs acc) Trie.Empty

    List.zip dirsList folder
    |> List.filter (fun (dirs, _) -> not <| Trie.IsSubfolder dirs trie)
    |> List.map snd

// ["/a","/c/d","/c/f"]
removeSubfolders [ "/a"; "/a/b"; "/c/d"; "/c/d/e"; "/c/f" ]

// ["/a"]
removeSubfolders [ "/a"; "/a/b/c"; "/a/b/d" ]

// ["/a/b/c","/a/b/ca","/a/b/d"]
removeSubfolders [ "/a/b/c"; "/a/b/ca"; "/a/b/d" ]
