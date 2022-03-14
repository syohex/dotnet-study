open System

let simplyfyPath (path: string) : string =
    let rec simplyfyPath' parts (acc: string list) =
        match parts with
        | [] -> sprintf "/%s" (String.Join("/", (acc |> List.rev)))
        | head :: tail ->
            match head with
            | "."
            | "" -> simplyfyPath' tail acc
            | ".." ->
                match acc with
                | [] -> simplyfyPath' tail acc
                | _ :: tail2 -> simplyfyPath' tail tail2
            | _ -> simplyfyPath' tail (head :: acc)

    let parts = path.Split '/' |> Array.toList
    simplyfyPath' parts []

// "/home"
simplyfyPath "/home/"

// "/"
simplyfyPath "/../"

// "/home/foo"
simplyfyPath "/home//foo/"

// "/a/b/c"
simplyfyPath "/a//b////c/d//././/.."
