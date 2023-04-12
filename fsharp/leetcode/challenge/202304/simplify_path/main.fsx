let simplifyPath (path: string) : string =
    let rec simplifyPath' ps acc =
        match ps with
        | [] ->
            match acc with
            | [] -> failwith "never reach here"
            | _ :: [] -> "/"
            | _ ->
                acc |> List.rev |> String.concat "/"
        | h :: t ->
            if h = "" || h = "." then
                simplifyPath' t acc
            elif h = ".." then
                match acc with
                | [] -> failwith "never reach here"
                | _ :: [] -> simplifyPath' t acc
                | _ :: rest -> simplifyPath' t rest
            else
                simplifyPath' t (h :: acc)

    let ps = path.Split "/" |> List.ofArray
    simplifyPath' ps [""]

// "/home"
simplifyPath "/home/"

// "/"
simplifyPath "/../"

// "/home/foo"
simplifyPath "/home//foo/"

// "/a/b/c"
simplifyPath "/a//b////c/d/././././.."
