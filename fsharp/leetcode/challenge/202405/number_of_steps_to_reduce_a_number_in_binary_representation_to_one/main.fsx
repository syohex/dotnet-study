let addOne (cs: char list) : char list =
    let rec addOne' cs acc =
        match cs with
        | [] -> List.rev ('1' :: acc)
        | h :: t ->
            if h = '0' then
                (List.rev ('1' :: acc)) @ t
            else
                addOne' t ('0' :: acc)

    addOne' (List.tail cs) [ '0' ]

let numSteps (s: string) : int =
    let rec numSteps' cs ret =
        match cs with
        | [] -> failwith "never reach here"
        | [ '1' ] -> ret
        | h :: t ->
            if h = '0' then
                numSteps' t (ret + 1)
            else
                let cs' = addOne cs
                numSteps' cs' (ret + 1)

    let cs = s |> Seq.skipWhile (fun c -> c = '0') |> Seq.rev |> Seq.toList
    numSteps' cs 0

// 6
numSteps "1101"

// 1
numSteps "10"

// 0
numSteps "1"
