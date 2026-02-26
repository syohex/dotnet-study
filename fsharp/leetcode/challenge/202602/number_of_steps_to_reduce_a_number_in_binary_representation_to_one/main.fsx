let plus1 ns =
    let rec plus1' ns acc =
        match ns with
        | [] -> '1' :: acc |> List.rev
        | h :: t ->
            if h = '0' then
                ('1' :: acc |> List.rev) @ t
            else
                plus1' t ('0' :: acc)

    plus1' ns []

let numSteps (s: string) : int =
    let rec numSteps' ns acc =
        match ns with
        | [] -> failwith "never reach here"
        | h :: [] when h = '1' -> acc
        | h :: t ->
            let acc = acc + 1

            if h = '0' then
                numSteps' t acc
            else
                numSteps' (plus1 ns) acc

    let ns = s |> Seq.toList |> List.rev
    numSteps' ns 0

// 6
numSteps "1101"

// 1
numSteps "10"

// 0
numSteps "1"
