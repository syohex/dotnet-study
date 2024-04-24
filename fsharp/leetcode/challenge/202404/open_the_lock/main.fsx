let changeDigit (digits: int[]) (digit: int) (n: int) : int[] =
    let ret = Array.copy digits
    let v = (ret.[digit] + n + 10) % 10
    ret.[digit] <- v
    ret

let nextDigits digits =
    let rec nextDigits' i digits acc =
        if i >= 4 then
            acc
        else
            let a1, a2 = changeDigit digits i 1, changeDigit digits i -1
            nextDigits' (i + 1) digits (a2 :: a1 :: acc)

    nextDigits' 0 digits []

nextDigits [| 0; 0; 0; 0 |]

let openLock (deadends: string list) (target: string) : int =
    let rec openLock' q steps deadends target visited =
        match q with
        | [] -> -1
        | _ ->
            match List.tryFind ((=) target) q with
            | Some(_) -> steps
            | None ->
                let q' =
                    q
                    |> List.fold
                        (fun acc a ->
                            let nexts = nextDigits a
                            nexts @ acc)
                        []
                    |> List.filter (fun a -> (not <| Set.contains a visited) && (not <| Set.contains a deadends))

                let visited' = q' |> List.fold (fun acc a -> Set.add a acc) visited
                openLock' q' (steps + 1) deadends target visited'

    let charToInt c = int c - int '0'

    let deadends' =
        deadends |> List.map (Seq.map charToInt >> Seq.toArray) |> Set.ofList

    let target' = target |> Seq.map charToInt |> Seq.toArray
    let init = "0000" |> Seq.map charToInt |> Seq.toArray

    if Set.contains init deadends' then
        -1
    else
        let q = [ init ]
        let visited = Set.empty |> Set.add init
        openLock' q 0 deadends' target' visited

let deadends1 = [ "0201"; "0101"; "0102"; "1212"; "2002" ]
// 6
openLock deadends1 "0202"

let deadends2 = [ "8888" ]
// 1
openLock deadends2 "0009"

let deadends3 = [ "8887"; "8889"; "8878"; "8898"; "8788"; "8988"; "7888"; "9888" ]
// -1
openLock deadends3 "8888"
