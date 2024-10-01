let canArrange (arr: int list) (k: int) : bool =
    let rec toModMap arr k acc =
        match arr with
        | [] -> acc
        | h :: t ->
            let modulo = ((h % k) + k) % k // avoid negative mod
            let v = Map.tryFind modulo acc |> Option.defaultValue 0
            toModMap t k (Map.add modulo (v + 1) acc)

    let rec canArrange' i limit k m =
        if i > limit then
            true
        else
            let v1 = Map.tryFind i m |> Option.defaultValue 0
            let v2 = Map.tryFind (k - i) m |> Option.defaultValue 0
            if v1 <> v2 then false else canArrange' (i + 1) limit k m

    let modMap = toModMap arr k Map.empty
    let v0 = Map.tryFind 0 modMap |> Option.defaultValue 0

    if v0 % 2 <> 0 then
        false
    else
        let limit = k / 2
        canArrange' 1 limit k modMap

// true
canArrange [ 1; 2; 3; 4; 5; 10; 6; 7; 8; 9 ] 5

// true
canArrange [ 1; 2; 3; 4; 5; 6 ] 7

// false
canArrange [ 1; 2; 3; 4; 5; 6 ] 10
