type TimeMap =
    { table: Map<string, Map<int, string>> }

    static member empty() : TimeMap = { table = Map.empty }

    static member set (key: string) (value: string) (timestamp: int) (tm: TimeMap) : TimeMap =
        match Map.tryFind key tm.table with
        | None -> { table = Map.add key (Map.add timestamp value Map.empty) tm.table }
        | Some (m) -> { table = Map.add key (Map.add timestamp value m) tm.table }

    static member get (key: string) (timestamp: int) (tm: TimeMap) : string =
        match Map.tryFind key tm.table with
        | None -> ""
        | Some (m) ->
            match Map.tryFind timestamp m with
            | Some (v) -> v
            | None ->
                m
                |> Map.fold
                    (fun (ret, max) k v ->
                        if k <= timestamp && k > max then
                            v, k
                        else
                            ret, max)
                    ("", System.Int32.MinValue)
                |> fst


let tm0 = TimeMap.empty ()
let tm1 = TimeMap.set "foo" "bar" 1 tm0
// "bar"
TimeMap.get "foo" 1 tm1
// "bar"
TimeMap.get "foo" 1 tm1

let tm2 = TimeMap.set "foo" "bar2" 4 tm1
// "bar2"
TimeMap.get "foo" 4 tm2
// "bar2"
TimeMap.get "foo" 5 tm2
