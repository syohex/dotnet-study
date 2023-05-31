type UnderGroundSystem =
    {
        checkIns : Map<int, (string * int)>
        times: Map<(string * string), (int * int)>
    }

    static member init() : UnderGroundSystem =
        {
            checkIns = Map.empty; times = Map.empty
        }

    static member checkIn (id: int) (stationName: string) (t: int) (u: UnderGroundSystem) : UnderGroundSystem =
        let checkIns' = Map.add id (stationName, t) u.checkIns
        { u with checkIns = checkIns'}

    static member checkOut(id :int) (stationName: string) (t: int) (u: UnderGroundSystem) : UnderGroundSystem =
        match Map.tryFind id u.checkIns with
        | None -> failwith "never reach here"
        | Some((startStation, startTime)) ->
            let checkIns' = Map.remove id u.checkIns
            let key = (startStation, stationName)
            let diff = t - startTime

            let times = match Map.tryFind key u.times with
                        | Some((numbers, total)) ->
                            Map.add key (numbers + 1, total + diff) u.times
                        | None ->
                            Map.add key (1, diff) u.times

            { checkIns = checkIns'; times = times }

    static member getAverageTime(startStation: string) (endStation: string) (u: UnderGroundSystem) : double =
        match Map.tryFind (startStation, endStation) u.times with
        | Some((count, total)) ->
            (double total) / (double count)
        | None ->
            match Map.tryFind (endStation, startStation) u.times with
            | Some((count, total)) ->
                (double total) / (double count)
            | None ->
                failwith "never reach here"
 
let u = UnderGroundSystem.init()
let u1 = UnderGroundSystem.checkIn 45 "Leyton" 3 u
let u2 = UnderGroundSystem.checkIn 32 "Paradise" 8 u1
let u3 = UnderGroundSystem.checkIn 27 "Leyton" 10 u2
let u4 = UnderGroundSystem.checkOut 45 "Waterloo" 15 u3
let u5 = UnderGroundSystem.checkOut 27 "Waterloo" 20 u4
let u6 = UnderGroundSystem.checkOut 32 "Cambridge" 22 u5

// 14.0
UnderGroundSystem.getAverageTime "Paradise" "Cambridge" u6
// 11.0
UnderGroundSystem.getAverageTime "Leyton" "Waterloo" u6

let u7 = UnderGroundSystem.checkIn 10 "Leyton" 24 u6

// 11.0
UnderGroundSystem.getAverageTime "Leyton" "Waterloo" u7

let u8 = UnderGroundSystem.checkOut 10 "Waterloo" 38 u7

// 12.0
UnderGroundSystem.getAverageTime "Leyton" "Waterloo" u8
