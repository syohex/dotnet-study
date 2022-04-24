type UndergroundSystem =
    { mutable Check: Map<int, (string * float)>
      mutable Time: Map<(string * string), (int * float)> }

    static member init() = { Check = Map.empty; Time = Map.empty }

    member this.checkIn (id: int) (stationName: string) (t: int) =
        this.Check <- Map.add id (stationName, float t) this.Check

    member this.checkOut (id: int) (stationName: string) (t: int) : unit =
        match Map.tryFind id this.Check with
        | None -> failwith "never reach here"
        | Some ((start, startTime)) ->
            let diff = (float t) - startTime
            let key = (start, stationName)

            match Map.tryFind key this.Time with
            | None -> this.Time <- Map.add key (1, diff) this.Time
            | Some ((num, total)) -> this.Time <- Map.add key (num + 1, total + diff) this.Time


    member this.getAverageTime (startStation: string) (endStation: string) : float =
        let key = (startStation, endStation)
        let (num, total) = Map.find key this.Time
        total / (float num)

let u = UndergroundSystem.init ()
u.checkIn 45 "Leyton" 3
u.checkIn 32 "Paradise" 8
u.checkIn 27 "Leyton" 10
u.checkOut 45 "Waterloo" 15
u.checkOut 27 "Waterloo" 20
u.checkOut 32 "Cambridge" 22
// 14.0
u.getAverageTime "Paradise" "Cambridge"
// 11.0
u.getAverageTime "Leyton" "Waterloo"
u.checkIn 10 "Leyton" 24
// 11
u.getAverageTime "Leyton" "Waterloo"
u.checkOut 10 "Waterloo" 38
// 12
u.getAverageTime "Leyton" "Waterloo"

let u2 = UndergroundSystem.init ()
u2.checkIn 10 "Leyton" 3
u2.checkOut 10 "Paradise" 8
// 5
u2.getAverageTime "Leyton" "Paradise"
u2.checkIn 5 "Leyton" 10
u2.checkOut 5 "Paradise" 16
// 5.5
u2.getAverageTime "Leyton" "Paradise"
u2.checkIn 2 "Leyton" 21
u2.checkOut 2 "Paradise" 30
// 6.66667
u2.getAverageTime "Leyton" "Paradise"
