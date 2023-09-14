let toGraphAndTicketCount (tickets: (string * string) list) : Map<string, string list> * Map<(string * string), int> =
    let rec toGraph' tickets graph ticketCount =
        match tickets with
        | [] -> graph, ticketCount
        | (departure, arrival) :: t ->
            let graph' =
                match Map.tryFind departure graph with
                | Some(v) -> Map.add departure (arrival :: v) graph
                | None -> Map.add departure [ arrival ] graph

            let ticketCount' =
                match Map.tryFind (departure, arrival) ticketCount with
                | Some(v) -> Map.add (departure, arrival) (v + 1) ticketCount
                | None -> Map.add (departure, arrival) 1 ticketCount

            toGraph' t graph' ticketCount'

    toGraph' tickets Map.empty Map.empty

let findItinerary (tickets: (string * string) list) : string list =
    let rec findItinerary' departure visited used limit graph ticketCount =
        if used = limit then
            Some(List.rev visited)
        else
            match Map.tryFind departure graph with
            | None -> None
            | Some(arrivals) ->
                arrivals
                |> List.fold
                    (fun acc arrival ->
                        match acc with
                        | Some(_) -> acc
                        | None ->
                            let key = departure, arrival

                            match Map.tryFind key ticketCount with
                            | None -> None
                            | Some(v) ->
                                if v > 0 then
                                    let ticketCount' = Map.add key (v - 1) ticketCount
                                    findItinerary' arrival (arrival :: visited) (used + 1) limit graph ticketCount'
                                else
                                    None)
                    None

    let graph, ticketCount = toGraphAndTicketCount tickets
    let graph' = Map.map (fun _ v -> List.sort v) graph
    let limit = List.length tickets

    match findItinerary' "JFK" [ "JFK" ] 0 limit graph' ticketCount with
    | Some(v) -> v
    | None -> failwith "never reach here"

let tickets1 = [ ("MUC", "LHR"); ("JFK", "MUC"); ("SFO", "SJC"); ("LHR", "SFO") ]
// ["JFK","MUC","LHR","SFO","SJC"]
findItinerary tickets1

let tickets2 =
    [ ("JFK", "SFO")
      ("JFK", "ATL")
      ("SFO", "ATL")
      ("ATL", "JFK")
      ("ATL", "SFO") ]
// ["JFK","ATL","JFK","SFO","ATL","SFO"]
findItinerary tickets2

let tickets3 =
    [ ("EZE", "AXA")
      ("TIA", "ANU")
      ("ANU", "JFK")
      ("JFK", "ANU")
      ("ANU", "EZE")
      ("TIA", "ANU")
      ("AXA", "TIA")
      ("TIA", "JFK")
      ("ANU", "TIA")
      ("JFK", "TIA") ]
// ["JFK","ANU""EZE","AXA","TIA","ANU","JFK","TIA","ANU","TIA","JFK"]
findItinerary tickets3
