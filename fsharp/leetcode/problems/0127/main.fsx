#r "nuget: Fsharpx.Collections"

open FSharpx.Collections

let strToCharArray (s: string) : char array = s |> Seq.toArray

let oneDistancePattern (s: string) : char [] list =
    let rec oneDistancePattern' (s: char []) i acc =
        if i >= (Array.length s) then
            acc |> List.rev
        else
            let pattern = Array.copy s
            pattern.[i] <- '?'
            oneDistancePattern' s (i + 1) (pattern :: acc)

    oneDistancePattern' (s |> strToCharArray) 0 []

let makeGraph (wordList: string list) : Map<char [], Set<string>> =
    let rec makeGraph' wordList m =
        match wordList with
        | [] -> m
        | head :: tail ->
            let patterns = oneDistancePattern head

            let newGraph =
                patterns
                |> List.fold
                    (fun m p ->
                        match Map.tryFind p m with
                        | None -> Map.add p (Set.empty |> Set.add head) m
                        | Some s -> Map.add p (Set.add head s) m)
                    m

            makeGraph' tail newGraph

    makeGraph' wordList Map.empty

let ladderLength (beginWord: string) (endWord: string) (wordList: string list) : int =
    let rec ladderLength' endWord graph (queue: IPriorityQueue<(int * string)>) (visited: Set<string>) : int =
        match PriorityQueue.tryPop queue with
        | None -> 0
        | Some ((count, next), restQ) ->
            if next = endWord then
                count
            else
                let patterns = oneDistancePattern next

                let nexts =
                    patterns
                    |> List.choose (fun p -> Map.tryFind p graph)
                    |> List.fold (fun acc s -> s |> Set.fold (fun a cand -> Set.add cand a) acc) Set.empty

                let notVisited =
                    nexts
                    |> Set.filter (fun n -> Set.contains n visited |> not)

                let updatedQ =
                    notVisited
                    |> Set.fold (fun q next -> PriorityQueue.insert (count + 1, next) q) restQ

                let updatedVisited =
                    notVisited
                    |> Set.fold (fun s next -> Set.add next s) visited

                ladderLength' endWord graph updatedQ updatedVisited

    let graph = makeGraph wordList

    let queue =
        PriorityQueue.empty false
        |> PriorityQueue.insert (1, beginWord)

    let visited = Set.empty |> Set.add beginWord

    ladderLength' endWord graph queue visited


// 5
let wordList1 =
    [ "hot"
      "dot"
      "dog"
      "lot"
      "log"
      "cog" ]

ladderLength "hit" "cog" wordList1

// 2
let wordList2 = [ "hot"; "dot"; "dog"; "lot"; "log" ]

ladderLength "hit" "cog" wordList2

// 5
let wordList3 =
    [ "si"
      "go"
      "se"
      "cm"
      "so"
      "ph"
      "mt"
      "db"
      "mb"
      "sb"
      "kr"
      "ln"
      "tm"
      "le"
      "av"
      "sm"
      "ar"
      "ci"
      "ca"
      "br"
      "ti"
      "ba"
      "to"
      "ra"
      "fa"
      "yo"
      "ow"
      "sn"
      "ya"
      "cr"
      "po"
      "fe"
      "ho"
      "ma"
      "re"
      "or"
      "rn"
      "au"
      "ur"
      "rh"
      "sr"
      "tc"
      "lt"
      "lo"
      "as"
      "fr"
      "nb"
      "yb"
      "if"
      "pb"
      "ge"
      "th"
      "pm"
      "rb"
      "sh"
      "co"
      "ga"
      "li"
      "ha"
      "hz"
      "no"
      "bi"
      "di"
      "hi"
      "qa"
      "pi"
      "os"
      "uh"
      "wm"
      "an"
      "me"
      "mo"
      "na"
      "la"
      "st"
      "er"
      "sc"
      "ne"
      "mn"
      "mi"
      "am"
      "ex"
      "pt"
      "io"
      "be"
      "fm"
      "ta"
      "tb"
      "ni"
      "mr"
      "pa"
      "he"
      "lr"
      "sq"
      "ye" ]

ladderLength "qa" "sq" wordList3
