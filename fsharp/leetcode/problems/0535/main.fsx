open System

type Solution =
    { mutable Cache: Map<string, string> }

    static member init() = { Cache = Map.empty }

    member this.encode(longUrl: string) : string =
        let rec randomStr len (rand: Random) (table: char []) (acc: char list) =
            if len = 0 then
                let s = sprintf "http://tinyurl.com/%s" (acc |> String.Concat)

                match Map.tryFind s this.Cache with
                | None -> s
                | Some (_) -> randomStr 8 rand table []
            else
                let idx = rand.Next(0, table.Length)
                randomStr (len - 1) rand table (table.[idx] :: acc)

        let table =
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
            |> Seq.toArray

        let tiny = randomStr 8 (new Random()) table []
        this.Cache <- Map.add tiny longUrl this.Cache
        tiny

    member this.decode(shortUrl: string) : string =
        match Map.tryFind shortUrl this.Cache with
        | Some (orig) -> orig
        | None -> failwith "never reach here"

let s = Solution.init()
let tiny = s.encode "https://leetcode.com/problems/design-tinyurl"
s.decode tiny
