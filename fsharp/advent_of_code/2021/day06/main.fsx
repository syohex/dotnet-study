open System
open System.IO

let readInput (file: string) : int list =
    File.ReadLines(file)
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.fold
        (fun acc (s: string) ->
            let nums =
                s.Split(',') |> Array.map int |> Array.toList

            nums @ acc)
        []

let problem1 (nums: int list) (limit: int) : int64 =
    let rec problem1' i limit (fishes: Map<int, int64>) =
        if i = limit then
            fishes |> Map.fold (fun acc _ v -> acc + v) 0L
        else
            let fishes' =
                fishes
                |> Map.fold
                    (fun acc k v ->
                        if k = 0 then
                            let sixes = Map.find 6 acc
                            let eights = Map.find 8 acc

                            acc
                            |> Map.add 0 0L
                            |> Map.add 6 (sixes + v)
                            |> Map.add 8 (eights + v)
                        else
                            let current = Map.find k acc
                            let previous = Map.find (k - 1) acc

                            acc
                            |> Map.add k (current - v)
                            |> Map.add (k - 1) (previous + v))
                    fishes

            problem1' (i + 1) limit fishes'

    let fishes =
        seq { 0 .. 8 }
        |> Seq.fold (fun acc n -> Map.add n 0L acc) Map.empty

    let fishes' =
        nums
        |> List.fold (fun acc n -> Map.add n ((Map.find n acc) + 1L) acc) fishes

    problem1' 0 limit fishes'

let testNums = readInput "test.txt"
// 18
problem1 testNums 18

// 5934
problem1 testNums 80

// 26984457539
problem1 testNums 256

let input = readInput "input.txt"
// 373378
let ret1 = problem1 input 80
if ret1 <> 373378 then
    failwith $"Part1 is wrong: got: {ret1} expected: 373378"

// 1682576647495
let ret2 = problem1 input 256
if ret2 <> 1682576647495L then
    failwith $"Part2 is wrong: got: {ret2} expected: 1682576647495"

printfn $"Part1={ret1}"
printfn $"Part2={ret2}"
