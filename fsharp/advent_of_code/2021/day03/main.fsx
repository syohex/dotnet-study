open System
open System.IO

let rec problem1' (n: int) (limit: int) (reports: string list) (gamma: int) (epsilon: int) : int =
    if n = limit then
        gamma * epsilon
    else
        let zeros, ones =
            reports
            |> List.fold
                (fun (zeros, ones) report ->
                    if report.[n] = '0' then
                        zeros + 1, ones
                    else
                        zeros, ones + 1)
                (0, 0)

        if zeros > ones then
            problem1' (n + 1) limit reports (gamma * 2) (epsilon * 2 + 1)
        else
            problem1' (n + 1) limit reports (gamma * 2 + 1) (epsilon * 2)

let problem1 (reports: string list) : int =
    let len = reports.Head.Length
    problem1' 0 len reports 0 0

let test () =
    let lines =
        File.ReadLines("test1.txt")
        |> Seq.filter (fun line -> String.IsNullOrWhiteSpace line |> not)
        |> Seq.toList

    let ret = problem1 lines

    if ret <> 198 then
        failwith $"answer is {ret}: expected: 198"
    else
        printf "OK\n"

test ()

let inputs =
    File.ReadLines("input.txt")
    |> Seq.filter (fun line -> String.IsNullOrWhiteSpace line |> not)
    |> Seq.toList

let ret1 = problem1 inputs

if ret1 <> 3912944 then
    failwith $"problem1: {ret1}, expect: 3912944"

printfn $"problem1: {ret1}"
