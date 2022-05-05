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

let rec problem2' (n: int) (isOxygen: bool) (reports: string list) : int =
    if (reports |> List.tail |> List.isEmpty) then
        reports
        |> List.head
        |> Seq.fold (fun acc c -> acc * 2 + (if c = '1' then 1 else 0)) 0
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

        if isOxygen then
            if zeros > ones then
                let reports' =
                    reports |> List.filter (fun s -> s.[n] = '0')

                problem2' (n + 1) isOxygen reports'
            else
                let reports' =
                    reports |> List.filter (fun s -> s.[n] = '1')

                problem2' (n + 1) isOxygen reports'
        else if zeros <= ones then
            let reports' =
                reports |> List.filter (fun s -> s.[n] = '0')

            problem2' (n + 1) isOxygen reports'
        else
            let reports' =
                reports |> List.filter (fun s -> s.[n] = '1')

            problem2' (n + 1) isOxygen reports'

let problem2 (reports: string list) : int =
    let oxygen = problem2' 0 true reports
    let co2 = problem2' 0 false reports
    oxygen * co2

let test () =
    let lines =
        File.ReadLines("test1.txt")
        |> Seq.filter (fun line -> String.IsNullOrWhiteSpace line |> not)
        |> Seq.toList

    let ret1 = problem1 lines

    if ret1 <> 198 then
        failwith $"answer1 is {ret1}: expected: 198"
    else
        printf "OK1\n"

    let ret2 = problem2 lines

    if ret2 <> 230 then
        failwith $"answer2 is {ret2}: expected: 230"
    else
        printf "OK2\n"

test ()

let inputs =
    File.ReadLines("input.txt")
    |> Seq.filter (fun line -> String.IsNullOrWhiteSpace line |> not)
    |> Seq.toList

let ret1 = problem1 inputs
let ret2 = problem2 inputs

if ret1 <> 3912944 then
    failwith $"problem1: {ret1}, expect: 3912944"

if ret2 <> 4996233 then
    failwith $"problem2: {ret2}, expect: 4996233"

printfn $"problem1: {ret1}"
printfn $"problem2: {ret2}"
