open System
open System.IO

type Line =
    { Start: (int * int)
      End: (int * int) }

let parseInput (input: string list) =
    input
    |> List.map (fun s ->
        let parts = s.Split(" -> ")
        let first = parts.[0].Split(',') |> Array.map int
        let last = parts.[1].Split(',') |> Array.map int

        { Start = (first.[1], first.[0])
          End = (last.[1], last.[0]) })

let calcArea (lines: Line list) : int [,] =
    let maxX, maxY =
        lines
        |> List.fold
            (fun (maxX, maxY) { Start = (x1, y1); End = (x2, y2) } ->
                let maxX' = Math.Max(maxX, Math.Max(x1, x2))
                let maxY' = Math.Max(maxY, Math.Max(y1, y2))
                maxX', maxY')
            (Int32.MinValue, Int32.MinValue)

    Array2D.init (maxX + 1) (maxY + 1) (fun _ _ -> 0)

let problem1 (lines: Line list) : int =
    let rect = calcArea lines

    lines
    |> List.filter (fun { Start = (x1, y1); End = (x2, y2) } -> (x1 = x2) || (y1 = y2))
    |> List.iter (fun { Start = (x1, y1); End = (x2, y2) } ->
        if x1 = x2 then
            let start = Math.Min(y1, y2)
            let last = Math.Max(y1, y2)

            for i in start .. last do
                rect.[x1, i] <- rect.[x1, i] + 1
        else
            let start = Math.Min(x1, x2)
            let last = Math.Max(x1, x2)

            for i in start .. last do
                rect.[i, y1] <- rect.[i, y1] + 1)

    rect
    |> Seq.cast<int>
    |> Seq.filter (fun n -> n >= 2)
    |> Seq.length

let problem2 (lines: Line list) : int =
    let rect = calcArea lines

    lines
    |> List.iter (fun { Start = (x1, y1); End = (x2, y2) } ->
        if x1 = x2 then
            let start = Math.Min(y1, y2)
            let last = Math.Max(y1, y2)

            for i in start .. last do
                rect.[x1, i] <- rect.[x1, i] + 1
        elif y1 = y2 then
            let start = Math.Min(x1, x2)
            let last = Math.Max(x1, x2)

            for i in start .. last do
                rect.[i, y1] <- rect.[i, y1] + 1
        else
            let xDiff = if x1 < x2 then 1 else -1
            let yDiff = if y1 < y2 then 1 else -1
            let limit = Math.Abs(x1 - x2)

            for i in 0 .. limit do
                let x = x1 + (xDiff * i)
                let y = y1 + (yDiff * i)
                rect.[x, y] <- rect.[x, y] + 1)

    rect
    |> Seq.cast<int>
    |> Seq.filter (fun n -> n >= 2)
    |> Seq.length

let readInputFile (path: string) : Line list =
    File.ReadLines(path)
    |> Seq.filter (String.IsNullOrEmpty >> not)
    |> Seq.toList
    |> parseInput

let check (title: string) (got: int) (expected: int) =
    if got = expected then
        printfn $"OK: {title}"
    else
        failwith $"Failed {title}: got: {got}, expected: {expected}"

let test1 () = readInputFile "test.txt" |> problem1
let test2 () = readInputFile "test.txt" |> problem2

let t1 = test1 ()
let t2 = test2 ()
check "test1" t1 5
check "test2" t2 12

let lines = readInputFile "input.txt"
let ret1 = problem1 lines
let ret2 = problem2 lines

check "part1" ret1 8622
check "part2" ret2 22037

printfn $"part1={ret1}"
printfn $"part2={ret2}"
