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

        { Start = (first.[0], first.[1])
          End = (last.[0], last.[1]) })

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


let test () = readInputFile "test.txt" |> problem1

let test1 = test ()
check "test1" test1 5

let lines = readInputFile "input.txt"
let ret1 = problem1 lines

check "part1" ret1 8622

printfn $"part1={ret1}"