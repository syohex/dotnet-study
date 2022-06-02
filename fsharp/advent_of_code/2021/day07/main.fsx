open System
open System.IO

let parseInput (file: string) : int list =
    file
    |> File.ReadLines
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.fold
        (fun acc s ->
            let nums =
                s.Split(',') |> Array.map int |> Array.toList

            (nums |> List.rev) @ acc)
        []
    |> List.rev

let part1 (nums: int list) : int =
    let rec part1' pos max (nums: int list) ret =
        if pos > max then
            ret
        else
            let ret' =
                nums
                |> List.fold (fun acc n -> acc + Math.Abs(n - pos)) 0

            part1' (pos + 1) max nums (Math.Min(ret, ret'))

    let min, max = nums |> List.min, nums |> List.max
    part1' min max nums Int32.MaxValue

let test = parseInput "test.txt"
// 37
part1 test

let input = parseInput "input.txt"
// 359648
part1 input
