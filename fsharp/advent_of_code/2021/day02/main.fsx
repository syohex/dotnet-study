open System
open System.IO

type Command =
    | Forward of int
    | Down of int
    | Up of int

let parseInput (lines: seq<string>) : Command list =
    lines
    |> Seq.filter (fun line -> String.IsNullOrWhiteSpace(line) |> not)
    |> Seq.map (fun line ->
        let parts = line.Split(' ')
        let direction = parts.[0]
        let steps = int parts.[1]

        match direction with
        | "forward" -> Forward(steps)
        | "down" -> Down(steps)
        | "up" -> Up(steps)
        | _ -> failwith $"unknown direction: {direction}")
    |> Seq.toList

let problem1 (commands: Command list) : int =
    let rec problem1' commands (pos, depth) =
        match commands with
        | [] -> pos * depth
        | h :: t ->
            match h with
            | Forward (n) -> problem1' t (pos + n, depth)
            | Down (n) -> problem1' t (pos, depth + n)
            | Up (n) -> problem1' t (pos, depth - n)

    problem1' commands (0, 0)

let problem2 (commands: Command list) : int64 =
    let rec problem2' commands ((pos, depth, aim): (int64 * int64 * int64)) : int64 =
        match commands with
        | [] -> pos * depth
        | h :: t ->
            match h with
            | Forward (n) -> problem2' t (pos + (int64 n), depth + (aim * (int64 n)), aim)
            | Down (n) -> problem2' t (pos, depth, aim + (int64 n))
            | Up (n) -> problem2' t (pos, depth, aim - (int64 n))

    problem2' commands (0L, 0L, 0L)

let test1 () =
    let lines = File.ReadLines("test1.txt")
    let ret = lines |> parseInput |> problem1
    ret = 150

let test2 () =
    let lines = File.ReadLines("test1.txt")
    let ret = lines |> parseInput |> problem2
    ret = 900

test1 ()
test2 ()

let lines = File.ReadLines("input.txt")
let part1 = lines |> parseInput |> problem1
let part2 = lines |> parseInput |> problem2

part1 = 1868935
part2 = 1965970888L

printfn $"part1: {part1}"

printfn $"part2: {part2}"
