open System
open System.IO

type Card = { Numbers: int [,]; Check: bool [,] }

type Bingo = { Input: int list; Cards: Card list }

let parseInput (lines: string list) : Bingo =
    let rec parseBingoNumbers (lines: string list) cards =
        match lines with
        | [] -> cards |> List.rev
        | _ ->
            let card =
                List.take 5 lines
                |> List.map (fun s ->
                    s.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map int
                    |> Array.toList)
                |> array2D

            let rest = List.skip 5 lines
            parseBingoNumbers rest (card :: cards)

    let input =
        lines.Head.Split(',')
        |> Array.map int
        |> Array.toList

    let lines' =
        lines.Tail
        |> List.filter (fun s -> String.IsNullOrWhiteSpace(s) |> not)
        |> List.map (fun s -> s.Trim())

    let numbers = parseBingoNumbers lines' []

    let cards =
        numbers
        |> List.map (fun n ->
            { Numbers = n
              Check = (Array2D.init 5 5 (fun _ _ -> false)) })

    { Input = input; Cards = cards }

let checkNumber (number: int) (card: Card) =
    for i in 0 .. 4 do
        for j in 0 .. 4 do
            if card.Numbers.[i, j] = number then
                card.Check.[i, j] <- true

let checkWin (card: Card) : bool =
    seq { 0 .. 4 }
    |> Seq.tryFind (fun i ->
        let row =
            card.Check.[i, *]
            |> Array.filter ((=) true)
            |> Array.length

        let col =
            card.Check.[*, i]
            |> Array.filter ((=) true)
            |> Array.length

        (row = 5) || (col = 5))
    |> Option.isSome

let unmarkedScore (card: Card) : int =
    let unmarks =
        seq {
            for i in 0 .. 4 do
                for j in 0 .. 4 do
                    if card.Check.[i, j] = false then
                        yield card.Numbers.[i, j]
        }

    unmarks |> Seq.sum

let rec problem1' (input: int list) (cards: Card list) : int =
    match input with
    | [] -> failwith "never reach here"
    | number :: rest ->
        cards |> List.iter (fun c -> checkNumber number c)

        match List.tryFind checkWin cards with
        | None -> problem1' rest cards
        | Some (c) ->
            let score = unmarkedScore c
            score * number

let rec problem2' (input: int list) (cards: Card list) : int =
    match input with
    | [] -> failwith "never reach here"
    | number :: rest ->
        cards |> List.iter (fun c -> checkNumber number c)
        let cards' = cards |> List.filter (fun c -> c |> checkWin |> not)
        match cards' with
        | [] ->
            let score = unmarkedScore (List.head cards)
            score * number
        | _ ->
            problem2' rest cards'

let problem1 (bingo: Bingo) : int = problem1' bingo.Input bingo.Cards

let problem2 (bingo: Bingo) : int = problem2' bingo.Input bingo.Cards

let test () =
    let input = File.ReadLines("test.txt") |> Seq.toList
    let bingo = parseInput input
    let ret1 = problem1 bingo

    if ret1 = 4512 then
        printfn "OK part1 test"
    else
        failwith $"Error: got: {ret1}, expected: 4512"

    let ret2 = problem2 bingo

    if ret2 = 1924 then
        printfn "OK part2 test"
    else
        failwith $"Error: got: {ret2}, expected: 1924"

test()

let input1 =
    File.ReadLines("input.txt") |> Seq.toList

let bingo1 = parseInput input1
let ret1 = problem1 bingo1

if ret1 = 10374 then
    printfn $"Part1: {ret1}"
else
    failwith $"Error Part1: got: {ret1}, expected: 10374"

let input2 =
    File.ReadLines("input.txt") |> Seq.toList

let bingo2 = parseInput input2
let ret2 = problem2 bingo2

if ret2 = 24742 then
    printfn $"Part2: {ret2}"
else
    failwith $"Error Part2: got: {ret2}, expected: 24742"
