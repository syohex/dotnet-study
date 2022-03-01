open System
open System.IO

// for Window
Console.OutputEncoding <- Text.Encoding.UTF8

let chooseWord () : string =
    let r = new Random()

    File.ReadAllLines("five_words")
    |> Array.sortBy (fun _ -> r.Next())
    |> Array.head

let check (input: string) (word: string) : string =
    let rec check' inputs words table acc =
        match inputs, words with
        | [], [] ->
            acc
            |> List.rev
            |> List.reduce (fun acc b -> acc + b)
        | x :: xs, y :: ys ->
            if x = y then
                check' xs ys table ("🟩" :: acc)
            elif Seq.contains x table then
                check' xs ys table ("🟨" :: acc)
            else
                check' xs ys table ("⬜" :: acc)
        | _ -> failwith "never reach here"

    let table =
        word
        |> Seq.fold (fun acc c -> Set.add c acc) Set.empty

    let inputs = input |> Seq.toList
    let words = word |> Seq.toList

    check' inputs words table []

let rec loop (word: string) (turns: int) : unit =
    let input = Console.ReadLine().Trim()

    if input.Length < 5 then
        printfn "Too short input. Please input 5 characters"
        loop word turns
    elif input.Length > 5 then
        printfn "Too long input. Please input 5 characters"
        loop word turns
    else
        let matched = check input word
        printfn "%s" matched

        if word = input then
            printfn "You win in %d turns" turns
        else
            loop word (turns + 1)

[<EntryPoint>]
let main _ =
    printfn "Input word"
    loop (chooseWord ()) 1
    0
