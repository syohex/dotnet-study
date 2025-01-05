open System

let shiftingLetters (s: string) (shifts: (int * int * int) list) : string =
    let rec shiftingLetters' cs mergedShifts shiftSize (acc: char list) =
        match cs, mergedShifts with
        | [], [] -> acc |> List.rev |> String.Concat
        | _, []
        | [], _ -> failwith "never reach here"
        | h1 :: t1, h2 :: t2 ->
            let shiftSize = shiftSize + h2
            let c = (int h1 - int 'a' + shiftSize) % 26
            let c = if c < 0 then c + 26 else c
            let c = char <| (c + int 'a')
            shiftingLetters' t1 t2 shiftSize (c :: acc)

    let mergedShifts =
        shifts
        |> List.fold
            (fun (acc: int[]) (from, to_, d) ->
                if d = 1 then
                    acc.[from] <- acc.[from] + 1

                    if to_ + 1 < s.Length then
                        acc.[to_ + 1] <- acc.[to_ + 1] - 1
                else
                    acc.[from] <- acc.[from] - 1

                    if to_ + 1 < s.Length then
                        acc.[to_ + 1] <- acc.[to_ + 1] + 1

                acc)
            (Array.zeroCreate s.Length)
        |> Array.toList

    shiftingLetters' (Seq.toList s) mergedShifts 0 []

// "ace"
shiftingLetters "abc" [ (0, 1, 0); (1, 2, 1); (0, 2, 1) ]

// "catz"
shiftingLetters "dztz" [ (0, 0, 0); (1, 1, 1) ]
