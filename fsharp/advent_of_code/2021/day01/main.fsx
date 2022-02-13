open System.IO

let readInput (file: string) : int list =
    File.ReadAllLines(file)
    |> Array.map int
    |> Array.toList

let pairs (nums: int list) : (int * int) list =
    let n1 =
        nums |> List.rev |> List.tail |> List.rev

    let n2 = nums |> List.tail

    List.zip n1 n2

let problem1 (nums: int list) : int =
    nums
    |> pairs
    |> List.filter (fun (a, b) -> b > a)
    |> List.length

let threeSumPairs (nums: int list) : int list =
    let n1 =
        nums |> List.rev |> List.skip 2 |> List.rev

    let n2 =
        nums
        |> List.rev
        |> List.tail
        |> List.rev
        |> List.tail

    let n3 = nums |> List.skip 2

    List.zip3 n1 n2 n3
    |> List.map (fun (a, b, c) -> a + b + c)

let problem2 (nums: int list) : int = nums |> threeSumPairs |> problem1

let testData =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]

// 7
problem1 testData

// 5
problem2 testData

let input = readInput "input.txt"
// 1448
problem1 input

// 1471
problem2 input
