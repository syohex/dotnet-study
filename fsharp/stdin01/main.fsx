open System

let rec cat () =
    let line = Console.ReadLine()

    if line = null then
        ()
    else
        printfn $"{line}"
        cat ()

cat ()
