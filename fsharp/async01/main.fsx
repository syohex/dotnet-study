open System.IO
open System.Threading.Tasks
open System

let readlines (path: string) : Task<string[]> =
    task {
        let! lines = File.ReadAllLinesAsync(path)
        return lines
    }

let path = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
let w = readlines (Path.Join(path, ".bashrc"))
let b = w.Result
printfn "%A" b
