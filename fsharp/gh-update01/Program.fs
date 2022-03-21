open System
open System.Diagnostics
open System.IO
open System.Net.Http
open ICSharpCode.SharpZipLib.GZip
open ICSharpCode.SharpZipLib.Tar

let downloadURL (version: string) : string =
    sprintf $"https://github.com/cli/cli/releases/download/v{version}/gh_{version}_linux_amd64.tar.gz"

let homeDirectory () : string =
    let v = Environment.GetEnvironmentVariable("HOME")
    match v with
    | null -> failwith "could not get home directory path"
    | _ -> v

let rec findArchiveAndExtract (archive: TarInputStream) (pattern: string) (dest: string) =
    let entry = archive.GetNextEntry()
    match entry with
    | null -> Error($"could not find entry matching with {pattern}")
    | entry ->
        if entry.IsDirectory || (entry.Name.EndsWith(pattern) |> not) then
            findArchiveAndExtract archive pattern dest
        else
            let outStream = new FileStream(dest, FileMode.Create)
            archive.CopyEntryContents(outStream)
            Ok(())


let downloadAndExtract (version: string) =
    let url = downloadURL version

    async {
        let client = new HttpClient()
        let! stream = client.GetStreamAsync(url) |> Async.AwaitTask
        let gzipStream = new GZipInputStream(stream)

        let tarArchive = new TarInputStream(gzipStream, Text.Encoding.UTF8)
        let dest = Path.Combine(homeDirectory(), "bin", "gh")

        return match findArchiveAndExtract tarArchive "bin/gh" dest with
                | Error(v) -> Error(v)
                | Ok(_) -> Ok(dest)
    }

let setExecutableFlag (path: string) =
    let mutable cmd = new Process()
    cmd.StartInfo.FileName <- "chmod"
    cmd.StartInfo.Arguments <- $"+x {path}"

    if cmd.Start() then
        cmd.WaitForExit()
        Ok(())
    else
        Error("failed to run chmod command")

let generateZshCompletion (ghPath: string) =
    let mutable cmd = new Process()
    cmd.StartInfo.FileName <- ghPath
    cmd.StartInfo.Arguments <- "completion --shell zsh"
    cmd.StartInfo.RedirectStandardInput <- true

    if cmd.Start() then
        let dest = Path.Combine(homeDirectory(), ".zsh", "completions", "_gh")
        let f = new StreamWriter(dest)
        cmd.StandardOutput.ReadToEnd() |> f.Write
        Ok(())
    else
        Error("failed to run 'gh completion' command")


[<EntryPoint>]
let main args =
    match args.Length with
    | 0 ->
        printfn "Usage: gh-update version"
        1
    | _ ->
        let version = args.[0]
        let r = downloadAndExtract version |> Async.RunSynchronously
        match r with
        | Error(v) ->
            printfn $"error: {v}"
            1
        | Ok(ghPath) ->
            printfn "Success download and extract 'gh' binary"
            match setExecutableFlag ghPath with
            | Error(v) ->
                printfn $"error: {v}"
                1
            | Ok(_) ->
                printfn $"Success to set executable flag to {ghPath}"
                match generateZshCompletion ghPath with
                | Error(v) ->
                    printfn $"error: {v}"
                    1
                | Ok(_) ->
                    printfn "Complete!!"
                    0
