open System.IO
open System.Diagnostics

let cmd = new Process()
cmd.StartInfo.FileName <- "ls"
cmd.StartInfo.Arguments <- "-l /usr/bin"
cmd.StartInfo.UseShellExecute <- false
cmd.StartInfo.RedirectStandardOutput <- true

if cmd.Start() |> not then
    failwith "Failed to execute command"

using (new StreamWriter("tmp.txt")) (fun writer ->
    cmd.OutputDataReceived.Add(fun e -> writer.WriteLine(e.Data))
    cmd.BeginOutputReadLine()
    cmd.WaitForExit())
