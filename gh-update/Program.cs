using System.Diagnostics;
using ICSharpCode.SharpZipLib.GZip;
using ICSharpCode.SharpZipLib.Tar;

if (args.Length == 0)
{
    Console.WriteLine("Usage: gh-update version");
    return;
}

var version = args[0];
var ghPath = await DownloadAndExtract(version);
SetExecutableFlag(ghPath);
GenerateZshCompletion(ghPath);

async Task<string> DownloadAndExtract(string version)
{
    var url = $"https://github.com/cli/cli/releases/download/v{version}/gh_{version}_linux_amd64.tar.gz";
    var client = new HttpClient();

    Console.WriteLine($"Download from {url}");

    var stream = await client.GetStreamAsync(url);
    var gzipStream = new GZipInputStream(stream);

    var tarArchive = new TarInputStream(gzipStream, System.Text.Encoding.UTF8);

    var dest = Path.Combine(HomeDirectory(), "bin", "gh");
    TarEntry entry;
    while ((entry = tarArchive.GetNextEntry()) != null)
    {
        if (entry.IsDirectory)
        {
            continue;
        }

        if (!entry.Name.EndsWith("bin/gh"))
        {
            continue;
        }

        using (var outStream = new FileStream(dest, FileMode.Create))
        {
            tarArchive.CopyEntryContents(outStream);
        }

        break;
    }

    tarArchive.Close();

    Console.WriteLine("success to download");

    return dest;
}

void GenerateZshCompletion(string path)
{
    var cmd = new Process();
    cmd.StartInfo.FileName = path;
    cmd.StartInfo.Arguments = "completion --shell zsh";
    cmd.StartInfo.RedirectStandardOutput = true;

    cmd.Start();

    var completion = cmd.StandardOutput.ReadToEnd();

    var dest = Path.Combine(HomeDirectory(), ".zsh", "completions", "_gh");
    using (StreamWriter f = new(dest))
    {
        f.Write(completion);
    }

    cmd.WaitForExit();

    Console.WriteLine($"Success to generate {dest}");
}

void SetExecutableFlag(string path)
{
    var cmd = new Process();
    cmd.StartInfo.FileName = "chmod";
    cmd.StartInfo.Arguments = $"+x {path}";

    cmd.Start();
    cmd.WaitForExit();
}

string HomeDirectory()
{
    string? d = Environment.GetEnvironmentVariable("HOME");
    if (d == null)
    {
        throw new Exception("Cannot get home directory");
    }

    return d;
}
