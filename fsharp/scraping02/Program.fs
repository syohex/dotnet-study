open FSharp.Data

let getTitle (url: string) : string =
    let doc = HtmlDocument.Load(url)
    let title = doc.CssSelect("head > title")
    title.[0].InnerText()

[<EntryPoint>]
let main args =
    let title = getTitle args.[0]
    printfn $"title: {title}"
    0
