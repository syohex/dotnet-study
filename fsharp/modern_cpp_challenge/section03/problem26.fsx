let joinStrings (strs: string list) (sep: char) : string = System.String.Join(sep, strs)

joinStrings [ "this"; "is"; "a"; "pen" ] ' '
joinStrings [ "this"; "is"; "a"; "pen" ] '_'
