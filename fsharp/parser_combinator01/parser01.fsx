open System

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

let run (parser: Parser<'T>) (input: string) : ParseResult<'T * string> =
    let (Parser innerFn) = parser
    innerFn input

// combine two parsers as "A andThen B"
let andThen<'a, 'b> (parser1: Parser<'a>) (parser2: Parser<'b>) : Parser<'a * 'b> =
    let innerFn (input: string) : ParseResult<('a * 'b) * string> =
        let result1 = run parser1 input

        match result1 with
        | Failure err -> Failure err
        | Success(value1, remaining1) ->
            let result2 = run parser2 remaining1

            match result2 with
            | Failure err -> Failure err
            | Success(value2, remaining2) -> Success((value1, value2), remaining2)

    Parser innerFn

let (.>>.) = andThen

// combine two parsers as "A orElse B"
let orElse parser1 parser2 =
    let innerFn (input: string) =
        let result1 = run parser1 input

        match result1 with
        | Success _ -> result1
        | Failure _ ->
            let result2 = run parser2 input
            result2

    Parser innerFn

let (<|>) = orElse

let choice parsers = List.reduce (<|>) parsers

// parser a single character
let pchar (charToMatch: char) : Parser<char> =
    let innerFn (str: string) : ParseResult<char * string> =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]

            if first = charToMatch then
                let remaining = str.[1..]
                Success(charToMatch, remaining)
            else
                let msg = $"Expecting '{charToMatch}'. Got '{first}'"
                Failure msg

    Parser innerFn

// Choose any of a list of characters
let anyOf chars = chars |> List.map pchar |> choice

run (anyOf ['a'..'z']) "hoge"
