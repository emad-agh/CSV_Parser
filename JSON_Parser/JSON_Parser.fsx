#load "ParserLibrary.fsx"

open System
open ParserLibrary

type JValue =
    |JString of string
    |JUnquotedString of string
    |JNumber  of float 
    |JBool   of bool
    |JNull
    |JObject  of Map<string, JValue>
    |JEmptyObject of string
    |JArray  of JValue list

// ======================================
// forward reference
// ======================================
let createParserForwardedToRef<'a>() =

    let dummyParser= 
        let innerFn input : Result<'a * Input> = failwith "unfixed forwarded parser"
        {parseFn=innerFn; label="unknown"}
    
    // ref to placeholder Parser
    let parserRef = ref dummyParser 

    // wrapper Parser
    let innerFn input = 
        // forward input to the placeholder
        runOnInput !parserRef input 
    let wrapperParser = {parseFn=innerFn; label="unknown"}

    wrapperParser, parserRef

let jValue,jValueRef = createParserForwardedToRef<JValue>()

// ======================================
// Utility function
// ======================================
let (>>%) p x =
    p 
    |>> (fun _ -> x)

// ======================================
// Parsing a JNull
// ======================================  
let jNull =
    pstring "null"
    >>% JNull
    <?> "null"

// ======================================
// parsing a  bool
// ======================================
let jBool =   
    let jtrue = 
        pstring "true" <|> pstring "True"
        >>% JBool true
    let jfalse = 
        pstring "false" <|> pstring "False"
        >>% JBool false 

    jtrue <|> jfalse
    <?> "bool"   

// ======================================
// parsing a  string
// ======================================
let jUnescapedChar = 
    satisfy (fun ch -> ch <> '\\' && ch <> '\"') "char"

/// Parse an escaped char
let jEscapedChar = 
    [ 
    // (stringToMatch, resultChar)
    ("\\\"",'\"')      // quote
    ("\\\\",'\\')      // reverse solidus 
    ("\\/",'/')        // solidus
    ("\\b",'\b')       // backspace
    ("\\f",'\f')       // formfeed
    ("\\n",'\n')       // newline
    ("\\r",'\r')       // cr
    ("\\t",'\t')       // tab
    ] 
    // convert each pair into a parser
    |> List.map (fun (toMatch,result) -> 
        pstring toMatch >>% result)
    // and combine them into one
    |> choice

/// Parse a unicode char
let jUnicodeChar = 
    
    // set up the "primitive" parsers  
    let backslash = pchar '\\'
    let uChar = pchar 'u'
    let hexdigit = anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])

    // convert the parser output (nested tuples)
    // to a char
    let convertToChar (((h1,h2),h3),h4) = 
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char

    // set up the main parser
    backslash  >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit
    |>> convertToChar 


/// Parse a quoted string
let quotedString = 
    let quote = pchar '\"' <?> "quote"
    let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar 

    // set up the main parser
    quote >>. manyChars jchar .>> quote 

let doublequotedString = 
    let quote = pstring "\"\"" <?> "quotes"
    let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar 

    // set up the main parser
    quote >>. manyChars jchar .>> quote 

/// Parse a JString
let jString = 
    // wrap the string in a JString
    doublequotedString  <|> quotedString
    |>> JString           // convert to JString
    <?> "quoted string"   // add label
    
// ======================================
// parsing a  number
// ======================================

let jNumber = 
    let optSign = 
        opt (pchar '-')

    let zero = 
        pstring "0"

    let point = 
        pchar '.'

    let digitOneNine =
        satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "digit 1-9"

    let digit =
        satisfy (Char.IsDigit) "digit"

    let e =
        pchar 'e' <|> pchar 'e'

    let optPlusMinus =
        opt (pchar '-' <|> pchar '+')

    let nonZeroInt = 
        digitOneNine .>>. manyChars digit 
        |>> fun (first,rest) -> string first + rest

    let intPart = 
        zero <|> nonZeroInt

    let fractionPart = 
        point >>. manyChars1 digit

    let exponentPart = 
        e >>. optPlusMinus .>>. manyChars1 digit

    let ( |>? ) opt f = 
            match opt with
            | None -> ""
            | Some x -> f x

    let convertToJNumber (((optSign,intPart),fractionPart),expPart) = 
        let signStr = 
                optSign 
                |>? string

        let fractionPartStr = 
            fractionPart 
            |>? (fun digits -> "." + digits )

        let expPartStr = 
            expPart 
            |>? fun (optSign, digits) ->
                let sign = optSign |>? string
                "e" + sign + digits

        (signStr + intPart + fractionPartStr + expPartStr)
        |> float
        |> JNumber

    optSign .>>. intPart .>>. opt fractionPart .>>. opt exponentPart
    |>> convertToJNumber
    <?> "number"
//let jNumber_ = jNumber .>> spaces1

// ======================================
// parsing a  array
// ======================================
let jArray =
    let left = 
        pchar '[' .>> spaces

    let right = 
        pchar ']' .>> spaces
    
    let comma =
        pchar ',' .>> spaces
    
    let value =
        jValue .>> spaces
    
    let values = 
        sepBy1 value comma 

    between left values right 
    |>> JArray
    <?> "array"

// ======================================
// parsing a  object
// ======================================
let jObject =

    let left = 
        pchar '{' .>> spaces
    
    let right = 
        pchar '}' .>> spaces
    
    let colon = 
        pchar ':' .>> spaces
    
    let comma = 
        pchar ',' .>> spaces
    
    (* let key = 
        quotedString .>> spaces  *)
    let key = 
        let jChar = satisfy (fun ch -> ch <> ':') "label"
        manyChars jChar .>> spaces

    let value = 
        jValue .>> spaces

    let keyValue =
        (key .>> colon) .>>. value
    
    let keyValues =
        sepBy1 keyValue comma
    
    between left keyValues right
    |>> Map.ofList
    |>> JObject
    <?> "object"

// ======================================
// parsing UnquotedString
// ======================================

let chars =
    let label = "char"
    satisfy (fun ch -> ch <> ',' && ch <> '{' && ch <> '}' && ch <> '[' && ch <> ']' && not (Char.IsDigit ch)) label
    //satisfy (fun ch -> ch <> 's') label

let numberChar =
    let label = "number-char"
    satisfy (Char.IsDigit) label
let unqouted =
    manyChars numberChar .>>. manyChars1 chars .>>. manyChars numberChar
    |>> (fun ((s,s1),s3)->s+s1+s3)
    |> many1

let ewr =
    unqouted
    |>> List.toArray
    |>> String.concat ""
let jUnquotedString =
    ewr
    |>> JUnquotedString
    <?> "Unquoted string"

// ======================================
// parsing JObjectQuoted
// ======================================
let jObjectQuoted =

    let left = 
        pstring "\"{" .>> spaces
    
    let right = 
        pstring "}\"" .>> spaces
    
    let colon = 
        pchar ':' .>> spaces
    
    let comma = 
        pchar ',' .>> spaces
    
    (* let key = 
        quotedString .>> spaces  *)
    let key = 
        let jChar = satisfy (fun ch -> ch <> ':') "label"
        manyChars jChar .>> spaces

    let value = 
        jValue .>> spaces

    let keyValue =
        (key .>> colon) .>>. value
    
    let keyValues =
        sepBy1 keyValue comma
    
    between left keyValues right
    |>> Map.ofList
    |>> JObject
    <?> "jObjectQuoted"

// ======================================
// parsing JEmptyObject
// ======================================
let jEmptyObject =
    let left = 
        pstring "\"{" .>> spaces
    
    let right = 
        pstring "}\"" .>> spaces
    
    let left1 = 
        pstring "{" .>> spaces
    
    let right1 = 
        pstring "}" .>> spaces
    
    (left.>>.right) <|> (left1.>>.right1)
    |>> fun (s1,s2)->sprintf "%s%s" s1 s2
    |>> JEmptyObject
    <?> "jEmptyObject"
// ======================================
// Final parser
// ======================================
jValueRef := choice 
    [
        jNull
        jBool
        jString
        jUnquotedString
        jNumber
        jArray
        jObjectQuoted
        jEmptyObject
        jObject
    ]