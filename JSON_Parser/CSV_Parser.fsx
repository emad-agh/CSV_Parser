#load "ParserLibrary.fsx"
open System.Runtime.Serialization
open System.Net
open System.Web.SessionState
#load "JSON_Parser.fsx"
open ParserLibrary
open JSON_Parser
open System.IO
open System

type httpMeth =
    |GET
    |POST
    |HEAD
    |PUT
    |DELETE
    |OPTIONS
    |CONNECT

type data = 
    |ID             of int
    |Date           of DateTime
    |HTTPMethod     of httpMeth
    |AbsoluteUri    of string
    |PathInfo       of string
    |ReqBody        of JValue
    |ReqDto         of JValue
    |UserAuthId     of JValue
    |SessionId      of JValue
    |IPAddr         of string
    |ForwardedFor   of JValue
    |Referer        of string
    |Headers        of JValue
    |FormData       of JValue
    |Items          of JValue
    |Session        of string
    |RsponseDto     of string
    |ErrorResponse  of string 
    |ReqDuration    of string

// ======================================
// helping funcs
// ======================================
let digitOneNine =
    satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "digit 1-9"

let digit =
    satisfy (Char.IsDigit) "digit"
let parseIntWith0 =    
    manyChars1 digit 
    |>> int
let parseInt =    
    digitOneNine .>>. manyChars digit 
    |>> fun (first,rest) -> string first + rest
    |>> int
let empty typee =
        spaces >>.
        satisfyEmpty "Empty"
        |>>typee
        .>>spaces

// ======================================
// Parsing ID
// ======================================  
let ID_P = 
    parseInt
    |>>ID
    <?> "id"

// ======================================
// Parsing Date ant time
// ======================================  
let Date_P = 
    let sep = 
        pchar '/'

    let colon =
        pchar ':'
    
    let space = 
        pchar ' '
    let convert ((((((((a, b), c), d), e), f), g), h), i) =
        let str = sprintf "%d%c%d%c%d%c%d%c%d" a b c d e f g h i
        str

    let convertToDateTime c = 
        DateTime(c)

    parseIntWith0 .>>. sep .>>. parseIntWith0 .>>. sep .>>. parseIntWith0 .>>. space .>>. parseIntWith0 .>>. colon .>>. parseIntWith0
    |>> convert
    |>> DateTime.Parse
    |>>Date
    <?> "date"

// ======================================
// Parsing HTTPMethon
// ====================================== 
let HTTPMethod_P =   
    let GET = 
        pstring "GET" 
        >>% HTTPMethod GET
    let POST = 
        pstring "POST" 
        >>% HTTPMethod POST
    let HEAD = 
        pstring "HEAD" 
        >>% HTTPMethod HEAD
    let PUT = 
        pstring "PUT" 
        >>% HTTPMethod PUT
    let DELETE = 
        pstring "DELETE" 
        >>% HTTPMethod DELETE
    let OPTIONS = 
        pstring "OPTIONS" 
        >>% HTTPMethod OPTIONS
    let CONNECT = 
        pstring "CONNECT" 
        >>% HTTPMethod CONNECT

    GET <|> POST <|> HEAD <|> PUT <|> DELETE <|> OPTIONS <|> CONNECT
    <?> "HttpMethod"  

// ======================================
// Parsing AbsoluteUri
// ====================================== 
let AbsoluteUri_P =
    let head =
        pstring "https://"

    let convert (s1,s2) =
        sprintf "%s%s" s1 s2
    
    head .>>. manyChars1 (satisfy (fun ch -> ch <> ',') "")
    |>> convert
    |>> AbsoluteUri 
    <?>"AbsoluteUri"

// ======================================
// Parsing PathInfo                    
// ====================================== 
let p1 =
    pchar '/' .>>. manyChars (satisfy (fun ch -> ch <> ',') "")
    |>> (fun (c, str)-> sprintf "%c%s" c str)

let PathInfo_P =
    many1 p1
    |>> String.Concat
    |>>PathInfo
    <?>"PathInfo"
    
// ======================================
// Parsing ReqBody
// ====================================== 
let ReqBody_P =    
    jObject <|> empty JUnquotedString
    |>>ReqBody
    <?>"ReqBody"

// ======================================
// Parsing ReqDto
// ====================================== 
let ReqDto_P =
    jObjectQuoted <|> jEmptyObject<|> jObject <|> empty JUnquotedString
    |>>ReqDto
    <?>"ReqDto"

// ======================================
// Parsing UserAuthId
// ====================================== 
let UserAuthId_P =
    jNumber <|> empty JUnquotedString
    |>>UserAuthId
    <?> "UserAuthId"

// ======================================
// Parsing SessionId
// ====================================== 
let SessionId_P =
    jUnquotedString
    |>> SessionId
    <?> "SessionId"


// ======================================
// Parsing IPAddr
// ====================================== 
let IPAddr_P =
    let dot =
        pchar '.'
    
    let digit =
        satisfy (Char.IsDigit) "digit"
    
    let parseIP =
        manyChars1 digit .>>. dot .>>. manyChars1 digit .>>. dot .>>. manyChars1 digit .>>. dot .>>. manyChars1 digit
    
    let convert ((((((s1,c1),s2),c2),s3),c3),s4) =
        let str = sprintf "%s%c%s%c%s%c%s" s1 c1 s2 c2 s3 c3 s4
        str

    parseIP
    |>>convert
    |>>IPAddr
    <?> "IPAddress"

// ======================================
// Parsing ForwardedFor
// ======================================   
let ForwardedFor_P =
    jUnquotedString <|> empty JUnquotedString
    |>> ForwardedFor
    <?> "ForwardedFor"
    
// ======================================
// Parsing Referer
// ======================================  
let Referer_P =
    let head =
        pstring "https://"

    let convert (s1,s2) =
        sprintf "%s%s" s1 s2
    
    head .>>. manyChars (satisfy (fun ch -> ch <> '\\' && ch <> ',') "")
    |>> convert <|> empty string
    |>> Referer 
    <?>"Referer"

// ======================================
// Parsing Headers                      ????
// ======================================
let Headers_P =
    jObjectQuoted
    |>>Headers
    <?>"Headers"  

// ======================================
// Parsing FormData                     
// ======================================
let FormData_P =
    jEmptyObject <|> jObject <|> empty JUnquotedString
    |>>FormData
    <?>"FormData"

// ======================================
// Parsing Items                        ????
// ======================================
let Items_P =
    jObjectQuoted
    |>>Items
    <?>"Items"

// ======================================
// Parsing Session
// ======================================
let Session_P =
    jUnquotedString <|> empty JUnquotedString
    |>> (fun x -> match x with |JUnquotedString a -> a)
    |>> Session
    <?> "Session"

// ======================================
// Parsing RsponseDto
// ======================================
let RsponseDto_P =
    jUnquotedString <|> empty JUnquotedString
    |>> (fun x -> match x with |JUnquotedString a -> a)
    |>> RsponseDto
    <?> "RsponseDto"
    
// ======================================
// Parsing ErrorResponse
// ======================================
let ErrorResponse_P =
    jUnquotedString <|> empty JUnquotedString
    |>> (fun x -> match x with |JUnquotedString a -> a)
    |>> ErrorResponse
    <?> "ErrorResponse"
    
// ======================================
// Parsing ReqDuration
// ======================================
let head = 
    pstring "PT"
let seconds =
    jNumber.>>.(pchar 'S' <|> pchar 's')
    |>> fun (s,c) -> match s with |JNumber ss -> sprintf "%f%c" ss c
let minutes =
    opt (jNumber.>>.(pchar 'M' <|> pchar 'm'))
    |>> fun m ->
        match m with 
        |None -> ""
        |Some (x,y) -> 
            match x with |JNumber n -> sprintf "%f%c" n y 
let hours =
    opt (jNumber.>>.(pchar 'H' <|> pchar 'h'))
    |>> fun m ->
        match m with 
        |None -> ""
        |Some (x,y) -> 
            match x with |JNumber n -> sprintf "%f%c" n y 
let ReqDuration_P =
    head .>>. hours .>>. minutes .>>. seconds
    |>> fun (((str1, str2),str3),str4) -> sprintf "%s%s%s%s" str1 str2 str3 str4
    |>>ReqDuration
    <?> "ReqDuration"
// ======================================
// ======================================

let comma =
    spaces >>. pchar ',' .>> spaces
    <?>"comma"
let CSVRecord = 
    [
        spaces >>.ID_P.>>comma
        Date_P.>>comma
        HTTPMethod_P.>> comma
        AbsoluteUri_P .>>comma
        PathInfo_P .>> comma
        ReqBody_P .>> comma
        ReqDto_P .>> comma
        UserAuthId_P.>> comma
        SessionId_P.>> comma
        IPAddr_P.>> comma
        ForwardedFor_P .>> comma
        Referer_P.>> comma
        Headers_P.>> comma
        FormData_P.>> comma
        Items_P.>> comma
        Session_P.>> comma
        RsponseDto_P.>> comma
        ErrorResponse_P.>> comma
        ReqDuration_P
    ]

let parseCSV =
    run (many (sequence CSVRecord))
