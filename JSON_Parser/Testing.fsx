#load "ParserLibrary.fsx"
#load "CSV_Parser.fsx"

open ParserLibrary
open CSV_Parser
open System.IO

let sr = new StreamReader(@"C:\Users\Emad\Desktop\JSON Parser\JSON_Parser\dataMin.csv")
sr.ReadLine() |> ignore
let input = sr.ReadToEnd()
let ss = parseCSV input |> printResult

sr.Close()