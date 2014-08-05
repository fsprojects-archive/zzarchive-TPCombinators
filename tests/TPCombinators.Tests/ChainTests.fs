#if INTERACTIVE
#r @"..\..\bin\FSharp.Data.dll"
#r @"..\..\bin\FSharp.Data.DesignTime.dll"
#r @"..\..\bin\TPCombinators.dll"
#else
module TPCombinators.ChainTests
#endif

open System
open System.IO
//open NUnit.Framework

module HyperlinkTests = 
    type Original = FSharp.Data.CsvProvider<"hyperlinks.csv">
    let orows = Original.GetSample().Rows 
    let olinks = orows |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Unsimplified1 = UnsimplifiedExample1.CsvProvider<"hyperlinks.csv">
    let urows = Unsimplified1.GetSample().Rows 
    let ulinks = urows |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Unsimplified2 = UnsimplifiedExample2.CsvProvider<"hyperlinks.csv">
    let urows2 = Unsimplified2.GetSample().Rows 
    let ulinks2 = urows2 |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Simplified1 = SimplifiedExample1.CsvProvider<"hyperlinks.csv">
    let rows = Simplified1.GetSample().Rows 
    let links = rows |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Simplified2 = SimplifiedExample2.CsvProvider<"hyperlinks.csv">
    let rows2 = Simplified2.GetSample().Rows 
    let links2 = rows2 |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray