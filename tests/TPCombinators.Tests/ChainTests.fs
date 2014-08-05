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

    type Original = FSharp.Data.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let row = Original.GetSample().Rows |> Seq.head
    row.Hyperlink



    type Unsimplified1 = UnsimplifiedExample1.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    Unsimplified1.GetSample().Rows |> Seq.map (fun x -> x.Hyperlink)

    type Unsimplified2 = UnsimplifiedExample2.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let rows2 = Unsimplified2.GetSample().Rows 

    let fake2 : Unsimplified2 = failwith ""
    let rows2b = fake2.Rows
    rows2 |> Seq.map (fun x -> x.Hyperlink)


    type Simplified1 = SimplifiedExample1.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let rows = Simplified1.GetSample().Rows 

    let fake : Simplified1 = failwith ""
    let rowsb = fake.Rows

    rows |> Seq.map (fun x -> x.Hyperlink)

    type Simplified2 = SimplifiedExample2.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    Simplified2.GetSample().Rows |> Seq.map (fun x -> x.Hyperlink)

    Simplified2.GetSample().Rows |> Seq.map (fun (x,y) -> x)

module MSFTTests = 

    type Original = FSharp.Data.CsvProvider<"""data/MSFT.csv""">
    let row = Original.GetSample().Rows |> Seq.head
    row.Hyperlink



    type Unsimplified1 = UnsimplifiedExample1.CsvProvider<"""data/MSFT.csv""">
    Unsimplified1.GetSample().Rows |> Seq.head

    type Unsimplified2 = UnsimplifiedExample2.CsvProvider<"""data/MSFT.csv""">
    Unsimplified2.GetSample().Rows |> Seq.head


    type Simplified1 = SimplifiedExample1.CsvProvider<"""data/MSFT.csv""">
    Simplified1.GetSample().Rows |> Seq.head

    type Simplified2 = SimplifiedExample2.CsvProvider<"""data/MSFT.csv""">
    Simplified2.GetSample().Rows |> Seq.head

    Simplified2.GetSample().Rows |> Seq.map (fun x -> x)
