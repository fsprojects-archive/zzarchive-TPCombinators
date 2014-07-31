#if INTERACTIVE
#r @"..\..\bin\FSharp.Data.dll"
#r @"..\..\bin\FSharp.Data.DesignTime.dll"
#r @"..\..\bin\TPCombinators.dll"
#else
module TPCombinators.Tests
#endif

open System
open System.IO
open NUnit.Framework

type Stocks = MySpace.CsvProvider<"data/MSFT.csv">
type Stocks2 = MyOtherSpace.CsvProvider<"data/MSFT.csv">


[<Test>]
let ``hello returns 42`` () =
    let res = Stocks.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
    let res2 = Stocks2.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))

    res.Save(Path.Combine(Path.GetTempPath(), "misc.csv"))
    res2.Save(Path.Combine(Path.GetTempPath(), "misc2.csv"))

    let data = [ for row in res.Rows -> row.Open, row.Close, row.Volume ]

    for row in res.Rows do 
       row.Close = 1.0M |> ignore
       row.Date = System.DateTime.Now |> ignore
       row.Volume = 1 |> ignore
