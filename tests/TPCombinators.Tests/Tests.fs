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
open FSharp.Data

[<Literal>]
let freebaseKey = "AIzaSyA6hqi2Kru4sEgKW11eAmlAwGBYxOovY74"
type FreebaseDataWithKey = FreebaseDataProvider<Key=freebaseKey>
type OriginalCsv = CsvProvider<"data/MSFT.csv">

[<Literal>]
let msft = "http://ichart.finance.yahoo.com/table.csv?s=MSFT"

//type Stocks = MySpace.CsvProvider<msft>
//type Hide = HideSpace.CsvProvider<msft>
//type Show = ShowSpace.CsvProvider<msft>
//type FreeHide = Free.FreebaseDataProvider<Key=freebaseKey, UseUnitsOfMeasure=false>
//type Ex = Corp.XmlProvider<"../../src/TPCombinators/data/a01.xml">
//type GeoJson = Geo.JsonProvider<"../../docs/content/data/nsg-090714.txt">
//type WB = BankSpace.WorldBankData
//type CsvStatic = StaticSpace.CsvProvider<msft, OneParameter="thing">
type FileSys = CachedFileSys.FileSystem<"C:\\Folder1\\">

//type FileSys = NewSpace.FileSystem<"C:\\">


//[<Test>]
//let ``hello returns 42`` () =
//    let res = Stocks.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
//    res.Save(Path.Combine(Path.GetTempPath(), "misc.csv"))
//    let data = [ for row in res.Rows -> row.Open, row.Close, row.Volume ]
//
//    for row in res.Rows do 
//       row.Close = 1.0M |> ignore
//       row.Date = System.DateTime.Now |> ignore
//       row.Volume = 1 |> ignore
//
//[<Test>]
//let ``Hide/show desired properties`` () =
//    //This will hide the Date property
//    let res = Hide.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv")) 
//    //This will hide all properties except for Date
//    let res2 = Show.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
//    
//    let row = Seq.head res.Rows
//    let row2 = Seq.head res2.Rows
//    try 
//        //All properties apart from "Date" should be shown
//        printfn "%A" row.Date
//        Assert.Fail()
//    with    
//    | _ -> ()
//
//    //"Date" should be the only visible property
//    printfn "%A" row2.Date
//
//[<Test>]
//let ``read xml corpus`` () =
//    let res = Ex.Load("../../src/TPCombinators/data/a01.xml")
//    printfn "%A" res
//
//[<Test>]
//let freebase () =
//    let res = FreeHide.GetDataContext()
//    let elements = res.``Science and Technology``.Chemistry.``Chemical Elements``.Individuals
//    printfn "%A" elements.Argon
//
//    let res2 = FreebaseDataWithKey.GetDataContext()
//    let elements2 = res2.``Science and Technology``.Chemistry.``Chemical Elements``.Individuals
//    printfn "%A" elements2.Argon
//
//
//[<Test>]
//let geojson () =
//    let first = GeoJson.Load("../../docs/content/data/nsg-090714.txt").Features |> Seq.head
////    let first = res.Features |> Seq.head
//    printfn "%A" first.Properties.Descriptor
//
////type BankSpace = Hide<".*[dD]ebt.*", typeof<FSharp.Data.WorldBankDataProvider>>
////type BankSpace = Hide<".*[dD]ebt.*", FSharp.Data.WorldBankData>
//
//
//[<Test>]
//let ``World Bank clone`` () =
//    let context = WB.GetDataContext()
//    printfn "%A" context.Countries.Romania.Indicators.``Commercial service exports (current US$)``.Values
//    printfn "%A" context.Countries.Afghanistan.Indicators.``CO2 emissions (kg per 2011 PPP $ of GDP)``.Years
//    printfn "%A" context.Countries.Romania.Indicators.``Debt buyback (current US$)``.Values
//    let context2 = WorldBankData.GetDataContext()
//    printfn "%A" context2.Countries.Andorra.Indicators.``(%) Generosity of All Social Insurance``.Values
//
//
//[<Test>]
//let ``CSV add static parameter`` () =
//    let res = CsvStatic.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
//    let res2 = OriginalCsv.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
//    
//    let firstRow = Seq.head res.Rows
//    for row in res.Rows do
//        printfn "%A" row.Close
//
//    printfn "%A" firstRow.High

[<Test>]
let ``CSV with metadata caching`` () =
    let res = FileSys.Folder11.``File2.txt``
    printfn "%A" res

//[<Test>]
//let ``Try filesystem`` () = 
//    let res = FileSys.GitHub.fsprojects.TPCombinators.tests.``TPCombinators.Tests``.data.``MSFT.csv``.Contains(".csv")
//    printfn "%A" (res.ToString())
