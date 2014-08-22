#if INTERACTIVE
#r @"..\..\bin\FSharp.Data.dll"
#r @"..\..\bin\FSharp.Data.DesignTime.dll"
#r @"..\..\bin\TPCombinators.dll"
#r @"..\..\bin\HiveTypeProvider.dll"
#else
module TPCombinators.Tests
#endif

open System.IO
open NUnit.Framework
open FSharp.Data

[<Literal>]
let freebaseKey = "AIzaSyA6hqi2Kru4sEgKW11eAmlAwGBYxOovY74"
type FreebaseDataWithKey = FreebaseDataProvider<Key=freebaseKey>
type OriginalCsv = CsvProvider<"data/MSFT.csv">

[<Literal>]
let msft = "http://ichart.finance.yahoo.com/table.csv?s=MSFT"

type Stocks = MySpace.CsvProvider<msft>
type Hide = HideSpace.CsvProvider<msft>
type Show = ShowSpace.CsvProvider<msft>
type FreeHide = Free.FreebaseDataProvider<Key=freebaseKey, UseUnitsOfMeasure=false>
type GeoJson = Geo.JsonProvider<"../../docs/content/data/nsg-090714.txt">
type WB = BankSpace.WorldBankData


[<Test>]
let ``hello returns 42`` () =
    let res = Stocks.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
    res.Save(Path.Combine(Path.GetTempPath(), "misc.csv"))
    let data = [ for row in res.Rows -> row.Open, row.Close, row.Volume ]

    for row in res.Rows do 
       row.Close = 1.0M |> ignore
       row.Date = System.DateTime.Now |> ignore
       row.Volume = 1 |> ignore

[<Test>]
let ``Hide/show desired properties`` () =
    //This will hide the Date property
    let res = Hide.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv")) 
    //This will hide all properties except for Date
    let res2 = Show.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
    
    let row = Seq.head res.Rows
    let row2 = Seq.head res2.Rows

    
    //Everything apart from "Date" should be visible
    printfn "%A" row.Close

    //"Date" should be the only visible property
    printfn "%A" row2.Date

[<Test>]
let freebase () =
    let res = FreeHide.GetDataContext()
    let elements = res.``Science and Technology``.Chemistry.``Chemical Elements``.Individuals

    //"Argon" should be filtered out
    printfn "%A" elements.Americium

    let res2 = FreebaseDataWithKey.GetDataContext()
    let elements2 = res2.``Science and Technology``.Chemistry.``Chemical Elements``.Individuals
    printfn "%A" elements2.Argon


[<Test>]
let geojson () =
    let first = GeoJson.Load("../../docs/content/data/nsg-090714.txt").Features |> Seq.head
    //No Geometry properties should be present
    printfn "%A" first.Properties.Descriptor

[<Test>]
let ``World Bank clone`` () =
    let context = WB.GetDataContext()
    //The only indicators show should be those containing "CO2"
    printfn "%A" context.Countries.Afghanistan.Indicators.``CO2 emissions (kg per 2011 PPP $ of GDP)``.Years
    
    let context2 = WorldBankData.GetDataContext()
    printfn "%A" context2.Countries.Andorra.Indicators.``(%) Generosity of All Social Insurance``.Values

type FileSys = CachedFileSys.FileSystem<path="C:\\Folder1\\">
[<Test>]
let ``FileSys with metadata caching`` () =
    //This will not fail even when these folders and files are not present in the
    //file system. The Combinator will substitute them with the cached metadata
    //(currently saved in the Test project folder, but the combinator could be
    //modified to save it to a different location.
    let res = FileSys.Folder11.``File2.txt``
    printfn "%A" res

type CsvStatic = StaticSpace.CsvProvider<msft, Regex="[cC]lose", Show=false>
[<Test>]
let ``CSV add static parameter`` () = 
    let res = CsvStatic.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
    
    //Should hide the "Close" property.
    //Another example of this combinator being used with a different type provider 
    //can be found in HiveTest.fs
    for row in res.Rows do
        printfn "%A" row.Open

