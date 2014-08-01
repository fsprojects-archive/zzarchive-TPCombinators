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
type Hide = HideSpace.CsvProvider<"data/MSFT.csv">
type Free = Yo.FreebaseData
type Ex = Corp.XmlProvider<"../../src/TPCombinators/data/a01.xml">
type GeoJson = Geo.JsonProvider<"../../docs/content/data/nsg-090714.txt">
type OptHide = OptSpace.CsvProvider<"data/MSFT.csv", show=false>
//type FileSys = NewSpace.FileSystem<"C:\\">


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
let ``Hide desired properties`` () =
    let res = Hide.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
    let row = Seq.head res.Rows
    try 
        printfn "%A" row.Date
        Assert.Fail()
    with    
    | _ -> ()

    let res2 = Stocks.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
    let row2 = Seq.head res2.Rows
    printfn "%A" row2.Date

[<Test>]
let ``read xml corpus`` () =
    let res = Ex.Load("../../src/TPCombinators/data/a01.xml")
    printfn "%A" res

[<Test>]
let freebase () =
    let res = Free.GetDataContext()
    let elements = res.``Science and Technology``.Chemistry.``Chemical Elements``
    printfn "%A" elements.Individuals.Argon.``Boiling Point``

[<Test>]
let geojson () =
    let first = GeoJson.Load("../../docs/content/data/nsg-090714.txt").Features |> Seq.head
//    let first = res.Features |> Seq.head
    printfn "%A" first.Properties.Descriptor


[<Test>]
let ``hide or show`` () =
    let res = OptHide.Load(Path.Combine(__SOURCE_DIRECTORY__, "data/MSFT.csv"))
    let row = res.Rows |> Seq.head
    printfn row.Close



//[<Test>]
//let ``Try filesystem`` () = 
//    let res = FileSys.GitHub.fsprojects.TPCombinators.tests.``TPCombinators.Tests``.data.``MSFT.csv``.Contains(".csv")
//    printfn "%A" (res.ToString())