module TPCombinators.Tests

open NUnit.Framework


type Stocks = MySpace.CsvProvider<"C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv">
let res = Stocks.Load("C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv")

type Stocks2 = HideSpace.CsvProvider<"C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv">
let res2 = Stocks2.Load("C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv")

[<Test>]
let ``hello returns 42`` () =
  let result = 42
  printfn "%i" result
  Assert.AreEqual(42,result)

[<Test>]
let ``get some data``() =
    let first10Rows = 
        res.Rows 
            |> Seq.take 10 
    Assert.IsNotNull first10Rows

[<Test>]
let ``get some data2``() =
    let first10Rows = 
        res2.Rows 
            |> Seq.take 10 
    Assert.IsNotNull first10Rows

//let firstRow = res2.Rows |> Seq.head
//let f = firstRow.Volume