module TPCombinators.Tests

open NUnit.Framework


type Stocks = MySpace.CsvProvider<"C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv">
type Stocks2 = MyOtherSpace.CsvProvider<"C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv">

let res = Stocks.Load("C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv")
let res2 = Stocks2.Load("C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv")

[<Test>]
let ``hello returns 42`` () =
  let result = 42
  printfn "%i" result
  Assert.AreEqual(42,result)

