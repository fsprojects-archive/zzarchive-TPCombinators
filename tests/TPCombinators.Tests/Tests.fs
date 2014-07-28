module TPCombinators.Tests

open NUnit.Framework


type Stocks = MySpace.CsvProvider<"C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv">
type Stocks2 = MyOtherSpace.CsvProvider<"C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv">

let res = Stocks.Load("C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv")
let res2 = Stocks2.Load("C:/GitHub/fsharp/FSharp.Data/docs/content/data/MSFT.csv")

res.Save("misc.csv")

for row in res.Rows do 
   row.Close = 1.0M |> ignore
   row.Date = System.DateTime.Now |> ignore
   row.Volume = 1 |> ignore

[<Test>]
let ``hello returns 42`` () =
  let result = 42
  printfn "%i" result
  Assert.AreEqual(42,result)

