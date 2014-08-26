#if INTERACTIVE
#r @"..\..\bin\TPCombinators.dll"
#r @"..\..\bin\HiveTypeProvider.dll"
#else
module TPCombinators.HiveTests
#endif

open Hive.HiveRuntime
open Microsoft.FSharp.Linq.NullableOperators

//This sample is set up to work with a Hortonworks Sandbox installation
//It should only expose "sample_07" and "sample_08"
//The Restriction parameter limits the filtering to properties whose declaring type matches the regex
[<Literal>]
let dsn = "Sample Hortonworks Hive DSN; pwd=hadoop"
type Conn = HideHive.HiveTypeProvider<dsn, DefaultMetadataTimeout=1000, Regex="sample_0.", Show=true, Restriction="DataService">

let context = Conn.GetDataContext()
let ``hiding hive tables`` () =

    let query = hiveQuery {for row in context.sample_07 do
                           where (row.salary ?< 20000)
                           select row.description }

    query.Run() |> ignore
