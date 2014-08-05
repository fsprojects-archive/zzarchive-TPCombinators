module ChainExamples

open FSharp.ProvidedTypes.CloneCombinator
open FSharp.ProvidedTypes.ChainCombinator
open FSharp.ProvidedTypes.GeneralCombinators
open Microsoft.FSharp.Core.CompilerServices

//FSharp.Data.DbPedia.GetDataContext().Ontology.Activity.Game.Individuals.``7 Wonders (board game)``.
//let r = FSharp.Data.DbPediaSearch<"Person","Antoine Bauza">.SearchResults()

// In this example, we chain two type providers:
//
//     CsvProvider --> DbPediaProvider
//
// such that any string value in the CSV file containing a DBpedia resource link will be replaced with a provided type
(*
let CsvDbPedia config = 
    let CsvProvider =
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

    let DbPediaProvider =
        let FSharpDataDbPediaAssembly = typeof<FSharp.Data.DbPedia>.Assembly
        new FSharp.Data.DbPediaTypeProvider.DbPediaTypeProvider(ConfigForOtherTypeProvider(config, FSharpDataDbPediaAssembly.Location))

    //let RenamedCsvProvider = Clone("FSharp.Data", "Combined", CsvProvider)
    //Chain(RenamedCsvProvider, DbPediaProvider)
    let resolver = DbPediaProvider.FreebaseToDbPedia >> (function None -> None | Some uri -> DbPediaProvider.GetTypeByUri uri)
    
    Chain(CsvProvider, resolver)

[<TypeProvider>]
type CsvDbPediaProvider(config) = inherit TypeProviderExpression(CsvDbPedia(config))
*)

[<assembly:TypeProviderAssembly>] 
do()

