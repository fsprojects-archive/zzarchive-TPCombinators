module Examples


open FSharp.ProvidedTypes.Combinators
open FSharp.ProvidedTypes.CloneCombinator
open Microsoft.FSharp.Core.CompilerServices



// In this example, we use a provided type space where a copy+rename has happened to namespaces:
//
//     FSharp.Data --> MySpace
//
// for all namespaces and types provided by the FSharp.Data CsvProvider.
let Example1 config = 

    // This fetches an instance of the type provider we wish to transform. 
    let CsvProvider(config) = 
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

    Clone("FSharp.Data", "MySpace", CsvProvider(config))

let Example2 config = 

    Clone("MySpace", "MyOtherSpace", Example1(config))

[<TypeProvider>]
type Example1Provider(config) = inherit TypeProviderExpression(Example1(config))

[<TypeProvider>]
type Example2Provider(config) = inherit TypeProviderExpression(Example2(config))

[<assembly:TypeProviderAssembly>] 
do()

