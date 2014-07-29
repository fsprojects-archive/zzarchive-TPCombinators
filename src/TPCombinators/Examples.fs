module Examples


open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.CloneCombinator
open FSharp.ProvidedTypes.RegexHideCombinator
open Microsoft.FSharp.Core.CompilerServices



// In this example, we use a provided type space where a copy+rename has happened to namespaces:
//
//     FSharp.Data --> MySpace
//
// for all namespaces and types provided by the FSharp.Data CsvProvider.
let Example1 config = 

    // This fetches an instance of the type provider we wish to transform. 
    let CsvProvider = 
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

    Clone("FSharp.Data", "MySpace", CsvProvider)

let Example2 config = 

    Clone("MySpace", "MyOtherSpace", Example1(config))

let Example3 config = 

    let Provider = 
        Clone("MyOtherSpace", "HideSpace", Example2(config))

    Hide("D.te", Provider)


[<TypeProvider>]
type Example1Provider(config) = inherit TypeProviderExpression(Example1(config))

[<TypeProvider>]
type Example2Provider(config) = inherit TypeProviderExpression(Example2(config))

//let HideProvider config = 
//    
//    //Take the provider cloned to MyOtherSpace and hide properties matching the regular expression
//    let ClonedCsv = 
//        let ProviderAssembly = typeof<Example3Provider>.Assembly
//        new Example3Provider(ConfigForOtherTypeProvider(config, ProviderAssembly.Location))
//
//    Hide("D.te", ClonedCsv)

[<TypeProvider>]
type ExampleHide (config) = inherit TypeProviderExpression(Example3(config))

[<assembly:TypeProviderAssembly>] 
do()

