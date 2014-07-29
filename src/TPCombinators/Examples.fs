module Examples


open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.CloneCombinator
open FSharp.ProvidedTypes.RegexHideCombinator
open FSharp.ProvidedTypes.ChainCombinator
open Microsoft.FSharp.Core.CompilerServices



// In this example, we use a provided type space where a copy+rename has happened to namespaces:
//
//     FSharp.Data --> MySpace
//
// for all namespaces and types provided by the FSharp.Data CsvProvider.


//let Example1 config = 
//
//    // This fetches an instance of the type provider we wish to transform. 
//    let CsvProvider = 
//        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
//        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))
//
//    Clone("FSharp.Data", "MySpace", CsvProvider)
//
//let Example2 config = 
//
//    Clone("MySpace", "MyOtherSpace", Example1(config))
//
//let Example3 config = 
//
//    let Provider = 
//        Clone("MyOtherSpace", "HideSpace", Example2(config))
//
//    Hide("D.te", Provider)

let Example4 config = 
    let FileSysProvider =
        let FileSysAssembly = typeof<FSharp.Management.FileSystem<path="C:\\">>.Assembly
        new FSharp.Management.NamespaceProvider.FileSystemProvider(ConfigForOtherTypeProvider(config, FileSysAssembly.Location))

    let CsvProvider = 
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

//    Clone("FSharp.Management", "NewSpace", FileSysProvider)
    Chain("FSharp.Management", "NewSpace", FileSysProvider, CsvProvider)



//[<TypeProvider>]
//type Example1Provider(config) = inherit TypeProviderExpression(Example1(config))
//
//[<TypeProvider>]
//type Example2Provider(config) = inherit TypeProviderExpression(Example2(config))
//
//[<TypeProvider>]
//type ExampleHide (config) = inherit TypeProviderExpression(Example3(config))

[<TypeProvider>]
type ExampleFileSys(config) = inherit TypeProviderExpression(Example4(config))

[<assembly:TypeProviderAssembly>] 
do()

