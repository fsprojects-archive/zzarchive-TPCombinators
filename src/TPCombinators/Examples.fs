module Examples

open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.CloneCombinator
open FSharp.ProvidedTypes.RegexHideCombinator
open FSharp.ProvidedTypes.ChainCombinator
open FSharp.ProvidedTypes.AddStaticCombinator
open Microsoft.FSharp.Core.CompilerServices
open FSharp.Data

let CloneExample config = 

    // This fetches an instance of the type provider we wish to transform. 
    let CsvProvider = 
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

    Clone("FSharp.Data", "MySpace", CsvProvider)

let HideExample config = 

    let Provider = 
        Clone("MySpace", "HideSpace", CloneExample(config))

    Hide("D.te", Provider)

let FreebaseHide config = 
    let FreebaseProvider = 
        let FreebaseAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.FreebaseTypeProvider(ConfigForOtherTypeProvider(config, FreebaseAssembly.Location))
    Hide("D.te", Clone("FSharp.Data", "Yo", FreebaseProvider))

let XmlHide config = 
    let XmlProvider =
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.XmlProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))
    Clone("FSharp.Data", "Corp", XmlProvider)

let JsonHide config =
    let JsonHideProvider = 
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.JsonProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))
    
    //Hide any geometry properties from the GeoJSON file.
    Hide(".*[gG]eometry.*", Clone("FSharp.Data", "Geo", JsonHideProvider))

let HideWithOption config = 
    let CsvProvider =
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

    Hide("D.te",
        AddStaticParameter("show", typeof<bool>, false, 
          Clone("FSharp.Data", "OptSpace", CsvProvider)))












//////////////// Chaining combinator NYI
let FileSystemExample config = 
    let FileSysProvider =
        let FileSysAssembly = typeof<FSharp.Management.FileSystem<path="C:\\">>.Assembly
        new FSharp.Management.NamespaceProvider.FileSystemProvider(ConfigForOtherTypeProvider(config, FileSysAssembly.Location))

    let CsvProvider = 
        let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
        new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

    Chain("FSharp.Management", "NewSpace", FileSysProvider, CsvProvider)



[<TypeProvider>]
type CloneProvider(config) = inherit TypeProviderExpression(CloneExample(config))

[<TypeProvider>]
type HideProvider(config) = inherit TypeProviderExpression(HideExample(config))

[<TypeProvider>]
type HideFreebaseProvider(config) = inherit TypeProviderExpression(FreebaseHide(config))

[<TypeProvider>]
type XmlHideProvider(config) = inherit TypeProviderExpression(XmlHide(config))

[<TypeProvider>]
type GeoJsonHide(config) = inherit TypeProviderExpression(JsonHide(config))

[<TypeProvider>]
type HideOption(config) = inherit TypeProviderExpression(HideWithOption(config))


[<assembly:TypeProviderAssembly>] 
do()

