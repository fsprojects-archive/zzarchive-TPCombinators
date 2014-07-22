namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("TPCombinators")>]
[<assembly: AssemblyProductAttribute("TPCombinators")>]
[<assembly: AssemblyDescriptionAttribute("Example implementation of type provider combinators")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
