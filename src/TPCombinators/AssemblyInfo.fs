namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("TPCombinators")>]
[<assembly: AssemblyProductAttribute("TPCombinators")>]
[<assembly: AssemblyDescriptionAttribute("Build F# type providers using combinators")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
