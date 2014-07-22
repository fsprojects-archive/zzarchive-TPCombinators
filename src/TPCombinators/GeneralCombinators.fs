module FSharp.ProvidedTypes.Combinators

open Microsoft.FSharp.Core.CompilerServices

/// A base type suitable for implementing a type provider component based on a computed ITypeProvider instance.
type TypeProviderExpression (inp: ITypeProvider) = 

    interface ITypeProvider with 
        override __.GetNamespaces() = inp.GetNamespaces() 
        override __.GetInvokerExpression(methodBase, parametersUsingReprTypes) = inp.GetInvokerExpression(methodBase, parametersUsingReprTypes) 
        override __.GetStaticParameters(typeWithoutArguments) = inp.GetStaticParameters(typeWithoutArguments) 
        override __.ApplyStaticArguments(typeWithoutArguments, typePathWithArguments, objs) = inp.ApplyStaticArguments(typeWithoutArguments, typePathWithArguments, objs) 
        override __.GetGeneratedAssemblyContents(assembly) = inp.GetGeneratedAssemblyContents(assembly)
        [<CLIEvent>]
        override __.Invalidate = inp.Invalidate 

    interface System.IDisposable with 
        override x.Dispose() = inp.Dispose()
                                                                        
/// Create a TypeProviderConfig suitable for passing to a slave type provider instance.
///
/// The slave type provider instance expects a configuration with a matching
/// RuntimeAssembly value. The other configuration parameters can remain unchanged.
let ConfigForOtherTypeProvider(config: TypeProviderConfig, runtimeAssembly) = 
    TypeProviderConfig(config.SystemRuntimeContainsType,
                       ResolutionFolder=config.ResolutionFolder,
                       RuntimeAssembly=runtimeAssembly,
                       ReferencedAssemblies=config.ReferencedAssemblies,
                       TemporaryFolder=config.TemporaryFolder,
                       IsInvalidationSupported=config.IsInvalidationSupported,
                       IsHostedExecution=config.IsHostedExecution,
                       SystemRuntimeAssemblyVersion=config.SystemRuntimeAssemblyVersion)

[<AutoOpen>]
module Utils = 
    let notRequired msg = failwith ("not required: " + msg)
    let NIX x = x   // Inner --> Outer
    let XIN x = x   // Outer --> Inner

    /// Indicates that an object is a simple wrapper for another object of the indicated type,
    /// used for implementing equality in terms of the underlying wrapped objects.
    type IWraps<'T> =
         abstract Value : 'T

    let unwrapObj<'T> (x:obj) = 
        match x with 
        | :? IWraps<'T> as t -> box t.Value
        | _ -> x

    let unwrap<'T> (x:'T) = 
        match box x with 
        | :? IWraps<'T> as t -> t.Value
        | _ -> x

type System.String with 
    member s.ReplacePrefix (s1:string, s2:string) =  
        if s.StartsWith(s1) then s2 + s.[s1.Length..] else s



