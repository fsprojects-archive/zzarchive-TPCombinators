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




