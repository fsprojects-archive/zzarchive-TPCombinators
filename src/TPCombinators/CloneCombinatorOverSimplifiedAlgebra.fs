// This file shows how implementing a transform using a simplified view of the algebra of provided constructs
// looks substantially easier.  
//

module FSharp.ProvidedTypes.CloneCombinatorOverSimplifiedAlgebra

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.SimplifiedAlgebra

[<AutoOpen>]
module private Utils = 
    let notRequired msg = failwith ("not required: " + msg)
    let NIX x = x   // Inner --> Outer
    let XIN x = x   // Outer --> Inner

    /// Indicates that an object is a simple wrapper for another object of the indicated type,
    /// used for implementing equality in terms of the underlying wrapped objects.
    type IWraps<'T> =
         abstract Value : 'T

    let unwrap<'T> (x:'T) = 
        match box x with 
        | :? IWraps<'T> as t -> t.Value
        | _ -> x

type System.String with 
    member s.ReplacePrefix (s1:string, s2:string) =  
        if s.StartsWith(s1) then s2 + s.[s1.Length..] else s


/// Clones namespaces, type providers, types and members provided by tp, renaming namespace nsp1 into namespace nsp2.
let Clone(nsp1:string, nsp2:string, tp: ISimpleTypeProvider) = 

    let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    // Unique wrapped type definition objects must be translated to unique wrapper objects, based 
    // on object identity.
    let txTable = TxTable<ISimpleTypeDefinition, ISimpleTypeDefinition>()

    // The transformation we perform on the assembly. isTarget indicates if this is a provided object we should transform. 
    //
    // For now we just transform ALL erased objects.  This assumes one set of provided types is closed,
    // i.e. doesn't refer to any other provided types.
    let TxAssembly isTarget (a:Assembly) = if isTarget then thisAssembly else a
    let TxNamespaceName isTarget (ns:string) = if isTarget then ns.ReplacePrefix(nsp1, nsp2) else ns


    let TxCustomAttributes (inp: seq<CustomAttributeData>) = inp

    let rec TxStaticParameter(inp: ISimpleStaticParameter) = 

        { new ISimpleStaticParameter with 

            override __.Name = inp.Name
            override __.ParameterType = inp.ParameterType
            override __.OptionalValue = inp.OptionalValue
            override __.CustomAttributes = inp.CustomAttributes  |> TxCustomAttributes

          interface IWraps<ISimpleStaticParameter> with 
              member x.Value = inp
        }


    and TxParameter(inp : ISimpleParameter) = 
        { new ISimpleParameter with 

            override __.Name = inp.Name |> NIX
            override __.ParameterType = inp.ParameterType |> TxType
            override __.OptionalValue = inp.OptionalValue |> NIX
            override __.IsOut = inp.IsOut |> NIX
            override __.CustomAttributes = inp.CustomAttributes  |> TxCustomAttributes
        }
 
    and TxConstructor(inp: ISimpleConstructor) = 
        { new ISimpleConstructor with

            //override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition
            override __.Parameters = inp.Parameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and TxMethod(inp: ISimpleMethod) =
        { new ISimpleMethod with 

            override __.Name              = inp.Name  |> NIX
            override __.IsStatic          = inp.IsStatic  |> NIX
            override __.Parameters        = inp.Parameters |> Array.map TxParameter
            //override __.DeclaringType     = inp.DeclaringType  |> TxTypeDefinition
            override __.ReturnType        = inp.ReturnType  |> TxType
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)

          interface IWraps<ISimpleMethod> with 
              member x.Value = inp
        }

    and TxProperty(inp: ISimpleProperty) = 
        { new ISimpleProperty with 

            override __.Name = inp.Name |> NIX
            //override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition
            override __.PropertyType = inp.PropertyType |> TxType
            override __.GetMethod = inp.GetMethod |> Option.map TxMethod
            override __.SetMethod = inp.SetMethod |> Option.map TxMethod
            override __.IndexParameters = inp.IndexParameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes

          interface IWraps<ISimpleProperty> with 
              member x.Value = inp
        }

    and TxEventDefinition(inp: ISimpleEvent) = 
        { new ISimpleEvent with 

            override __.Name = inp.Name |> NIX
            //override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition

            override __.EventHandlerType = inp.EventHandlerType |> TxType
            override __.AddMethod = inp.AddMethod |> TxMethod
            override __.RemoveMethod = inp.RemoveMethod |> TxMethod

            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes

          interface IWraps<ISimpleEvent> with 
              member x.Value = inp
        }

    and TxField(inp: ISimpleLiteralField) = 
        { new ISimpleLiteralField with 

            override __.Name = inp.Name |> NIX
            //override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition
            override __.FieldType = inp.FieldType |> TxType
            override __.LiteralValue  = inp.LiteralValue |> NIX
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes

          interface IWraps<ISimpleLiteralField> with 
              member x.Value = inp
        }

    and TxType(inp: ISimpleType) = 
        match inp with 
        | TyApp(td, args) -> TyApp(TxTypeDefinitionReference td, Array.map TxType args)
        | TyArray(n, arg) -> TyArray(n, TxType arg)
        | TyPointer(arg) -> TyPointer(TxType arg)

    and TxTypeDefinitionReference inp = 
        match inp with
        | ISimpleTypeDefinitionReference.OtherTyDef _ -> inp
        | ISimpleTypeDefinitionReference.SimpleTyDef x -> SimpleTyDef (TxTypeDefinition x)

(*
    /// Reverse the mapping for types
    and UnTxType(inp: ISimpleType) = 
        match inp with 
        | TyApp(td, args) -> TyApp(UnTxTypeDefinition td, Array.map UnTxType args)
        | TyArray(n, arg) -> TyArray(n, UnTxType arg)
        | TyPointer(arg) -> TyPointer(UnTxType arg)

    /// Reverse the mapping for type definitions
    and UnTxTypeDefinition(res: ISimpleTypeDefinition) = unwrap<ISimpleTypeDefinition> res
*)

    /// Reverse the mapping for method definitions
    and UnTxMethod(res: ISimpleMethod) = unwrap<ISimpleMethod> res

    and TxTypeDefinition(inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->
        let isTarget =  true

        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name |> NIX
            override __.Assembly = inp.Assembly |> TxAssembly isTarget
            override __.Namespace = inp.Namespace |> Option.map (TxNamespaceName isTarget)
            override __.DeclaringType = inp.DeclaringType |> Option.map TxTypeDefinition

            override __.BaseType = inp.BaseType |> Option.map TxType
            override __.Interfaces = inp.Interfaces |> Array.map TxType

            override __.Constructors = inp.Constructors |> Array.map TxConstructor
            override __.Methods = inp.Methods |> Array.map TxMethod
            override __.Fields = inp.Fields |> Array.map TxField
            override __.Events = inp.Events |> Array.map TxEventDefinition
            override __.Properties = inp.Properties |> Array.map TxProperty
            override __.NestedTypes = inp.NestedTypes |> Array.map TxTypeDefinition

            override __.GetField(name) = inp.GetField(name) |> Option.map TxField
            override __.GetEvent(name) = inp.GetEvent(name) |> Option.map TxEventDefinition
            override __.GetNestedType(name) = inp.GetNestedType(name) |> Option.map TxTypeDefinition
            override __.GetProperty(name) = inp.GetProperty(name) |> Option.map TxProperty

            //override __.UnderlyingSystemType = inp.UnderlyingSystemType |> TxType
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes

            override __.ApplyStaticArguments(typePathWithArguments, objs) = inp.ApplyStaticArguments(XIN typePathWithArguments, objs) |> TxTypeDefinition

            override __.StaticParameters = inp.StaticParameters |> Array.map TxStaticParameter

          interface IWraps<ISimpleTypeDefinition> with 
              member x.Value = inp
        }

    /// Transform a provided member definition
    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: ISimpleNamespace) = 
        { new ISimpleNamespace with
            override __.NestedNamespaces = inp.NestedNamespaces |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName |> TxNamespaceName true
            override __.TypeDefinitions = inp.TypeDefinitions |> Array.map TxTypeDefinition
            override __.GetTypeDefinition(name) =  inp.GetTypeDefinition(name) |> Option.map TxTypeDefinition
         }

    /// Transform an input ITypeProvider
    let TxTypeProviderDefinition (inp: ISimpleTypeProvider) = 
        { new ISimpleTypeProvider with 
            override __.Namespaces = inp.Namespaces |> Array.map TxNamespaceDefinition

            [<CLIEvent>]
            override __.Invalidate = inp.Invalidate 

          interface System.IDisposable with 
            override x.Dispose() = inp.Dispose()
        }
    
    TxTypeProviderDefinition(tp)


