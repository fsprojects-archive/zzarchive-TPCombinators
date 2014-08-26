module FSharp.ProvidedTypes.ExtendCombinator

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.SimplifiedAlgebra

type ExtendResolver<'T> =
  { ContextCreator : obj[] * ISimpleTypeDefinition -> 'T option
    MemberGenerator : 'T option * ISimpleTypeDefinition -> ISimpleMember [] }

let defaultExtendResolver =
  { ContextCreator      = fun _ -> None
    MemberGenerator     = fun _ -> [||] }

/// Extends a type provider by adding fields, methods and properties to provided types, controlled by the implementation of 'resolver'
let Extend<'T> (resolver : ExtendResolver<'T>) (tp: ISimpleTypeProvider) = 

    let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    let txTable = TxTable<ISimpleTypeDefinition, ISimpleTypeDefinition>()

    let TxAssembly (a:Assembly) = thisAssembly 

    let TxStaticParameter(inp: ISimpleStaticParameter) = inp

    let rec TxParameter(inp : ISimpleParameter) = 
        { new ISimpleParameter with 

            override __.Name = inp.Name 
            override __.ParameterType = inp.ParameterType |> TxType
            override __.OptionalValue = inp.OptionalValue 
            override __.IsOut = inp.IsOut 
            override __.CustomAttributes = inp.CustomAttributes  
        }
 
    and TxConstructor(inp: ISimpleConstructor) = 
        { new ISimpleConstructor with

            override __.Parameters = inp.Parameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and FindGoverningTransObj declTy  =
        let rec loop (ty:ISimpleTypeDefinition) = 
            match ty.DeclaringType with 
            | Some t -> loop t
            | None -> ty
        match loop declTy with 
        | :? IWraps<'T option> as state ->  state.Value
        | _ -> None

    and TxMethod (inp: ISimpleMethod) =

        { new ISimpleMethod with 
            override __.Name              = inp.Name  
            override __.IsStatic          = inp.IsStatic  
            override __.Parameters        = inp.Parameters |> Array.map TxParameter
            override __.ReturnType        = inp.ReturnType  |> TxType
            override __.CustomAttributes = inp.CustomAttributes 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and TxAssociatedMethod (inp: ISimpleAssociatedMethod) =
        { new ISimpleAssociatedMethod with 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }


    and TxProperty (inp: ISimpleProperty) = 
        { new ISimpleProperty with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic
            override __.PropertyType = inp.PropertyType |> TxType
            override __.GetMethod = inp.GetMethod |> Option.map TxAssociatedMethod
            override __.SetMethod = inp.SetMethod |> Option.map TxAssociatedMethod
            override __.IndexParameters = inp.IndexParameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes 

        }

    and TxEventDefinition (inp: ISimpleEvent) = 
        { new ISimpleEvent with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic
            override __.EventHandlerType = inp.EventHandlerType |> TxType
            override __.AddMethod = inp.AddMethod |> TxAssociatedMethod 
            override __.RemoveMethod = inp.RemoveMethod |> TxAssociatedMethod 
            override __.CustomAttributes = inp.CustomAttributes 
        }

    and TxField (inp: ISimpleLiteralField) = 
        { new ISimpleLiteralField with 

            override __.Name = inp.Name 
            override __.FieldType = inp.FieldType |> TxType
            override __.LiteralValue  = inp.LiteralValue 
            override __.CustomAttributes = inp.CustomAttributes 
        }

    and TxMember(inp: ISimpleMember) =
        match inp with
        | Constructor ctor -> Constructor(TxConstructor ctor)
        | Method meth -> Method(TxMethod meth)
        | Field fld -> Field(TxField fld)
        | Property prop -> Property(TxProperty prop)
        | Event evt -> Event(TxEventDefinition evt)

    and TxType(inp: ISimpleType) = 
        match inp with 
        | TyApp(td, args) -> TyApp(TxTypeDefinitionReference td, Array.map TxType args)
        | TyArray(n, arg) -> TyArray(n, TxType arg)
        | TyPointer(arg) -> TyPointer(TxType arg)
        | TyByRef(arg) -> TyByRef(TxType arg)

    and TxTypeDefinitionReference inp = 
        match inp with
        | ISimpleTypeDefinitionReference.OtherTyDef _ -> inp
        | ISimpleTypeDefinitionReference.SimpleTyDef x -> SimpleTyDef (TxTypeDefinition None x)

    and TxTypeDefinition transState (inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->

        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name 
            override __.Assembly = inp.Assembly |> TxAssembly 
            override __.Namespace = inp.Namespace
            override __.DeclaringType = inp.DeclaringType |> Option.map (TxTypeDefinition None)

            override __.BaseType = inp.BaseType |> Option.map TxType
            override __.Interfaces = inp.Interfaces |> Array.map TxType

            override this.Members = 
                [| yield! inp.Members |> Array.map TxMember             // transform existing methods unchanged
                   yield! resolver.MemberGenerator(transState, this) |] // add new members given by resolver

            override __.NestedTypes = inp.NestedTypes |> Array.map (TxTypeDefinition None)
            override __.GetNestedType(name, declaredOnly) = inp.GetNestedType(name, declaredOnly) |> Option.map (TxTypeDefinition None)

            override __.CustomAttributes = inp.CustomAttributes 
            override __.ApplyStaticArguments(typePathWithArguments, objs) = 
                let inpApplied = inp.ApplyStaticArguments(typePathWithArguments, objs) 
                let dataContextObjOpt = resolver.ContextCreator (objs, inpApplied)
                let inpWrapped = inpApplied |> TxTypeDefinition dataContextObjOpt
                inpWrapped

            override __.StaticParameters = inp.StaticParameters |> Array.map TxStaticParameter
          interface IWraps<'T option> with 
              member __.Value = transState
        }

    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: ISimpleNamespace) = 
        { new ISimpleNamespace with
            override __.NestedNamespaces = inp.NestedNamespaces |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName 
            override __.TypeDefinitions = 
                inp.TypeDefinitions 
                |> Array.map (fun ty -> 
                    let ctxtObjOpt = resolver.ContextCreator ([||], ty)
                    ty |> TxTypeDefinition ctxtObjOpt )
            override __.GetTypeDefinition(name) =  inp.GetTypeDefinition(name) |> Option.map (TxTypeDefinition None)
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



